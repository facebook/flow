(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Custom bucketing scheme for dynamically growing and shrinking workloads when
   merging files.

   We start out with files that have no dependencies: these files are available
   for scheduling merge jobs. All other files are "blocked", i.e., they are *not
   ready* for scheduling.

   NOTE: Scheduling merge jobs too early will cause crashes, since they will
   need stuff that has not been computed yet! A more sophisticated scheme may be
   designed to be tolerant to such failures, but the merge process is
   complicated enough as is. Also, performance-wise blocking does not seem to be
   an issue because files get unblocked pretty regularly (see below).

   Each blocked file maintains a counter on the number of files blocking
   them. As files are done, they decrement the counters of other files blocked
   on them. As soon as some of the counters go to zero, the corresponding files
   are made available for scheduling.

   Finally, we maintain a counter on the total number of blocked files. When
   that goes to zero, we prepare to exit!

   The underlying worker management scheme needs to know when to wait for more
   work vs. when it can safely exit. We signal the former by returning a `Wait`
   bucket, and the latter by returning a `Done` bucket.
*)

open Utils_js

type element = Component of File_key.t Nel.t

type 'a merge_result = (File_key.t * 'a) list

type node = {
  component: File_key.t Nel.t;
  mutable dependents: node FilenameMap.t;
  (* the number of leaders this node is currently blocking on *)
  mutable blocking: int;
  mutable recheck: bool;
  size: int;
}

type 'a t = {
  graph: node FilenameMap.t;
  ready: node Queue.t;
  num_workers: int;
  total_components: int;
  total_files: int;
  mutable ready_components: int;
  mutable ready_files: int;
  mutable blocked_components: int;
  mutable blocked_files: int;
  mutable merged_components: int;
  mutable merged_files: int;
  mutable skipped_components: int;
  mutable skipped_files: int;
  mutable new_or_changed_files: FilenameSet.t;
  intermediate_result_callback: 'a merge_result Lazy.t -> unit;
}

let add_ready node stream =
  assert (node.blocking = 0);
  Queue.add node stream.ready;
  stream.ready_components <- stream.ready_components + 1;
  stream.ready_files <- stream.ready_files + node.size;
  ()

let pop_ready stream =
  let node = Queue.pop stream.ready in
  stream.ready_components <- stream.ready_components - 1;
  stream.ready_files <- stream.ready_files - node.size;
  node

(* hard-coded, as in Bucket *)
let max_bucket_size = 500

let bucket_size stream =
  (* NB: num_workers can be zero *)
  let max_bucket_size =
    if stream.ready_files < stream.num_workers * max_bucket_size then
      1 + (stream.ready_files / stream.num_workers)
    else
      max_bucket_size
  in
  min max_bucket_size stream.ready_files

let is_done stream = stream.blocked_components = 0

let create
    ~num_workers
    ~sig_dependency_graph
    ~leader_map
    ~component_map
    ~recheck_leader_set
    ~intermediate_result_callback =
  (* create node for each component *)
  let graph =
    FilenameMap.mapi
      (fun leader component ->
        {
          component;
          (* computed later *)
          dependents = FilenameMap.empty;
          (* computed later *)
          blocking = 0;
          recheck = FilenameSet.mem leader recheck_leader_set;
          size = Nel.length component;
        })
      component_map
  in
  let (total_components, total_files) =
    FilenameMap.fold (fun _ node (c, f) -> (c + 1, f + node.size)) graph (0, 0)
  in
  (* calculate dependents, blocking for each node *)
  let () =
    let leader f = FilenameMap.find f leader_map in
    FilenameMap.iter
      (fun f dep_fs ->
        let leader_f = leader f in
        let node = FilenameMap.find leader_f graph in
        FilenameSet.iter
          (fun dep_f ->
            let dep_leader_f = leader dep_f in
            if dep_leader_f = leader_f then
              ()
            else
              let dep_node = FilenameMap.find dep_leader_f graph in
              let dependents = FilenameMap.add leader_f node dep_node.dependents in
              if dependents != dep_node.dependents then (
                dep_node.dependents <- dependents;
                node.blocking <- node.blocking + 1
              ))
          dep_fs)
      sig_dependency_graph
  in
  let stream =
    {
      graph;
      ready = Queue.create ();
      num_workers;
      total_components;
      total_files;
      ready_components = 0;
      ready_files = 0;
      blocked_components = 0;
      blocked_files = 0;
      merged_components = 0;
      merged_files = 0;
      skipped_components = 0;
      skipped_files = 0;
      new_or_changed_files = FilenameSet.empty;
      intermediate_result_callback;
    }
  in
  (* calculate the components ready to schedule and blocked counts *)
  FilenameMap.iter
    (fun _ node ->
      if node.blocking = 0 then
        add_ready node stream
      else (
        stream.blocked_components <- stream.blocked_components + 1;
        stream.blocked_files <- stream.blocked_files + node.size
      ))
    graph;

  stream

let update_server_status stream =
  let status =
    ServerStatus.(
      Merging_types_progress { finished = stream.merged_files; total = Some stream.total_files })
  in
  MonitorRPC.status_update status

let next stream =
  let rec take acc n =
    if n <= 0 then
      acc
    else
      let node = pop_ready stream in
      take (Component node.component :: acc) (n - node.size)
  in
  fun () ->
    let n = bucket_size stream in
    match take [] n with
    | [] ->
      if is_done stream then
        Bucket.Done
      else
        Bucket.Wait
    | components -> Bucket.Job components

let merge ~master_mutator ~reader stream =
  (* If a component is unchanged, either because we merged it and the sig hash
   * was unchanged or because the component was skipped entirely, we need to
   * revive the shared heap entires corresponding to the component. These heap
   * entries were oldified before merge began. *)
  let revive node =
    node.component
    |> Nel.to_list
    |> FilenameSet.of_list
    |> Context_heaps.Merge_context_mutator.revive_files master_mutator
  in
  let mark_new_or_changed node =
    stream.new_or_changed_files <-
      node.component
      |> Nel.fold_left (fun acc x -> FilenameSet.add x acc) stream.new_or_changed_files
  in
  (* Record that a component was merged (or skipped) and recursively unblock its
   * dependents. If a dependent has no more unmerged dependencies, make it
   * available for scheduling. *)
  let rec push diff node =
    stream.merged_components <- stream.merged_components + 1;
    stream.merged_files <- stream.merged_files + node.size;
    if diff then
      mark_new_or_changed node
    else
      revive node;
    FilenameMap.iter (fun _ node -> unblock diff node) node.dependents
  and unblock diff node =
    (* dependent blocked on one less *)
    node.blocking <- node.blocking - 1;

    (* dependent should be rechecked if diff *)
    node.recheck <- diff || node.recheck;

    (* no more waiting, yay! *)
    if node.blocking = 0 then (
      stream.blocked_components <- stream.blocked_components - 1;
      stream.blocked_files <- stream.blocked_files - node.size;
      if node.recheck then
        add_ready node stream
      else
        skip node
    )
  and skip node =
    stream.skipped_components <- stream.skipped_components + 1;
    stream.skipped_files <- stream.skipped_files + node.size;
    push false node
  in
  fun merged acc ->
    stream.intermediate_result_callback (lazy merged);
    List.iter
      (fun (leader_f, _) ->
        let node = FilenameMap.find leader_f stream.graph in
        let diff = Context_heaps.Mutator_reader.sig_hash_changed ~reader leader_f in
        push diff node)
      merged;
    update_server_status stream;
    List.rev_append merged acc

(* NOTE: call these functions only at the end of merge, not during. *)
let total_files stream = stream.total_files

let skipped_count stream = stream.skipped_files

let sig_new_or_changed stream = stream.new_or_changed_files
