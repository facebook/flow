(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

module MergeStats : sig
  type t
  val make: unit -> t
  val get_total_files: t -> int
  val increment_total_files: t -> int -> unit
  val get_skipped_files: t -> int
  val increment_skipped_files: t -> int -> unit
end = struct
  type t = int ref (* total files *) * int ref (* skipped files *)
  let make () = (ref 0, ref 0)
  let get_total_files (total, _) = !total
  let increment_total_files (total, _) x =
    total := !total + x
  let get_skipped_files (_, skipped) = !skipped
  let increment_skipped_files (_, skipped) x =
    skipped := !skipped + x
end

type merge_stats = MergeStats.t
let get_total_files = MergeStats.get_total_files
let get_skipped_files = MergeStats.get_skipped_files

type element = Component of File_key.t Nel.t

type 'a merge_result = (File_key.t * 'a) list

type 'a merge_stream = {
  next: unit -> element list Bucket.bucket;
  merge:
    master_mutator: Context_heaps.Merge_context_mutator.master_mutator ->
    reader: Mutator_state_reader.t ->
    (* merged *)
    'a merge_result ->
    (* accumulator *)
    'a merge_result ->
    (* accumulated results *)
    'a merge_result;
  stats: merge_stats
}

type node = {
  component: File_key.t Nel.t;
  mutable dependents: node FilenameMap.t;
  (* the number of leaders this node is currently blocking on *)
  mutable blocking: int;
  mutable recheck: bool;
  size: int;
}

let make
  ~num_workers
  ~dependency_graph
  ~leader_map
  ~component_map
  ~recheck_leader_map
  ~intermediate_result_callback =

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
     bucket, and the latter by returning a `Job []` bucket.
  *)
  let max_bucket_size = 500 in (* hard-coded, as in Bucket *)

  let total_number_of_files = FilenameMap.fold (fun _ files acc ->
    Nel.length files + acc
  ) component_map 0 in
  let stats = MergeStats.make () in
  let record_merged x =
    MergeStats.increment_total_files stats x
  in
  let record_skipped x =
    record_merged x;
    MergeStats.increment_skipped_files stats x
  in

  let graph = FilenameMap.mapi (fun leader component -> {
    component;
    dependents = FilenameMap.empty; (* computed later *)
    blocking = 0; (* computed later *)
    recheck = FilenameMap.find_unsafe leader recheck_leader_map;
    size = Nel.length component;
  }) component_map in

  (* calculate dependents, blocking for each node *)
  let () =
    let leader f = FilenameMap.find_unsafe f leader_map in
    FilenameMap.iter (fun f dep_fs ->
      let leader_f = leader f in
      let node = FilenameMap.find_unsafe leader_f graph in
      FilenameSet.iter (fun dep_f ->
        let dep_leader_f = leader dep_f in
        if dep_leader_f = leader_f then () else
        let dep_node = FilenameMap.find_unsafe dep_leader_f graph in
        let dependents = FilenameMap.add leader_f node dep_node.dependents in
        if dependents != dep_node.dependents then begin
          dep_node.dependents <- dependents;
          node.blocking <- node.blocking + 1;
        end
      ) dep_fs
    ) dependency_graph
  in

  (* calculate the components available to schedule and number blocked *)
  let stream = Queue.create () in
  let blocked = ref 0 in
  FilenameMap.iter (fun _ node ->
    if node.blocking > 0
    then incr blocked
    else Queue.add node stream
  ) graph;

  (* Take n files from stream. We take an entire component at once, which might
     cause us to take more than n files. *)
  let take =
    let rec loop acc len n =
      if n <= 0 then (acc, len)
      else begin
        let node = Queue.pop stream in
        loop ((Component node.component)::acc) (node.size+len) (n-node.size)
      end
    in
    loop [] 0
  in

  (* leader_map is a map from files to leaders *)
  (* component_map is a map from leaders to components *)
  (* dependency_graph is a map from files to dependencies *)
  let next =
    fun () ->
      let jobs = Queue.length stream in
      if jobs = 0 && !blocked <> 0 then Bucket.Wait
      else
        (* NB: num_workers can be zero *)
        let bucket_size =
          if jobs < num_workers * max_bucket_size
          then 1 + (jobs / num_workers)
          else max_bucket_size
        in
        let n = min bucket_size jobs in
        let components, num_files = take n in
        if components <> [] then begin
          MonitorRPC.status_update ServerStatus.(Merging_progress {
            finished = MergeStats.get_total_files stats;
            total = Some total_number_of_files;
          });
          record_merged num_files;
          Bucket.Job components
        end else
          Bucket.Done
  in

  (* We know when files are done by having jobs return the files they processed,
     and trapping the function that joins results. ;), yeah. *)
  let merge =
    (* Once a component is merged, unblock dependent components to make them
     * available to workers. Accumulate list of skipped components. *)
    let rec push skipped node diff =
      FilenameMap.fold (fun _ dep_node skipped ->
        let n = dep_node.blocking - 1 in
        (* dependent blocked on one less *)
        dep_node.blocking <- n;
        (* dependent should be rechecked if diff *)
        let recheck = diff || dep_node.recheck in
        dep_node.recheck <- recheck;
        (* no more waiting, yay! *)
        if n = 0 then (
          (* one less blocked; add dep node to stream if we need to recheck,
             otherwise recursively unblock dependents *)
          decr blocked;
          if recheck
          then (
            Queue.add dep_node stream;
            skipped
          ) else push (dep_node::skipped) dep_node false
        ) else skipped
      ) node.dependents skipped
    in
    fun ~master_mutator ~reader merged merged_acc ->
      let () = intermediate_result_callback (lazy merged) in
      let skipped = List.fold_left (fun skipped (leader_f, _) ->
        let node = FilenameMap.find_unsafe leader_f graph in
        let diff = Context_heaps.Mutator_reader.sig_hash_changed ~reader leader_f in
        let () =
          if not diff
          then
            FilenameMap.find_unsafe leader_f component_map
            |> Nel.to_list
            |> FilenameSet.of_list
            |> Context_heaps.Merge_context_mutator.revive_files master_mutator
        in
        push skipped node diff
      ) [] merged in
      let skipped_length = List.fold_left (fun acc node ->
        node.component
        |> Nel.to_list
        |> FilenameSet.of_list
        |> Context_heaps.Merge_context_mutator.revive_files master_mutator;
        node.size + acc
      ) 0 skipped in
      if skipped_length > 0 then begin
        record_skipped skipped_length;
        MonitorRPC.status_update ServerStatus.(Merging_progress {
          finished = MergeStats.get_total_files stats;
          total = Some total_number_of_files;
        })
      end;
      List.rev_append merged merged_acc
  in

  { next; merge; stats }
