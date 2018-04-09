(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type 'a merge_result = (File_key.t * 'a) list

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
let max_bucket_size = 500 (* hard-coded, as in Bucket *)

(* For each leader, maps the number of leaders it is currently blocking on. *)
let blocking = Hashtbl.create 0
(* Counts the number of blocked leaders. *)
let blocked = ref 0

let total_number_of_files = ref 0
let files_merged_so_far = ref 0

(* For each leader, maps other leaders that are dependent on it. *)
let dependents = ref FilenameMap.empty

(* For each leader, maps the files in its component *)
let components = ref FilenameMap.empty

(* For each leader, specifies whether to recheck its component *)
let to_recheck: bool FilenameMap.t ref = ref FilenameMap.empty

(* stream of files available to schedule *)
let stream = ref []

(* take n files from stream *)
let rec take n =
  if n = 0 then []
  else match !stream with
  | [] -> assert false
  | x::rest ->
      stream := rest;
      x::(take (n-1))

type element = Component of File_key.t Nel.t

let component f = Component (FilenameMap.find_unsafe f !components)

(* leader_map is a map from files to leaders *)
(* component_map is a map from leaders to components *)
(* dependency_graph is a map from files to dependencies *)
let make dependency_graph leader_map component_map recheck_leader_map =
  components := component_map;
  to_recheck := recheck_leader_map;

  total_number_of_files := FilenameMap.fold (fun _ files acc ->
    Nel.length files + acc
  ) component_map 0;
  files_merged_so_far := 0;

  let leader f = FilenameMap.find_unsafe f leader_map in
  let dependency_dag = FilenameMap.fold (fun f fs dependency_dag ->
    let leader_f = leader f in
    let dep_leader_fs = match FilenameMap.get leader_f dependency_dag with
      | Some dep_leader_fs -> dep_leader_fs
      | _ -> FilenameSet.empty
    in
    let dep_leader_fs = FilenameSet.fold (fun f dep_leader_fs ->
      let f = leader f in
      if f = leader_f then dep_leader_fs
      else FilenameSet.add f dep_leader_fs
    ) fs dep_leader_fs in
    FilenameMap.add leader_f dep_leader_fs dependency_dag
  ) dependency_graph FilenameMap.empty in

  FilenameMap.iter (fun leader_f dep_leader_fs ->
    let n = FilenameSet.cardinal dep_leader_fs in
    (* n files block leader_f *)
    Hashtbl.add blocking leader_f n;
    if n = 0
    then (* leader_f isn't blocked, add to stream *)
      stream := leader_f::!stream
    else (* one more blocked *)
      incr blocked
  ) dependency_dag;

  (* TODO: remember reverse dependencies to quickly calculate remerge sets *)
  dependents := Sort_js.reverse dependency_dag;

  let procs = Sys_utils.nbr_procs in
  fun () ->
    let jobs = List.length !stream in
    if jobs = 0 && !blocked <> 0 then Bucket.Wait
    else
      let bucket_size =
        if jobs < procs * max_bucket_size
        then 1 + (jobs / procs)
        else max_bucket_size
      in
      let n = min bucket_size jobs in
      let result = take n |> List.map component in
      if result <> [] then begin
        let length = List.fold_left (fun acc (Component files) ->
          Nel.length files + acc
        ) 0 result in
        MonitorRPC.status_update ServerStatus.(Merging_progress {
          finished = !files_merged_so_far;
          total = Some !total_number_of_files;
        });
        files_merged_so_far := !files_merged_so_far + length;
        Bucket.Job result
      end else
        Bucket.Done

(* We know when files are done by having jobs return the files they processed,
   and trapping the function that joins results. ;), yeah. *)
let join result_callback =
  (* Once a component is merged, unblock dependent components to make them
   * available to workers. Accumulate list of skipped components. *)
  let rec push skipped leader_f diff =
    FilenameSet.fold (fun dep_leader_f skipped ->
      let n = (Hashtbl.find blocking dep_leader_f) - 1 in
      (* dep_leader blocked on one less *)
      Hashtbl.replace blocking dep_leader_f n;
      (* dep_leader should be rechecked if diff *)
      let recheck = diff || FilenameMap.find_unsafe dep_leader_f !to_recheck in
      to_recheck := FilenameMap.add dep_leader_f recheck !to_recheck;
      (* no more waiting, yay! *)
      if n = 0 then (
        (* one less blocked; add dep_leader_f to stream if we need to recheck,
           otherwise recursively unblock dependents *)
        decr blocked;
        if recheck
        then (
          stream := dep_leader_f::!stream;
          skipped
        ) else push (dep_leader_f::skipped) dep_leader_f false
      ) else skipped
    ) (FilenameMap.find_unsafe leader_f !dependents) skipped
  in
  fun merged merged_acc ->
    let () = result_callback (lazy merged) in
    let skipped = List.fold_left (fun skipped (leader_f, _) ->
      let diff = Context_cache.sig_hash_changed leader_f in
      let () =
        let fs =
          FilenameMap.find_unsafe leader_f !components
          |> Nel.to_list
          |> FilenameSet.of_list
        in
        if diff
        then Context_cache.remove_old_merge_batch fs
        else Context_cache.revive_merge_batch fs
      in
      push skipped leader_f diff
    ) [] merged in
    let skipped_length = List.fold_left (fun acc leader_f ->
      let fs =
        FilenameMap.find_unsafe leader_f !components
        |> Nel.to_list
        |> FilenameSet.of_list
      in
      Context_cache.revive_merge_batch fs;
      FilenameSet.cardinal fs + acc
    ) 0 skipped in
    if skipped_length > 0 then begin
      files_merged_so_far := !files_merged_so_far + skipped_length;
      MonitorRPC.status_update ServerStatus.(Merging_progress {
        finished = !files_merged_so_far;
        total = Some !total_number_of_files;
      })
    end;
    List.rev_append merged merged_acc
