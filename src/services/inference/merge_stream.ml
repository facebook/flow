(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

module Stream: sig
  type 'a t
  val empty: 'a t
  val push: 'a -> 'a t -> 'a t
  val pop_unsafe: 'a t -> ('a * 'a t)
  val length: 'a t -> int
end = struct
  type 'a t = int * 'a list
  let empty = (0, [])
  let push x (n, xs) = (n+1, x::xs)
  let pop_unsafe xs =
    match xs with
    | (_, []) -> assert_false "pop_unsafe"
    | (n, x::xs) -> x, (n-1, xs)
  let length (n, _) = n
end

type element = Component of File_key.t Nel.t

type 'a merge_result = (File_key.t * 'a) list

type 'a merge_stream = {
  next: unit -> element list Bucket.bucket;
  merge:
    master_mutator: Context_heaps.Merge_context_mutator.master_mutator ->
    (* merged *)
    'a merge_result ->
    (* accumulator *)
    'a merge_result ->
    (* accumulated results *)
    'a merge_result
}

let make
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

  (* For each leader, maps the number of leaders it is currently blocking on. *)
  let blocking = Hashtbl.create 0 in
  (* Counts the number of blocked leaders. *)
  let blocked = ref 0 in

  let total_number_of_files = ref (FilenameMap.fold (fun _ files acc ->
    Nel.length files + acc
  ) component_map 0) in
  let files_merged_so_far = ref 0 in

  (* stream of files available to schedule *)
  let stream = ref Stream.empty in

  (* For each leader, maps other leaders that are dependent on it. *)
  let dependents =
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
        stream := Stream.push leader_f !stream
      else (* one more blocked *)
        incr blocked
    ) dependency_dag;

    (* TODO: remember reverse dependencies to quickly calculate remerge sets *)
    ref (Sort_js.reverse dependency_dag)
  in

  (* For each leader, maps the files in its component *)
  let components = ref component_map in

  (* For each leader, specifies whether to recheck its component *)
  let to_recheck: bool FilenameMap.t ref = ref recheck_leader_map in

  (* Take n files from stream. We take an entire component at once, which might
     cause us to take more than n files. *)
  let take =
    let rec loop acc len n =
      if n <= 0 then (acc, len)
      else begin
        let (f, stream') = Stream.pop_unsafe !stream in
        stream := stream';
        let fs = FilenameMap.find_unsafe f !components in
        let fs_len = Nel.length fs in
        loop ((Component fs)::acc) (fs_len+len) (n-fs_len)
      end
    in
    loop [] 0
  in

  (* leader_map is a map from files to leaders *)
  (* component_map is a map from leaders to components *)
  (* dependency_graph is a map from files to dependencies *)
  let next =
    let procs = Sys_utils.nbr_procs in
    fun () ->
      let jobs = Stream.length !stream in
      if jobs = 0 && !blocked <> 0 then Bucket.Wait
      else
        let bucket_size =
          if jobs < procs * max_bucket_size
          then 1 + (jobs / procs)
          else max_bucket_size
        in
        let n = min bucket_size jobs in
        let components, num_files = take n in
        if components <> [] then begin
          MonitorRPC.status_update ServerStatus.(Merging_progress {
            finished = !files_merged_so_far;
            total = Some !total_number_of_files;
          });
          files_merged_so_far := !files_merged_so_far + num_files;
          Bucket.Job components
        end else
          Bucket.Done
  in

  (* We know when files are done by having jobs return the files they processed,
     and trapping the function that joins results. ;), yeah. *)
  let merge =
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
            stream := Stream.push dep_leader_f !stream;
            skipped
          ) else push (dep_leader_f::skipped) dep_leader_f false
        ) else skipped
      ) (FilenameMap.find_unsafe leader_f !dependents) skipped
    in
    fun ~master_mutator merged merged_acc ->
      let () = intermediate_result_callback (lazy merged) in
      let skipped = List.fold_left (fun skipped (leader_f, _) ->
        let diff = Context_heaps.sig_hash_changed leader_f in
        let () =
          let fs =
            FilenameMap.find_unsafe leader_f !components
            |> Nel.to_list
            |> FilenameSet.of_list
          in
          if not diff
          then Context_heaps.Merge_context_mutator.revive_files master_mutator fs
        in
        push skipped leader_f diff
      ) [] merged in
      let skipped_length = List.fold_left (fun acc leader_f ->
        let fs =
          FilenameMap.find_unsafe leader_f !components
          |> Nel.to_list
          |> FilenameSet.of_list
        in
        Context_heaps.Merge_context_mutator.revive_files master_mutator fs;
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
  in

  { next; merge }
