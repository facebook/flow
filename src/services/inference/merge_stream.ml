(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

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
   work vs. when it can safely exit. We signal the former by returning a `None`
   bucket, and the latter by returning a `Some []` bucket.
*)
let max_bucket_size = 500 (* hard-coded, as in Bucket *)

(* For each leader, maps the number of leaders it is currently blocking on. *)
let blocking = Hashtbl.create 0
(* Counts the number of blocked leaders. *)
let blocked = ref 0

(* For each leader, maps other leaders that are dependent on it. *)
let dependents = ref FilenameMap.empty

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

let rev_append_pair (x1, y1) (x2, y2) =
  (List.rev_append x1 x2, List.rev_append y1 y2)

(* leader_map is a map from files to leaders *)
(* component_map is a map from leaders to components *)
(* dependency_graph is a map from files to dependencies *)
let make dependency_graph leader_map component_map =
  (* TODO: clear or replace state *)
  let procs = Sys_utils.nbr_procs in
  let leader f = FilenameMap.find_unsafe f leader_map in
  let component f = FilenameMap.find_unsafe f component_map in

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

  fun () ->
    let jobs = List.length !stream in
    if jobs = 0 && !blocked <> 0 then MultiWorker.Wait
    else
      let bucket_size =
        if jobs < procs * max_bucket_size
        then 1 + (jobs / procs)
        else max_bucket_size
      in
      let n = min bucket_size jobs in
      let result = take n |> List.map component in
      MultiWorker.Job result

(* We know when files are done by having jobs return the files they processed,
   and trapping the function that joins results. ;), yeah. *)
let join =
  let push (leader_fs: filename list) =
    List.iter (fun leader_f ->
      FilenameSet.iter (fun dep_leader_f ->
        let n = (Hashtbl.find blocking dep_leader_f) - 1 in
        (* dep_leader blocked on one less *)
        Hashtbl.replace blocking dep_leader_f n;
        if n = 0 then
          (* one less blocked; add dep_leader_f to stream *)
          (decr blocked; stream := dep_leader_f::!stream)
      ) (FilenameMap.find_unsafe leader_f !dependents)
    ) leader_fs
  in
  fun res acc ->
    let leader_fs, _ = res in
    push leader_fs;
    rev_append_pair res acc
