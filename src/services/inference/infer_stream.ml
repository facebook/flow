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
   inferring files.

   Inferring files should be embarassingly parallel. However, currently the AST
   walk in Type_inference_js.infer_ast not only emits type constraints but also
   does the (much more mundane) job of collecting requires. The latter could be
   split out, but until that happens, we need a streaming version of inferring
   files to implement focused checks: one that discovers dependencies of a given
   file and infers only those dependencies (recursively).
*)

let max_bucket_size = 500

(* Set of files already discovered. Need to remember this set to not keep
   visiting files in cycles. *)
let seen = ref FilenameSet.empty

(* Number of files currently waiting, either because they have not been
   scheduled, or because they have been scheduled but the worker doing the work
   is not done yet. Should be no greater than the size of `seen`. *)
let blocked = ref 0

(* Stream of files available to schedule. Should be a subset of `seen`. Also,
   `blocked` should be at least the length of `stream`. *)
let stream = ref ([]: filename list)

(* take n files from stream *)
let rec take n =
  if n = 0 then []
  else match !stream with
  | [] -> assert false
  | x::rest ->
      stream := rest;
      x::(take (n-1))

let push deps =
  let new_deps = FilenameSet.diff deps !seen in
  seen := FilenameSet.union new_deps !seen;
  blocked := (FilenameSet.cardinal new_deps) + !blocked;
  stream := List.rev_append (FilenameSet.elements new_deps) !stream

let make f =
  push (FilenameSet.singleton f);
  let procs = Sys_utils.nbr_procs in
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
      let result = take n in
      if result <> [] then
        MultiWorker.Job result
      else
        MultiWorker.Done

let pop nfiles =
  blocked := !blocked - nfiles

let join =
  fun streaming_infer_results acc ->
    pop (List.length streaming_infer_results);
    push (List.fold_left (fun acc (_, _, _, deps) ->
      FilenameSet.union deps acc
    ) FilenameSet.empty streaming_infer_results);
    List.rev_append streaming_infer_results acc
