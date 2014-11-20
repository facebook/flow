(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* dfind is a binary we call whenever we want to know if something changed   *)
(*****************************************************************************)
open Utils


(*****************************************************************************)
(* dfind doesn't always pick up the token
 * however, it seems that in some cases, despite the fact that the token was
 * not picked up, dfind is still healthy.
 * So here is our strategy:
 * 1) retry calling dfind
 * 2) every five times, retry calling dfind from scratch
 * 3) after 20 times, kill the server
 * It should make things a lot more resilient to failure, in the mean time
 * we will try to figure out what is going on.
 *)
 (*****************************************************************************)

let dfind_proc = ref None
let dfind_pid = ref None

let dfind_init root =
  let proc, pid = DfindLib.start (Path.string_of_path root) in
  PidLog.log ~reason:(Some "dfind") pid;
  dfind_proc := Some proc;
  dfind_pid := Some pid

let dfind (root:Path.path) retries =
  (match !dfind_proc with
  | None -> assert false
  | Some x ->
      DfindEnv.SSet.fold SSet.add (DfindLib.get_changes x) SSet.empty)

let dfind root = dfind root 20

let rec get_updates_ acc root =
  let diff = dfind root in
  if SSet.is_empty diff
  then acc
  else begin
    let acc = SSet.union diff acc in
    get_updates_ acc root
  end

let get_updates root =
  get_updates_ SSet.empty root
