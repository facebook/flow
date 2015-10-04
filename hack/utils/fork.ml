(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

(* Forking duplicates data in all buffers, so we flush them beforehand to avoid
 * writing the same thing twice *)
let fork_callbacks : (unit -> unit) list ref = ref [flush_all]

let on_fork f = fork_callbacks := f :: !fork_callbacks

(* You should always use this instead of Unix.fork, so that the callbacks get
 * invoked *)
let fork () =
  List.iter !fork_callbacks (fun f -> f());
  Unix.fork ()

(* should only be called from hh_server, which initializes the PidLog *)
let fork_and_log ?reason () =
  let result = fork() in
  (match result with
  | -1 | 0 -> ()
  | pid -> PidLog.log ?reason pid);
  result
