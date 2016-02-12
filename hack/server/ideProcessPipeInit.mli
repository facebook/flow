(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

open IdeProcessPipe

(* Create a pipe and send it's ends to processes that will use it to
 * communicate *)
val monitor_make_and_send: Unix.file_descr ->  Unix.file_descr -> unit

(* Receive typechecker's end of the pipe *)
val typechecker_recv: Unix.file_descr -> to_ide

(* Receive ide's end of the pipe *)
val ide_recv: Unix.file_descr -> to_typechecker
