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
(* Code relative to the client/server communication *)
(*****************************************************************************)

(* The kind of message sent to the server *)
(* dfind my_dir my_handle => Find_handle (my_dir, my_handle) *)
type message =
(* Find all the files younger than the handle in the directory, 
 * incrementally
 *)
  | Find_handle_follow of DfindEnv.dir * DfindEnv.handle

(* Find all the files younger than the handle in the directory *)
  | Find_handle   of DfindEnv.dir * DfindEnv.handle

(* Check that the current state is consistent *)
  | Check

(* Kill the server*)
  | Kill   
  | Ping

(* We only allow one dfind server by machine *)
val is_running: unit -> bool

(* Wait for the server to be ready (when we just forked it) *)
val wait_for_server: Unix.file_descr -> unit

val fork_in_pipe: string -> Unix.file_descr * Unix.file_descr * int

(* code used client side *)
val client_socket: unit -> in_channel * out_channel
