(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

exception Error of string * Unix.error

(* Contains all the fsevents context *)
type env

(* A subscription to events for a directory *)
type watch

type event = {
  path : string; (* The full path for the file/directory that changed *)
  wpath : string; (* The watched path that triggered this event *)
}

val init : string list -> env

(* Returns None if we're already watching that path and Some watch otherwise *)
val add_watch : env -> string -> watch option

(* A file descriptor and what to do when it is selected *)
type fd_select = Unix.file_descr * (unit -> unit)
val select :
  (* The fsevents context *)
  env ->
  (* Additional file descriptor to select for reading *)
  ?read_fdl:(fd_select list) ->
  (* Additional file descriptor to select for writing *)
  ?write_fdl:(fd_select list) ->
  (* Timeout...like Unix.select *)
  timeout:float ->
  (* The callback for file system events *)
  (event list -> unit) ->
  unit
