(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

exception Error of string * int

type event = {
  path : string; (* The full path for the file/directory that changed *)
  wpath : string; (* The watched path that triggered this event *)
}

type fd_select = Unix.file_descr * (unit -> unit)

type watch

type env

let init roots = assert false

let add_watch env path = assert false

let select env ?(read_fdl=[]) ?(write_fdl=[]) ~timeout callback = assert false
