(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

module SSet = Set.Make(String)
exception Error of string * int

type watch = string
type env = {
  fsevents : Fsevents.env;
  mutable roots : string list;
  mutable wpaths : SSet.t;
}

type event = {
  path : string; (* The full path for the file/directory that changed *)
  wpath : string; (* The watched path that triggered this event *)
}

let is_prefixed_by_dir fn dir =
  let prefix = dir ^ Filename.dir_sep in
  String.length fn > String.length prefix &&
  String.sub fn 0 (String.length prefix) = prefix

(* Returns None if we're already watching that path and Some watch otherwise *)
let add_watch env path =
  if SSet.mem path env.wpaths
  then None
  else begin
    (* FSEvents is watching the root directory. You don't actually need to tell
     * it to watch subdirectories and files too *)
    if not (List.exists ~f:(is_prefixed_by_dir path) env.roots)
    then begin
      ignore (Fsevents.add_watch env.fsevents path);
      env.roots <- path :: env.roots;
    end;

    env.wpaths <- SSet.add path env.wpaths;
    Some path
  end

let init () = {
  fsevents = Fsevents.init ();
  wpaths = SSet.empty;
  roots = [];
}

let read env =
  List.map
    (Fsevents.read_events env.fsevents)
    (fun (path, wpath) -> {path; wpath;})

module FDMap = Map.Make(
  struct type t = Unix.file_descr let compare = compare end
)
type fd_select = Unix.file_descr * (unit -> unit)
let make_callback fdmap (fd, callback) = FDMap.add fd callback fdmap
let invoke_callback fdmap fd  =
  let callback = try
    (FDMap.find fd fdmap)
  with _ -> assert false in
  callback ()

let select env ?(read_fdl=[]) ?(write_fdl=[]) ~timeout callback =
  let callback () = callback (Unix.handle_unix_error read env) in
  let read_fdl = (Fsevents.get_event_fd env.fsevents, callback) :: read_fdl in
  let read_callbacks =
    List.fold_left ~f:make_callback ~init:FDMap.empty read_fdl in
  let write_callbacks =
    List.fold_left ~f:make_callback ~init:FDMap.empty write_fdl in
  let read_ready, write_ready, _ =
    Unix.select (List.map read_fdl fst) (List.map write_fdl fst) [] timeout
  in
  List.iter write_ready (invoke_callback write_callbacks);
  List.iter read_ready (invoke_callback read_callbacks)
