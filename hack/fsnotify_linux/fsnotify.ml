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

exception Error of string * int
let wrap f () =
  try
    f ()
  with
  | Inotify.Error (reason, err) -> raise (Error (reason, err))
  | e -> raise e

type watch = Inotify.wd

(* A watch in this case refers to an inotify watch. Inotify watches are used
 * to subscribe to events on files in linux kernel.
 * Once a watch has been added to a file, the kernel notifies us every time
 * the file changes (by sending an event to a pipe, cf env.inotify).
 * We need to be able to compare watches because there could be multiple
 * paths that lead to the same watch (because of symlinks).
*)
module WMap = Map.Make(struct type t = watch let compare = compare end)

type env = {
          fd     : Unix.file_descr;
  mutable wpaths : string WMap.t;
}

type event = {
  path : string; (* The full path for the file/directory that changed *)
  wpath : string; (* The watched path that triggered this event *)
}

let init _roots = {
  fd     = wrap (Inotify.init) ();
  wpaths = WMap.empty;
}

let select_events = Inotify.(
  [ S_Create; S_Delete; S_Delete_self;
    S_Modify; S_Move_self; S_Moved_from;
    S_Moved_to; S_Attrib;
  ])

(* Returns None if we're already watching that path and Some watch otherwise *)
let add_watch env path =
  let watch = wrap (fun () -> Inotify.add_watch env.fd path select_events) () in
  if WMap.mem watch env.wpaths && WMap.find watch env.wpaths = path
  then None
  else begin
    env.wpaths <- WMap.add watch path env.wpaths;
    Some watch
  end

let check_event_type = function
  | Inotify.Access
  | Inotify.Attrib
  | Inotify.Close_write
  | Inotify.Close_nowrite
  | Inotify.Create
  | Inotify.Delete
  | Inotify.Delete_self
  | Inotify.Move_self
  | Inotify.Moved_from
  | Inotify.Moved_to
  | Inotify.Open
  | Inotify.Ignored
  | Inotify.Modify
  | Inotify.Isdir -> ()
  | Inotify.Q_overflow ->
      Printf.printf "INOTIFY OVERFLOW!!!\n";
      exit 5
  | Inotify.Unmount ->
      Printf.printf "UNMOUNT EVENT!!!\n";
      exit 5

let process_event env events event =
  match event with
  | _, _, _, None -> events
  | watch, type_list, _, Some filename ->
      List.iter type_list check_event_type;
      let wpath = try WMap.find watch env.wpaths with _ -> assert false in
      let path = Filename.concat wpath filename in
      { path; wpath; }::events

let read env =
  let inotify_events = wrap (fun () -> Inotify.read env.fd) () in
  List.fold_left inotify_events ~f:(process_event env) ~init:[]

module FDMap = Map.Make(
  struct type t = Unix.file_descr let compare = compare end
)
type fd_select = Unix.file_descr * (unit -> unit)
let make_callback fdmap (fd, callback) = FDMap.add fd callback fdmap
let invoke_callback fdmap fd  = (FDMap.find fd fdmap) ()
let select env ?(read_fdl=[]) ?(write_fdl=[]) ~timeout callback =
  let callback () = callback (Unix.handle_unix_error read env) in
  let read_fdl = (env.fd, callback) :: read_fdl in
  let read_callbacks =
    List.fold_left read_fdl ~f:make_callback ~init:FDMap.empty in
  let write_callbacks =
    List.fold_left write_fdl ~f:make_callback ~init:FDMap.empty in
  let read_ready, write_ready, _ =
    Unix.select (List.map read_fdl fst) (List.map write_fdl fst) [] timeout
  in
  List.iter write_ready (invoke_callback write_callbacks);
  List.iter read_ready (invoke_callback read_callbacks)

(********** DEBUGGING ****************)
(* Can be useful to see what the event actually is, for debugging *)
let _string_of inotify_ev =
  let wd, mask, cookie, s = inotify_ev in
  let mask = String.concat ":" (List.map mask Inotify.string_of_event) in
  let s = match s with
  | Some s -> s
  | None -> "\"\"" in
  Printf.sprintf "wd [%u] mask[%s] cookie[%ld] %s" (Inotify.int_of_wd wd) mask cookie s
