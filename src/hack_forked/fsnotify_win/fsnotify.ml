(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*

   Principles:

   We create a new thread for every roots directory to be watched (see
   `raw_add_watch`). This thread loops on the function
   `ReadDirectoryChangesW` and store received events in a lock-less
   shared list. Whenever a new events is inserted in the shared list,
   this thread also pushes a single character in a given pipe.

   On the OCaml side, we wait for new event by passing the pipe to
   `Unix.select`. Then we recover events by calling `raw_read_events`.

*)

open Hh_core

(* Abstract data type for notifier context. *)
type fsenv

(* Abstract data type for a watching thread. *)
type watcher_id

module SSet = Set.Make (String)

type env = {
  fsenv: fsenv;
  fd: Unix.file_descr;
  watchers: watcher_id list;
  mutable wpaths: SSet.t;
}

type event = {
  path: string;
  (* The full path for the file/directory that changed *)
  wpath: string; (* The watched path that triggered this event *)
}

(** Stubs *)

external raw_init : Unix.file_descr -> fsenv = "caml_fsnotify_init"

(* [raw_add_watch out_fd dir] creates a thread that monitor [dir] and
   push a single charactes into the pipe 'out_fd' whenever a events is
   ready to be read with [raw_read_events].

   The return value is an opaque `watcher_id`, currently it contains
   the corresponding thread id. *)
external raw_add_watch : fsenv -> string -> watcher_id = "caml_fsnotify_add_watch"

external raw_read_events : fsenv -> event list = "caml_fsnotify_read_events"

(** Init *)

let init roots =
  let (in_fd, out_fd) = Unix.pipe () in
  Unix.set_close_on_exec in_fd;
  Unix.set_close_on_exec out_fd;
  let fsenv = raw_init out_fd in
  let watchers = List.map roots ~f:(raw_add_watch fsenv) in
  { fsenv; fd = in_fd; watchers; wpaths = SSet.empty }

(** Faked add_watch, as for `fsnotify_darwin`. *)

(* Returns None if we're already watching that path and Some watch otherwise *)
let add_watch env path =
  (* The function `ReadDirectoryChangesW` used in `init` is watching
   * the whole directory. No need to register every files in it. *)
  if SSet.mem path env.wpaths then
    None
  else (
    env.wpaths <- SSet.add path env.wpaths;
    Some ()
  )

(** Select *)

module FDMap = Flow_map.Make (struct
  type t = Unix.file_descr

  let compare = compare
end)

type fd_select = Unix.file_descr * (unit -> unit)

let make_callback fdmap (fd, callback) = FDMap.add fd callback fdmap

let invoke_callback fdmap fd =
  let callback =
    try FDMap.find fd fdmap with
    | _ -> assert false
  in
  callback ()

let read_events env =
  (* read pop only one char from pipe, in order never to block. *)
  let buf = Bytes.create 1 in
  ignore (Unix.read env.fd buf 0 1 : int);

  (* prefix the root path *)
  List.map (raw_read_events env.fsenv) ~f:(fun ev ->
      { ev with path = Filename.concat ev.wpath ev.path })

let select env ?(read_fdl = []) ?(write_fdl = []) ~timeout callback =
  let callback () = callback (read_events env) in
  let read_fdl = (env.fd, callback) :: read_fdl in
  let read_callbacks = List.fold_left ~f:make_callback ~init:FDMap.empty read_fdl in
  let write_callbacks = List.fold_left ~f:make_callback ~init:FDMap.empty write_fdl in
  let (read_ready, write_ready, _) =
    Unix.select (List.map read_fdl fst) (List.map write_fdl fst) [] timeout
  in
  List.iter write_ready (invoke_callback write_callbacks);
  List.iter read_ready (invoke_callback read_callbacks)

(** Unused, for compatibility with `fsnotify_linux/fsnotify.mli` only. *)

type watch = unit

exception Error of string * Unix.error
