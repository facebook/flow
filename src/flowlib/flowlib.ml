(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let hash no_flowlib : string =
  if no_flowlib then
    [%prelude_hash]
  else
    [%flowlib_hash]

let contents no_flowlib : (string * string) array =
  if no_flowlib then
    [%prelude_contents]
  else
    [%flowlib_contents]

type libdir =
  | Flowlib of Path.t
  | Prelude of Path.t

(** [libdir ~no_flowlib parent_dir] returns the directory under [parent_dir]
    within which the flowlib files will be extracted. This directory is
    named uniquely based on the flowlib contents, as well as the effective
    user ID (euid) of the current process. The euid is used to ensure that
    the directory is writable by the current user. *)
let libdir ~no_flowlib parent_dir =
  let euid = Unix.geteuid () in
  let basename = Printf.sprintf "flowlib_%s_%d" (hash no_flowlib) euid in
  let path = Path.concat parent_dir basename in
  if no_flowlib then
    Prelude path
  else
    Flowlib path

let path_of_libdir = function
  | Prelude path -> path
  | Flowlib path -> path

let mkdir libdir =
  let path = path_of_libdir libdir |> Path.to_string in
  let parent_dir = Filename.dirname path in
  Sys_utils.mkdir_no_fail parent_dir;
  Sys_utils.mkdir_no_fail path

let write_flowlib dir (filename, contents) =
  let file = Path.(concat dir filename |> to_string) in
  Sys_utils.write_file ~file contents

let extract libdir =
  mkdir libdir;
  let (path, no_flowlib) =
    match libdir with
    | Prelude path -> (path, true)
    | Flowlib path -> (path, false)
  in
  Array.iter (write_flowlib path) (contents no_flowlib)
