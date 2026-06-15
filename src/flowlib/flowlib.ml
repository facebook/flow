(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type builtin_lib =
  | Builtin_flowlib
  | Builtin_prelude
  | Builtin_tslib

let builtin_lib_of_no_flowlib ~no_flowlib =
  if no_flowlib then
    Builtin_prelude
  else
    Builtin_flowlib

let hash = function
  | Builtin_flowlib -> [%flowlib_hash]
  | Builtin_prelude -> [%prelude_hash]
  | Builtin_tslib -> [%tslib_hash]

let contents = function
  | Builtin_flowlib -> [%flowlib_contents]
  | Builtin_prelude -> [%prelude_contents]
  | Builtin_tslib -> [%tslib_contents]

let contents_list ~no_flowlib = builtin_lib_of_no_flowlib ~no_flowlib |> contents |> Array.to_list

type libdir =
  | Flowlib of File_path.t
  | Prelude of File_path.t
  | Tslib of File_path.t

(** [libdir ~builtin_lib parent_dir] returns the directory under [parent_dir]
    within which the flowlib files will be extracted. This directory is named
    uniquely based on the flowlib contents, as well as the effective user ID
    (euid) of the current process. The euid is used to ensure that the directory
    is writable by the current user. *)
let libdir ~builtin_lib parent_dir =
  let euid = Unix.geteuid () in
  let basename = Printf.sprintf "flowlib_%s_%d" (hash builtin_lib) euid in
  let path = File_path.concat parent_dir basename in
  match builtin_lib with
  | Builtin_flowlib -> Flowlib path
  | Builtin_prelude -> Prelude path
  | Builtin_tslib -> Tslib path

let path_of_libdir = function
  | Prelude path -> path
  | Flowlib path -> path
  | Tslib path -> path

let mkdir libdir =
  let path = path_of_libdir libdir |> File_path.to_string in
  let parent_dir = Filename.dirname path in
  Sys_utils.mkdir_no_fail parent_dir;
  Sys_utils.mkdir_no_fail path

let write_flowlib dir (filename, contents) =
  let file = File_path.(concat dir filename |> to_string) in
  Sys_utils.write_file ~file contents

let extract libdir =
  mkdir libdir;
  let (path, builtin_lib) =
    match libdir with
    | Prelude path -> (path, Builtin_prelude)
    | Flowlib path -> (path, Builtin_flowlib)
    | Tslib path -> (path, Builtin_tslib)
  in
  Array.iter (write_flowlib path) (contents builtin_lib)

let extract_if_missing libdir =
  let sentinel_name =
    match libdir with
    | Flowlib _ -> "core.js"
    | Prelude _ -> "prelude.js"
    | Tslib _ -> "lib.d.ts"
  in
  let libdir_path = path_of_libdir libdir |> File_path.to_string in
  let sentinel = Filename.concat libdir_path sentinel_name in
  if not (Sys.file_exists sentinel) then extract libdir

let libdir_of_files_libdir = function
  | Files.Prelude path -> Prelude path
  | Files.Flowlib path -> Flowlib path
  | Files.Tslib path -> Tslib path

let extract_if_missing_or_exit files_libdir_opt =
  match files_libdir_opt with
  | Some files_libdir ->
    let libdir = libdir_of_files_libdir files_libdir in
    (try extract_if_missing libdir with
    | e ->
      let e = Exception.wrap e in
      let err = Exception.get_ctor_string e in
      let libdir_str = libdir |> path_of_libdir |> File_path.to_string in
      let msg = Printf.sprintf "Could not extract flowlib files into %s: %s" libdir_str err in
      Exit.(exit ~msg Could_not_extract_flowlibs))
  | None -> ()
