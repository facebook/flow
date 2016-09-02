(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* utilities for supported filenames *)

val global_file_name: string
val flow_ext: string

val is_json_file: string -> bool
val is_flow_file: options: Options.t -> string -> bool

(* true if a file path matches an [ignore] entry in config *)
val is_ignored: Options.t -> string -> bool
(* true if a file path matches an [include] path in config *)
val is_included: Options.t -> string -> bool

val init: Options.t -> string list * SSet.t

val lib_module: string

(* regexp for Filename constants *)
val dir_sep: Str.regexp
val current_dir_name: Str.regexp
val parent_dir_name: Str.regexp
val absolute_path: Str.regexp

(* given a root, make a filter for file names *)
val wanted:
  options: Options.t ->
  SSet.t ->
  string -> bool

(* given a root, make a next_files function for MultiWorker *)
val make_next_files:
  subdir: Path.t option ->
  options: Options.t ->
  libs: SSet.t ->
  unit -> string list

val get_all: (unit -> string list) -> SSet.t

(* given a base directory and a relative path, return an absolute path *)
val normalize_path: string -> string -> string

(* given a base directory and a relative path, return an absolute path *)
val construct_path: string -> string list -> string

val relative_path: string -> string -> string

(* TODO: this doesn't match the signatures of the related functions above *)
val make_path_absolute: Path.t -> string -> Path.t

val is_prefix: string -> string -> bool

val get_flowtyped_path: Path.t -> Path.t

val filename_from_string: options: Options.t -> string -> Loc.filename

val mkdirp: string -> Unix.file_perm -> unit
