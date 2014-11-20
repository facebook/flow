(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* utilities for supported filenames *)

val flow_extensions: string list

val is_flow_file: string -> bool

(* name of library directory defining builtins *)
val init: string option -> unit

val get_flowlib_root: unit -> string

(* names of library files defining builtins *)
val lib_files: string list ref

val is_lib_file: string -> bool
val lib_module: string

(* regexp for Filename constants *)
val dir_sep: Str.regexp
val current_dir_name: Str.regexp
val parent_dir_name: Str.regexp

(* given a root, make a filter for file names *)
val wanted: FlowConfig.config -> string -> bool

(* given a root, make a next_files function for MultiWorker *)
val make_next_files: Path.path -> unit -> string list

(* given a base directory and a relative path, return an absolute path *)
val normalize_path: string -> string -> string
