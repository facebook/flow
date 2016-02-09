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

val is_json_file: string -> bool
val is_flow_file: options: Options.options -> string -> bool

val init: Options.options -> string list * SSet.t

val lib_module: string

(* regexp for Filename constants *)
val dir_sep: Str.regexp
val current_dir_name: Str.regexp
val parent_dir_name: Str.regexp
val absolute_path: Str.regexp

(* given a root, make a filter for file names *)
val wanted:
  FlowConfig.config ->
  SSet.t ->
  string -> bool

(* given a root, make a next_files function for MultiWorker *)
val make_next_files:
  options: Options.options ->
  libs: SSet.t ->
  unit -> string list

(* given a base directory and a relative path, return an absolute path *)
val normalize_path: string -> string -> string

(* given a base directory and a relative path, return an absolute path *)
val construct_path: string -> string list -> string

val relative_path: string -> string -> string

val is_prefix: string -> string -> bool
