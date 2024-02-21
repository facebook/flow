(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* utilities for supported filenames *)

type lib_dir =
  | Prelude of File_path.t
  | Flowlib of File_path.t

type options = {
  default_lib_dir: lib_dir option;
  ignores: (string * Str.regexp) list;
  untyped: (string * Str.regexp) list;
  declarations: (string * Str.regexp) list;
  includes: Path_matcher.t;
  lib_paths: File_path.t list;
  module_file_exts: string list;
  module_resource_exts: SSet.t;
  multi_platform: bool;
  multi_platform_extensions: string list;
  multi_platform_ambient_supports_platform_directory_overrides: (string * string list) list;
  node_resolver_dirnames: string list;
}

val default_options : options

val default_lib_dir : options -> lib_dir option

val ignores : options -> (string * Str.regexp) list

val untyped : options -> (string * Str.regexp) list

val declarations : options -> (string * Str.regexp) list

val includes : options -> Path_matcher.t

val lib_paths : options -> File_path.t list

val module_file_exts : options -> string list

val module_resource_exts : options -> SSet.t

val node_resolver_dirnames : options -> string list

val node_modules_containers : SSet.t SMap.t ref

val global_file_name : string

val flow_ext : string

val has_flow_ext : File_key.t -> bool

val chop_flow_ext : File_key.t -> File_key.t

val eponymous_module : File_key.t -> Modulename.t

val relative_interface_mref_of_possibly_platform_specific_file :
  options:options -> File_key.t -> string option

val platform_specific_extension_and_index_opt : options:options -> string -> (int * string) option

val platform_specific_extension_opt : options:options -> string -> string option

val chop_platform_suffix : options:options -> File_key.t -> File_key.t

val is_json_file : string -> bool

val is_flow_file : options:options -> string -> bool

(* true if a file path matches an [ignore] entry in config *)
val is_ignored : options -> string -> bool

(* true if a file path matches an [untyped] entry in config *)
val is_untyped : options -> string -> bool

(* true if a file path matches a [declarations] entry in config *)
val is_declaration : options -> string -> bool

(* true if a file path matches an [include] path in config *)
val is_included : options -> string -> bool

val is_valid_path : options:options -> string -> bool

val is_in_flowlib : options -> string -> bool

val get_all_watched_extensions : options -> SSet.t

val init : ?flowlibs_only:bool -> options -> string list * SSet.t

(* regexp for Filename constants *)
val dir_sep : Str.regexp

val current_dir_name : Str.regexp

val parent_dir_name : Str.regexp

val absolute_path_regexp : Str.regexp

val watched_paths : options -> File_path.t list

(* given a root, make a filter for file names *)
val wanted : options:options -> include_libdef:bool -> SSet.t -> string -> bool

(* given a root, make a next_files function for MultiWorker *)
val make_next_files :
  root:File_path.t ->
  all:bool ->
  sort:bool ->
  subdir:File_path.t option ->
  options:options ->
  include_libdef:bool ->
  libs:SSet.t ->
  unit ->
  string list

val get_all : (unit -> string list) -> SSet.t

(* given a base directory and a relative path, return an absolute path *)
val normalize_path : string -> string -> string

(* given a base directory and a relative path, return an absolute path *)
val construct_path : string -> string list -> string

val split_path : string -> string list

val relative_path : string -> string -> string

val absolute_path : string -> string -> string

(* TODO: this doesn't match the signatures of the related functions above *)
val make_path_absolute : File_path.t -> string -> File_path.t

val is_prefix : string -> string -> bool

val get_flowtyped_path : File_path.t -> File_path.t

val filename_from_string :
  options:options -> consider_libdefs:bool -> libs:SSet.t -> string -> File_key.t

val mkdirp : string -> Unix.file_perm -> unit

val is_within_node_modules : root:File_path.t -> options:options -> string -> bool

val imaginary_realpath : string -> string

val canonicalize_filenames :
  cwd:string -> handle_imaginary:(string -> string) -> string list -> string list

val expand_project_root_token : root:File_path.t -> string -> string
