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

type options

val mk_options :
  default_lib_dir:lib_dir option ->
  ignores:((string * string option) * Str.regexp) list ->
  untyped:(string * Str.regexp) list ->
  declarations:(string * Str.regexp) list ->
  implicitly_include_root:bool ->
  includes:Path_matcher.t ->
  haste_paths_excludes:Str.regexp list ->
  haste_paths_includes:Str.regexp list ->
  lib_paths:(string option * File_path.t) list ->
  module_declaration_dirnames:string list ->
  module_file_exts:string list ->
  module_resource_exts:SSet.t ->
  multi_platform:bool ->
  multi_platform_extensions:string list ->
  multi_platform_extension_group_mapping:(string * string list) list ->
  node_resolver_dirnames:string list ->
  options

val default_options : options

val default_lib_dir : options -> lib_dir option

val ignores : options -> ((string * string option) * Str.regexp) list

val untyped : options -> (string * Str.regexp) list

val implicitly_include_root : options -> bool

val includes : options -> Path_matcher.t

val module_declaration_dirnames : options -> string list

val module_file_exts : options -> string list

val module_resource_exts : options -> SSet.t

val multi_platform : options -> bool

val multi_platform_extensions : options -> string list

val multi_platform_extension_group_mapping : options -> (string * string list) list

val node_resolver_dirnames : options -> string list

val node_modules_containers : SSet.t SMap.t ref

val global_file_name : string

val flow_ext : string

val has_flow_ext : File_key.t -> bool

val chop_flow_ext : File_key.t -> File_key.t

val eponymous_module : File_key.t -> Modulename.t

(* If the given file is a Haste file, return Some(haste name of file) *)
val haste_name_opt : options:options -> File_key.t -> string option

val relative_interface_mref_of_possibly_platform_specific_file :
  options:options -> File_key.t -> string option

val grouped_platform_extension_opt : options:options -> string -> (string * string list) option

val platform_specific_extensions_and_indices_opt :
  options:options -> string -> (int * string) list option

val chop_platform_suffix_for_file : options:options -> File_key.t -> File_key.t

val chop_platform_suffix_for_haste_module : options:options -> string -> string

val is_json_file : string -> bool

val is_flow_file : options:options -> string -> bool

(* true if a file path matches an [ignore] entry in config, also returns the backup flowconfig if
 * one is specified
 *)
val is_ignored : options -> string -> bool * string option

(* true if a file path matches an [untyped] entry in config *)
val is_untyped : options -> string -> bool

(* true if a file path matches a [declarations] entry in config *)
val is_declaration : options -> string -> bool

(* true if a file path matches an [include] path in config *)
val is_included : options -> string -> bool

val is_valid_path : options:options -> string -> bool

val is_in_flowlib : options -> string -> bool

val get_all_watched_extensions : options -> SSet.t

val ordered_and_unordered_lib_paths : options -> (string option * string) list * SSet.t

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
  all_unordered_libs:SSet.t ->
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
  options:options -> consider_libdefs:bool -> all_unordered_libs:SSet.t -> string -> File_key.t

val mkdirp : string -> Unix.file_perm -> unit

val is_within_node_modules : root:File_path.t -> options:options -> string -> bool

val imaginary_realpath : string -> string

val canonicalize_filenames :
  cwd:string -> handle_imaginary:(string -> string) -> string list -> string list

val expand_project_root_token : root:File_path.t -> string -> string
