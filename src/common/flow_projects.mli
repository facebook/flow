(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type options

type t

val default_options : options

val mk_options :
  projects:string Nel.t ->
  projects_overlap_mapping:SSet.t SMap.t ->
  map_path:(string -> Str.regexp) ->
  projects_path_mapping:(string * string list) list ->
  projects_strict_boundary:bool ->
  projects_strict_boundary_import_pattern_opt_outs:Str.regexp list ->
  multi_platform_ambient_supports_platform_project_overrides:(string * string list) list ->
  options

val equal : t -> t -> bool

val from_bitset_unchecked : Bitset.t -> t

val to_bitset : t -> Bitset.t

val bitset_of_project_string : opts:options -> string -> t

val projects_bitset_of_path : opts:options -> string -> t option

val is_common_code_path : opts:options -> string -> bool

val is_import_specifier_that_opt_out_of_strict_boundary :
  opts:options -> import_specifier:string -> bool

val reachable_projects_bitsets_from_projects_bitset :
  opts:options -> import_specifier:string -> t -> t list

val individual_projects_bitsets_from_common_project_bitset : opts:options -> t -> t list option

val individual_projects_bitsets_from_common_project_bitset_excluding_first :
  opts:options -> t -> t list option

val multi_platform_ambient_supports_platform_for_project : opts:options -> t -> string list option
