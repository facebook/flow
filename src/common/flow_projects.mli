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
  options

val equal : t -> t -> bool

val from_bitset_unchecked : Bitset.t -> t

val to_bitset : t -> Bitset.t

val bitset_of_project_string : opts:options -> string -> t

val projects_bitset_of_path : opts:options -> string -> t option

val reachable_projects_bitsets_from_projects_bitset : opts:options -> t -> t list

val individual_projects_bitsets_from_common_project_bitset : opts:options -> t -> t list option
