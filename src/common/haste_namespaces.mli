(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type options

type t

val mk_options :
  haste_namespaces:string Nel.t ->
  haste_overlapping_namespaces_mapping:SSet.t SMap.t ->
  map_path:(string -> Str.regexp) ->
  haste_namespaces_path_mapping:(string * string list) list ->
  options

val from_bitset_unchecked : Bitset.t -> t

val to_bitset : t -> Bitset.t

val index_of_namespace_string : opts:options -> string -> int

val namespaces_bitset_of_path : opts:options -> string -> t option

val reachable_namespace_bitsets_from_namespace_bitset : opts:options -> t -> t list
