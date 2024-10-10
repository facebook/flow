(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t [@@deriving show]

val available_platforms :
  file_options:Files.options ->
  filename:string ->
  explicit_available_platforms:string list option ->
  t option

val is_subset : t -> t -> bool

val no_overlap : t -> t -> bool

val to_platform_string_set : file_options:Files.options -> t -> SSet.t

(* Return a partitioned list of platform specific implementation mrefs to check existence,
 * of the form (unconditional_extensions, grouped_extensions_with_conditional_extensions).

 * - Every mref in unconditional_extensions must always exist.
 * - grouped_extensions_with_conditional_extensions is a list of (group_ext_mref, platform_ext_mref).
 *   We first check if group_ext_mref exists. If that fails then platform_ext_mref
 *   must exist. *)
val platform_specific_implementation_mrefs_of_possibly_interface_file :
  file_options:Files.options ->
  platform_set:t option ->
  file:File_key.t ->
  (string list * (string * string list) list) option
