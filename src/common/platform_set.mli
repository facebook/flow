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
