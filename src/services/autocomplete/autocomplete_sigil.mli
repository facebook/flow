(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val add : string -> int -> int -> string * string

module Canonical : sig
  type token

  val cursor : token -> Loc.t

  val to_relative_ac_results : canon:token -> Loc.t -> string -> ALoc.t * string
end

val add_canonical :
  File_key.t option -> string -> int -> int -> string * string * Canonical.token option

val remove_from_loc : Loc.t -> Loc.t

val remove_opt : string -> (string * string) option

val remove : string -> string * string

val split_opt : string -> (string * string) option

val extract_cursor : string -> (string * (int * int)) option
