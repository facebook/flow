(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val add : string -> int -> int -> string * string

val remove_from_loc : Loc.t -> Loc.t

val remove_opt : string -> (string * string) option

val remove : string -> string * string

val extract_cursor : string -> (string * (int * int)) option
