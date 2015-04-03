(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type severity =
  | Error
  | Warning
  | Advice

type 'a t
val get_code : 'a t -> int
val get_pos : 'a t -> 'a Pos.pos

val add : int -> severity -> Pos.t -> string -> unit
val to_absolute : Relative_path.t t -> string t
val to_string : string t -> string
val to_json : string t -> Hh_json.json

val lowercase_constant : Pos.t -> string -> unit
val use_collection_literal : Pos.t -> string -> unit
val static_string : ?no_consts:bool -> Pos.t -> unit

val do_ : (unit -> 'a) -> Relative_path.t t list * 'a
