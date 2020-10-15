(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type id

type spread_id

type sat_result =
  | Satisfied
  | Lower of id
  | Upper of id

val to_string : id -> string

val equal_id : id -> id -> bool

val collapse : id -> id -> id option

val spread_empty : spread_id

val make_spread : id -> spread_id

val make_bound_id : ALoc.id -> string -> id

val make_spread_id : spread_id -> id option

val make_spread_id_exn : spread_id -> id

val spread_subtract : spread_id -> spread_id -> spread_id

val spread_append : spread_id -> spread_id -> spread_id

val spread_exists : spread_id -> bool

val aloc_of_id : id -> ALoc.id

val fold_ids : f:(ALoc.id -> string -> 'a -> 'a) -> acc:'a -> id -> 'a

val satisfies : id -> id -> sat_result

module ArraySpread : sig
  type ro_status =
    | NonROSpread
    | ROSpread

  type t =
    | Bottom
    | Top
    | Generic of (id * ro_status)

  val merge : t -> id option -> ro_status -> t

  val to_option : t -> id option
end
