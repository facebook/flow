(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = Loc.t * int [@@deriving show]

let compare (a_loc, a_id) (b_loc, b_id) =
  let i = Loc.compare a_loc b_loc in
  if i = 0 then
    a_id - b_id
  else
    i

let equal (a_loc, a_id) (b_loc, b_id) = Loc.equal a_loc b_loc && a_id = b_id

let none = (Loc.none, 0)

let debug_to_string ?include_source (loc, id) =
  Printf.sprintf "(%s, %s)" (Loc.debug_to_string ?include_source loc) (string_of_int id)
