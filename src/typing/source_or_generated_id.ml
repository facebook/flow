(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

(* In order to minimize the frequency with which we unnecessarily compare
    equivalent structures, we assign structures created at the top level of a
    source program an id of their location instead of an int. This way, if we
    see the structure twice between the merge and check phases, we consider them
    equal just by looking at their ids *)
type id =
  | Source of ALoc.id
  | Generated of int

let compare_id a b =
  match (a, b) with
  | (Source a, Source b) -> ALoc.quick_compare (a :> ALoc.t) (b :> ALoc.t)
  | (Generated a, Generated b) -> a - b
  | (Source _, Generated _) -> -1
  | (Generated _, Source _) -> 1

let equal_id a b = compare_id a b = 0

let id_of_int i = Generated i

let id_as_int = function
  | Generated i -> Some i
  | _ -> None

let generate_id = Reason.mk_id %> id_of_int

let id_of_aloc_id aloc_id = Source aloc_id

let string_of_id = function
  | Generated id -> string_of_int id
  | Source id -> Reason.string_of_aloc (id :> ALoc.t)
