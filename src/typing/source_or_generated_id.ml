(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* In order to minimize the frequency with which we unnecessarily compare
 * equivalent structures, we assign structures created at the top level of a
 * source program an id of their location instead of an int. This way, if we see
 * the structure twice between the type-sig and check phases, we consider them
 * equal just by looking at their ids (see equal_id).
 *
 * Note that we still store both versions of the type (distinguished through the
 * value of the 'type_sig' field), since the types generated in each phase do not
 * have identical properties. One example is that type-sig produces FullyResolved
 * types exclusively, whereas check can create types that are unresolved. It is
 * important to keep these types separate and not use them interchangeably, or we
 * risk breaking the invariant that the FullyResolved constructor deeply contains
 * other FullyResolved types. (Regression tests under tests/recursive_defs)
 *
 * Source ids with the same location are considered equal w.r.t. type checking
 * regardless of the value of type_sig, which means regardless of whether they
 * were generated during type-sig of check phase.
 *)
type id =
  | Source of {
      loc: ALoc.id;
      type_sig: bool;
    }
  | Generated of int

let compare_id a b =
  match (a, b) with
  | (Source { loc = loc1; type_sig = b1 }, Source { loc = loc2; type_sig = b2 }) ->
    let result = compare b1 b2 in
    if result = 0 then
      ALoc.quick_compare (loc1 :> ALoc.t) (loc2 :> ALoc.t)
    else
      result
  | (Generated a, Generated b) -> a - b
  | (Source _, Generated _) -> -1
  | (Generated _, Source _) -> 1

let equal_id a b =
  match (a, b) with
  | (Source { loc = loc1; _ }, Source { loc = loc2; _ }) ->
    ALoc.quick_compare (loc1 :> ALoc.t) (loc2 :> ALoc.t) = 0
  | (Generated a, Generated b) -> a = b
  | (Source _, Generated _) -> false
  | (Generated _, Source _) -> false

let generate_id () = Generated (Reason.mk_id ())

let id_of_aloc_id ~type_sig aloc_id = Source { loc = aloc_id; type_sig }

let string_of_id = function
  | Generated id -> string_of_int id
  | Source { type_sig; loc = id } ->
    Utils_js.spf "%s (type_sig: %b)" (Reason.string_of_aloc (id :> ALoc.t)) type_sig
