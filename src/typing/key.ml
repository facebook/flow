(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

(* keys for refinements *)
type proj = Prop of string | Elem of t
and t = string * proj list

let rec string_of_key (base, projs) =
  base ^ String.concat "" (
    (List.rev projs) |> List.map (function
      | Prop name -> spf ".%s" name
      | Elem expr -> spf "[%s]" (string_of_key expr)
    ))

(* true if the given key uses the given property name *)
let rec uses_propname propname (_base, proj) =
  proj_uses_propname propname proj

(* true if the given projection list uses the given property name *)
and proj_uses_propname propname = function
| Prop name :: tail ->
  name = propname || proj_uses_propname propname tail
| Elem key :: tail ->
  uses_propname propname key || proj_uses_propname propname tail
| [] ->
  false

let compare = Pervasives.compare

let is_simple (_, ps) = List.length ps = 0
