(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

(* keys for refinements *)
type proj =
  | Prop of string
  | Elem of t
  | PrivateField of string

and t = string * proj list

let rec string_of_key (base, projs) =
  base
  ^ String.concat
      ""
      ( List.rev projs
      |> Base.List.map ~f:(function
             | Prop name -> spf ".%s" name
             | PrivateField name -> spf "private.%s" name
             | Elem expr -> spf "[%s]" (string_of_key expr)) )

(* true if the given key uses the given property name *)
let rec uses_propname propname ~private_ (_base, proj) = proj_uses_propname ~private_ propname proj

(* true if the given projection list uses the given property name *)
and proj_uses_propname ~private_ propname = function
  | Prop name :: tail ->
    (name = propname && not private_) || proj_uses_propname ~private_ propname tail
  | PrivateField name :: tail ->
    (name = propname && private_) || proj_uses_propname ~private_ propname tail
  | Elem key :: tail ->
    uses_propname ~private_ propname key || proj_uses_propname ~private_ propname tail
  | [] -> false

let compare = Stdlib.compare

let is_simple (_, ps) = List.length ps = 0

let reason_desc =
  Reason.(
    function
    | (name, []) when not (is_internal_name name) -> RIdentifier name
    | (name, []) -> RCustom name
    | (_, projs) ->
      (match List.hd (List.rev projs) with
      | Prop x -> RProperty (Some x)
      | PrivateField x -> RPrivateProperty x
      | Elem _ -> RProperty None))
