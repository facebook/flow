(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Loc_collections
open Scope_id

(* This module tracks what generics are currently in scope. We track a stack
   of scopes, each with its own unique ID.  This isn't actually where we look
   to see if a tvar is in scope for a given generic--the context tracks that
   information. This module just helps us figure out what's in-scope at the
   moment a tvar is created and set up the context.

    When we add a new set of generics to the scope, we:
    - Create a new ID for the scope
    - Put the scope ID and the new set of generic IDs (which are ALoc.id's)
       on the stack
    - Update the context's generic_scope_map to map from the generic ID to *all*
       of the generics
       currently in scope (not just the ones that were just added)
*)

type scope = ALocIDSet.t

type stack = (Scope_id.t * scope) Nel.t

let initial_id = new_id ()

let stack : stack ref = ref (Nel.one (initial_id, ALocIDSet.empty))

let flatten_stack = Nel.fold_left (fun a (_, b) -> ALocIDSet.union a b) ALocIDSet.empty

let update_scope gcx this_id =
  let scope = flatten_stack !stack in
  Generic_cx.add_scope_map gcx this_id scope

let push_scope gcx (tparams : ALoc.id Nel.t) =
  let this_id = new_id () in
  let scope = Nel.to_list tparams |> ALocIDSet.of_list in
  stack := Nel.cons (this_id, scope) !stack;
  update_scope gcx this_id

let pop_scope () =
  match Nel.tl !stack with
  | [] -> failwith "Stack pop without push"
  | hd :: tl -> stack := (hd, tl)

let scope_id () =
  let (id, _) = Nel.hd !stack in
  id

let in_scope gcx tparams f =
  match tparams with
  | hd :: tl ->
    let gcx = push_scope gcx (hd, tl) in
    let res = f gcx in
    pop_scope ();
    res
  | [] -> f gcx

let init gcx = update_scope gcx initial_id
