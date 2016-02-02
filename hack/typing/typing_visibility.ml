(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 **)

open Nast
open Typing_defs
open Utils

module Env = Typing_env
module TUtils = Typing_utils

let is_protected_visible env x self_id =
  if x = self_id then None else
  let my_class = Env.get_class env self_id in
  let their_class = Env.get_class env x in
  match my_class, their_class with
  | Some my_class, Some their_class ->
    (* Children can call parent's protected methods and
     * parents can call children's protected methods (like a
     * constructor) *)
    if SSet.mem x my_class.tc_extends
       || SSet.mem self_id their_class.tc_extends
       || SSet.mem x my_class.tc_req_ancestors_extends
       || not my_class.tc_members_fully_known
    then None
    else Some (
      "Cannot access this protected member, you don't extend "^
        (strip_ns x)
    )
  | _, _ -> None

let is_private_visible_for_class env x self_id cid class_ =
  match cid with
  | CIstatic ->
    let my_class = Env.get_class env self_id in
    (match my_class with
     | Some {tc_final = true; _} -> None
     | _ ->
       Some "Private members cannot be accessed with static:: since \
             a child class may also have an identically \
             named private member")
  | CIparent ->
    Some "You cannot access a private member with parent::"
  | CIself -> None
  | CI (_, called_ci) ->
    (if x = self_id then None else
     match Env.get_class env called_ci with
     | Some {tc_kind = Ast.Ctrait; _} ->
       Some "You cannot access private members \
             using the trait's name (did you mean to use self::?)"
     | _ ->
       Some "You cannot access this member")
  | CIexpr _ ->
    if class_.tc_final then None
    else Some "Private members cannot be accessed dynamically. \
               Did you mean to use 'self::'?"

let is_visible_for_obj env vis =
  let self_id = Env.get_self_id env in
  match vis with
  | Vpublic -> None
  | (Vprivate _ | Vprotected _) when Env.is_outside_class env ->
    Some "You cannot access this member"
  | Vprivate x ->
    if x = self_id then None
    else Some "You cannot access this member"
  | Vprotected x ->
    is_protected_visible env x self_id

let is_visible_for_class env vis cid cty =
  let self_id = Env.get_self_id env in
  match vis with
  | Vpublic -> None
  | (Vprivate _ | Vprotected _) when Env.is_outside_class env ->
    Some "You cannot access this member"
  | Vprivate x -> is_private_visible_for_class env x self_id cid cty
  | Vprotected x ->
    let their_class = Env.get_class env x in
    (match cid, their_class with
     | CI _, Some {tc_kind = Ast.Ctrait; _} ->
       Some "You cannot access protected members using the trait's name \
             (did you mean to use static:: or self::?)"
     | _ -> is_protected_visible env x self_id)

let is_visible env vis cid class_ =
  let msg_opt = match cid with
    | Some cid -> is_visible_for_class env vis cid class_
    | None -> is_visible_for_obj env vis
  in
  Option.is_none msg_opt

let visibility_error p msg (p_vis, vis) =
  let s = TUtils.string_of_visibility vis in
  let msg_vis = "This member is "^s in
  Errors.visibility p msg p_vis msg_vis

let check_obj_access p env (p_vis, vis) =
  match is_visible_for_obj env vis with
  | None -> ()
  | Some msg ->
    visibility_error p msg (p_vis, vis)

let check_class_access p env (p_vis, vis) cid class_ =
  match is_visible_for_class env vis cid class_ with
  | None -> ()
  | Some msg ->
    visibility_error p msg (p_vis, vis)

(*****************************************************************************)
(* Keep the most restrictive visibility (private < protected < public).
 * This is useful when dealing with unresolved types.
 * When there are several candidates for a given visibility we need to be
 * conservative and consider the most restrictive one.
 *)
(*****************************************************************************)

let min_vis vis1 vis2 =
  match vis1, vis2 with
  | x, Vpublic | Vpublic, x -> x
  | Vprotected _, x | x, Vprotected _ -> x
  | Vprivate _ as vis, Vprivate _ -> vis

let min_vis_opt vis_opt1 vis_opt2 =
  Option.merge vis_opt1 vis_opt2 begin fun (pos1, x) (pos2, y) ->
    let pos = if pos1 = Pos.none then pos2 else pos1 in
    pos, min_vis x y
  end
