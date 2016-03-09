(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Nast
open Typing_defs

let parent_init_prop = "parent::" ^ SN.Members.__construct

(* If we need to call parent::__construct, we treat it as if it were
 * a class variable that needs to be initialized. It's a bit hacky
 * but it works. The idea here is that if the parent needs to be
 * initialized, we add a phony class variable. *)
let add_parent_construct c decl_env props parent_hint =
  match parent_hint with
    | (_, Happly ((_, parent), _)) ->
      let class_ = Decl_env.get_class_dep decl_env parent in
      (match class_ with
        | Some class_ when
            class_.Typing_defs.tc_need_init && c.c_constructor <> None
            -> SSet.add parent_init_prop props
        | _ -> props
      )
    | _ -> props

let parent decl_env props c =
  if c.c_mode = FileInfo.Mdecl then props
  else
    if c.c_kind = Ast.Ctrait
    then List.fold_left c.c_req_extends
      ~f:(add_parent_construct c decl_env) ~init:props
    else match c.c_extends with
    | [] -> props
    | parent_hint :: _ -> add_parent_construct c decl_env props parent_hint

let prop_needs_init add_to_acc acc cv =
  if cv.cv_is_xhp then acc else begin
    match cv.cv_type with
      | None
      | Some (_, Hoption _)
      | Some (_, Hmixed) -> acc
      | Some _ when cv.cv_expr = None -> add_to_acc cv acc
      | Some _ -> acc
  end

let parent_props decl_env acc c =
  List.fold_left (c.c_extends @ c.c_uses) ~f:begin fun acc parent ->
    match parent with
    | _, Happly ((_, parent), _) ->
      let tc = Decl_env.get_class_dep decl_env parent in
      (match tc with
        | None -> acc
        | Some { tc_deferred_init_members = members; _ } ->
          SSet.union members acc)
    | _ -> acc
  end ~init:acc

(* return a tuple of the private init-requiring props of the class
 * and the other init-requiring props of the class and its ancestors *)
let classify_props_for_decl decl_env c =
  let adder = begin fun cv (private_props, hierarchy_props) ->
    let cname = snd cv.cv_id in
    if cv.cv_visibility = Private then
      (SSet.add cname private_props), hierarchy_props
    else
      private_props, (SSet.add cname hierarchy_props)
  end in
  let acc = (SSet.empty, SSet.empty) in
  let priv_props, props =
    List.fold_left ~f:(prop_needs_init adder) ~init:acc c.c_vars in
  let props = parent_props decl_env props c in
  let props = parent decl_env props c in
  priv_props, props

let class_ ~has_own_cstr decl_env c =
  if not has_own_cstr && (c.c_kind = Ast.Ctrait || c.c_kind = Ast.Cabstract)
  then
    (* private properties cannot be initialized without a constructor *)
    let priv_props, props = classify_props_for_decl decl_env c in
    if priv_props <> SSet.empty && (c.c_kind = Ast.Cabstract) then
      (* XXX: should priv_props be checked for a trait?
       *  see chown_privates in typing_inherit *)
      Errors.constructor_required c.c_name priv_props;
    SSet.union priv_props props
  else SSet.empty
