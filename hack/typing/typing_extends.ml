(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* Checks that a class implements an interface *)
(*****************************************************************************)

open Utils
open Typing_defs
open Typing_ops

module Env = Typing_env
module TUtils = Typing_utils
module Inst = Typing_instantiate

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let is_private = function
  | { ce_visibility = Vprivate _; _ } -> true
  | _ -> false

(*****************************************************************************)
(* Given a map of members, check that the overriding is correct.
 * Please note that 'members' has a very general meaning here.
 * It can be class variables, methods, static methods etc ... The same logic
 * is applied to verify that the overriding is correct.
 *)
(*****************************************************************************)

let use_parent_for_known = false
let check_partially_known_method_returns = true
let check_partially_known_method_params = false
let check_partially_known_method_visibility = true

(* Rules for visibility *)
let check_visibility parent_class_elt class_elt =
  match parent_class_elt.ce_visibility, class_elt.ce_visibility with
  | Vpublic      , Vpublic
  | Vprivate _   , Vprivate _
  | Vprotected _ , Vprotected _
  | Vprotected _ , Vpublic       -> ()
  | _ ->
    let parent_pos = Reason.to_pos (fst parent_class_elt.ce_type) in
    let pos = Reason.to_pos (fst class_elt.ce_type) in
    let parent_vis = TUtils.string_of_visibility parent_class_elt.ce_visibility in
    let vis = TUtils.string_of_visibility class_elt.ce_visibility in
    Errors.visibility_extends vis pos parent_pos parent_vis

(* Check that all the required members are implemented *)
let check_members_implemented parent_reason reason parent_members members =
  SMap.iter begin fun member_name class_elt ->
    match class_elt.ce_visibility with
    | Vprivate _ -> ()
    | _ when not (SMap.mem member_name members) ->
        let defn_reason = Reason.to_pos (fst class_elt.ce_type) in
        Errors.member_not_implemented member_name parent_reason reason defn_reason
    | _ -> ()
  end parent_members

(* Check that overriding is correct *)
let check_override env ?(ignore_fun_return = false)
    parent_class class_ parent_class_elt class_elt =
  let class_known = if use_parent_for_known then parent_class.tc_members_fully_known
    else class_.tc_members_fully_known in
  let check_vis = class_known || check_partially_known_method_visibility in
  if check_vis then check_visibility parent_class_elt class_elt else ();
  let check_params = class_known || check_partially_known_method_params in
  if check_params then
    (* Replace the parent's this type with the child's. This avoids complaining
     * about how this as Base and this as Child are different types *)
    let self = Env.get_self env in
    let this_ty = fst self, Tgeneric ("this", Some self) in
    let env, parent_ce_type =
      Inst.instantiate_this env parent_class_elt.ce_type this_ty in
    match parent_ce_type, class_elt.ce_type with
      | (r_parent, Tfun ft_parent), (r_child, Tfun ft_child) ->
        let subtype_funs =
          if (not ignore_fun_return) &&
            (class_known || check_partially_known_method_returns) then
            SubType.subtype_funs
          else SubType.subtype_funs_no_return in
        ignore (subtype_funs env r_parent ft_parent r_child ft_child)
      | fty_parent, fty_child ->
        let pos = Reason.to_pos (fst fty_child) in
        ignore (unify pos Typing_reason.URnone env fty_parent fty_child)

(* Privates are only visible in the parent, we don't need to check them *)
let filter_privates members =
  SMap.fold begin fun name class_elt acc ->
    if is_private class_elt
    then acc
    else SMap.add name class_elt acc
  end members SMap.empty

let check_members env parent_class class_ parent_members members =
  let parent_members = filter_privates parent_members in
  SMap.iter begin fun member_name parent_class_elt ->
    match SMap.get member_name members with
    | Some class_elt  ->
      check_override env parent_class class_ parent_class_elt class_elt
    | None -> ()
 end parent_members

(*****************************************************************************)
(* Before checking that a class implements an interface, we have to
 * substitute the type parameters with their real type.
 *)
(*****************************************************************************)

(* Instantiation basically applies the substitution *)
let instantiate_members subst env members =
  SMap.map_env (Inst.instantiate_ce subst) env members

(* TODO constant inheritance is broken. We don't inherit constants that
 * come from interfaces. *)
let make_all_members class_ = [
(* class_.tc_consts; *)
  class_.tc_cvars;
  class_.tc_scvars;
  class_.tc_methods;
  class_.tc_smethods;
]

(* The phantom class element that represents the default constructor:
 * public function __construct() {}
 *
 * It isn't added to the tc_construct only because that's used to
 * determine whether a child class needs to call parent::__construct *)
let default_constructor_ce class_ =
  let pos, name = class_.tc_pos, class_.tc_name in
  let r = Reason.Rwitness pos in (* reason doesn't get used in, e.g. arity checks *)
  let ft = { ft_pos      = pos;
             ft_unsafe   = false;
             ft_deprecated = None;
             ft_abstract = false;
             ft_arity    = Fstandard (0, 0);
             ft_tparams  = [];
             ft_params   = [];
             ft_ret      = r, Tprim Nast.Tvoid;
           }
  in { ce_final       = false;
       ce_is_xhp_attr = false;
       ce_override    = false;
       ce_synthesized = true;
       ce_visibility  = Vpublic;
       ce_type        = r, Tfun ft;
       ce_origin      = name;
     }

(* When an interface defines a constructor, we check that they are compatible *)
let check_constructors env parent_class class_ psubst subst =
  let explicit_consistency = snd parent_class.tc_construct in
  if parent_class.tc_kind = Ast.Cinterface || explicit_consistency
  then (
    match (fst parent_class.tc_construct), (fst class_.tc_construct) with
      | Some parent_cstr, _  when parent_cstr.ce_synthesized -> ()
      | Some parent_cstr, None ->
        let pos = fst parent_cstr.ce_type in
        Errors.missing_constructor (Reason.to_pos pos)
      | _, Some cstr when cstr.ce_override -> (* <<__UNSAFE_Construct>> *)
        ()
      | Some parent_cstr, Some cstr ->
        let env, parent_cstr = Inst.instantiate_ce psubst env parent_cstr in
        let env, cstr = Inst.instantiate_ce subst env cstr in
        check_override env ~ignore_fun_return:true parent_class class_ parent_cstr cstr
      | None, Some cstr when explicit_consistency ->
        let parent_cstr = default_constructor_ce parent_class in
        let env, parent_cstr = Inst.instantiate_ce psubst env parent_cstr in
        let env, cstr = Inst.instantiate_ce subst env cstr in
        check_override env ~ignore_fun_return:true parent_class class_ parent_cstr cstr
      | None, _ -> ()
  ) else ()

let tconst_subsumption this_ty env parent_typeconst child_typeconst =
  match parent_typeconst, child_typeconst with
  | { ttc_constraint = Some parent_cstr; _}, (
      { ttc_type = Some child_ty; _ }
      | { ttc_constraint = Some child_ty; _ }
    ) ->
      let env, parent_cstr = Inst.instantiate_this env parent_cstr this_ty in
      ignore (TUtils.sub_type env parent_cstr child_ty)
  | { ttc_type = Some parent_ty; _ }, { ttc_type = Some child_ty; _ } ->
      let env, parent_ty = Inst.instantiate_this env parent_ty this_ty in
      ignore (TUtils.unify env parent_ty child_ty)
  | _, _ -> ()

(* For type constants we need to check that a child respects the constraints
 * specified by its parent.
 *)
let check_tconsts env parent_class class_ =
  let self = Env.get_self env in
  let this_ty = fst self, Tgeneric ("this", Some self) in
  let parent_pos, parent_class, _ = parent_class in
  let pos, class_, _ = class_ in
  let ptypeconsts = parent_class.tc_typeconsts in
  let typeconsts = class_.tc_typeconsts in
  SMap.iter begin fun tconst_name parent_tconst ->
    match SMap.get tconst_name typeconsts with
    | Some tconst ->
        tconst_subsumption this_ty env parent_tconst tconst
    | None ->
        Errors.member_not_implemented
          tconst_name parent_pos pos (fst parent_tconst.ttc_name)
 end ptypeconsts

let check_class_implements env parent_class class_ =
  check_tconsts env parent_class class_;
  let parent_pos, parent_class, parent_tparaml = parent_class in
  let pos, class_, tparaml = class_ in
  let fully_known = class_.tc_members_fully_known in
  let psubst = Inst.make_subst parent_class.tc_tparams parent_tparaml in
  let subst = Inst.make_subst class_.tc_tparams tparaml in
  let pmemberl = make_all_members parent_class in
  let memberl = make_all_members class_ in
  check_constructors env parent_class class_ psubst subst;
  let env, pmemberl = lfold (instantiate_members psubst) env pmemberl in
  let env, memberl = lfold (instantiate_members subst) env memberl in
  if not fully_known then () else
    List.iter2 (check_members_implemented parent_pos pos) pmemberl memberl;
  List.iter2 (check_members env parent_class class_) pmemberl memberl;
  ()

(*****************************************************************************)
(* The externally visible function *)
(*****************************************************************************)

let open_class_hint = function
  | r, Tapply (name, tparaml) -> Reason.to_pos r, name, tparaml
  | _ -> assert false

let check_implements env parent_type type_ =
  let parent_pos, parent_name, parent_tparaml = open_class_hint parent_type in
  let pos, name, tparaml = open_class_hint type_ in
  let parent_class = Env.get_class env (snd parent_name) in
  let class_ = Env.get_class env (snd name) in
  match parent_class, class_ with
  | None, _ | _, None -> ()
  | Some parent_class, Some class_ ->
      let parent_class = parent_pos, parent_class, parent_tparaml in
      let class_ = pos, class_, tparaml in
      Errors.try_
        (fun () -> check_class_implements env parent_class class_)
        (fun errorl ->
          let p_name_pos, p_name_str = parent_name in
          let name_pos, name_str = name in
          Errors.override p_name_pos p_name_str name_pos name_str errorl)
