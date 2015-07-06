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
module Phase = Typing_phase
module SN = Naming_special_names

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
  | Vprivate _   , _ ->
    (* The only time this case should come into play is when the
     * parent_class_elt comes from a trait *)
    ()
  | Vpublic      , Vpublic
  | Vprotected _ , Vprotected _
  | Vprotected _ , Vpublic       -> ()
  | _ ->
    let parent_pos = Reason.to_pos (fst parent_class_elt.ce_type) in
    let pos = Reason.to_pos (fst class_elt.ce_type) in
    let parent_vis = TUtils.string_of_visibility parent_class_elt.ce_visibility in
    let vis = TUtils.string_of_visibility class_elt.ce_visibility in
    Errors.visibility_extends vis pos parent_pos parent_vis

(* Check that all the required members are implemented *)
let check_members_implemented check_private parent_reason reason parent_members members =
  SMap.iter begin fun member_name class_elt ->
    match class_elt.ce_visibility with
      | Vprivate _ when not check_private -> ()
      | Vprivate _ ->
        (* This case cannot be removed as long as we're forced to
         * check against every extended parent by the fact that // decl
         * parents aren't fully checked against grandparents; when
         * (class) extends (class // decl) use (trait), the grandchild
         * won't have access to private members of the grandparent
         * trait *)
        ()
      | _ when not (SMap.mem member_name members) ->
        let defn_reason = Reason.to_pos (fst class_elt.ce_type) in
        Errors.member_not_implemented member_name parent_reason reason defn_reason
      | _ -> ()
  end parent_members

(* When constant is overridden we need to check if the type is
 * compatible with the previous type defined in the parent.
 *
 * Note that we determine if a constant is abstract by seeing if it is
 * a Tgeneric.
 *)
let check_types_for_const env parent_type class_type =
  match (snd parent_type, snd class_type) with
    | Tgeneric (_, None), _ -> ()
      (* parent abstract constant; no constraints *)
    | Tgeneric (_, Some (Ast.Constraint_as, fty_parent)),
      Tgeneric (_, Some (Ast.Constraint_as, fty_child)) ->
      (* redeclaration of an abstract constant *)
      ignore (Phase.sub_type_decl env fty_parent fty_child)
    | Tgeneric (_, Some (Ast.Constraint_as, fty_parent)), _ ->
      (* const definition constrained by parent abstract const *)
      ignore (Phase.sub_type_decl env fty_parent class_type)
    | (_, _) ->
      (* types should be the same *)
      ignore (Phase.unify_decl env parent_type class_type)
(* An abstract member can be declared in multiple ancestors. Sometimes these
 * declarations can be different, but yet compatible depending on which ancestor
 * we inherit the member from. For example:
 *
 * interface I1 { abstract public function foo(): int; }
 * interface I2 { abstract public function foo(): mixed; }
 *
 * abstract class C implements I1, I2 {}
 *
 * I1::foo() is compatible with I2::foo(), but not vice versa. Hack chooses the
 * signature for C::foo() arbitrarily and can report an error if we make a
 * "wrong" choice. We check for this case and emit an extra line in the error
 * instructing the programmer to redeclare the member to remove the ambiguity.
 *
 * Note: We could detect this case and make the correct choice for the user, but
 * this would require invalidating the current entry we have in the typing heap
 * for this class. We cannot make this choice earlier during typing_decl because
 * a class we depend on during the subtyping may not have been declared yet.
 *)
let check_ambiguous_inheritance f parent child pos class_ origin =
    Errors.try_when
      (f parent child)
      ~when_: (fun () -> class_.tc_name <> origin &&
        Errors.has_no_errors (f child parent))
      ~do_: (fun error ->
        Errors.ambiguous_inheritance pos class_.tc_name origin error)

(* Check that overriding is correct *)
let check_override env ?(ignore_fun_return = false) ?(check_for_const = false)
    parent_class class_ parent_class_elt class_elt =
  let class_known = if use_parent_for_known then parent_class.tc_members_fully_known
    else class_.tc_members_fully_known in
  let check_vis = class_known || check_partially_known_method_visibility in
  if check_vis then check_visibility parent_class_elt class_elt else ();
  let check_params = class_known || check_partially_known_method_params in
  if check_params then
    if check_for_const
    then check_types_for_const env parent_class_elt.ce_type class_elt.ce_type
    else match parent_class_elt.ce_type, class_elt.ce_type with
      | (r_parent, Tfun ft_parent), (r_child, Tfun ft_child) ->
        let subtype_funs = SubType.subtype_method ~check_return:(
            (not ignore_fun_return) &&
            (class_known || check_partially_known_method_returns)
          ) in
        let check (r1, ft1) (r2, ft2) () = ignore(subtype_funs env r1 ft1 r2 ft2) in
        check_ambiguous_inheritance check (r_parent, ft_parent) (r_child, ft_child)
          (Reason.to_pos r_child) class_ class_elt.ce_origin
      | fty_parent, fty_child ->
        let pos = Reason.to_pos (fst fty_child) in
        ignore (unify_decl pos Typing_reason.URnone env fty_parent fty_child)

(* Privates are only visible in the parent, we don't need to check them *)
let filter_privates members =
  SMap.fold begin fun name class_elt acc ->
    if is_private class_elt
    then acc
    else SMap.add name class_elt acc
  end members SMap.empty

let check_members check_private env parent_class class_ parent_members members =
  let parent_members = if check_private then parent_members
    else filter_privates parent_members in
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

let make_all_members class_ = [
  class_.tc_props;
  class_.tc_sprops;
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

(* Checks if a child is compatible with the type constant of its parent.
 * This requires the child's constraint and assigned type to be a subtype of
 * the parent's type constant.
 *)
let tconst_subsumption env parent_typeconst child_typeconst =
  let pos = fst child_typeconst.ttc_name in
  let parent_pos = fst parent_typeconst.ttc_name in
  let is_final =
    Option.is_none parent_typeconst.ttc_constraint &&
    Option.is_some parent_typeconst.ttc_type in

  (* Check that the child's constraint is compatible with the parent. If the
   * parent has a constraint then the child must also have a constraint if it
   * is abstract
   *)
  let child_is_abstract = Option.is_none child_typeconst.ttc_type in
  let default = Reason.Rtconst_no_cstr child_typeconst.ttc_name,
                Tgeneric (snd child_typeconst.ttc_name, None) in
  let child_cstr =
    if child_is_abstract
    then Some (Option.value child_typeconst.ttc_constraint ~default)
    else child_typeconst.ttc_constraint in
  ignore @@ Option.map2
    parent_typeconst.ttc_constraint
    child_cstr
    ~f:(sub_type_decl pos Reason.URsubsume_tconst_cstr env);

  (* Check that the child's assigned type satisifies parent constraint *)
  ignore @@ Option.map2
    parent_typeconst.ttc_constraint
    child_typeconst.ttc_type
    ~f:(sub_type_decl parent_pos Reason.URtypeconst_cstr env);

  (* If the parent cannot be overridden, we unify the types otherwise we ensure
   * the child's assigned type is compatible with the parent's *)
  let check x y =
    if is_final
    then ignore(unify_decl pos Reason.URsubsume_tconst_assign env x y)
    else ignore(sub_type_decl pos Reason.URsubsume_tconst_assign env x y) in
  ignore @@ Option.map2
    parent_typeconst.ttc_type
    child_typeconst.ttc_type
    ~f:check

(* For type constants we need to check that a child respects the
 * constraints specified by its parent.  *)
let check_typeconsts env parent_class class_ =
    let parent_pos, parent_class, _ = parent_class in
  let pos, class_, _ = class_ in
  let ptypeconsts = parent_class.tc_typeconsts in
  let typeconsts = class_.tc_typeconsts in
  let tconst_check parent_tconst tconst () =
    tconst_subsumption env parent_tconst tconst in
  SMap.iter begin fun tconst_name parent_tconst ->
    match SMap.get tconst_name typeconsts with
      | Some tconst ->
          check_ambiguous_inheritance tconst_check parent_tconst tconst
            (fst tconst.ttc_name) class_ tconst.ttc_origin
      | None ->
        Errors.member_not_implemented
          tconst_name parent_pos pos (fst parent_tconst.ttc_name)
  end ptypeconsts

let check_consts env parent_class class_ psubst subst =
  let pconsts, consts = parent_class.tc_consts, class_.tc_consts in
  let env, pconsts = instantiate_members psubst env pconsts in
  let env, consts = instantiate_members subst env consts in
  let check_const_override = check_override env ~check_for_const:true parent_class class_ in
  SMap.iter begin fun const_name parent_const ->
    if const_name <> SN.Members.mClass then
      match SMap.get const_name consts with
        | Some const -> check_const_override parent_const const
        | None ->
          let parent_pos = Reason.to_pos (fst parent_const.ce_type) in
          Errors.member_not_implemented const_name parent_pos
            class_.tc_pos parent_class.tc_pos;
  end pconsts;
  ()

let check_class_implements env parent_class class_ =
  check_typeconsts env parent_class class_;
  let parent_pos, parent_class, parent_tparaml = parent_class in
  let pos, class_, tparaml = class_ in
  let fully_known = class_.tc_members_fully_known in
  let psubst = Inst.make_subst parent_class.tc_tparams parent_tparaml in
  let subst = Inst.make_subst class_.tc_tparams tparaml in
  check_consts env parent_class class_ psubst subst;
  let pmemberl = make_all_members parent_class in
  let memberl = make_all_members class_ in
  check_constructors env parent_class class_ psubst subst;
  let env, pmemberl = lfold (instantiate_members psubst) env pmemberl in
  let env, memberl = lfold (instantiate_members subst) env memberl in
  let check_privates:bool = (parent_class.tc_kind = Ast.Ctrait) in
  if not fully_known then () else
    List.iter2 (check_members_implemented check_privates parent_pos pos) pmemberl memberl;
  List.iter2 (check_members check_privates env parent_class class_) pmemberl memberl;
  ()

(*****************************************************************************)
(* The externally visible function *)
(*****************************************************************************)

let check_implements env parent_type type_ =
  let parent_r, parent_name, parent_tparaml =
    Typing_hint.open_class_hint parent_type in
  let r, name, tparaml = Typing_hint.open_class_hint type_ in
  let parent_class = Env.get_class env (snd parent_name) in
  let class_ = Env.get_class env (snd name) in
  match parent_class, class_ with
  | None, _ | _, None -> ()
  | Some parent_class, Some class_ ->
      let parent_class =
        (Reason.to_pos parent_r), parent_class, parent_tparaml in
      let class_ = (Reason.to_pos r), class_, tparaml in
      Errors.try_
        (fun () -> check_class_implements env parent_class class_)
        (fun errorl ->
          let p_name_pos, p_name_str = parent_name in
          let name_pos, name_str = name in
          Errors.bad_decl_override p_name_pos p_name_str name_pos name_str errorl)
