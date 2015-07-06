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
(* Module used to declare the types.
 * For each class we want to build a complete type, that is the type of
 * the methods defined in the class plus everything that was inherited.
 *)
(*****************************************************************************)
open Utils
open Typing_defs
open Nast
open Typing_deps

module Env = Typing_env
module DynamicYield = Typing_dynamic_yield
module Reason = Typing_reason
module Inst = Typing_instantiate
module Attrs = Attributes
module TUtils = Typing_utils

module SN = Naming_special_names
module Phase = Typing_phase

(*****************************************************************************)
(* Module used to track what classes are declared and which ones still need
 * to be processed. The declaration phase happens in parallel. Because of that
 * a worker needs to know if the type of a class is ready or if it needs to
 * be recomputed. When ClassStatus.mem *class-name* returns true, we know
 * the class has already been recomputed by a different worker.
 * TypedefHeap is a similar mechanism for typedefs.
 *)
(*****************************************************************************)

module ClassStatus = SharedMem.NoCache (String) (struct
  type t = unit
  let prefix = Prefix.make()
end)

let is_class_ready class_name =
  ClassStatus.mem class_name

let report_class_ready class_name =
  ClassStatus.add class_name ()

let remove_classes class_set =
  ClassStatus.remove_batch class_set


(*****************************************************************************)
(* Checking that the kind of a class is compatible with its parent
 * For example, a class cannot extend an interface, an interface cannot
 * extend a trait etc ...
 *)
(*****************************************************************************)

let check_extend_kind parent_pos parent_kind child_pos child_kind =
  match parent_kind, child_kind with
    (* What is allowed *)
  | (Ast.Cabstract | Ast.Cnormal), (Ast.Cabstract | Ast.Cnormal)
  | Ast.Cabstract, Ast.Cenum (* enums extend BuiltinEnum under the hood *)
  | Ast.Ctrait, Ast.Ctrait
  | Ast.Cinterface, Ast.Cinterface ->
      ()
  | _ ->
      (* What is disallowed *)
      let parent = Ast.string_of_class_kind parent_kind in
      let child  = Ast.string_of_class_kind child_kind in
      Errors.wrong_extend_kind child_pos child parent_pos parent

(*****************************************************************************)
(* Functions used retrieve everything implemented in parent classes
 * The return values:
 * env: the new environment
 * parents: the name of all the parents and grand parents of the class this
 *          includes traits.
 * is_complete: true if all the parents live in Hack
 *)
(*****************************************************************************)
let desugar_class_hint = function
  | (_, Happly ((pos, class_name), type_parameters)) ->
    pos, class_name, type_parameters
  | _ -> assert false

let check_arity pos class_name class_type class_parameters =
  let arity = List.length class_type.tc_tparams in
  if List.length class_parameters <> arity
  then Errors.class_arity pos class_type.tc_pos class_name arity;
  ()

let make_substitution pos class_name class_type class_parameters =
  check_arity pos class_name class_type class_parameters;
  Inst.make_subst class_type.tc_tparams class_parameters

(*-------------------------- end copypasta *)

(**
 * Adds the traits/classes which are part of a class' hierarchy.
 *
 * Traits are included in the parent list so that the class can access the trait
 * members which are declared as private/protected.
 *)
let add_grand_parents_or_traits parent_pos class_nast acc parent_type =
  let env, extends, is_complete, is_trait = acc in
  let class_pos = fst class_nast.c_name in
  let class_kind = class_nast.c_kind in
  if not is_trait
  then check_extend_kind parent_pos parent_type.tc_kind class_pos class_kind;
  let extends = SSet.union extends parent_type.tc_extends in
  env, extends, parent_type.tc_members_fully_known && is_complete, is_trait

let get_class_parent_or_trait class_nast (env, parents, is_complete, is_trait) hint =
  let parent_pos, parent, _ = desugar_class_hint hint in
  let parents = SSet.add parent parents in
  let parent_type = Env.get_class_dep env parent in
  match parent_type with
  | None ->
      (* The class lives in PHP *)
      env, parents, false, is_trait
  | Some parent_type ->
      (* The parent class lives in Hack *)
      let acc = env, parents, is_complete, is_trait in
      add_grand_parents_or_traits parent_pos class_nast acc parent_type

let get_class_parents_and_traits env class_nast =
  let parents = SSet.empty in
  let is_complete = true in
  (* extends parents *)
  let acc = env, parents, is_complete, false in
  let env, parents, is_complete, _ =
    List.fold_left (get_class_parent_or_trait class_nast) acc class_nast.c_extends in
  (* traits *)
  let acc = env, parents, is_complete, true in
  let env, parents, is_complete, _ =
    List.fold_left (get_class_parent_or_trait class_nast) acc class_nast.c_uses in
  (* XHP classes whose attributes were imported via "attribute :foo;" syntax *)
  let acc = env, parents, is_complete, true in
  let env, parents, is_complete, _ =
    List.fold_left (get_class_parent_or_trait class_nast) acc class_nast.c_xhp_attr_uses in
  env, parents, is_complete

let merge_single_req env subst inc_req_ty existing_req_opt
    incoming_pos =
  match existing_req_opt with
    | Some ex_req_ty ->
      (* If multiple uses/impls require the *exact same* ancestor, ... *)
      let env, inc_req_ty = Inst.instantiate subst env inc_req_ty in
      (* ... ensure that they're compatible and select
       * the one that's more restrictive (subtype of the other) *)
      let env, result_ty = Errors.try_
        (fun () ->
          let env = Typing_ops.sub_type_decl incoming_pos
            Reason.URclass_req_merge env ex_req_ty inc_req_ty
          in env, inc_req_ty)
        (fun _ ->
          let env = Typing_ops.sub_type_decl incoming_pos
            Reason.URclass_req_merge env inc_req_ty ex_req_ty
          in env, ex_req_ty
        )
      in
      (env : Env.env), (result_ty: decl ty)
    | None ->
      let env, inc_req_ty = Inst.instantiate subst env inc_req_ty in
      (env : Env.env), (inc_req_ty: decl ty)

(* for non-traits, check that requirements inherited from
 * traits/interfaces have been satisfied; for traits/interfaces,
 * accumulate requirements so that we can successfully check the bodies
 * of trait methods *)
let merge_parent_class_reqs class_nast impls
    (env, req_ancestors, req_ancestors_extends) parent_hint =
  let parent_pos, parent_name, parent_params = desugar_class_hint parent_hint in
  let env, parent_params = lfold Typing_hint.hint env parent_params in
  let parent_type = Env.get_class_dep env parent_name in

  match parent_type with
    | None ->
      (* The class lives in PHP *)
      env, req_ancestors, req_ancestors_extends
    | Some parent_type ->
      let subst =
        make_substitution parent_pos parent_name parent_type parent_params in
      match class_nast.c_kind with
        | Ast.Cnormal | Ast.Cabstract ->
          (* Check inherited requirements and check their compatibility *)
          let env = SMap.fold begin fun req_name req_ty env ->
            match SMap.get req_name impls with
              | None ->
                let req_pos = Reason.to_pos (fst req_ty) in
                Errors.unsatisfied_req parent_pos req_name req_pos;
                env
              | Some impl_ty ->
                let env, req_ty = Inst.instantiate subst env req_ty in
                Typing_ops.sub_type_decl parent_pos Reason.URclass_req env req_ty impl_ty
          end parent_type.tc_req_ancestors env
          in
          env, req_ancestors, req_ancestors_extends

        | Ast.Ctrait | Ast.Cinterface ->
          (* Merge together requirements and make sure they're compatible *)
          let acc = env, req_ancestors in
          let env, req_ancestors =
            SMap.fold (begin fun req_name added_req_ty acc ->
              let env, existing_reqs = acc in
              let ex_ty_opt = SMap.get req_name existing_reqs in
              let env, merged_ty = (merge_single_req env subst
                added_req_ty ex_ty_opt parent_pos) in
              env, SMap.add req_name merged_ty existing_reqs
            end) parent_type.tc_req_ancestors acc
          in
          let req_ancestors_extends =
            SSet.union parent_type.tc_req_ancestors_extends req_ancestors_extends in
          env, req_ancestors, req_ancestors_extends
        | Ast.Cenum -> assert false

let declared_class_req class_nast impls (env, requirements, req_extends) hint =
  let env, req_ty = Typing_hint.hint env hint in
  let req_pos, req_name, req_params = desugar_class_hint hint in
  let env, _ = lfold Typing_hint.hint env req_params in
  let req_type = Env.get_class_dep env req_name in

  (* for concrete classes, check required ancestors against actual
   * ancestors; for traits and interfaces, the required extends classes
   * are only going to be present in the ancestors of
   * implementing/using classes, so there's nothing to do *)
  let env = match class_nast.c_kind with
    | Ast.Ctrait | Ast.Cinterface | Ast.Cenum -> env
    | Ast.Cnormal | Ast.Cabstract ->
      (match SMap.get req_name impls with
        | None ->
          Errors.unsatisfied_req req_pos req_name req_pos; env
        | Some impl_ty ->
          (* Due to checking of incompatibility when accumulating
           * requirements, subtype violations in this case might not
           * actually be possible *)
          Typing_ops.sub_type_decl req_pos Reason.URclass_req env req_ty impl_ty
      )
  in

  let req_extends = SSet.add req_name req_extends in
  match req_type with
    | None -> (* The class lives in PHP : error?? *)
      let requirements = SMap.add req_name req_ty requirements in
      env, requirements, req_extends
    | Some parent_type -> (* The parent class lives in Hack *)

      (* since the req is declared on this class, we should
       * emphatically *not* substitute: a require extends Foo<T> is
       * going to be this class's <T> *)
      let subst = Inst.make_subst [] [] in
      let ex_ty_opt = SMap.get req_name requirements in
      let env, merged = merge_single_req env subst
        req_ty ex_ty_opt req_pos in
      let requirements = SMap.add req_name merged requirements in

      let req_extends = SSet.union parent_type.tc_extends req_extends in
      (* the req may be of an interface that has reqs of its own; the
       * flattened ancestry required by *those* reqs need to be added
       * in to, e.g., interpret accesses to protected functions inside
       * traits *)
      let req_extends =
        SSet.union parent_type.tc_req_ancestors_extends req_extends in
      env, requirements, req_extends

let get_class_requirements env class_nast impls =
  let req_ancestors = SMap.empty in
  let req_ancestors_extends = SSet.empty in
  let acc = (env, req_ancestors, req_ancestors_extends) in
  let acc =
    List.fold_left (declared_class_req class_nast impls)
      acc class_nast.c_req_extends in
  let acc =
    List.fold_left (declared_class_req class_nast impls)
      acc class_nast.c_req_implements in
  let acc =
    List.fold_left (merge_parent_class_reqs class_nast impls)
      acc class_nast.c_uses in
  let acc =
    List.fold_left (merge_parent_class_reqs class_nast impls)
      acc (if class_nast.c_kind == Ast.Cinterface then
          class_nast.c_extends else class_nast.c_implements)
  in
  match class_nast.c_kind with
    | Ast.Ctrait | Ast.Cinterface ->
      (* for a requirement-bearing construct, return the accumulated
       * list of direct and inherited requirements *)
      acc
    | Ast.Cnormal | Ast.Cabstract | Ast.Cenum ->
      (* for a non-requirement-bearing construct, requirements have
       * been checked, nothing to save *)
      env, SMap.empty, SSet.empty

(*****************************************************************************)
(* Section declaring the type of a function *)
(*****************************************************************************)

let ifun_decl nenv (f: Ast.fun_) =
  let f = Naming.fun_ nenv f in
  let cid = snd f.f_name in
  Naming_heap.FunHeap.add cid f;
  Typing.fun_decl nenv f;
  ()

(*****************************************************************************)
(* Section declaring the type of a class *)
(*****************************************************************************)

type class_env = {
  nenv: Naming.env;
  stack: SSet.t;
  all_classes: Relative_path.Set.t SMap.t;
}

let check_if_cyclic class_env (pos, cid) =
  let stack = class_env.stack in
  let is_cyclic = SSet.mem cid stack in
  if is_cyclic
  then Errors.cyclic_class_def stack pos;
  is_cyclic

let rec class_decl_if_missing_opt class_env = function
  | None -> ()
  | Some c -> class_decl_if_missing class_env c

and class_decl_if_missing class_env c =
  let _, cid as c_name = c.Ast.c_name in
  if check_if_cyclic class_env c_name
  then ()
  else begin
    if Naming_heap.ClassHeap.mem cid then () else
      class_naming_and_decl class_env cid c
  end

and class_naming_and_decl (class_env:class_env) cid c =
  let class_env = { class_env with stack = SSet.add cid class_env.stack } in
  let c = Naming.class_ class_env.nenv c in
  class_parents_decl class_env c;
  class_decl (Naming.typechecker_options class_env.nenv) c;
  (* It is important to add the "named" ast (nast.ml) only
   * AFTER we are done declaring the type type of the class.
   * Otherwise there is a subtle race condition.
   *
   * Worker 1: looks up class A. Sees that class A needs
   * to be recomputed, starts to recompute the type of A
   *
   * Worker 2: loops up class A, sees that the named Ast for
   * A is there, deduces that the type has already been computed
   * and could end up using an old version of the class type if
   * Worker 1 didn't finish.
   *
   * This race doesn't occur if we set the named Ast for class A
   * AFTER we are done declaring the type of A.
   * The worst case scenario is both workers recompute the same type
   * which is OK.
   *)
  Naming_heap.ClassHeap.add cid c;
  report_class_ready cid

and class_parents_decl class_env c =
  let class_hint = class_hint_decl class_env in
  List.iter class_hint c.c_extends;
  List.iter class_hint c.c_implements;
  List.iter class_hint c.c_uses;
  List.iter class_hint c.c_xhp_attr_uses;
  List.iter class_hint c.c_req_extends;
  List.iter class_hint c.c_req_implements;
  ()

and class_hint_decl class_env hint =
  match hint with
    | _, Happly ((_, cid), _)
      when SMap.mem cid class_env.all_classes && not (is_class_ready cid) ->
      (* We are supposed to redeclare the class *)
      let files = SMap.find_unsafe cid class_env.all_classes in
      Relative_path.Set.iter begin fun fn ->
        let class_opt = Parser_heap.find_class_in_file fn cid in
        class_decl_if_missing_opt class_env class_opt
      end files
    | _ ->
      (* This class lives in PHP land *)
      ()

and class_is_abstract c =
  match c.c_kind with
    | Ast.Cabstract | Ast.Cinterface | Ast.Ctrait | Ast.Cenum -> true
    | _ -> false

and class_decl tcopt c =
  let is_abstract = class_is_abstract c in
  let cls_pos, cls_name = c.c_name in
  let env = Typing_env.empty tcopt (Pos.filename cls_pos) in
  let env = Env.set_mode env c.c_mode in
  let class_dep = Dep.Class cls_name in
  let env = Env.set_root env class_dep in
  let env, inherited = Typing_inherit.make env c in
  let props = inherited.Typing_inherit.ih_props in
  let env, props = List.fold_left (class_var_decl c) (env, props) c.c_vars in
  let m = inherited.Typing_inherit.ih_methods in
  let env, m = List.fold_left (method_decl_acc c) (env, m) c.c_methods in
  let consts = inherited.Typing_inherit.ih_consts in
  let env, consts =
    List.fold_left (class_const_decl c) (env, consts) c.c_consts in
  let consts = SMap.add SN.Members.mClass (class_class_decl c.c_name) consts in
  let typeconsts = inherited.Typing_inherit.ih_typeconsts in
  let env, typeconsts =
    List.fold_left (typeconst_decl c) (env, typeconsts) c.c_typeconsts in
  let sclass_var = static_class_var_decl c in
  let sprops = inherited.Typing_inherit.ih_sprops in
  let env, sprops = List.fold_left sclass_var (env, sprops) c.c_static_vars in
  let sm = inherited.Typing_inherit.ih_smethods in
  let env, sm = List.fold_left (method_decl_acc c) (env, sm) c.c_static_methods in
  SMap.iter (check_static_method m) sm;
  let parent_cstr = inherited.Typing_inherit.ih_cstr in
  let env, cstr = constructor_decl env parent_cstr c in
  let has_concrete_cstr = match (fst cstr) with
    | None
    | Some {ce_type = (_, Tfun ({ft_abstract = true; _})); _} -> false
    | _ -> true in
  let impl = c.c_extends @ c.c_implements @ c.c_uses in
  let env, impl = lmap Typing_hint.hint env impl in
  let impl = match SMap.get SN.Members.__toString m with
    | Some {ce_type = (_, Tfun ft); _} when cls_name <> SN.Classes.cStringish ->
      (* HHVM implicitly adds Stringish interface for every class/iface/trait
       * with a __toString method; "string" also implements this interface *)
      let pos = ft.ft_pos in
      let ty = (Reason.Rhint pos, Tapply ((pos, SN.Classes.cStringish), [])) in
      ty :: impl
    | _ -> impl
  in
  let env, impl = lfold get_implements env impl in
  let impl = List.fold_right (SMap.fold SMap.add) impl SMap.empty in
  let env, extends, ext_strict = get_class_parents_and_traits env c in
  let extends = if c.c_is_xhp
    then SSet.add "XHP" extends
    else extends
  in
  let env, req_ancestors, req_ancestors_extends =
    get_class_requirements env c impl in
  let env, m = if DynamicYield.is_dynamic_yield (snd c.c_name)
    then DynamicYield.clean_dynamic_yield env m
    else env, m in
  let dy_check = match c.c_kind with
    | Ast.Cenum -> false
    | Ast.Cabstract
    | Ast.Cnormal -> DynamicYield.contains_dynamic_yield extends
    | Ast.Cinterface
    | Ast.Ctrait ->
      (* NOTE: Only the DynamicYield trait should provide
       * IUseDynamicYield via implementation; all other traits should
       * use 'require implements IUseDynamicYield' *)
      (DynamicYield.implements_dynamic_yield_interface impl
       || DynamicYield.contains_dynamic_yield_interface req_ancestors_extends
       || DynamicYield.contains_dynamic_yield req_ancestors_extends)
  in
  let env, m = if dy_check
    then DynamicYield.decl env m
    else env, m
  in
  let ext_strict = List.fold_left (trait_exists env) ext_strict c.c_uses in
  let unsafe_xhp = TypecheckerOptions.unsafe_xhp tcopt in
  let not_strict_because_xhp = unsafe_xhp && c.c_is_xhp in
  if not ext_strict && not not_strict_because_xhp && (Env.is_strict env) then
    let p, name = c.c_name in
    Errors.strict_members_not_known p name
  else ();
  let ext_strict = if not_strict_because_xhp then false else ext_strict in
  let env, tparams = lfold Typing.type_param env (fst c.c_tparams) in
  let env, enum = match c.c_enum with
    | None -> env, None
    | Some e ->
      let env, base_hint = Typing_hint.hint env e.e_base in
      let env, constraint_hint = opt Typing_hint.hint env e.e_constraint in
      env, Some
        { te_base       = base_hint;
          te_constraint = constraint_hint } in
  let consts = Typing_enum.enum_class_decl_rewrite c.c_name enum impl consts in
  let has_own_cstr = has_concrete_cstr && (None <> c.c_constructor) in
  let deferred_members = NastInitCheck.class_decl ~has_own_cstr env c in
  let tc = {
    tc_final = c.c_final;
    tc_abstract = is_abstract;
    tc_need_init = has_concrete_cstr;
    tc_deferred_init_members = deferred_members;
    tc_members_fully_known = ext_strict;
    tc_kind = c.c_kind;
    tc_name = snd c.c_name;
    tc_pos = fst c.c_name;
    tc_tparams = tparams;
    tc_consts = consts;
    tc_typeconsts = typeconsts;
    tc_props = props;
    tc_sprops = sprops;
    tc_methods = m;
    tc_smethods = sm;
    tc_construct = cstr;
    tc_ancestors = impl;
    tc_extends = extends;
    tc_req_ancestors = req_ancestors;
    tc_req_ancestors_extends = req_ancestors_extends;
    tc_user_attributes = c.c_user_attributes;
    tc_enum_type = enum;
  } in
  if Ast.Cnormal = c.c_kind then
    begin
      SMap.iter (method_check_trait_overrides c) m;
      SMap.iter (method_check_trait_overrides c) sm;
    end
  else ();
  SMap.iter begin fun x _ ->
    Typing_deps.add_idep (Some class_dep) (Dep.Class x)
  end impl;
  Env.add_class (snd c.c_name) tc

and get_implements (env: Env.env) ht =
  let _r, (_p, c), paraml = Typing_hint.open_class_hint ht in
  let class_ = Env.get_class_dep env c in
  match class_ with
  | None ->
      (* The class lives in PHP land *)
      env, SMap.singleton c ht
  | Some class_ ->
      let subst = Inst.make_subst class_.tc_tparams paraml in
      let sub_implements =
        SMap.map
          (fun ty -> snd (Inst.instantiate subst env ty))
          class_.tc_ancestors
      in
      env, SMap.add c ht sub_implements

and trait_exists env acc trait =
  match trait with
    | (_, Happly ((_, trait), _)) ->
      let class_ = Env.get_class_dep env trait in
      (match class_ with
        | None -> false
        | Some _class -> acc
      )
    | _ -> false

and check_static_method obj method_name { ce_type = (reason_for_type, _); _ } =
  if SMap.mem method_name obj
  then begin
    let static_position = Reason.to_pos reason_for_type in
    let dyn_method = SMap.find_unsafe method_name obj in
    let dyn_position = Reason.to_pos (fst dyn_method.ce_type) in
    Errors.static_dynamic static_position dyn_position method_name
  end
  else ()

and constructor_decl env (pcstr, pconsist) class_ =
  (* constructors in children of class_ must be consistent? *)
  let cconsist = class_.c_final ||
    Attrs.mem
      SN.UserAttributes.uaConsistentConstruct
      class_.c_user_attributes in
  match class_.c_constructor, pcstr with
    | None, _ -> env, (pcstr, cconsist || pconsist)
    | Some method_, Some {ce_final = true; ce_type = (r, _); _ } ->
      Errors.override_final ~parent:(Reason.to_pos r) ~child:(fst method_.m_name);
      let env, (cstr, mconsist) = build_constructor env class_ method_ in
      env, (cstr, cconsist || mconsist || pconsist)
    | Some method_, _ ->
      let env, (cstr, mconsist) = build_constructor env class_ method_ in
      env, (cstr, cconsist || mconsist || pconsist)

and build_constructor env class_ method_ =
  let env, ty = method_decl env method_ in
  let _, class_name = class_.c_name in
  let vis = visibility class_name method_.m_visibility in
  let mconsist = method_.m_final || class_.c_kind == Ast.Cinterface in
  (* due to the requirement of calling parent::__construct, a private
   * constructor cannot be overridden *)
  let mconsist = mconsist || method_.m_visibility == Private in
  let mconsist = match ty with
    | (_, Tfun ({ft_abstract = true; _})) -> true
    | _ -> mconsist in
  (* the alternative to overriding
   * UserAttributes.uaConsistentConstruct is marking the corresponding
   * 'new static()' UNSAFE, potentially impacting the safety of a large
   * type hierarchy. *)
  let consist_override =
    Attrs.mem SN.UserAttributes.uaUnsafeConstruct method_.m_user_attributes in
  let cstr = {
    ce_final = method_.m_final;
    ce_is_xhp_attr = false;
    ce_override = consist_override;
    ce_synthesized = false;
    ce_visibility = vis;
    ce_type = ty;
    ce_origin = class_name;
  } in
  env, (Some cstr, mconsist)

and class_const_decl c (env, acc) (h, id, e) =
  let c_name = (snd c.c_name) in
  let env, ty =
    match h, e with
      | Some h, Some _ -> Typing_hint.hint env h
      | Some h, None ->
        let env, h_ty = Typing_hint.hint env h in
        let pos, name = id in
        env, (Reason.Rwitness pos,
          Tgeneric (c_name^"::"^name, Some (Ast.Constraint_as, h_ty)))
      | None, Some e -> begin
        let rec infer_const (p, expr_) = match expr_ with
          | String _
          | String2 ([], _) -> Reason.Rwitness p, Tprim Tstring
          | True
          | False -> Reason.Rwitness p, Tprim Tbool
          | Int _ -> Reason.Rwitness p, Tprim Tint
          | Float _ -> Reason.Rwitness p, Tprim Tfloat
          | Unop ((Ast.Uminus | Ast.Uplus | Ast.Utild | Ast.Unot), e2) ->
            infer_const e2
          | _ ->
            (* We can't infer the type of everything here. Notably, if you
             * define a const in terms of another const, we need an annotation,
             * since the other const may not have been declared yet.
             *
             * Also note that a number of expressions are considered invalid
             * as constant initializers, even if we can infer their type; see
             * Naming.check_constant_expr. *)
            if c.c_mode = FileInfo.Mstrict && c.c_kind <> Ast.Cenum
            then Errors.missing_typehint (fst id);
            Reason.Rwitness (fst id), Tany
        in
        (env, infer_const e)
      end
      | None, None ->
        let pos, name = id in
        if c.c_mode = FileInfo.Mstrict then Errors.missing_typehint pos;
        let r = Reason.Rwitness pos in
        let const_ty = r, Tgeneric (c_name^"::"^name,
          Some (Ast.Constraint_as, (r, Tany))) in
        env, const_ty
  in
  let ce = { ce_final = true; ce_is_xhp_attr = false; ce_override = false;
             ce_synthesized = false; ce_visibility = Vpublic; ce_type = ty;
             ce_origin = c_name;
           } in
  let acc = SMap.add (snd id) ce acc in
  env, acc

(* Every class, interface, and trait implicitly defines a ::class to
 * allow accessing its fully qualified name as a string *)
and class_class_decl class_id =
  let pos, name = class_id in
  let reason = Reason.Rclass_class (pos, name) in
  let classname_ty = (reason, Tprim (Tclassname name)) in
  {
    ce_final       = false;
    ce_is_xhp_attr = false;
    ce_override    = false;
    ce_synthesized = true;
    ce_visibility  = Vpublic;
    ce_type        = classname_ty;
    ce_origin      = name;
  }

and class_var_decl c (env, acc) cv =
  let env, ty = match cv.cv_type with
    | None -> env, (Reason.Rwitness (fst cv.cv_id), Tany)
    | Some ty' when cv.cv_is_xhp ->
      (* If this is an XHP attribute and we're in strict mode,
         relax to partial mode to allow the use of the "array"
         annotation without specifying type parameters. Until
         recently HHVM did not allow "array" with type parameters
         in XHP attribute declarations, so this is a temporary
         hack to support existing code for now. *)
      (* Task #5815945: Get rid of this Hack *)
      let env = if (Env.is_strict env)
        then Env.set_mode env FileInfo.Mpartial
        else env
      in
      Typing_hint.hint env ty'
    | Some ty' -> Typing_hint.hint env ty'
  in
  let id = snd cv.cv_id in
  let vis = visibility (snd c.c_name) cv.cv_visibility in
  let ce = {
    ce_final = true; ce_is_xhp_attr = cv.cv_is_xhp; ce_override = false;
    ce_synthesized = false; ce_visibility = vis; ce_type = ty;
    ce_origin = (snd c.c_name);
  } in
  let acc = SMap.add id ce acc in
  env, acc

and static_class_var_decl c (env, acc) cv =
  let env, ty = match cv.cv_type with
    | None -> env, (Reason.Rwitness (fst cv.cv_id), Tany)
    | Some ty -> Typing_hint.hint env ty in
  let id = snd cv.cv_id in
  let vis = visibility (snd c.c_name) cv.cv_visibility in
  let ce = { ce_final = true; ce_is_xhp_attr = cv.cv_is_xhp; ce_override = false;
             ce_synthesized = false; ce_visibility = vis; ce_type = ty;
             ce_origin = (snd c.c_name);
           } in
  let acc = SMap.add ("$"^id) ce acc in
  if cv.cv_expr = None && FileInfo.(c.c_mode = Mstrict || c.c_mode = Mpartial)
  then begin match cv.cv_type with
    | None
    | Some (_, Hmixed)
    | Some (_, Hoption _) -> ()
    | _ -> Errors.missing_assign (fst cv.cv_id)
  end;
  env, acc

and visibility cid = function
  | Public    -> Vpublic
  | Protected -> Vprotected cid
  | Private   -> Vprivate cid

and typeconst_decl c (env, acc) {
  c_tconst_name = (pos, name);
  c_tconst_constraint = constr;
  c_tconst_type = type_;
} =
  match c.c_kind with
  | Ast.Ctrait | Ast.Cenum ->
      let kind = match c.c_kind with
        | Ast.Ctrait -> `trait
        | Ast.Cenum -> `enum
        | _ -> assert false in
      Errors.cannot_declare_constant kind pos c.c_name;
      env, acc
  | Ast.Cinterface | Ast.Cabstract | Ast.Cnormal ->
      let env, constr = opt Typing_hint.hint env constr in
      let env, ty = opt Typing_hint.hint env type_ in
      let tc = {
        ttc_name = (pos, name);
        ttc_constraint = constr;
        ttc_type = ty;
        ttc_origin = snd c.c_name;
      } in
      let acc = SMap.add name tc acc in
      env, acc

and method_decl env m =
  let env, arity_min, params = Typing.make_params env true 0 m.m_params in
  let env, ret = match m.m_ret with
    | None -> env, Typing.ret_from_fun_kind (fst m.m_name) m.m_fun_kind
    | Some ret -> Typing_hint.hint env ret in
  let env, arity = match m.m_variadic with
    | FVvariadicArg param ->
      assert param.param_is_variadic;
      assert (param.param_expr = None);
      let r = Reason.Rvar_param (fst param.param_id) in
      let env, (p_name, p_ty) = Typing.make_param_ty env r param in
      env, Fvariadic (arity_min, (p_name, p_ty))
    | FVellipsis    -> env, Fellipsis arity_min
    | FVnonVariadic -> env, Fstandard (arity_min, List.length m.m_params)
  in
  let env, tparams = lfold Typing.type_param env m.m_tparams in
  let ft = {
    ft_pos      = fst m.m_name;
    ft_deprecated =
      Attrs.deprecated ~kind:"method" m.m_name m.m_user_attributes;
    ft_abstract = m.m_abstract;
    ft_arity    = arity;
    ft_tparams  = tparams;
    ft_params   = params;
    ft_ret      = ret;
  } in
  let ty = Reason.Rwitness (fst m.m_name), Tfun ft in
  env, ty

and method_check_override c m acc =
  let pos, id = m.m_name in
  let _, class_id = c.c_name in
  let override = Attrs.mem SN.UserAttributes.uaOverride m.m_user_attributes in
  if m.m_visibility = Private && override then
    Errors.private_override pos class_id id;
  match SMap.get id acc with
    | Some { ce_final = is_final; ce_type = (r, _); _ } ->
      if is_final then
        Errors.override_final ~parent:(Reason.to_pos r) ~child:pos;
      false
    | None when override && c.c_kind = Ast.Ctrait -> true
    | None when override ->
      Errors.should_be_override pos class_id id;
      false
    | None -> false

and method_decl_acc c (env, acc) m =
  let check_override = method_check_override c m acc in
  let env, ty = method_decl env m in
  let _, id = m.m_name in
  let vis =
    match SMap.get id acc, m.m_visibility with
      | Some { ce_visibility = Vprotected _ as parent_vis; _ }, Protected ->
        parent_vis
      | _ -> visibility (snd c.c_name) m.m_visibility
  in
  let ce = {
    ce_final = m.m_final; ce_is_xhp_attr = false; ce_override = check_override;
    ce_synthesized = false; ce_visibility = vis; ce_type = ty;
    ce_origin = snd (c.c_name);
  } in
  let acc = SMap.add id ce acc in
  env, acc

and method_check_trait_overrides c id method_ce =
  if method_ce.ce_override then
    Errors.override_per_trait
      c.c_name id (Reason.to_pos (fst method_ce.ce_type))

(*****************************************************************************)
(* Dealing with typedefs *)
(*****************************************************************************)

let rec type_typedef_decl_if_missing nenv typedef =
  let _, tid = typedef.Ast.t_id in
  if Naming_heap.TypedefHeap.mem tid
  then ()
  else
    type_typedef_naming_and_decl nenv typedef

and type_typedef_naming_and_decl nenv tdef =
  let pos, tid = tdef.Ast.t_id in
  let is_abstract =
    match tdef.Ast.t_kind with
    | Ast.Alias _ -> false
    | Ast.NewType _ -> true
  in let {
    t_tparams = params;
    t_constraint = tcstr;
    t_kind = concrete_type;
    t_user_attributes = _;
  } as decl = Naming.typedef nenv tdef in
  let filename = Pos.filename pos in
  let tcopt = Naming.typechecker_options nenv in
  let env = Typing_env.empty tcopt filename in
  let env = Typing_env.set_mode env tdef.Ast.t_mode in
  let env = Env.set_root env (Typing_deps.Dep.Class tid) in
  let env, params = lfold Typing.type_param env params in
  let env, concrete_type = Typing_hint.hint env concrete_type in
  let _env, tcstr =
    match tcstr with
    | None -> env, None
    | Some constraint_type ->
      let env, constraint_type = Typing_hint.hint env constraint_type in
      let sub_type = Typing_ops.sub_type_decl pos Reason.URnewtype_cstr in
      let env = sub_type env constraint_type concrete_type in
      env, Some constraint_type
  in
  let visibility = if is_abstract
    then Typing_heap.Typedef.Private
    else Typing_heap.Typedef.Public in
  let tdecl = visibility, params, tcstr, concrete_type, pos in
  Env.add_typedef tid tdecl;
  Naming_heap.TypedefHeap.add tid decl;
  ()

(*****************************************************************************)
(* Global constants *)
(*****************************************************************************)

let iconst_decl nenv cst =
  let cst = Naming.global_const nenv cst in
  let _cst_pos, cst_name = cst.cst_name in
  Naming_heap.ConstHeap.add cst_name cst;
  Typing.gconst_decl (Naming.typechecker_options nenv) cst;
  ()

(*****************************************************************************)

let name_and_declare_types_program nenv all_classes prog =
  List.iter begin fun def ->
    match def with
    | Ast.Namespace _
    | Ast.NamespaceUse _ -> assert false
    | Ast.Fun f -> ifun_decl nenv f
    | Ast.Class c ->
      let class_env = {
        nenv = nenv;
        stack = SSet.empty;
        all_classes = all_classes;
      } in
      class_decl_if_missing class_env c
    | Ast.Typedef typedef ->
      type_typedef_decl_if_missing nenv typedef
    | Ast.Stmt _ -> ()
    | Ast.Constant cst ->
        iconst_decl nenv cst
  end prog

let make_env nenv all_classes fn =
  match Parser_heap.ParserHeap.get fn with
  | None -> ()
  | Some prog -> name_and_declare_types_program nenv all_classes prog
