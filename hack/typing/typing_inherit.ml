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
(* Module dealing with inheritance.
 * When we want to declare a new class, we first have to retrieve all the
 * types that were inherited from their parents.
 *)
(*****************************************************************************)

open Utils
open Nast
open Typing_defs

module Env = Typing_env
module Inst = Typing_instantiate
module TUtils = Typing_utils
module Phase = Typing_phase

type env = Env.env

(*****************************************************************************)
(* This is what we are trying to produce for a given class. *)
(*****************************************************************************)

type inherited = {
  ih_cstr     : class_elt option * bool (* consistency required *);
  ih_consts   : class_elt SMap.t ;
  ih_typeconsts : typeconst_type SMap.t ;
  ih_props    : class_elt SMap.t ;
  ih_sprops   : class_elt SMap.t ;
  ih_methods  : class_elt SMap.t ;
  ih_smethods : class_elt SMap.t ;
}

let empty = {
  ih_cstr     = None, false;
  ih_consts   = SMap.empty;
  ih_typeconsts = SMap.empty;
  ih_props    = SMap.empty;
  ih_sprops   = SMap.empty;
  ih_methods  = SMap.empty;
  ih_smethods = SMap.empty;
}

(*****************************************************************************)
(* Functions used to merge an additional inherited class to the types
 * we already inherited.
 *)
(*****************************************************************************)

let is_abstract_method x =
  match x.ce_type with
  | _, Tfun x when x.ft_abstract -> true
  | _ ->  false

let add_method name sig_ methods =
  match (fst sig_.ce_type), sig_.ce_synthesized with
    | Reason.Rdynamic_yield _, true ->
      (* DynamicYield::__call-derived pseudo-methods need to be
       * rederived at each level *)
      methods
    | _ ->
      (
        match SMap.get name methods with
          | None ->
            (* The method didn't exist so far, let's add it *)
            SMap.add name sig_ methods
          | Some old_sig ->
            if ((not (is_abstract_method old_sig) && is_abstract_method sig_)
                || (is_abstract_method old_sig = is_abstract_method sig_
                   && not (old_sig.ce_synthesized) && sig_.ce_synthesized))

            (* The then-branch of this if is encountered when the method being
             * added shouldn't *actually* be added. When's that?
             * In isolation, we can say that
             *   - We don't want to override a concrete method with
             *     an abstract one.
             *   - We don't want to override a method that's actually
             *     implemented by the programmer with one that's "synthetic",
             *     e.g. arising merely from a require-extends declaration in
             *     a trait.
             * When these two considerations conflict, we give precedence to
             * abstractness for determining priority of the method.
             *)
            then methods

            (* Otherwise, we *are* overwriting a method definition. This is
             * OK when a naming conflict is parent class vs trait (trait
             * wins!), but not really OK when the naming conflict is trait vs
             * trait (we rely on HHVM to catch the error at runtime) *)
            else SMap.add name {sig_ with ce_override = false} methods
      )

let add_methods methods' acc =
  SMap.fold add_method methods' acc

let add_members members acc =
  SMap.fold SMap.add members acc

let is_abstract_typeconst x = x.ttc_type = None

let can_override_typeconst x =
  (is_abstract_typeconst x) || x.ttc_constraint <> None

let add_typeconst name sig_ typeconsts =
  match SMap.get name typeconsts with
  | None ->
      (* The type constant didn't exist so far, let's add it *)
      SMap.add name sig_ typeconsts
  (* This covers the following case
   *
   * interface I1 { abstract const type T; }
   * interface I2 { const type T = int; }
   *
   * class C implements I1, I2 {}
   *
   * Then C::T == I2::T since I2::T is not abstract
   *)
  | Some old_sig
    when not (is_abstract_typeconst old_sig) && (is_abstract_typeconst sig_) ->
      typeconsts
  (* This covers the following case
   *
   * abstract P { const type T as arraykey = arraykey; }
   * interface I { const type T = int; }
   *
   * class C extends P implements I {}
   *
   * Then C::T == I::T since P::T has a constraint and thus can be overridden
   * by it's child, while I::T cannot be overridden.
   *)
  | Some old_sig
    when not (can_override_typeconst old_sig) && (can_override_typeconst sig_) ->
      typeconsts
  (* When a type constant is declared in multiple parents we need to make a
   * subtle choice of what type we inherit. For example in:
   *
   * interface I1 { abstract const type t as Container<int>; }
   * interface I2 { abstract const type t as KeyedContainer<int, int>; }
   * abstract class C implements I1, I2 {}
   *
   * Depending on the order the interfaces are declared, we may report an error.
   * Since this could be confusing there is special logic in Typing_extends that
   * checks for this potentially ambiguous situation and warns the programmer to
   * explicitly declare T in C.
   *)
  | _ ->
      SMap.add name sig_ typeconsts

let add_constructor (cstr, cstr_consist) (acc, acc_consist) =
  let ce = match cstr, acc with
    | None, _ -> acc
    | Some ce, Some acce when ce.ce_synthesized && not acce.ce_synthesized ->
      acc
    | _ -> cstr
  in ce, cstr_consist || acc_consist

let add_inherited inherited acc = {
  ih_cstr     = add_constructor inherited.ih_cstr acc.ih_cstr;
  ih_consts   = add_members inherited.ih_consts acc.ih_consts;
  ih_typeconsts =
    SMap.fold add_typeconst inherited.ih_typeconsts acc.ih_typeconsts;
  ih_props    = add_members inherited.ih_props acc.ih_props;
  ih_sprops   = add_members inherited.ih_sprops acc.ih_sprops;
  ih_methods  = add_methods inherited.ih_methods acc.ih_methods;
  ih_smethods = add_methods inherited.ih_smethods acc.ih_smethods;
}

(*****************************************************************************)
(* Helpers *)
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

let constructor env subst (cstr, consistent) = match cstr with
  | None -> env, (None, consistent)
  | Some ce ->
    let env, ty = Inst.instantiate subst env ce.ce_type in
    env, (Some {ce with ce_type = ty}, consistent)

let map_inherited f inh =
  {
    ih_cstr     = (opt_map f (fst inh.ih_cstr)), (snd inh.ih_cstr);
    ih_typeconsts = inh.ih_typeconsts;
    ih_consts   = SMap.map f inh.ih_consts;
    ih_props    = SMap.map f inh.ih_props;
    ih_sprops   = SMap.map f inh.ih_sprops;
    ih_methods  = SMap.map f inh.ih_methods;
    ih_smethods = SMap.map f inh.ih_smethods;
  }

(*****************************************************************************)
(* Code filtering the private members (useful for inheritance) *)
(*****************************************************************************)

let filter_private x =
  SMap.fold begin fun name class_elt acc ->
    match class_elt.ce_visibility with
    | Vprivate _ -> acc
    | Vpublic | Vprotected _ -> SMap.add name class_elt acc
  end x SMap.empty

let chown_private owner =
  SMap.map begin fun class_elt ->
    match class_elt.ce_visibility with
      | Vprivate _ -> {class_elt with ce_visibility = Vprivate owner}
      | _ -> class_elt end

let apply_fn_to_class_elts fn class_type = {
  class_type with
  tc_consts = fn class_type.tc_consts;
  tc_typeconsts = class_type.tc_typeconsts;
  tc_props = fn class_type.tc_props;
  tc_sprops = fn class_type.tc_sprops;
  tc_methods = fn class_type.tc_methods;
  tc_smethods = fn class_type.tc_smethods;
}

let filter_privates = apply_fn_to_class_elts filter_private
let chown_privates owner = apply_fn_to_class_elts (chown_private owner)

(*****************************************************************************)
(* Builds the inherited type when the class lives in Hack *)
(*****************************************************************************)

let inherit_hack_class c env p class_name class_type argl =
  let subst = make_substitution p class_name class_type argl in
  let instantiate = SMap.map_env (Inst.instantiate_ce subst) in
  let class_type =
    match class_type.tc_kind with
    | Ast.Ctrait ->
        (* Change the private visibility to point to the inheriting class *)
        chown_privates (snd c.c_name) class_type
    | Ast.Cnormal | Ast.Cabstract | Ast.Cinterface ->
        filter_privates class_type
    | Ast.Cenum -> class_type
  in
  let env, typeconsts = SMap.map_env (Inst.instantiate_typeconst subst)
    env class_type.tc_typeconsts in
  let env, consts   = instantiate env class_type.tc_consts in
  let env, props    = instantiate env class_type.tc_props in
  let env, sprops   = instantiate env class_type.tc_sprops in
  let env, methods  = instantiate env class_type.tc_methods in
  let env, smethods = instantiate env class_type.tc_smethods in
  let cstr          = Env.get_construct env class_type in
  let env, cstr     = constructor env subst cstr in
  let result = {
    ih_cstr     = cstr;
    ih_consts   = consts;
    ih_typeconsts = typeconsts;
    ih_props    = props;
    ih_sprops   = sprops;
    ih_methods  = methods;
    ih_smethods = smethods;
  } in
  env, result

(* mostly copy paste of inherit_hack_class *)
let inherit_hack_class_constants_only env p class_name class_type argl =
  let subst = make_substitution p class_name class_type argl in
  let instantiate = SMap.map_env (Inst.instantiate_ce subst) in
  let env, consts  = instantiate env class_type.tc_consts in
  let env, typeconsts = SMap.map_env (Inst.instantiate_typeconst subst)
    env class_type.tc_typeconsts in
  let result = { empty with
    ih_consts   = consts;
    ih_typeconsts = typeconsts;
  } in
  env, result

(* This logic deals with importing XHP attributes from an XHP class
   via the "attribute :foo;" syntax. *)
let inherit_hack_xhp_attrs_only env p class_name class_type argl =
  let subst = make_substitution p class_name class_type argl in
  (* Filter out properties that are not XHP attributes *)
  let props =
    SMap.fold begin fun name class_elt acc ->
      if class_elt.ce_is_xhp_attr then SMap.add name class_elt acc else acc
    end class_type.tc_props SMap.empty in
  let env, props = SMap.map_env (Inst.instantiate_ce subst) env props in
  let result = { empty with ih_props = props; } in
  env, result

(*****************************************************************************)

let from_class c env hint =
  let pos, class_name, class_params = desugar_class_hint hint in
  let env, class_params = lfold Typing_hint.hint env class_params in
  let class_type = Env.get_class_dep env class_name in
  match class_type with
  | None ->
      (* The class lives in PHP, we don't know anything about it *)
      env, empty
  | Some class_ ->
      (* The class lives in Hack *)
      inherit_hack_class c env pos class_name class_ class_params

(* mostly copy paste of from_class *)
let from_class_constants_only env hint =
  let pos, class_name, class_params = desugar_class_hint hint in
  let env, class_params = lfold Typing_hint.hint env class_params in
  let class_type = Env.get_class_dep env class_name in
  match class_type with
  | None ->
      (* The class lives in PHP, we don't know anything about it *)
      env, empty
  | Some class_ ->
      (* The class lives in Hack *)
    inherit_hack_class_constants_only env pos class_name class_ class_params

let from_class_xhp_attrs_only env hint =
  let pos, class_name, class_params = desugar_class_hint hint in
  let env, class_params = lfold Typing_hint.hint env class_params in
  let class_type = Env.get_class_dep env class_name in
  match class_type with
  | None ->
      (* The class lives in PHP, we don't know anything about it *)
      env, empty
  | Some class_ ->
      (* The class lives in Hack *)
      inherit_hack_xhp_attrs_only env pos class_name class_ class_params

let from_parent env c =
  let extends =
    (* In an abstract class or a trait, we assume the interfaces
     * will be implemented in the future, so we take them as
     * part of the class (as requested by dependency injection implementers)
     *)
    match c.c_kind with
      | Ast.Cabstract -> c.c_implements @ c.c_extends
      | Ast.Ctrait -> c.c_implements @ c.c_extends @ c.c_req_implements
      | _ -> c.c_extends
  in
  let env, inherited_l = lfold (from_class c) env extends in
  env, List.fold_right add_inherited inherited_l empty

let from_requirements c (env, acc) reqs =
  let env, inherited = from_class c env reqs in
  let inherited = map_inherited
    (fun ce -> { ce with ce_synthesized = true })
    inherited in
  env, add_inherited inherited acc

let from_trait c (env, acc) uses =
  let env, inherited = from_class c env uses in
  env, add_inherited inherited acc

let from_xhp_attr_use (env, acc) uses =
  let env, inherited = from_class_xhp_attrs_only env uses in
  env, add_inherited inherited acc

let from_interface_constants (env, acc) impls =
  let env, inherited = from_class_constants_only env impls in
  env, add_inherited inherited acc

(*****************************************************************************)
(* The API to the outside *)
(*****************************************************************************)

let make env c =
  (* members inherited from parent class ... *)
  let acc = from_parent env c in
  let acc = List.fold_left (from_requirements c) acc c.c_req_extends in
  (* ... are overridden with those inherited from used traits *)
  let acc = List.fold_left (from_trait c) acc c.c_uses in
  let acc = List.fold_left from_xhp_attr_use acc c.c_xhp_attr_uses in
  (* todo: what about the same constant defined in different interfaces
   * we implement? We should forbid and say "constant already defined".
   * to julien: where is the logic that check for duplicated things?
   * todo: improve constant handling, see task #2487051
   *)
  let acc = List.fold_left from_interface_constants acc c.c_req_implements in
  List.fold_left from_interface_constants acc c.c_implements
