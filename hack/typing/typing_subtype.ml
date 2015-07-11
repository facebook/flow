(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils
open Typing_defs
open Typing_dependent_type

module Reason = Typing_reason
module Inst = Typing_instantiate
module Unify = Typing_unify
module Env = Typing_env
module DefsDB = Typing_heap
module TDef = Typing_tdef
module TSubst = Typing_subst
module TUtils = Typing_utils
module TUEnv = Typing_unification_env
module ShapeMap = Nast.ShapeMap
module SN = Naming_special_names
module TAccess = Typing_taccess
module Phase = Typing_phase

(* This function checks that the method ft_sub can be used to replace
 * (is a subtype of) ft_super *)
let rec subtype_funs_generic ~check_return env r_super ft_super r_sub ft_sub =
  let p_sub = Reason.to_pos r_sub in
  let p_super = Reason.to_pos r_super in
  if (arity_min ft_sub.ft_arity) > (arity_min ft_super.ft_arity)
  then Errors.fun_too_many_args p_sub p_super;
  (match ft_sub.ft_arity, ft_super.ft_arity with
    | Fellipsis _, Fvariadic _ ->
      (* The HHVM runtime ignores "..." entirely, but knows about
       * "...$args"; for contexts for which the runtime enforces method
       * compatibility (currently, inheritance from abstract/interface
       * methods), letting "..." override "...$args" would result in method
       * compatibility errors at runtime. *)
      Errors.fun_variadicity_hh_vs_php56 p_sub p_super;
    | Fstandard (_, sub_max), Fstandard (_, super_max) ->
      if sub_max < super_max
      then Errors.fun_too_few_args p_sub p_super;
    | Fstandard _, _ -> Errors.fun_unexpected_nonvariadic p_sub p_super;
    | _, _ -> ()
  );

  (* We are dissallowing contravariant arguments, they are not supported
   * by the runtime *)
  (* However, if we are polymorphic in the upper-class we have to be
   * polymorphic in the subclass. *)
  let env, var_opt = match ft_sub.ft_arity, ft_super.ft_arity with
    | Fvariadic (_, (n_super, var_super)), Fvariadic (_, (_, var_sub)) ->
      let env, var = Unify.unify env var_super var_sub in
      env, Some (n_super, var)
    | _ -> env, None
  in
  let env, _ =
    Unify.unify_params env ft_super.ft_params ft_sub.ft_params var_opt in
  (* Checking that if the return type was defined in the parent class, it
   * is defined in the subclass too (requested by Gabe Levi).
   *)
  (* We agreed this was too painful for now, breaks too many things *)
  (*  (match ft_super.ft_ret, ft_sub.ft_ret with
      | (_, Tany), _ -> ()
      | (r_super, ty), (r_sub, Tany) ->
      let p_super = Reason.to_pos r_super in
      let p_sub = Reason.to_pos r_sub in
      error_l [p_sub, "Please add a return type";
      p_super, "Because we want to be consistent with this annotation"]
      | _ -> ()
      );
  *)
  let env = if check_return then sub_type env ft_super.ft_ret ft_sub.ft_ret else env in
  env

(* Checking subtyping for methods is different than normal functions. Since
 * methods are declarations we do not want to instantiate their function type
 * parameters as unresolved, instead it should stay as a Tgeneric.
 *)
and subtype_method ~check_return env r_super ft_super r_sub ft_sub =
  let ety_env = Phase.env_with_self env in
  let env, ft_super_no_tvars =
    Phase.localize_ft ~ety_env ~instantiate_tparams:false env ft_super in
  let env, ft_sub_no_tvars =
    Phase.localize_ft ~ety_env ~instantiate_tparams:false env ft_sub in
  subtype_funs_generic
    ~check_return env
    r_super ft_super_no_tvars
    r_sub ft_sub_no_tvars

and subtype_tparams env c_name variancel super_tyl children_tyl =
  match variancel, super_tyl, children_tyl with
  | [], [], [] -> env
  | [], _, _
  | _, [], _
  | _, _, [] -> env
  | variance :: variancel, super :: superl, child :: childrenl ->
      let env = subtype_tparam env c_name variance super child in
      subtype_tparams env c_name variancel superl childrenl

and subtype_tparam env c_name variance (r_super, _ as super) child =
  match variance with
  | Ast.Covariant -> sub_type env super child
  | Ast.Contravariant ->
      Errors.try_
        (fun () ->
          Env.invert_grow_super env (fun env -> sub_type env child super))
        (fun err ->
          let pos = Reason.to_pos r_super in
          Errors.explain_contravariance pos c_name err; env)
  | Ast.Invariant -> fst (Unify.unify env super child)

(* Distinction b/w sub_type and sub_type_with_uenv similar to unify and
 * unify_with_uenv, see comment there. *)
and sub_type env ty_super ty_sub =
  sub_type_with_uenv env (TUEnv.empty, ty_super) (TUEnv.empty, ty_sub)

and get_super_typevar_set_ env set ty_super =
  let env, ety_super = Env.expand_type env ty_super in
  match ety_super with
  | _, Tabstract (AKgeneric (x_super, super), _) ->
    let set = SSet.add x_super set in
    Option.value_map super ~f:(get_super_typevar_set_ env set) ~default:set
  | _ -> set

(* If ty_super is a typevar, this function returns a set of the names of all
 * typevars that are known to be subtypes of ty_super via "super" constraints,
 * including ty_super itself. If ty_super is not a typevar, this returns the
 * empty set. *)
and get_super_typevar_set env ty_super =
  get_super_typevar_set_ env SSet.empty ty_super

and match_typevars_ env super_typevar_set ty_sub =
  let env, ety_sub = Env.expand_type env ty_sub in
  match ety_sub with
  | _, Tabstract (AKgeneric (x_sub, _), cstr) ->
    if SSet.mem x_sub super_typevar_set then true else
    Option.value_map cstr
      ~f:(match_typevars_ env super_typevar_set)
      ~default:false
  | _ -> false

(* This function traverses over all the typevars known to be supertypes of
 * ty_sub via "as" constraints (including ty_sub itself), and returns true
 * if any of these typevars are in the set of typevars known to be subtypes
 * of ty_super (as computed by get_super_typevar_set). Otherwise, this
 * function returns false. *)
and match_typevars env ty_super ty_sub =
  match_typevars_ env (get_super_typevar_set env ty_super) ty_sub

and typevars_subtype_ env (uenv_super, ety_super) (uenv_sub, ety_sub) =
  match ety_super, ety_sub with
  | _, (r_sub, Tabstract (AKgeneric (x_sub, _), Some ty_sub)) ->
    Errors.try_
      (fun () ->
        let env, ety_sub = Env.expand_type env ty_sub in
        typevars_subtype_ env (uenv_super, ety_super) (uenv_sub, ety_sub))
      (fun l ->
        Reason.explain_generic_constraint env.Env.pos r_sub x_sub l; env)
  | (r_super, Tabstract (AKgeneric (x_super, Some ty_super), _)), _ ->
    Errors.try_
      (fun () ->
        let env, ety_super = Env.expand_type env ty_super in
        typevars_subtype_ env (uenv_super, ety_super) (uenv_sub, ety_sub))
      (fun l ->
        Reason.explain_generic_constraint env.Env.pos r_super x_super l; env)
  | _ ->
    sub_type_with_uenv env (uenv_super, ety_super) (uenv_sub, ety_sub)

(* Checks if one typevar is a subtype of another typevar. *)
and typevars_subtype env (uenv_super, ety_super) (uenv_sub, ety_sub) =
  (* First, check if there exists some typevar that is a subtype of
     ety_super (via "super" constraints) AND that is a supertype of ety_sub
     (via "as" constraints). If such a typevar exists, then ety_sub must be a
     subtype of ety_super. *)
  if match_typevars env ety_super ety_sub then env else
  (* Otherwise, traverse "super" constraints starting at ety_super,
     traverse "as" constraints starting at ety_sub, and then check if the
     latter is a subtype of the former. This logic is needed to support cases
     such as `Tu as C, Tv super C` when we're checking if Tu is a subtype of
     Tv.

     Note that `Tu as Tv super Tw as C` cannot be a subtype of `Tx super C`
     because `Tv` is not constrained in any way by `C`. Thus, if we encounter
     any `super` constraints in the subtype or `as` constraints in the
     supertype, it is safe to say that we have a type error. *)
  typevars_subtype_ env (uenv_super, ety_super) (uenv_sub, ety_sub)

(**
 * Checks that ty_sub is a subtype of ty_super, and returns an env.
 *
 * E.g. sub_type env ?int int   => env
 *      sub_type env int alpha  => env where alpha==int
 *      sub_type env ?int alpha => env where alpha==?int
 *      sub_type env int string => error
 *)
and sub_type_with_uenv env (uenv_super, ty_super) (uenv_sub, ty_sub) =
  let env, seen_tvars_super, ety_super =
    Env.expand_type_recorded env uenv_super.TUEnv.seen_tvars ty_super in
  let env, seen_tvars_sub, ety_sub =
    Env.expand_type_recorded env uenv_sub.TUEnv.seen_tvars ty_sub in
  (* UGLY: We don't update uenv_super with seen_tvars_super just yet because
   * sometimes we call sub_type_with_uenv recursively with the same ty_super
   * (i.e. ty_sub has changed, but ty_super remains the same). If we pass
   * through the updated uenv_super, we would not be able to expand ty_super
   * a second time.
   * The converse goes for seen_tvars_sub and ty_sub.
   * TODO: Right now we only update seen_tvars when recursing into
   * Tunresolveds and Toptions, because we are encountering actual code that
   * creates such recursive types. Should probably update seen_tvars regardless
   * of which types we are recursing into, or prove that recursive types can't
   * happen in those cases.
   * TODO: Figure out a nicer (type-enforced?) way to associate the right
   * uenv with the right type. *)
  match ety_super, ety_sub with
  | (_, Tunresolved _), (_, Tunresolved _) ->
      let env, _ =
        Unify.unify_with_uenv env (uenv_super, ty_super) (uenv_sub, ty_sub) in
      env
(****************************************************************************)
(* ### Begin Tunresolved madness ###
 * If grow_super is true (the common case), then if the supertype is a
 * Tunresolved, we allow it to keep growing, which is the desired behavior for
 * e.g. figuring out the type of a generic, but if the subtype is a
 * Tunresolved, then we check that all the members are indeed subtypes of the
 * given supertype, which is the desired behavior for e.g. checking function
 * return values. In general, if a supertype is Tunresolved, then we
 * consider it to be "not yet finalized", but if a subtype is Tunresolved
 * and the supertype isn't, we've probably hit a type annotation
 * and should consider the supertype to be definitive.
 *
 * However, sometimes we want this behavior reversed, e.g. when the type
 * annotation has a contravariant generic parameter or a `super` constraint --
 * now the definitive type is the subtype.
 *
 * I considered splitting this out into a separate function and swapping the
 * order of the super / sub types passed to it, so we would only have to handle
 * one set of cases, but it doesn't look much better since that function still
 * has to recursively call sub_type and therefore needs to remember whether its
 * arguments had been swapped.
 *)
(****************************************************************************)
  | (_, Tunresolved _), (r_sub, _) when Env.grow_super env ->
      let ty_sub = (r_sub, Tunresolved [ty_sub]) in
      let env, _ =
        Unify.unify_with_uenv env (uenv_super, ty_super) (uenv_sub, ty_sub) in
      env
  | (_, Tany), (_, Tunresolved _) when Env.grow_super env ->
      (* This branch is necessary in the following case:
       * function foo<T as I>(T $x)
       * if I call foo with an intersection type, T is a Tvar,
       * it's expanded version (ety_super in this case) is Tany and what
       * we end up doing is unifying all the elements of the intersection
       * together ...
       * Thanks to this branch, the type variable unifies with the intersection
       * type.
       *)
      fst (Unify.unify_with_uenv env (uenv_super, ty_super) (uenv_sub, ty_sub))
  | _, (_, Tunresolved tyl) when Env.grow_super env ->
      let uenv_sub = {uenv_sub with TUEnv.seen_tvars = seen_tvars_sub} in
      List.fold_left begin fun env x ->
        sub_type_with_uenv env (uenv_super, ty_super) (uenv_sub, x)
      end env tyl
(****************************************************************************)
(* Repeat the previous 3 cases but with the super / sub order reversed *)
(****************************************************************************)
  | (r_super, _), (_, Tunresolved _) when not (Env.grow_super env) ->
      let ty_super = (r_super, Tunresolved [ty_super]) in
      let env, _ =
        Unify.unify_with_uenv env (uenv_super, ty_super) (uenv_sub, ty_sub) in
      env
  | (_, Tunresolved _), (_, Tany) when not (Env.grow_super env) ->
      fst (Unify.unify_with_uenv env (uenv_super, ty_super) (uenv_sub, ty_sub))
  | (_, Tunresolved tyl), _ when not (Env.grow_super env) ->
      let uenv_super = {uenv_super with TUEnv.seen_tvars = seen_tvars_super} in
      List.fold_left begin fun env x ->
        sub_type_with_uenv env (uenv_super, x) (uenv_sub, ty_sub)
      end env tyl
(****************************************************************************)
(* OCaml doesn't inspect `when` clauses when checking pattern matching
 * exhaustiveness, so just assert false here *)
(****************************************************************************)
  | _, (_, Tunresolved _)
  | (_, Tunresolved _), _ -> assert false
(****************************************************************************)
(* ### End Tunresolved madness ### *)
(****************************************************************************)
  | (r1, Tabstract (AKdependent d1, Some ty_super)),
    (r2, Tabstract (AKdependent d2, Some ty_sub))
        when d1 = d2 ->
      let uenv_super =
        { uenv_super with
          TUEnv.dep_tys = (r1, d1)::uenv_super.TUEnv.dep_tys } in
      let uenv_sub =
        { uenv_sub with
          TUEnv.dep_tys = (r2, d2)::uenv_sub.TUEnv.dep_tys } in
      sub_type_with_uenv env (uenv_super, ty_super) (uenv_sub, ty_sub)
  (* This is sort of a hack because our handling of Toption is highly
   * dependent on how the type is structured. When we see a bare
   * dependent type we strip it off at this point since it shouldn't be
   * relevant to subtyping any more.
   *)
  | _, (r, Tabstract (AKdependent (`expr _, [] as d), Some ty_sub)) ->
      let uenv_sub =
        { uenv_sub with
          TUEnv.dep_tys = (r, d)::uenv_sub.TUEnv.dep_tys } in
      sub_type_with_uenv env (uenv_super, ty_super) (uenv_sub, ty_sub)
  | (_, Tabstract (AKdependent d1, _)),
    (r, Tabstract (AKdependent d2, Some sub)) when d1 <> d2 ->
      let uenv_sub =
        { uenv_sub with
          TUEnv.dep_tys = (r, d2)::uenv_sub.TUEnv.dep_tys } in
      (* If an error occurred while subtyping, we produce a unification error
       * so we get the full information on how the dependent type was
       * generated
       *)
      Errors.try_when
        (fun () ->
          sub_type_with_uenv env (uenv_super, ty_super) (uenv_sub, sub))
        ~when_: begin fun () ->
          match TUtils.get_base_type ty_super, sub with
          | (_, Tclass ((_, x), _)), (_, Tclass ((_, y), _)) when x = y -> false
          | _, _ -> true
        end
        ~do_: (fun _ -> TUtils.simplified_uerror env ty_super ty_sub)
  | (_, Tabstract (AKgeneric _, _)), (_, Tabstract (AKgeneric _, Some _))
  | (_, Tabstract (AKgeneric (_, Some _), _)),
      (_, Tabstract (AKgeneric _, _)) ->
      typevars_subtype env (uenv_super, ety_super) (uenv_sub, ety_sub)
  | (_, Tclass ((_, stringish), _)), (_, Tabstract (ak, _))
    when stringish = SN.Classes.cStringish &&
      AbstractKind.is_classname ak -> env
  | (p_super, (Tclass (x_super, tyl_super) as ty_super_)),
      (p_sub, (Tclass (x_sub, tyl_sub) as ty_sub_))
      when Typing_env.get_enum_constraint (snd x_sub) = None  ->
    let cid_super, cid_sub = (snd x_super), (snd x_sub) in
    if cid_super = cid_sub then
      if tyl_super <> [] && List.length tyl_super = List.length tyl_sub
      then
        match Env.get_class env cid_super with
        | None -> fst (Unify.unify env ety_super ety_sub)
        | Some { tc_tparams; _} ->
            let variancel =
              List.map (fun (variance, _, _) -> variance) tc_tparams
            in
            subtype_tparams env cid_super variancel tyl_super tyl_sub
      else fst (Unify.unify env ety_super ety_sub)
    else begin
      let class_ = Env.get_class env cid_sub in
      (match class_ with
        | None -> env
        | Some class_ ->
          let subtype_req_ancestor =
            if class_.tc_kind = Ast.Ctrait || class_.tc_kind = Ast.Cinterface then
              (* a trait is never the runtime type, but it can be used
               * as a constraint if it has requirements for its using
               * classes *)
              let _, ret = SMap.fold begin fun _ elt_type acc ->
                match acc with
                  | _, Some _ -> acc
                  | env, None ->
                    Errors.try_ begin fun () ->
                      let ety_env = {
                        type_expansions = [];
                        substs = SMap.empty;
                        this_ty = ExprDepTy.apply uenv_sub.TUEnv.dep_tys ty_sub;
                        from_class = None;
                      } in
                      let env, elt_type =
                        Phase.localize ~ety_env env elt_type in
                      let _, elt_ty = elt_type in
                      env, Some (sub_type env ty_super (p_sub, elt_ty))
                    end (fun _ -> acc)
              end class_.tc_req_ancestors (env, None) in
              ret
            else None in
          (match subtype_req_ancestor with
            | Some ret -> ret
            | None ->
              let up_obj = SMap.get cid_super class_.tc_ancestors in
              match up_obj with
                | Some up_obj ->
                  (* We handle the case where a generic A<T> is used as A *)
                  let tyl_sub =
                    if tyl_sub = [] && not (Env.is_strict env)
                    then List.map (fun _ -> (p_sub, Tany)) class_.tc_tparams
                    else tyl_sub
                  in
                  if List.length class_.tc_tparams <> List.length tyl_sub
                  then
                    Errors.expected_tparam
                      (Reason.to_pos p_sub) (List.length class_.tc_tparams);
                  (* NOTE: We rely on the fact that we fold all ancestors of
                   * ty_sub in its class_type so we will never hit this case
                   * again. If this ever changes then we would need to store
                   * ty_sub as the 'this_ty' in the uenv and be careful to
                   * thread it through.
                   *
                   * This is covered by test/typecheck/this_tparam2.php
                   *)
                  let ety_env = {
                    type_expansions = [];
                    substs = TSubst.make class_.tc_tparams tyl_sub;
                    this_ty = ExprDepTy.apply uenv_sub.TUEnv.dep_tys ty_sub;
                    from_class = None;
                  } in
                  let env, up_obj = Phase.localize ~ety_env env up_obj in
                  sub_type env ty_super up_obj
                | None when class_.tc_members_fully_known ->
                  TUtils.uerror p_super ty_super_ p_sub ty_sub_;
                  env
                | _ -> env
          )
      )
    end
  | (_, Tmixed), _ -> env
  | (_, Tprim Nast.Tnum), (_, Tprim (Nast.Tint | Nast.Tfloat)) -> env
  | (_, Tprim Nast.Tarraykey), (_, Tprim (Nast.Tint | Nast.Tstring | Nast.Tclassname _)) -> env
  | (_, Tprim Nast.Tstring), (_, Tprim (Nast.Tclassname _)) -> env
  | (_, Tprim (Nast.Tstring | Nast.Tarraykey)), (_, Tabstract (ak, _))
    when AbstractKind.is_classname ak -> env
  | (_, Tclass ((_, coll), [tv_super])), (_, Tarray (ty3, ty4))
    when (coll = SN.Collections.cTraversable ||
        coll = SN.Collections.cContainer) ->
      (match ty3, ty4 with
      | None, _ -> env
      | Some ty3, None ->
          sub_type env tv_super ty3
      | Some _ty3, Some ty4 ->
          sub_type env tv_super ty4
      )
  | (_, Tclass ((_, coll), [tk_super; tv_super])), (r, Tarray (ty3, ty4))
    when (coll = SN.Collections.cKeyedTraversable
         || coll = SN.Collections.cKeyedContainer
         || coll = SN.Collections.cIndexish) ->
      (match ty3 with
      | None -> env
      | Some ty3 ->
          (match ty4 with
          | None ->
              let env = sub_type env tk_super (r, Tprim Nast.Tint) in
              sub_type env tv_super ty3
          | Some ty4 ->
              let env = sub_type env tk_super ty3 in
              sub_type env tv_super ty4
          )
      )
  | (_, Tclass ((_, stringish), _)), (_, Tprim (Nast.Tstring | Nast.Tclassname _))
    when stringish = SN.Classes.cStringish -> env
  | (_, Tclass ((_, xhp_child), _)), (_, Tarray _)
  | (_, Tclass ((_, xhp_child), _)), (_, Tprim (Nast.Tint | Nast.Tfloat | Nast.Tstring | Nast.Tclassname _ | Nast.Tnum))
    when xhp_child = SN.Classes.cXHPChild -> env
  | (_, (Tarray (Some ty_super, None))), (_, (Tarray (Some ty_sub, None))) ->
      sub_type env ty_super ty_sub
  | (_, (Tarray (Some tk_super, Some tv_super))), (_, Tarray (Some tk_sub, Some tv_sub)) ->
      let env = sub_type env tk_super tk_sub in
      sub_type env tv_super tv_sub
  | (_, Tarray (Some _, Some _)), (reason, Tarray (Some elt_ty, None)) ->
      let int_reason = Reason.Ridx (Reason.to_pos reason) in
      let int_type = int_reason, Tprim Nast.Tint in
      sub_type env ty_super (reason, Tarray (Some int_type, Some elt_ty))
  | _, (_, Tany) -> env
  | (_, Tany), _ -> fst (Unify.unify env ty_super ty_sub)
    (* recording seen_tvars for Toption variants to avoid infinte recursion
       in case of type variable X = ?X *)
  | (_, Toption ty_super), _ when uenv_super.TUEnv.non_null ->
      let uenv_super = {uenv_super with TUEnv.seen_tvars = seen_tvars_super} in
      sub_type_with_uenv env (uenv_super, ty_super) (uenv_sub, ty_sub)
  | _, (_, Toption ty_sub) when uenv_sub.TUEnv.non_null ->
      let uenv_sub = {uenv_sub with TUEnv.seen_tvars = seen_tvars_sub} in
      sub_type_with_uenv env (uenv_super, ty_super) (uenv_sub, ty_sub)
  | (_, Toption ty_super), (_, Toption ty_sub) ->
      let uenv_super = {
        TUEnv.non_null = true;
        TUEnv.seen_tvars = seen_tvars_super;
        TUEnv.dep_tys = uenv_super.TUEnv.dep_tys;
      } in
      let uenv_sub = {
        TUEnv.non_null = true;
        TUEnv.seen_tvars = seen_tvars_sub;
        TUEnv.dep_tys = uenv_sub.TUEnv.dep_tys;
      } in
      sub_type_with_uenv env (uenv_super, ty_super) (uenv_sub, ty_sub)
  | (_, Toption ty_opt), _ ->
      let uenv_super = {
        TUEnv.non_null = true;
        TUEnv.seen_tvars = seen_tvars_super;
        TUEnv.dep_tys = uenv_super.TUEnv.dep_tys;
      } in
      sub_type_with_uenv env (uenv_super, ty_opt) (uenv_sub, ty_sub)
  | (_, Ttuple tyl_super), (_, Ttuple tyl_sub)
    when List.length tyl_super = List.length tyl_sub ->
    wfold_left2 sub_type env tyl_super tyl_sub
  | (r_super, Tfun ft_super), (r_sub, Tfun ft_sub) ->
      subtype_funs_generic ~check_return:true env r_super ft_super r_sub ft_sub
  | (r_super, Tfun ft), (r_sub, Tanon (anon_arity, id)) ->
      (match Env.get_anonymous env id with
      | None ->
          Errors.anonymous_recursive_call (Reason.to_pos r_sub);
          env
      | Some anon ->
          let p_super = Reason.to_pos r_super in
          let p_sub = Reason.to_pos r_sub in
          if not (Unify.unify_arities
                    ~ellipsis_is_variadic:true anon_arity ft.ft_arity)
          then Errors.fun_arity_mismatch p_super p_sub;
          let env, ret = anon env ft.ft_params in
          let env = sub_type env ft.ft_ret ret in
          env
      )
  | (r_super, Tshape (fields_known_super, fdm_super)),
      (r_sub, Tshape (fields_known_sub, fdm_sub)) ->
      fst (TUtils.apply_shape
        ~on_common_field:(fun (env, acc) _ x y -> sub_type env x y, acc)
        ~on_missing_optional_field:(fun acc _ _ -> acc)
        (env, None)
        (r_super, fields_known_super, fdm_super)
        (r_sub, fields_known_sub, fdm_sub))
  | (_, Tabstract (AKnewtype (name_classname, [tyl1]), _)),
    (r_sub, Tprim (Nast.Tclassname name_cls))
    when name_classname = SN.Classes.cClassname ->
    (* XXX: Do we need to look up the class and add missing Tanys? *)
    let p_sub = Reason.to_pos r_sub in
    let ty_cls = r_sub, Tclass ((p_sub, name_cls), []) in
    sub_type env tyl1 ty_cls
  | (_, Tabstract (AKnewtype (name_super, tyl_super), _)),
    (_, Tabstract (AKnewtype (name_sub, tyl_sub), _))
    when name_super = name_sub ->
      let td = Env.get_typedef env name_super in
      (match td with
      | Some (DefsDB.Typedef.Ok (_, tparams, _, _, _)) ->
          let variancel =
            List.map (fun (variance, _, _) -> variance) tparams
          in
          subtype_tparams env name_super variancel tyl_super tyl_sub
      | _ -> env
      )
  | _, (_, Tabstract (AKnewtype (_, _), Some x)) ->
      Errors.try_
        (fun () ->
          fst @@
            Unify.unify_with_uenv env (uenv_super, ty_super) (uenv_sub, ty_sub)
        )
        (fun _ -> sub_type_with_uenv env (uenv_super, ty_super) (uenv_sub, x))
  | _, (r, Tabstract (AKdependent d, Some ty)) ->
      Errors.try_
        (fun () -> fst (
          Unify.unify_with_uenv env (uenv_super, ty_super) (uenv_sub, ty_sub)))
        (fun _ ->
          let uenv_sub =
            { uenv_sub with
              TUEnv.dep_tys = (r, d)::uenv_sub.TUEnv.dep_tys } in
          sub_type_with_uenv env (uenv_super, ty_super) (uenv_sub, ty))
  (* Handle enums with subtyping constraints. *)
  | _, (_, (Tclass ((_, x), [])))
    when Typing_env.get_enum_constraint x <> None ->
    (match Typing_env.get_enum_constraint x with
      | Some base ->
        (* Handling is the same as abstracts with as *)
        Errors.try_
          (fun () -> fst (Unify.unify env ty_super ty_sub))
          (fun _ ->
           let ety_env = {
             type_expansions = [];
             substs = SMap.empty;
             this_ty = ExprDepTy.apply uenv_sub.TUEnv.dep_tys ty_sub;
             from_class = None;
           } in
           let env, base = Phase.localize ~ety_env env base in
           sub_type env ty_super base)
      | None -> assert false)
  (* If all else fails we fall back to the super/as constraint on a generics. *)
  | _, (r_sub, Tabstract (AKgeneric (x, _), Some ty_sub)) ->
      (Errors.try_
         (fun () ->
           sub_type_with_uenv env (uenv_super, ty_super) (uenv_sub, ty_sub))
         (fun l ->
           Reason.explain_generic_constraint env.Env.pos r_sub x l; env)
      )
  | (r_super, Tabstract (AKgeneric (x, Some ty), _)), _ ->
      (Errors.try_
         (fun () ->
           sub_type_with_uenv env (uenv_super, ty) (uenv_sub, ty_sub))
         (fun l ->
           Reason.explain_generic_constraint env.Env.pos r_super x l; env)
      )
  | (_, (Tarray (_, _) | Tprim _ | Tvar _
    | Tabstract (_, _) | Ttuple _ | Tanon (_, _) | Tfun _
    | Tobject | Tshape _ | Tclass (_, _))
    ), _ -> fst (Unify.unify env ty_super ty_sub)

and is_sub_type env ty_super ty_sub =
  Errors.try_
    (fun () -> ignore(sub_type env ty_super ty_sub); true)
    (fun _ -> false)

and sub_string p env ty2 =
  let env, ety2 = Env.expand_type env ty2 in
  match ety2 with
  | (_, Toption ty2) -> sub_string p env ty2
  | (_, Tunresolved tyl) ->
      List.fold_left (sub_string p) env tyl
  | (_, Tprim _) ->
      env
  | (_, Tabstract (ak, _)) when AbstractKind.is_classname ak ->
    (* A classname is the string 'Foo' obtained via Foo::class *)
    env
  | (_, Tabstract (_, Some ty)) ->
      sub_string p env ty
  | (r2, Tclass (x, _)) ->
      let class_ = Env.get_class env (snd x) in
      (match class_ with
      | None -> env
      | Some tc
          (* A Stringish is a string or an object with a __toString method
           * that will be converted to a string *)
          when tc.tc_name = SN.Classes.cStringish
          || SMap.mem SN.Classes.cStringish tc.tc_ancestors
          (* Apply enum means that we're dealing with enum values,
           * which are primitives (int/string) *)
          || tc.tc_kind = Ast.Cenum ->
        env
      | Some _ ->
        Errors.object_string p (Reason.to_pos r2);
        env
      )
  | _, Tany ->
    env (* Unifies with anything *)
  | _, Tobject -> env
  | _, (Tmixed | Tarray (_, _) | Tvar _ | Tabstract (_, _)
    | Ttuple _ | Tanon (_, _) | Tfun _ | Tshape _) ->
      fst (Unify.unify env (Reason.Rwitness p, Tprim Nast.Tstring) ty2)

(* and subtype_params env l_super l_sub def_super = *)
(*   match l_super, l_sub with *)
(*   | l, [] -> env, l *)
(*   | [], l -> *)
(*   | (name_super, x_super) :: rl_super, (name_sub, x_sub) :: rl_sub -> *)
(*     let name = if name_super = name_sub then name_super else None in *)
(*     let env = { env with Env.pos = Reason.to_pos (fst x_super) } in *)
(*     let env, _ = Unify.unify env x_super x_sub in *)
(*     let env, rl = Unify.unify_params env rl_super rl_sub in *)
(*     env, (name, x_sub) :: rl *)

(*****************************************************************************)
(* Exporting *)
(*****************************************************************************)

let () = Typing_utils.sub_type_ref := sub_type
