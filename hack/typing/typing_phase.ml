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
open Typing_defs
open Typing_dependent_type
open Utils

module Env = Typing_env
module TUtils = Typing_utils
module TSubst = Typing_subst
module ShapeMap = Nast.ShapeMap

(* Here is the general problem the delayed application of the phase solves.
 * Let's say you have a function that you want to operate generically across
 * phases. In most cases when you do this you can use the 'ty' GADT and locally
 * abstract types to write code in a phase agonistic way.
 *
 *  let yell_any: type a. a ty -> string = fun ty ->
 *    match ty with
 *    | _, Tany -> "Any"
 *    | _ -> ""
 *
 * Now let's add a function that works for all phases, but whose logic is phase
 * dependent. For this we can use 'phase_ty' ADT:
 *
 *  let yell_locl phase_ty =
 *     match phase_ty with
 *     | DeclTy ty -> ""
 *     | LoclTy ty -> "Locl"
 *
 * Let's say you want to write a function that has behavior that works across
 * phases, but needs to invoke a function that is phase dependent. Our options
 * are as follows.
 *
 *  let yell_any_or_locl phase_ty =
 *    let ans = yell_locl phase_ty in
 *    match phase_ty with
 *    | DeclTy ty -> ans ^ (yell_any ty)
 *    | LoclTy ty -> ans ^ (yell_any ty)
 *
 * This would lead to code duplication since we can't generically operate on the
 * underlying 'ty' GADT. If we want to eliminate this code duplication there are
 * two options.
 *
 *  let generic_ty: type a. phase_ty -> a ty = function
 *    | DeclTy ty -> ty
 *    | LoclTy ty -> ty
 *
 *  let yell_any_or_locl phase_ty =
 *    let ans = yell_locl phase_ty in
 *    ans ^ (yell_any (generic_ty phase_ty))
 *
 * generic_ty allows us to extract a generic value which we can use. This
 * approach is limiting because we lose all information about what phase 'a ty
 * is.
 *
 * The other approach is to pass in a function that goes from 'a ty -> phase_ty'
 *
 *  let yell_any_or_locl phase ty =
 *    let ans = yell_locl (phase ty) in
 *    ans ^ (yell_any ty)
 *
 * Here we can use 'ty' generically (without losing information about what phase
 * 'a ty' is), and we rely on the caller passing in an appropriate function that
 * converts into the 'phase_ty' when we need to hop into phase specific code.
 *)
type 'a t = 'a ty -> phase_ty
let decl ty = DeclTy ty
let locl ty = LoclTy ty

type env = expand_env

(*****************************************************************************)
(* Transforms a declaration phase type into a localized type. This performs
 * common operations that are necessary for this operation, specifically:
 *   > Expand newtype/types
 *   > Resolves the "this" type
 *   > Instantiate generics
 *   > ...
 *
 * When keep track of additional information while localizing a type such as
 * what type defs were expanded to detect potentially recursive definitions..
 *)
(*****************************************************************************)

let rec localize_with_env ~ety_env env (dty: decl ty) =
  match dty with
  | _, (Tany | Tmixed | Tprim _ ) as x -> env, (ety_env, x)
  | r, Tthis ->
      let ty = match ety_env.this_ty with
        | Reason.Rnone, ty -> r, ty
        | Reason.Rexpr_dep_type (_, pos, s), ty ->
            Reason.Rexpr_dep_type (r, pos, s), ty
        | reason, ty when ety_env.from_class <> None -> reason, ty
        | reason, ty ->
            Reason.Rinstantiate (reason, SN.Typehints.this, r), ty in
      let ty =
        match ety_env.from_class with
        | Some cid -> ExprDepTy.make env cid ty
        | _ -> ty in
      env, (ety_env, ty)
  | r, Tarray (ty1, ty2) ->
      let env, ty = match ty1, ty2 with
        | None, None -> env, Tarraykind AKany
        | Some tv, None ->
            let env, tv = localize ~ety_env env tv in
            env, Tarraykind (AKvec tv)
        | Some tk, Some tv ->
            let env, tk = localize ~ety_env env tk in
            let env, tv = localize ~ety_env env tv in
            env, Tarraykind (AKmap (tk, tv))
        | None, Some _ ->
            failwith "Invalid array declaration type" in
      env, (ety_env, (r, ty))
  | r, Tgeneric (x, cstr_opt) ->
      (match SMap.get x ety_env.substs with
      | Some x_ty ->
          let env =
            match cstr_opt with
            | Some (ck, ty) ->
                let env, ty = localize ~ety_env env ty in
                TSubst.add_check_constraint_todo env r x ck ty x_ty
            | None -> env
          in
          env, (ety_env, (Reason.Rinstantiate (fst x_ty, x, r), snd x_ty))
      | None ->
          (match cstr_opt with
          | None ->
              env, (ety_env, (r, Tabstract (AKgeneric (x, None), None)))
          | Some (Ast.Constraint_as, ty) ->
              let env, ty = localize ~ety_env env ty in
              env, (ety_env, (r, Tabstract (AKgeneric (x, None), Some ty)))
          | Some (Ast.Constraint_super, ty) ->
              let env, ty = localize ~ety_env env ty in
              env, (ety_env, (r, Tabstract (AKgeneric (x, Some ty), None)))
          )
      )
  | r, Toption ty ->
       let env, ty = localize ~ety_env env ty in
       let ty_ =
         if TUtils.is_option env ty then
           snd ty
         else
           Toption ty in
       env, (ety_env, (r, ty_))
  | r, Tfun ft ->
      let env, ft = localize_ft ~ety_env env ft in
      env, (ety_env, (r, Tfun ft))
  | r, Tapply ((_, x), argl) when Env.is_typedef x ->
      let env, argl = List.map_env env argl (localize ~ety_env) in
      TUtils.expand_typedef ety_env env r x argl
  | r, Tapply ((p, x), _argl) when Env.is_enum x ->
      (* if argl <> [], nastInitCheck would have raised an error *)
      if Typing_defs.has_expanded ety_env x then begin
        Errors.cyclic_enum_constraint p;
        env, (ety_env, (r, Tany))
      end else begin
        let type_expansions = (p, x) :: ety_env.type_expansions in
        let ety_env = {ety_env with type_expansions} in
        let env, cstr =
          opt (localize ~ety_env) env (Env.get_enum_constraint x) in
        env, (ety_env, (r, Tabstract (AKenum x, cstr)))
      end
  | r, Tapply (cls, tyl) ->
      let env, tyl = List.map_env env tyl (localize ~ety_env) in
      env, (ety_env, (r, Tclass (cls, tyl)))
  | r, Ttuple tyl ->
      let env, tyl = List.map_env env tyl (localize ~ety_env) in
      env, (ety_env, (r, Ttuple tyl))
  | r, Taccess (root_ty, ids) ->
      let env, root_ty = localize ~ety_env env root_ty in
      TUtils.expand_typeconst ety_env env r root_ty ids
  | r, Tshape (fields_known, tym) ->
      let env, tym = ShapeMap.map_env (localize ~ety_env) env tym in
      env, (ety_env, (r, Tshape (fields_known, tym)))

and localize ~ety_env env ty =
  let env, (_, ty) = localize_with_env ~ety_env env ty in
  env, ty

(* For the majority of cases when we localize a function type we instantiate
 * the function's type parameters to be a Tunresolved wrapped in a Tvar so the
 * type can grow. There is ONLY ONE case where we do not do this, in
 * Typing_subtype.subtype_method. See the comment for that function for why
 * this is necessary.
 *)
and localize_ft ?(instantiate_tparams=true) ~ety_env env ft =
  (* Set the instantiated type parameter to initially point to unresolved, so
   * that it can grow and eventually be a subtype of something like "mixed".
   *)
  let env, substs =
    if instantiate_tparams
    then let env, tvarl =
      List.map_env env ft.ft_tparams TUtils.unresolved_tparam in
      let ft_subst = TSubst.make ft.ft_tparams tvarl in
      env, SMap.union ft_subst ety_env.substs
    else env, List.fold_left ft.ft_tparams ~f:begin fun subst (_, (_, x), _) ->
      SMap.remove x subst
    end ~init:ety_env.substs in
  let ety_env = {ety_env with substs = substs} in
  let env, params = List.map_env env ft.ft_params begin fun env (name, param) ->
    let env, param = localize ~ety_env env param in
    env, (name, param)
  end in
  let env, arity = match ft.ft_arity with
    | Fvariadic (min, (name, var_ty)) ->
       let env, var_ty = localize ~ety_env env var_ty in
       env, Fvariadic (min, (name, var_ty))
    | Fellipsis _ | Fstandard (_, _) as x -> env, x in
  let env, ret = localize ~ety_env env ft.ft_ret in
  env, { ft with ft_arity = arity; ft_params = params; ft_ret = ret }

let env_with_self env =
  {
    type_expansions = [];
    substs = SMap.empty;
    this_ty = Reason.none, TUtils.this_of (Env.get_self env);
    from_class = None;
  }

(* Performs no substitutions of generics and initializes Tthis to
 * Env.get_self env
 *)
let localize_with_self env ty =
  localize env ty ~ety_env:(env_with_self env)

let unify_decl env ty1 ty2 =
  let env, ty1 = localize_with_self env ty1 in
  let env, ty2 = localize_with_self env ty2 in
  TUtils.unify env ty1 ty2

let sub_type_decl env ty1 ty2 =
  let env, ty1 = localize_with_self env ty1 in
  let env, ty2 = localize_with_self env ty2 in
  TUtils.sub_type env ty1 ty2
