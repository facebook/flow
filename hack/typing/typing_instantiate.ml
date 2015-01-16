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

module Env    = Typing_env
module SN     = Naming_special_names
module TUtils = Typing_utils

type env   = Env.env
type subst = ty SMap.t

(*****************************************************************************)
(* Builds a substitution out of a list of type parameters and a list of types.
 *
 * Typical use-case:
 *   class Y<T> { ... }
 *   class X extends Y<int>
 *
 * To build the type of X, we need to replace all the occurrences of T in Y by
 * int. The function make_subst, builds the substitution (the map associating
 * types to a type parameter name), in this case, it would build the map(T =>
 * int).
 *)
(*****************************************************************************)

let rec make_subst tparams tyl =
  (* We tolerate missing types in silent_mode. When that happens, we bind
   * all the parameters we can, and bind the remaining ones to "Tany".
   *)
  let subst = ref SMap.empty in
  let tyl = ref tyl in
  List.iter (make_subst_tparam subst tyl) tparams;
  !subst

and make_subst_with_this ~this tparams tyl =
  make_subst ((Ast.Invariant, (Pos.none, SN.Typehints.this), None)::tparams)
    (this::tyl)

and make_subst_tparam subst tyl (_, (_, tparam_name), _) =
  let ty =
    match !tyl with
    | [] -> Reason.Rnone, Tany
    | ty :: rl -> tyl := rl; ty
  in
  subst := SMap.add tparam_name ty !subst

(*****************************************************************************)
(* Code dealing with instantiation. *)
(*****************************************************************************)

let rec instantiate_fun env fty el =
  let env, efty = Env.expand_type env fty in
  match efty with
  | r, Tfun ft ->
      (* TODO: this is a horrible hack, instantiating a function should not
       * require the arguments (el).
       *)
      let env, ft = Typing_exts.retype_magic_func env ft el in
      let env, ft = instantiate_ft env ft in
      let fty = r, Tfun ft in
      env, fty
  | r, Tapply ((_, x), argl) when Typing_env.is_typedef x ->
      let env, fty = TUtils.expand_typedef env r x argl in
      instantiate_fun env fty el
  | _, (Tany | Tmixed | Tarray (_, _) | Tprim _ | Tgeneric (_, _) | Toption _
    | Tvar _ | Tabstract (_, _, _) | Tapply (_, _) | Ttuple _ | Tanon (_, _)
    | Tunresolved _ | Tobject | Tshape _ | Taccess (_, _, _)) -> env, fty

and instantiate_ft env ft =
  let env, tvarl = List.fold_left begin fun (env, vars) (_, (pos, _), _) ->
    (* Set the instantiated type parameter to initially point to unresolved, so
     * that it can grow and eventually be a subtype of something like "mixed".
     *)
    let r = Reason.Rwitness pos in
    let env, var = TUtils.in_var env (r, Tunresolved []) in
    env, var :: vars
  end (env, []) ft.ft_tparams in
  let subst = make_subst ft.ft_tparams tvarl in
  let names, params = List.split ft.ft_params in
  let env, params = lfold (instantiate subst) env params in
  let env, arity = match ft.ft_arity with
    | Fvariadic (min, (name, var_ty)) ->
      let env, var_ty = instantiate subst env var_ty in
      env, Fvariadic (min, (name, var_ty))
    | _ -> env, ft.ft_arity
  in
  let env, ret = instantiate subst env ft.ft_ret in
  let params = List.map2 (fun x y -> x, y) names params in
  env, { ft with ft_arity = arity; ft_params = params; ft_ret = ret }

and check_constraint env ty x_ty =
  let env, ety = Env.expand_type env ty in
  let env, ex_ty = Env.expand_type env x_ty in
  match snd ety, snd ex_ty with
  | _, Tany ->
      (* This branch is only reached when we have an unbound type variable,
       * when this is the case, the constraint should always succeed.
       *)
      env
  | Tany, _ -> fst (TUtils.unify env ty x_ty)
  | (Tmixed | Tarray (_, _) | Tprim _ | Tgeneric (_, _) | Toption _ | Tvar _
    | Tabstract (_, _, _) | Tapply (_, _) | Ttuple _ | Tanon (_, _) | Tfun _
    | Tunresolved _ | Tobject | Tshape _
    | Taccess _), _ -> TUtils.sub_type env ty x_ty

and instantiate subst env (r, ty) =
  match ty with
  | Tgeneric (x, ty_opt) ->
      (match SMap.get x subst with
      | Some x_ty ->
          let env =
            match ty_opt with
            | None -> env
            | Some ty ->
                let env, ty = instantiate subst env ty in
                (* Once the typing environment is "fully" solved, we check
                 * the constraints on generics
                 *)
                Env.add_todo env begin fun env ->
                  Errors.try_
                    (fun () -> check_constraint env ty x_ty)
                    (fun l ->
                      Reason.explain_generic_constraint r x l;
                      env
                    )
                end
          in
          env, (Reason.Rinstantiate (fst x_ty, x, r), snd x_ty)
      | None ->
          match ty_opt with
          | None -> env, (r, ty)
          | Some ty ->
              let env, ty = instantiate subst env ty in
              env, (r, Tgeneric (x, Some ty))
      )
  | Tany | Tmixed | Tarray (_, _) | Tprim _ | Toption _ | Tvar _
  | Tabstract (_, _, _) | Tapply (_, _) | Ttuple _ | Tanon (_, _) | Tfun _
  | Tunresolved _ | Tobject | Tshape _ | Taccess (_, _, _) ->
      let env, ty = instantiate_ subst env ty in
      env, (r, ty)

and instantiate_ subst env = function
  | Tgeneric _ -> assert false
  (* IMPORTANT: We cannot expand Taccess during instantiation because this can
   * be called before all type consts have been declared and inherited
   *)
  | Taccess (rt, id, idl) ->
      (* We resolve static with the "this" type if it is available. This is
       * necessary because the type may appear outside the context of a class
       * such as
       *
       * class A { type Foo = int; function getFoo(): static::Foo {}}
       *
       * function foo(A $x): A::Foo { return $x->getFoo(); }
       *
       * Without doing this, $x->getFoo() would have type "static::Foo" which
       * cannot be resolved. However when $x is instantiated "this" will be
       * substitued for "A" and we can use this to resolve "static::Foo" to
       * "A::Foo".
       *
       * Note: This does not handle all cases where we need to resolve "static".
       * This serves only as a way of boot strapping Taccess expansion when we
       * need to resolve "static" outside the scope of the class where the type
       * was declared. See Typing_taccess for how we resolve "static" in other
       * cases.
       *)
      let rt =
        match SMap.get "this" subst with
        | Some (_, Tapply(static, _)) when rt = SCIstatic -> SCI static
        | _ -> rt
      in
      env, Taccess(rt, id, idl)
  | Tanon _ as x -> env, x
  | Tarray (ty1, ty2) ->
      let env, ty1 = opt (instantiate subst) env ty1 in
      let env, ty2 = opt (instantiate subst) env ty2 in
      env, Tarray (ty1, ty2)
  | Tmixed -> env, Tmixed
  | Tvar n ->
      let env, ty = Env.get_type env n in
      let n' = Env.fresh() in
      let env = Env.rename env n n' in
      let env, ty = instantiate subst env ty in
      let env = Env.add env n' ty in
      env, Tvar n'
  | Tany
  | Tprim _ as x -> env, x
  | Ttuple tyl ->
      let env, tyl = lfold (instantiate subst) env tyl in
      env, Ttuple tyl
  | Tunresolved tyl ->
      let env, tyl = lfold (instantiate subst) env tyl in
      env, Tunresolved tyl
  | Toption ty ->
      let env, ty = instantiate subst env ty in
      (* we want to avoid double option: ??T *)
      if TUtils.is_option env ty
      then env, snd ty
      else env, Toption ty
  | Tfun ft ->
      let subst = List.fold_left begin fun subst (_, (_, x), _) ->
        SMap.remove x subst
      end subst ft.ft_tparams in
      let names, params = List.split ft.ft_params in
      let env, params = lfold (instantiate subst) env params in
      let env, arity = match ft.ft_arity with
        | Fvariadic (min, (name, var_ty)) ->
          let env, var_ty = instantiate subst env var_ty in
          env, Fvariadic (min, (name, var_ty))
        | _ -> env, ft.ft_arity
      in
      let env, ret = instantiate subst env ft.ft_ret in
      let params = List.map2 (fun x y -> x, y) names params in
      env, Tfun { ft with ft_arity = arity; ft_params = params; ft_ret = ret }
  | Tabstract (x, tyl, tcstr) ->
      let env, tcstr =
        match tcstr with
        | None ->
            env, None
        | Some ty ->
            let env, ty = instantiate subst env ty in
            env, Some ty
      in
      let env, tyl = lfold (instantiate subst) env tyl in
      env, Tabstract (x, tyl, tcstr)
  | Tapply (x, tyl) ->
      let env, tyl = lfold (instantiate subst) env tyl in
      env, Tapply (x, tyl)
  | Tobject -> env, Tobject
  | Tshape fdm ->
      let env, fdm = Nast.ShapeMap.map_env (instantiate subst) env fdm in
      env, Tshape fdm

and instantiate_ce subst env ({ ce_type = x; _ } as ce) =
  let env, x = instantiate subst env x in
  env, { ce with ce_type = x }

let instantiate_this env ty this_ty =
  let subst = make_subst_with_this this_ty [] [] in
  instantiate subst env ty
