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
module Phase = Typing_phase

type env   = Env.env
type 'a subst = ('a Phase.t * 'a ty) SMap.t

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

let rec make_subst ~phase tparams tyl =
  (* We tolerate missing types in silent_mode. When that happens, we bind
   * all the parameters we can, and bind the remaining ones to "Tany".
   *)
  let subst = ref SMap.empty in
  let tyl = ref tyl in
  List.iter (make_subst_tparam ~phase subst tyl) tparams;
  !subst

and make_subst_with_this ~phase ~this tparams tyl =
  make_subst ~phase
    ((Ast.Invariant, (Pos.none, SN.Typehints.this), None)::tparams)
    (this::tyl)

and make_subst_tparam ~phase subst tyl (_, (_, tparam_name), _) =
  let ty =
    match !tyl with
    | [] -> Reason.Rnone, Tany
    | ty :: rl -> tyl := rl; ty
  in
  subst := SMap.add tparam_name (phase, ty) !subst

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
  | _, (Tany | Tmixed | Tarray (_, _) | Tprim _ | Tgeneric (_, _) | Toption _
    | Tvar _ | Tabstract (_, _, _) | Tclass (_, _) | Ttuple _ | Tanon (_, _)
    | Tunresolved _ | Tobject | Tshape _ | Taccess (_, _)) -> env, fty

and instantiate_ft env ft =
  let env, tvarl = List.fold_left begin fun (env, vars) (_, (pos, _), _) ->
    (* Set the instantiated type parameter to initially point to unresolved, so
     * that it can grow and eventually be a subtype of something like "mixed".
     *)
    let r = Reason.Rwitness pos in
    let env, var = TUtils.in_var env (r, Tunresolved []) in
    env, var :: vars
  end (env, []) ft.ft_tparams in
  let subst = make_subst Phase.locl ft.ft_tparams tvarl in
  let names, params = List.split ft.ft_params in
  let env, params = lfold (instantiate subst) env params in
  let env, arity = match ft.ft_arity with
    | Fvariadic (min, (name, var_ty)) ->
      let env, var_ty = instantiate subst env var_ty in
      env, Fvariadic (min, (name, var_ty))
    | Fellipsis x -> env, Fellipsis x
    | Fstandard (x, y) -> env, Fstandard (x, y)
  in
  let env, ret = instantiate subst env ft.ft_ret in
  let params = List.map2 (fun x y -> x, y) names params in
  env, { ft with ft_arity = arity; ft_params = params; ft_ret = ret }

and check_constraint env ck cstr_ty ty =
  let env, ty = Phase.localize_phase env ty in
  let env, cstr_ty = Phase.localize_phase env cstr_ty in
  let env, ety = Env.expand_type env ty in
  let env, ecstr_ty = Env.expand_type env cstr_ty in
  match snd ecstr_ty, snd ety with
  | _, Tany ->
      (* This branch is only reached when we have an unbound type variable,
       * when this is the case, the constraint should always succeed.
       *)
      env
  | Tany, _ -> fst (TUtils.unify env cstr_ty ty)
  | (Tmixed | Tarray (_, _) | Tprim _ | Tgeneric (_, _) | Toption _ | Tvar _
    | Tabstract (_, _, _) | Tclass (_, _) | Ttuple _ | Tanon (_, _) | Tfun _
    | Tunresolved _ | Tobject | Tshape _
    | Taccess _), _ -> begin
        match ck with
        | Ast.Constraint_as ->
            TUtils.sub_type env cstr_ty ty
        | Ast.Constraint_super ->
            (* invert_grow_super is intentionally not used here. Consider
             * the following:
             *
             * class A {}
             * class B extends A {}
             * class C extends A {}
             * class Foo<T> {
             *   public function bar<Tu super T>(Tu $x): Tu {}
             * }
             * function f(Foo<C> $x, B $y): A {
             *   return $x->bar($y);
             * }
             *
             * C is not a supertype of B. However, this doesn't mean the
             * constraint is violated: All's good if Tu = A. Figuring out the
             * most specific supertype is expensive, though, so we just put the
             * constraint into a Tunresolved. (Tunresolved only grows if
             * grow_super is true.) The return type hint provides the
             * supertype we want, and we just have to check that the hint is
             * consistent with all the types in the Tunresolved.
             *
             * Note that since Tu = mixed is always a solution, a `super`
             * constraint itself should never create a type error. That is,
             * it should only result in the type growing as a Tunresolved.
             * Errors will only arise when we encounter conflicting type hints.
             * Thus we ensure that ty is wrapped in a Tunresolved here.
             *)
            let env = match ty, ety with
              | (_, Tvar _), (_, Tunresolved _) -> env
              | (_, Tvar n), (r, _) ->
                  let ety = r, Tunresolved [ety] in
                  let env = Env.add env n ety in
                  env
              | _ ->
                  (* I don't think ty will ever be anything but a Tvar...
                   * might be able to assert false here *)
                  env in
            (* If cstr_ty is a Tvar, we don't want to unify that Tvar with
             * ty; we merely want the constraint itself to be added to the
             * ty's list of unresolved types. Thus we pass the expanded
             * constraint type. *)
            TUtils.sub_type env ty ecstr_ty
      end

and instantiate: type a. a subst -> env -> a ty -> env * a ty =
  fun subst env (r, ty) ->
  (* PERF: If subst is empty then instantiation is a no-op. We can save a
   * significant amount of CPU by avoiding recursively deconstructing the ty
   * data type.
   *)
  if SMap.is_empty subst then env, (r, ty) else
  match ty with
  | Tgeneric (x, cstr_opt) ->
      (match SMap.get x subst with
      | Some (phase, x_ty) ->
          let env =
            (* Once the typing environment is "fully" solved, we
               check the constraints on generics *)
            match cstr_opt with
            | Some (ck, ty) ->
                let env, ty = instantiate subst env ty in
                Env.add_todo env begin fun env ->
                  Errors.try_
                    (fun () -> check_constraint env ck (phase ty) (phase x_ty))
                    (fun l ->
                      Reason.explain_generic_constraint env.Env.pos r x l;
                      env
                    )
                end
            | None -> env
          in
          env, (Reason.Rinstantiate (fst x_ty, x, r), snd x_ty)
      | None -> begin
          match cstr_opt with
          | Some (ck, ty) ->
              let env, ty = instantiate subst env ty in
              env, (r, Tgeneric (x, Some (ck, ty)))
          | None -> env, (r, ty)
        end
      )
  | _ ->
      let env, ty = instantiate_ subst env ty in
      env, (r, ty)

and instantiate_: type a. a subst -> env -> a ty_ -> env * a ty_ =
  fun subst env x -> match x with
  | Tgeneric _ -> assert false
  (* IMPORTANT: We cannot expand Taccess during instantiation because this can
   * be called before all type consts have been declared and inherited
   *)
  | Taccess (ty, ids) ->
      let env, ty = instantiate subst env ty in
      env, Taccess (ty, ids)
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
        | Fellipsis _ | Fstandard _ as x -> env, x
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
  | Tclass (x, tyl) ->
     let env, tyl = lfold (instantiate subst) env tyl in
     env, Tclass (x, tyl)
  | Tapply (x, tyl) ->
      let env, tyl = lfold (instantiate subst) env tyl in
      env, Tapply (x, tyl)
  | Tobject -> env, Tobject
  | Tshape fdm ->
      let env, fdm = Nast.ShapeMap.map_env (instantiate subst) env fdm in
      env, Tshape fdm

let instantiate_ce subst env ({ ce_type = x; _ } as ce) =
  let env, x = instantiate subst env x in
  env, { ce with ce_type = x }

let instantiate_typeconst subst env (
  { ttc_constraint = x; ttc_type = y; _ } as tc) =
    let env, x = opt (instantiate subst) env x in
    let env, y = opt (instantiate subst) env y in
    env, { tc with ttc_constraint = x; ttc_type = y }

let instantiate_this ~phase env ty this_ty =
  let subst = make_subst_with_this ~phase ~this:this_ty [] [] in
  instantiate subst env ty
