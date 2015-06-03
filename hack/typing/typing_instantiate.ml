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
module TSubst = Typing_subst
module TUtils = Typing_utils
module Phase  = Typing_phase

type env   = Env.env
type 'a subst = 'a Phase.t * 'a ty SMap.t

let make_subst ~phase tparams tyl =
  phase, TSubst.make tparams tyl

(*****************************************************************************)
(* Code dealing with instantiation. *)
(*****************************************************************************)

let rec instantiate: type a. a subst -> env -> a ty -> env * a ty =
  fun (phase, subst as phase_subst) env (r, ty) ->
  (* PERF: If subst is empty then instantiation is a no-op. We can save a
   * significant amount of CPU by avoiding recursively deconstructing the ty
   * data type.
   *)
  if SMap.is_empty subst then env, (r, ty) else
  match ty with
  | Tgeneric (x, cstr_opt) ->
      (match SMap.get x subst with
      | Some x_ty ->
          let env =
            (* Once the typing environment is "fully" solved, we
               check the constraints on generics *)
            match cstr_opt with
            | Some (ck, ty) ->
                let env, ty = instantiate phase_subst env ty in
                let ety_env = Phase.env_with_self env in
                let env, ty = Phase.localize_phase ~ety_env env (phase ty) in
                let env, x_ty =
                  Phase.localize_phase ~ety_env env (phase x_ty) in
                TSubst.add_check_constraint_todo env r x ck ty x_ty
            | None -> env
          in
          env, (Reason.Rinstantiate (fst x_ty, x, r), snd x_ty)
      | None -> begin
          match cstr_opt with
          | Some (ck, ty) ->
              let env, ty = instantiate phase_subst env ty in
              env, (r, Tgeneric (x, Some (ck, ty)))
          | None -> env, (r, ty)
        end
      )
  | _ ->
      let env, ty = instantiate_ phase_subst env ty in
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
  | Tthis -> env, Tthis
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
      let subst = fst subst, List.fold_left begin fun subst (_, (_, x), _) ->
        SMap.remove x subst
      end (snd subst) ft.ft_tparams in
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
