(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This module implements the typing for type_structure. *)
open Core
open Nast
open Typing_defs

module Env = Typing_env
module Phase = Typing_phase
module Reason = Typing_reason
module SN = Naming_special_names
module TSubst = Typing_subst
module TUtils = Typing_utils

let make_ts env ty =
  match Env.get_typedef env SN.FB.cTypeStructure with
  | Some {td_tparams; _} ->
      let params = List.map ~f:begin fun (_, (p, x), cstr) ->
        Reason.Rwitness p, Tgeneric (x, cstr)
      end td_tparams in
      let ts = fst ty, Tapply ((Pos.none, SN.FB.cTypeStructure), params) in
      let ety_env = { (Phase.env_with_self env) with
                      substs = TSubst.make td_tparams [ty] } in
      Phase.localize ~ety_env env ts
  | _ ->
      (* Should not hit this because TypeStructure should always be defined *)
      env, (fst ty, Tany)

let rec transform_shapemap ?(nullable = false) env ty shape =
  let env, ty = TUtils.fold_unresolved env ty in
  let env, ety = Env.expand_type env ty in
  (* If there are Tanys, be conservative and don't try to represent the
   * type more precisely
   *)
  if TUtils.HasTany.check ty then env, shape else
  match ety with
  | _, Toption ty ->
      transform_shapemap ~nullable:true env ty shape
  | _ ->
      (* If the abstract type is unbounded we do not specialize at all *)
      let is_unbound = match ety |> TUtils.get_base_type |> snd with
        (* An enum is considered a valid bound *)
        | Tabstract (AKenum _, _) -> false
        | Tabstract (_, None) -> true
        | _ -> false in
      if is_unbound then env, shape else
      let is_generic =
        match snd ety with Tabstract (AKgeneric _, _) -> true | _ -> false in
      ShapeMap.fold begin fun field field_ty (env, shape) ->
        match field, field_ty, TUtils.get_base_type ety with
        | Nast.SFlit (_, "nullable"), (_, Toption (fty)), _ when nullable ->
            env, ShapeMap.add field fty shape
        | Nast.SFlit (_, "nullable"), (_, Toption (fty)), (_, Toption _) ->
            env, ShapeMap.add field fty shape
        | Nast.SFlit (_, "classname"), (_, Toption (fty)),
          (_, (Tclass _ | Tabstract (AKenum _, _))) ->
            env, ShapeMap.add field fty shape
        | Nast.SFlit (_, "elem_types"), _, (r, Ttuple tyl) ->
            let env, tyl = List.map_env env tyl make_ts in
            env, ShapeMap.add field (r, Ttuple tyl) shape
        | Nast.SFlit (_, "param_types"), _, (r, (Tfun funty)) ->
            let tyl = List.map ~f:snd funty.ft_params in
            let env, tyl = List.map_env env tyl make_ts in
            env, ShapeMap.add field (r, Ttuple tyl) shape
        | Nast.SFlit (_, "return_type"), _, (r, Tfun funty) ->
            let env, ty = make_ts env funty.ft_ret in
            env, ShapeMap.add field (r, Ttuple [ty]) shape
        | Nast.SFlit (_, "fields"), _, (r, Tshape (fk, fields)) ->
            let env, fields = ShapeMap.map_env make_ts env fields in
            env, ShapeMap.add field (r, Tshape (fk, fields)) shape
        (* For generics we cannot specialize the generic_types field. Consider:
         *
         *  class C<T> {}
         *  class D extends C<int> {}
         *
         *  function test<T as C<int>>(TypeStructure<T> $ts): TypeStructure<int> {
         *    return $ts['generic_types'][0];
         *  }
         *
         * For test(TypeStructure<D>) there will not be a generic_types field
         *)
        | Nast.SFlit (_, "generic_types"), _, _ when is_generic ->
            env, ShapeMap.add field field_ty shape
        | Nast.SFlit (_, "generic_types"), _, (r, Tarraykind (AKvec ty))
              when not is_generic ->
            let env, ty = make_ts env ty in
            env, ShapeMap.add field (r, Ttuple [ty]) shape
        | Nast.SFlit (_, "generic_types"), _,
          (r, Tarraykind (AKmap (ty1, ty2)))
              when not is_generic ->
            let tyl = [ty1; ty2] in
            let env, tyl = List.map_env env tyl make_ts in
            env, ShapeMap.add field (r, Ttuple tyl) shape
        | Nast.SFlit (_, "generic_types"), _, (r, Tclass (_, tyl))
              when List.length tyl > 0 ->
            let env, tyl = List.map_env env tyl make_ts in
            env, ShapeMap.add field (r, Ttuple tyl) shape
        | Nast.SFlit (_, ("kind" | "name" | "alias")), _, _ ->
            env, ShapeMap.add field field_ty shape
        | _, _, _ ->
            env, shape
      end shape (env, ShapeMap.empty)
