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
open Nast
open Typing_defs

module Env          = Typing_env
module Reason       = Typing_reason
module TUtils       = Typing_utils
module Type         = Typing_ops

(*****************************************************************************)
(* Remove a field from all the shapes found in a given type.
 * The function leaves all the other types (non-shapes) unchanged.
 *)
(*****************************************************************************)

let rec shrink_shape pos field_name env shape =
  let _, shape = Env.expand_type env shape in
  match shape with
  | _, Tshape (fields_known, fields) ->
      (* remember that we have unset this field *)
      let fields_known = match fields_known with
        | FieldsFullyKnown ->
            FieldsFullyKnown
        | FieldsPartiallyKnown unset_fields ->
            FieldsPartiallyKnown (ShapeMap.add field_name pos unset_fields) in
      let fields = ShapeMap.remove field_name fields in
      let result = Reason.Rwitness pos, Tshape (fields_known, fields) in
      env, result
  | _, Tunresolved tyl ->
      let env, tyl = List.map_env env tyl(shrink_shape pos field_name) in
      let result = Reason.Rwitness pos, Tunresolved tyl in
      env, result
  | x ->
      env, x

let idx env fty shape_ty field default =
  let env, shape_ty = Env.expand_type env shape_ty in
  let env, res = Typing_utils.in_var env (Reason.Rnone, Tunresolved []) in
  match TUtils.shape_field_name env (fst field) (snd field) with
  | None -> env, (Reason.Rwitness (fst field), Tany)
  | Some field_name ->
    let fake_shape = (
      (* Rnone because we don't want the fake shape to show up in messages about
       * field non existing. Errors.missing_optional_field filters them out *)
      Reason.Rnone,
      Tshape (
        FieldsPartiallyKnown Nast.ShapeMap.empty,
        Nast.ShapeMap.singleton field_name (Reason.Rnone, Toption res)
      )
    ) in
    let env =
      Type.sub_type (fst field) Reason.URparam env fake_shape shape_ty in
    match default with
      | None when TUtils.is_option env res -> env, res
      | None ->
        (* no default: result is nullable, point to
         * Shapes::idx definition as reason*)
        env, (fst fty, Toption res)
      | Some (default_pos, default_ty) ->
        let env, default_ty = Typing_utils.unresolved env default_ty in
        Type.sub_type default_pos Reason.URparam env default_ty res, res

let remove_key p env shape_ty field  =
  match TUtils.shape_field_name env (fst field) (snd field) with
   | None -> env, (Reason.Rwitness (fst field), Tany)
   | Some field_name -> shrink_shape p field_name env shape_ty

let to_array env shape_ty res =
  let mapper = object
    inherit Type_mapper.shallow_type_mapper as super
    inherit! Type_mapper.tunresolved_type_mapper
    inherit! Type_mapper.tvar_expanding_type_mapper

    method! on_tshape (env, seen) r fields_known fdm =
      match fields_known with
      | FieldsFullyKnown ->
        let env, values =
          List.map_env env (ShapeMap.values fdm) (Typing_utils.unresolved) in
        let keys = ShapeMap.keys fdm in
        let env, keys = List.map_env env keys begin fun env key ->
          let env, ty = match key with
          | SFlit (p, _) -> env, (Reason.Rwitness p, Tprim Tstring)
          | SFclass_const ((_, cid), (_, mid)) ->
            begin match Env.get_class env cid with
              | Some class_ -> begin match Env.get_const env class_ mid with
                  | Some const ->
                      Typing_phase.localize_with_self env const.ce_type
                  | None -> env, (Reason.Rnone, Tany)
                end
              | None -> env, (Reason.Rnone, Tany)
            end in
          Typing_utils.unresolved env ty
        end in
        let env, key =
          Typing_arrays.array_type_list_to_single_type env keys in
        let env, value =
          Typing_arrays.array_type_list_to_single_type env values in
        (env, seen), (r, Tarraykind (AKmap (key, value)))
      | FieldsPartiallyKnown _ ->
        (env, seen), res

    method! on_type env (r, ty) = match ty with
      | Tvar _ | Tunresolved _ | Tshape _ ->  super#on_type env (r, ty)
      | _ -> env, res

  end in
  let (env, _), ty = mapper#on_type (Type_mapper.fresh_env env) shape_ty in
  env, ty
