(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Nast
open Typing_defs
open Utils

module Env          = Typing_env
module Type         = Typing_ops
module Reason       = Typing_reason
module TUtils       = Typing_utils

(*****************************************************************************)
(* Adds a new field to all the shapes found in a given type.
 * The function leaves all the other types (non-shapes) unchanged.
 *)
(*****************************************************************************)

let rec grow_shape pos lvalue field_name ty env shape =
  let _, shape = Env.expand_type env shape in
  match shape with
  | _, Tshape (fields_known, fields) ->
      let fields = ShapeMap.add field_name ty fields in
      let result = Reason.Rwitness pos, Tshape (fields_known, fields) in
      env, result
  | _, Tunresolved tyl ->
      let env, tyl = lfold (grow_shape pos lvalue field_name ty) env tyl in
      let result = Reason.Rwitness pos, Tunresolved tyl in
      env, result
  | x ->
      env, x
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
      let env, tyl = lfold (shrink_shape pos field_name) env tyl in
      let result = Reason.Rwitness pos, Tunresolved tyl in
      env, result
  | x ->
      env, x

let idx env fty shape_ty field default =
  let env, shape_ty = Env.expand_type env shape_ty in
  let env, res = Typing_utils.in_var env (Reason.Rnone, Tunresolved []) in
  let field_name = TUtils.shape_field_name (fst field) (snd field) in
  let fake_shape = (
    (* Rnone because we don't want the fake shape to show up in messages about
     * field non existing. Errors.missing_optional_field filters them out *)
    Reason.Rnone,
    Tshape (
      FieldsPartiallyKnown Nast.ShapeMap.empty,
      Nast.ShapeMap.singleton field_name (Reason.Rnone, Toption res)
    )
  ) in
  let env = Type.sub_type (fst field) Reason.URparam env fake_shape shape_ty in
  match default with
    | None when TUtils.is_option env res -> env, res
    | None ->
      (* no default: result is nullable, point to
       * Shapes::idx definition as reason*)
      env, (fst fty, Toption res)
    | Some (default_pos, default_ty) ->
      Type.sub_type default_pos Reason.URparam env default_ty res, res

let remove_key p env shape_ty field  =
  let field_name = TUtils.shape_field_name (fst field) (snd field) in
  shrink_shape p field_name env shape_ty
