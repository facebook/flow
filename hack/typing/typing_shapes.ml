(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Typing_defs

module Env          = Typing_env
module Type         = Typing_ops
module Reason       = Typing_reason
module TUtils       = Typing_utils

let idx env fty shape_ty field default =
  let env, shape_ty = Env.expand_type env shape_ty in
  let env, res = Typing_utils.in_var env (Reason.Rnone, Tunresolved []) in
  let field_name = TUtils.shape_field_name (fst field) (snd field) in
  let fake_shape = (
    (* Rnone because we don't want the fake shape to show up in messages about
     * field non existing. Errors.missing_field filters them out *)
    Reason.Rnone,
    Tshape (
      false,
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
