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
open Utils
open Typing_defs
open Type_mapper

module Env = Typing_env
module TUtils = Typing_utils
module Reason = Typing_reason
module ShapeMap = Nast.ShapeMap

(* Mapper used by update_array* functions. It traverses Tunresolved and
 * modifies the type "inside" the Tvars - so it has side effects on the input
 * type (the type variables inside env change)! *)
class update_array_type_mapper: type_mapper_type = object
  inherit shallow_type_mapper
  inherit! tunresolved_type_mapper
  inherit! tvar_substituting_type_mapper
end

(* Abstract types declared "as array<...>" permit array operations, but if
 * those operations modify the array it has to be downgraded from generic
 * to just an array .*)
class virtual downcast_tabstract_to_array_type_mapper = object(this)
  method on_tabstract env r ak cstr =
    match cstr with
    | Some ty -> this#on_type env ty
    | None -> env, (r, Tabstract (ak, cstr))

  method virtual on_type : env -> locl ty -> result
end


let downcast_akshape_to_akmap_ env r fdm =
  let keys, values = List.unzip (ShapeMap.values fdm) in
  let env, values = lmap Typing_env.unbind env values in
  let env, value = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
  let env, value = fold_left_env TUtils.unify env value values in
  let env, keys = lmap Typing_env.unbind env keys in
  let env, key = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
  let env, key =  fold_left_env TUtils.unify env key keys in
  env, (r, Tarraykind (AKmap (key, value)))

(* Given a type that might be an AKshape (possibly inside Tunresolved or type
 * var) returns an AKmap which is a supertype of the input. Leaves other types
 * unchanged. *)
let downcast_akshape_to_akmap env ty =
  let mapper = object
    inherit update_array_type_mapper
    inherit! downcast_tabstract_to_array_type_mapper

    method! on_tarraykind_akshape (env, seen) r fdm =
      let env, ty = downcast_akshape_to_akmap_ env r fdm in
      (env, seen), ty
  end in
  let (env, _), ty = mapper#on_type (fresh_env env) ty in
  env, ty

let fold_akshape_as_akmap_with_acc f env acc r fdm =
  Nast.ShapeMap.fold begin fun _ (tk, tv) (env, acc) ->
    (* AKshape field types are wrapped in vars so they can grow, but
     * we don't want to permanently unify them with each other when
     * temporarily treating it as AKmap, so unbinding before proceeding. *)
    let env, tk = Typing_env.unbind env tk in
    let env, tv = Typing_env.unbind env tv in
    f env acc (r, Tarraykind (AKmap (tk, tv)))
  end fdm (env, acc)

let fold_akshape_as_akmap f env r fdm =
  fst (fold_akshape_as_akmap_with_acc begin fun env acc ty ->
    f env ty, acc
  end env () r fdm)

(* Is the field_name type consistent with ones already in field map?
 * Shape field names must all be constant strings or constants from
 * same class. *)
let akshape_keys_consistent field_name_x field_name_y =
  let open Nast in
    match field_name_x, field_name_y with
      | (SFlit _, SFlit _) -> true
      | (SFclass_const ((_, cls1), _)), (SFclass_const ((_, cls2), _))
          -> cls1 = cls2
      | _ -> false

let akshape_key_consistent_with_map field_name fdm =
  try
    akshape_keys_consistent field_name (fst (ShapeMap.min_binding fdm))
  with Not_found -> true

let is_shape_like_array env = function
  | [] -> false
  | x::rl ->
    let field_name = function
      | Nast.AFkvalue (ex, _) -> TUtils.maybe_shape_field_name env (snd ex)
      | _ -> None in
    let x_field_name = field_name x in
    Option.is_some x_field_name && List.for_all rl begin fun y ->
      match x_field_name, field_name y with
        | Some x_field_name, Some y_field_name ->
          akshape_keys_consistent x_field_name y_field_name
        | _ -> false
    end

(* Update any AKempty found in the type (inside Tvars and Tunresolved too)
 * to an AKmap type *)
let update_array_type_to_akmap p env ty =
  let mapper = object
    inherit update_array_type_mapper
    inherit! downcast_tabstract_to_array_type_mapper

    method! on_tarraykind_akempty (env, seen) _ =
      let env, tk = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      let env, tv = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      (env, seen), (Reason.Rused_as_map p, Tarraykind (AKmap (tk, tv)))

    method! on_tarraykind_akshape (env, seen) r fdm =
      let env, ty =
        downcast_akshape_to_akmap env (r, Tarraykind (AKshape fdm)) in
      (env, seen), ty
  end in
  let (env, _), ty = mapper#on_type (fresh_env env) ty in
  env, ty

(* Update any AKempty found in the type (inside Tvars and Tunresolved too)
 * to an AKvec type *)
let update_array_type_to_akvec p env ty =
  let mapper = object
    inherit update_array_type_mapper
    inherit! downcast_tabstract_to_array_type_mapper

    method! on_tarraykind_akempty (env, seen) _ =
      let env, tv = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      (env, seen), (Reason.Rappend p, Tarraykind (AKvec tv))
  end in
  let (env, _), ty = mapper#on_type (fresh_env env) ty in
  env, ty

(* Update any AKempty found in the type (inside Tvars and Tunresolved too)
 * to an AKshape type.
 * Update any AKshape to AKshape with additional field, or back to AKmap
 * if it's impossible *)
let update_array_type_to_akshape p field_name env ty =
  let mapper = object
    inherit update_array_type_mapper
    inherit! downcast_tabstract_to_array_type_mapper

    method! on_tarraykind_akempty (env, seen) _ =
      let env, tk = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      let env, tv = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      let fdm = ShapeMap.singleton field_name (tk, tv) in
      (env, seen), (Reason.Rwitness p, Tarraykind (AKshape fdm))

    method! on_tarraykind_akshape (env, seen) r fdm =
      let env, tk = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      let env, tv = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      let env, ty =
        if akshape_key_consistent_with_map field_name fdm then begin
          let fdm = if ShapeMap.mem field_name fdm then fdm else
            ShapeMap.add field_name (tk, tv) fdm in
            env, (Reason.Rwitness p, Tarraykind (AKshape fdm))
        end else
          downcast_akshape_to_akmap env (r, Tarraykind (AKshape fdm))
        in
      (env, seen), ty
  end in
  let (env, _), ty = mapper#on_type (fresh_env env) ty in
  env, ty

let fully_remove_akshapes_and_tvars env ty =
  let mapper = object(this)
    inherit deep_type_mapper
    inherit! tvar_expanding_type_mapper
    method! on_tarraykind_akshape (env, seen) r fdm =
      let env, ty = downcast_akshape_to_akmap_ env r fdm in
      this#on_type (env, seen) ty
  end in
  snd (mapper#on_type (fresh_env env) ty)
