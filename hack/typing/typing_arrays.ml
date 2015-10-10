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

module Env = Typing_env
module TUtils = Typing_utils
module Reason = Typing_reason
module ShapeMap = Nast.ShapeMap

(* Signature of visitor class mapping from type to type  *)
class type type_mapper_type = object
  method on_tunresolved: Env.env -> Reason.t -> locl ty list
    -> (Env.env * locl ty)
  method on_tvar: Env.env -> Reason.t -> int -> (Env.env * locl ty)
  method on_tarraykind_akempty: Env.env -> Reason.t -> (Env.env * locl ty)
  method on_tarraykind_akshape:
    Env.env -> Reason.t -> (locl ty * locl ty) ShapeMap.t
      -> (Env.env * locl ty)

  method on_type: Env.env -> locl ty -> (Env.env * locl ty)
end

(* Naive implementation of this visitor - passes everything unchanged *)
class type_mapper: type_mapper_type = object(this)
  method on_tunresolved env r tyl = env, (r, Tunresolved tyl)
  method on_tvar env r n = env, (r, Tvar n)
  method on_tarraykind_akempty env r = env, (r, Tarraykind AKempty)
  method on_tarraykind_akshape env r fdm = env, (r, Tarraykind (AKshape fdm))

  method on_type env (r, ty) = match ty with
    | Tunresolved tyl -> this#on_tunresolved env r tyl
    | Tvar n -> this#on_tvar env r n
    | Tarraykind AKempty -> this#on_tarraykind_akempty env r
    | Tarraykind AKshape fdm -> this#on_tarraykind_akshape env r fdm
    | _ -> env, (r, ty)
end

(* Mapper used by update_array* functions. It traverses Tunresolved and
 * modifies the type "inside" the Tvars - so it has side effects on the input
 * type (the type variables inside env change)! *)
class update_array_type_mapper: type_mapper_type = object(this)
  inherit type_mapper

  method! on_tunresolved env r tyl =
    let env, tyl = lmap this#on_type env tyl in
    env, (r, Tunresolved tyl)

  method! on_tvar env _ n =
    let env, ty = Env.get_type env n in
    let env, ty = this#on_type env ty in
    let env = Env.add env n ty in
    env, ty
end

(* Mapper which traverses Tunresolved and goes inside the type vars, but
 * doesn't modify them - returns a new type with type variables expanded *)
class type_expanding_mapper: type_mapper_type = object(this)
  inherit type_mapper

  method! on_tunresolved env r tyl =
    let env, tyl = lmap this#on_type env tyl in
    env, (r, Tunresolved tyl)

  method! on_tvar env r n =
    let _, ety = Env.expand_type env (r, Tvar n) in
    this#on_type env ety
end

(* Given a type that might be an AKshape (possibly inside Tunresolved or type
 * var) returns an AKmap which is a supertype of the input. Leaves other types
 * unchanged. *)
let downcast_akshape_to_akmap = object
  inherit type_expanding_mapper

    method! on_tarraykind_akshape env r fdm =
      let keys, values = List.unzip (ShapeMap.values fdm) in
      let env, values = lmap Typing_env.unbind env values in
      let env, value = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      let env, value = fold_left_env TUtils.unify env value values in
      let env, keys = lmap Typing_env.unbind env keys in
      let env, key = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      let env, key =  fold_left_env TUtils.unify env key keys in
      env, (r, Tarraykind (AKmap (key, value)))
end
let downcast_akshape_to_akmap = downcast_akshape_to_akmap#on_type

(* Is the field_name type consistent with ones already in field map?
 * Shape field names must all be constant strings or constants from
 * same class. *)
let akshape_keys_consistent field_name fdm =
  let open Nast in try
    match field_name, (fst (ShapeMap.min_binding fdm)) with
      | (SFlit _, SFlit _) -> true
      | (SFclass_const ((_, cls1), _)), (SFclass_const ((_, cls2), _))
          -> cls1 = cls2
      | _ -> false
  with Not_found -> true

(* Update any AKempty found in the type (inside Tvars and Tunresolved too)
 * to an AKmap type *)
let update_array_type_to_akmap p = object
  inherit update_array_type_mapper

    method! on_tarraykind_akempty env _ =
      let env, tk = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      let env, tv = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      env, (Reason.Rused_as_map p, Tarraykind (AKmap (tk, tv)))

    method! on_tarraykind_akshape env r fdm =
      downcast_akshape_to_akmap env (r, Tarraykind (AKshape fdm))
  end

(* Update any AKempty found in the type (inside Tvars and Tunresolved too)
 * to an AKvec type *)
let update_array_type_to_akvec p = object
  inherit update_array_type_mapper

    method! on_tarraykind_akempty env _ =
      let env, tv = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      env, (Reason.Rappend p, Tarraykind (AKvec tv))
  end

(* Update any AKempty found in the type (inside Tvars and Tunresolved too)
 * to an AKshape type.
 * Update any AKshape to AKshape with additional field, or back to AKmap
 * if it's impossible *)
let update_array_type_to_akshape p field_name =  object
  inherit update_array_type_mapper

    method! on_tarraykind_akempty env _ =
      let env, tk = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      let env, tv = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      let fdm = ShapeMap.singleton field_name (tk, tv) in
      env, (Reason.Rwitness p, Tarraykind (AKshape fdm))

    method! on_tarraykind_akshape env r fdm =
      let env, tk = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      let env, tv = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      if akshape_keys_consistent field_name fdm then begin
        let fdm = if ShapeMap.mem field_name fdm then fdm else
          ShapeMap.add field_name (tk, tv) fdm in
          env, (Reason.Rwitness p, Tarraykind (AKshape fdm))
      end else
        downcast_akshape_to_akmap env (r, Tarraykind (AKshape fdm))
  end
