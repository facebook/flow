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

type static_array_access_type =
  | AKshape_key of Nast.shape_field_name
  | AKtuple_index of int
  | AKappend
  | AKother

let static_array_access env = function
  | Some (_, x) -> begin match x with
    | Nast.Int (_, x) ->
      (try AKtuple_index (int_of_string x) with Failure _ -> AKother)
    | _ -> begin match TUtils.maybe_shape_field_name env x with
      | Some x -> AKshape_key x
      | None -> AKother
      end
    end
  | None -> AKappend

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

let array_type_list_to_single_type env values =
  let unknown = List.find values (fun ty ->
    snd (snd (TUtils.fold_unresolved env ty)) = Tany)
  in match unknown with
    | Some (r, _) -> env, (r, Tany)
    | None ->
      let env, value = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      List.fold_left_env env values ~init:value ~f:TUtils.unify

let downcast_akshape_to_akmap_ env r fdm =
  let keys, values = List.unzip (ShapeMap.values fdm) in
  let env, values = List.map_env env values Typing_env.unbind in
  let env, value = array_type_list_to_single_type env values in
  let env, keys = List.map_env env keys Typing_env.unbind in
  let env, key = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
  let env, key = List.fold_left_env env keys ~init:key ~f:TUtils.unify in
  env, (r, Tarraykind (AKmap (key, value)))

let downcast_aktuple_to_akvec_ env r fields =
  let tyl = List.rev (IMap.values fields) in
  let env, tyl = List.map_env env tyl Typing_env.unbind in
  let env, value = array_type_list_to_single_type env tyl in
  env, (r, Tarraykind (AKvec (value)))

class virtual downcast_aktypes_mapper = object(this)
  method on_tarraykind_akshape (env, seen) r fdm =
    let env, ty = downcast_akshape_to_akmap_ env r fdm in
    this#on_type (env, seen) ty

  method on_tarraykind_aktuple (env, seen) r fields =
    let env, ty = downcast_aktuple_to_akvec_ env r fields in
    this#on_type (env, seen) ty

  method virtual on_type : env -> locl ty -> result
end

(* Given a type that might be an AKshape/AKtuple (possibly inside Tunresolved
 * or type var) returns an AKmap/AKvec which is a supertype of the input. Leaves
 * other types unchanged. *)
let downcast_aktypes env ty =
  let mapper = object
    inherit update_array_type_mapper
    inherit! downcast_tabstract_to_array_type_mapper
    inherit! downcast_aktypes_mapper
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

let fold_aktuple_as_akvec_with_acc f env acc r fields =
  IMap.fold begin fun _ tv (env, acc) ->
    let env, tv = Typing_env.unbind env tv in
    f env acc (r, Tarraykind (AKvec tv))
  end fields (env, acc)

let fold_aktuple_as_akvec f env r fields =
  fst (fold_aktuple_as_akvec_with_acc begin fun env acc ty ->
    f env ty, acc
  end env () r fields)

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

(* Apply this function to a type after lvalue array access that should update
 * array type (e.g from AKempty to AKmap after using it as a map, or to add a
 * new field to AKshape after setting a statically known field name). *)
let update_array_type p access_type ~lvar_assignment env ty =
  let mapper = object
    inherit update_array_type_mapper
    inherit! downcast_tabstract_to_array_type_mapper

    method! on_tarraykind_akempty (env, seen) _ =
      match access_type with
        | AKshape_key field_name ->
          let env, tk = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
          let env, tv = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
          let fdm = ShapeMap.singleton field_name (tk, tv) in
          (env, seen), (Reason.Rused_as_shape p, Tarraykind (AKshape fdm))
        | AKappend ->
          let env, tv = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
          (env, seen), (Reason.Rappend p, Tarraykind (AKvec tv))
        | AKother | AKtuple_index _ ->
          let env, tk = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
          let env, tv = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
          (env, seen), (Reason.Rused_as_map p, Tarraykind (AKmap (tk, tv)))

    method! on_tarraykind_akshape (env, seen) r fdm =
      match access_type with
        | AKshape_key field_name ->
          let env, tk = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
          let env, tv = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
          let env, ty =
            if akshape_key_consistent_with_map field_name fdm then begin
              let fdm = if ShapeMap.mem field_name fdm && (not lvar_assignment)
              then fdm else ShapeMap.add field_name (tk, tv) fdm in
              env, (Reason.Rused_as_shape p, Tarraykind (AKshape fdm))
            end else
              downcast_akshape_to_akmap_ env r fdm
            in
          (env, seen), ty
        | _ ->
          let env, ty = downcast_akshape_to_akmap_ env r fdm in
          (env, seen), ty

    method! on_tshape (env, seen) r fields_known fdm =
      match access_type with
        | AKshape_key field_name when lvar_assignment ->
          let env, tv = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
          let fdm = ShapeMap.add field_name tv fdm in
          (env, seen), (Reason.Rwitness p, Tshape (fields_known, fdm))
        | _ ->
          (env, seen), (r, Tshape (fields_known, fdm))

    method! on_tarraykind_aktuple (env, seen) r fields =
      match access_type with
        | AKtuple_index index when IMap.mem index fields ->
           let env, fields = if lvar_assignment then
             let env, ty = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
             env, IMap.add index ty fields
           else env, fields in
           (env, seen), (Reason.Rappend p, Tarraykind (AKtuple fields))
        | _ ->
           (* no growing of tuples for now *)
          let env, ty = downcast_aktuple_to_akvec_ env r fields in
          (env, seen), ty
  end in
  let (env, _), ty = mapper#on_type (fresh_env env) ty in
  env, ty

(* When the type is updated because of "$a[...] = ..." statement, we can infer
 * a bit more about the new type than when it happens in nested expression like
 * $a[$i][...] = ... *)
let update_array_type_on_lvar_assignment p access_type env ty =
  update_array_type p access_type ~lvar_assignment:true env ty

let update_array_type p access_type env ty =
  update_array_type p access_type ~lvar_assignment:false env ty

(* Expand tvars, replace all AKshapes and AKtuples with AKmaps and AKvecs *)
let fully_expand_tvars_downcast_aktypes env ty =
  let mapper = object
    inherit deep_type_mapper
    inherit! tvar_expanding_type_mapper
    inherit! downcast_aktypes_mapper
  end in
  snd (mapper#on_type (fresh_env env) ty)
