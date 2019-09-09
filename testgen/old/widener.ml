(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module S = Flow_ast.Statement
module E = Flow_ast.Expression
module T = Flow_ast.Type
module P = Flow_ast.Pattern
module Utils = Flowtestgen_utils
module Config = Flowtestgen_config
module FTypes = Flowtestgen_types
module FRandom = Utils.FRandom

(* Do a single widening step up. If we are at the top, return t *)
let rec widen_type (t : T.t') : T.t' =
  FTypes.TypeSet.(
    let new_type =
      match t with
      (* Add a type to make a union type *)
      | T.Number
      | T.String
      | T.Boolean ->
        let available_set = diff FTypes.primitive_types (singleton t) in
        FTypes.mk_union_type [|t; FTypes.TypeSet.choose FRandom.rchoice available_set|]
      (* Add a type into the existing union type *)
      | T.Union ((l1, t1), (l2, t2), trest) ->
        let tlist = Core_list.map ~f:snd trest in
        let used_set = of_list (t1 :: t2 :: tlist) in
        let available_set = diff FTypes.primitive_types used_set in
        if is_empty available_set then
          t
        else
          let new_t = FTypes.TypeSet.choose FRandom.rchoice available_set in
          T.Union ((l1, t1), (l2, t2), (Loc.none, new_t) :: trest)
      | T.Object obj_t -> widen_obj_type obj_t
      | T.StringLiteral _ -> T.String
      | T.NumberLiteral _ -> T.Number
      | T.BooleanLiteral _ -> T.Boolean
      | T.Tuple tlist ->
        (* Randomly select a type and widen that *)
        let tarray = Array.of_list (Core_list.map ~f:snd tlist) in
        let old_t_index = FRandom.rint (Array.length tarray) in
        let old_t = tarray.(old_t_index) in
        let new_t = widen_type old_t in
        if new_t = old_t then
          T.Array (Loc.none, new_t)
        else
          T.Tuple
            (List.mapi
               (fun i t ->
                 if i = old_t_index then
                   (Loc.none, new_t)
                 else
                   t)
               tlist)
      | T.Array (_, array_type) -> T.Array (Loc.none, widen_type array_type)
      | _ -> failwith "Widen: unsupported type\n"
    in
    (* Randomly widen the type again *)
    if new_type = t || FRandom.rbool () then
      new_type
    else
      widen_type new_type)

(* Widen an object property type *)
and widen_obj_prop (prop : T.Object.Property.t') : T.Object.Property.t' =
  T.Object.Property.(
    let old_t =
      match prop.value with
      | Init (_, t) -> t
      | _ -> failwith "widen_obj_prop: Unsupported prop value"
    in
    let new_t = widen_type old_t in
    (* We make it optional if we cannot widen the type anymore *)
    let optional =
      if new_t = old_t then
        true
      else
        false
    in
    {
      key = prop.key;
      value = Init (Loc.none, new_t);
      optional;
      static = prop.static;
      _method = prop._method;
      variance = prop.variance;
    })

(* Widen an object type *)
and widen_obj_type (t : T.Object.t) : T.t' =
  (* We randomly delete a property *)
  let count = List.length T.Object.(t.properties) in
  if count < 2 then
    T.Object t
  else
    let sel = FRandom.rint count in
    let rec helper (plist : T.Object.property list) (i : int) : T.Object.property list =
      match plist with
      | [] -> []
      | hd :: tl when i != sel -> hd :: helper tl (1 + i)
      | T.Object.Property (loc, prop) :: tl ->
        let new_prop = widen_obj_prop prop in
        if new_prop = prop then
          tl
        else
          T.Object.Property (loc, new_prop) :: tl
      | _ -> failwith "Unsupported property type"
    in
    T.Object.(T.Object { exact = false; properties = helper t.properties 0 })
