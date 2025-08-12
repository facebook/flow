(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type 'a condition_value =
  | Null
  | Path of string
  | Nested of (string * 'a condition_value) list

type t = { conditions: (string * t condition_value) list }

let empty = { conditions = [] }

let create ~conditions = { conditions }

let create_from_shorthand_value ~value =
  let conditions = [("default", value)] in
  create ~conditions

let create_from_shorthand ~path = create_from_shorthand_value ~value:(Path path)

let is_targeted_condition valid_conditions candidate_condition =
  List.mem candidate_condition valid_conditions || candidate_condition = "default"

let rec pick_target valid_conditions pattern_match = function
  | (candidate_condition, Nested child_condition_map) :: rest ->
    if is_targeted_condition valid_conditions candidate_condition then
      match pick_target valid_conditions pattern_match child_condition_map with
      | Some t -> Some t
      | None -> pick_target valid_conditions pattern_match rest
    else
      pick_target valid_conditions pattern_match rest
  | (candidate_condition, Path target_path) :: rest ->
    if is_targeted_condition valid_conditions candidate_condition then
      match pattern_match with
      | Some pattern_match ->
        Some (Base.String.substr_replace_first ~pattern:"*" ~with_:pattern_match target_path)
      | None -> Some target_path
    else
      pick_target valid_conditions pattern_match rest
  | (_, Null) :: _ -> None
  | [] -> None

let resolve_package_target condition_map pattern_match valid_conditions =
  pick_target valid_conditions pattern_match condition_map.conditions

let rec parse_condition_property_value = function
  | Ast.Expression.NullLiteral _ -> Some Null
  | Ast.Expression.StringLiteral { Ast.StringLiteral.value; _ } -> Some (Path value)
  | Ast.Expression.Object { Ast.Expression.Object.properties; comments = _ } ->
    Some (Nested (parse_condition_map properties))
  | _ -> None

and parse_condition_property property =
  let open Ast in
  let open Expression.Object in
  match property with
  | Property
      ( _,
        Property.Init
          {
            key = Property.StringLiteral (_, { StringLiteral.value = key; _ });
            value = (_, value);
            _;
          }
      ) ->
    (match parse_condition_property_value value with
    | Some value -> Some (key, value)
    | None -> None)
  | _ -> None

and parse_condition_map properties = List.filter_map parse_condition_property properties

let parse = function
  | Ast.Expression.NullLiteral _ -> Some (create_from_shorthand_value ~value:Null)
  | Ast.Expression.StringLiteral { Ast.StringLiteral.value : string; _ } ->
    Some (create_from_shorthand ~path:value)
  | Ast.Expression.Object { Ast.Expression.Object.properties; comments = _ } ->
    let conditions = parse_condition_map properties in
    Some (create ~conditions)
  | _ -> None
