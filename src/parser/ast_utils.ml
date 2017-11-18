(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

type binding = Loc.t * string

let rec bindings_of_pattern =
  let open Pattern in
  let property acc =
    let open Object in
    function
    | Property (_, { Property.pattern = (_, p); _ })
    | RestProperty (_, { RestProperty.argument = (_, p) }) ->
      bindings_of_pattern acc p
  in
  let element acc =
    let open Array in
    function
    | None -> acc
    | Some (Element (_, p))
    | Some (RestElement (_, { RestElement.argument = (_, p) })) ->
      bindings_of_pattern acc p
  in
  fun acc ->
    function
    | Identifier { Identifier.name; _ } ->
      name::acc
    | Object { Object.properties; _ } ->
      List.fold_left property acc properties
    | Array { Array.elements; _ } ->
      List.fold_left element acc elements
    | Assignment { Assignment.left = (_, p); _ } ->
      bindings_of_pattern acc p
    | Expression _ ->
      failwith "expression pattern"

let bindings_of_variable_declarations =
  let open Ast.Statement.VariableDeclaration in
  List.fold_left (fun acc -> function
    | _, { Declarator.id = (_, pattern); _ } ->
      bindings_of_pattern acc pattern
  ) []

let partition_directives statements =
  let open Ast.Statement in
  let rec helper directives = function
    | ((_, Expression { Expression.directive = Some _; _ }) as directive)::rest ->
      helper (directive::directives) rest
    | rest -> List.rev directives, rest
  in
  helper [] statements
