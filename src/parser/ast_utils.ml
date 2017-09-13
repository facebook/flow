(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
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

let bindings_of_export_specifiers =
  let open Ast.Statement.ExportNamedDeclaration in
  List.fold_left ExportSpecifier.(fun acc -> function
    | loc, { local = id; exported = None }
    | loc, { exported = Some id; _ } ->
      (loc, snd id)::acc
  ) []
