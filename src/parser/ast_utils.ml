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


let function_type_of_function =
  let open Option.Monad_infix in
  let rec param_type_of_pattern ?(optional=false) (loc, patt) =
    let open Pattern in
    match patt with
    | Expression _ -> None
    | Assignment { Assignment.left; _ } ->
      param_type_of_pattern ~optional:true left
    | Identifier { Identifier.name; typeAnnotation; optional } ->
      typeAnnotation >>| fun (_, typeAnnotation) ->
      let name = Some name in
      (loc, { Type.Function.Param.name; typeAnnotation; optional })
    | Object { Object.typeAnnotation; _ }
    | Array { Array.typeAnnotation; _ } ->
      typeAnnotation >>| fun (_, typeAnnotation) ->
      let name = None in
      (loc, { Type.Function.Param.name; typeAnnotation; optional })
  in
  let rest_type_of_rest = function
    | None -> Some None
    | Some (loc, { Function.RestElement.argument }) ->
      param_type_of_pattern argument >>| fun argument ->
      Some (loc, { Type.Function.RestParam.argument })
  in
  let params_type_of_params (loc, { Function.Params.params; rest }) =
    Option.all (List.map param_type_of_pattern params) >>= fun params ->
    rest_type_of_rest rest >>| fun rest ->
    (loc, { Type.Function.Params.params; rest })
  in
  fun loc { Function.params; returnType; typeParameters; _ } ->
    params_type_of_params params >>= fun params ->
    returnType >>| fun (_, returnType) ->
    (loc, Type.Function { Type.Function.params; returnType; typeParameters })
