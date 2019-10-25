(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type flow_t = { types_versions: (string * string) list }

type t = {
  name: string option;
  main: string option;
  flow: flow_t option;
}

type 'a t_or_error = (t, 'a * string) result

let ( >>= ) = Core_result.( >>= )

let empty = { name = None; main = None; flow = None }

let name package = package.name

let main package = package.main

let flow_types_versions package =
  match package.flow with
  | Some t -> t.types_versions
  | None -> []

let statement_of_program = function
  | (_, [statement], _) -> Ok statement
  | (loc, _, _) -> Error (loc, "Expected a single statement.")

let object_of_statement statement =
  Ast.(
    match statement with
    | ( _,
        Statement.Expression
          {
            Statement.Expression.expression =
              ( _,
                Expression.Assignment
                  { Expression.Assignment.operator = None; left = _; right = obj } );
            directive = _;
          } ) ->
      Ok obj
    | (loc, _) -> Error (loc, "Expected an assignment"))

let properties_of_object = function
  | (_, Ast.Expression.Object { Ast.Expression.Object.properties; comments = _ }) -> Ok properties
  | (loc, _) -> Error (loc, "Expected an object literal")

let parse_flow properties =
  Ast.(
    Expression.Object.(
      let extract_property (flow : flow_t) = function
        | Property
            ( _,
              Property.Init
                {
                  key = Property.Literal (_, { Literal.value = Literal.String key; _ });
                  value = (_, Expression.Object { Expression.Object.properties; _ });
                  _;
                } ) ->
          begin
            match key with
            | "typesVersions" ->
              let value =
                List.fold_left
                  (fun list property ->
                    match property with
                    | Property
                        ( _,
                          Property.Init
                            {
                              key = Property.Literal (_, { Literal.value = Literal.String key; _ });
                              value =
                                (_, Expression.Literal { Literal.value = Literal.String value; _ });
                              _;
                            } ) ->
                      (key, value) :: list
                    | _ -> list)
                  []
                  properties
              in
              { types_versions = List.rev value }
            | _ -> flow
          end
        | _ -> flow
      in
      List.fold_left extract_property { types_versions = [] } properties))

let parse ast : 'a t_or_error =
  statement_of_program ast
  >>= object_of_statement
  >>= properties_of_object
  >>= fun properties ->
  Ast.(
    Expression.Object.(
      let extract_property package = function
        | Property
            ( _,
              Property.Init
                {
                  key = Property.Literal (_, { Literal.value = Literal.String key; _ });
                  value = (_, Expression.Literal { Literal.value = Literal.String value; _ });
                  _;
                } ) ->
          begin
            match key with
            | "name" -> { package with name = Some value }
            | "main" -> { package with main = Some value }
            | _ -> package
          end
        | Property
            ( _,
              Property.Init
                {
                  key = Property.Literal (_, { Literal.value = Literal.String key; _ });
                  value = (_, Expression.Object { Expression.Object.properties; _ });
                  _;
                } ) ->
          begin
            match key with
            | "flow" ->
              let value = parse_flow properties in
              { package with flow = Some value }
            | _ -> package
          end
        | _ -> package
      in
      Ok (List.fold_left extract_property empty properties)))
