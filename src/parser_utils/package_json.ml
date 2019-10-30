(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type t = {
  name: string option;
  main: string option;
}

type 'a t_or_error = (t, 'a * string) result

let ( >>= ) = Base.Result.( >>= )

let empty = { name = None; main = None }

let name package = package.name

let main package = package.main

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
        | _ -> package
      in
      Ok (List.fold_left extract_property empty properties)))
