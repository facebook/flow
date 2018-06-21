(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  name: string option;
  main: string option;
}

let (>>=) = Core_result.(>>=)

let empty = { name = None; main = None; }
let name package = package.name
let main package = package.main

let statement_of_program = function
  | (_, [statement], _) -> Ok statement
  | _ -> Error "Expected a single statement."

let object_of_statement statement =
  let open Ast in
  match statement with
  | _, Statement.Expression { Statement.Expression.
      expression = _, Expression.Assignment { Expression.Assignment.
        operator = Expression.Assignment.Assign;
        left = _;
        right = obj;
      };
      directive = _;
    } -> Ok obj
  | _ -> Error "Expected an assignment"

let properties_of_object = function
  | (_, Ast.Expression.Object {Ast.Expression.Object.properties}) -> Ok properties
  | _ -> Error "Expected an object literal"

let parse ast =
  statement_of_program ast
  >>= object_of_statement
  >>= properties_of_object
  >>= fun properties ->
    let open Ast in
    let open Expression.Object in
    let extract_property package = function
      | Property (_, Property.Init {
          key = Property.Literal(_, { Literal.value = Literal.String key; _ });
          value = (_, Expression.Literal { Literal.
            value = Literal.String value;
            _
          });
          _;
        }) ->
          begin match key with
          | "name" -> { package with name = Some value }
          | "flow:main" -> { package with main = Some value }
          | "main" -> { package with main = Some value }
          | _ -> package
          end
      | _ -> package
    in
    Ok (List.fold_left extract_property empty properties)
