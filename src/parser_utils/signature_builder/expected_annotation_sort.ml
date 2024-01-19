(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let spf = Printf.sprintf

type t =
  | Identifier
  | ArrayPattern
  | ObjectPattern
  | FunctionReturn
  | Property of { name: string }
  | VariableDefinition of { name: string }
[@@deriving show]

let to_string = function
  | Identifier -> "identifier"
  | ArrayPattern -> "array pattern"
  | ObjectPattern -> "object pattern"
  | FunctionReturn -> "function return"
  | Property { name } -> spf "property `%s`" name
  | VariableDefinition { name } -> spf "declaration of variable `%s`" name
