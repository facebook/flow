(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let spf = Printf.sprintf

type property_key =
  | Literal of string
  | Identifier of string
  | PrivateName of string
  | Computed of string
[@@deriving show]

type t =
  | ArrayPattern
  | FunctionReturn
  | PrivateField of { name: string }
  | Property of { name: property_key }
  | VariableDefinition of { name: string }
[@@deriving show]

let property_key_to_string = function
  | Literal lit -> spf "literal property %s" lit
  | Identifier name -> spf "property `%s`" name
  | PrivateName name -> spf "property `%s`" name
  | Computed e -> spf "computed property `[%s]`" e

let to_string = function
  | ArrayPattern -> "array pattern"
  | FunctionReturn -> "function return"
  | Property { name } -> property_key_to_string name
  | PrivateField { name } -> spf "private field `#%s`" name
  | VariableDefinition { name } -> spf "declaration of variable `%s`" name
