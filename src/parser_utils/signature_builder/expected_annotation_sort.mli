(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

val to_string : t -> string
