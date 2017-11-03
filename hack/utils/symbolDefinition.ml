(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Hh_core

type kind =
  | Function
  | Class
  | Method
  | Property
  | Const
  | Enum
  | Interface
  | Trait
  | LocalVar
  | Typeconst
  | Param
  | Typedef

and modifier =
  | Final
  | Static
  | Abstract
  | Private
  | Public
  | Protected
  | Async
  | Inout

and 'a t = {
  kind : kind;
  name : string;
  full_name : string;
  id : string option;
  pos : 'a Pos.pos;
  span : 'a Pos.pos;
  modifiers : modifier list;
  children : 'a t list option;
  params : 'a t list option;
  docblock : string option;
}

let rec to_absolute x = {
  kind = x.kind;
  name = x.name;
  full_name = x.full_name;
  id = x.id;
  pos = Pos.to_absolute x.pos;
  span = Pos.to_absolute x.span;
  modifiers = x.modifiers;
  children = Option.map x.children (fun x -> List.map x to_absolute);
  params = Option.map x.params (fun x -> List.map x to_absolute);
  docblock = x.docblock;
}

let string_of_kind = function
  | Function -> "function"
  | Class -> "class"
  | Method -> "method"
  | Property -> "property"
  | Const -> "const"
  | Enum -> "enum"
  | Interface -> "interface"
  | Trait -> "trait"
  | Typeconst -> "typeconst"
  | LocalVar -> "local"
  | Param -> "param"
  | Typedef -> "typedef"

let string_of_modifier = function
  | Final -> "final"
  | Static -> "static"
  | Abstract -> "abstract"
  | Private -> "private"
  | Public -> "public"
  | Protected -> "protected"
  | Async -> "async"
  | Inout -> "inout"

let function_kind_name = "function"
let type_id_kind_name = "type_id"
let method_kind_name = "method"
let property_kind_name = "property"
let class_const_kind_name = "class_const"

let get_symbol_id kind parent_class name =
  let prefix = match kind with
    | Function -> Some function_kind_name
    | Class | Typedef | Enum | Interface | Trait -> Some type_id_kind_name
    | Method -> Some method_kind_name
    | Property -> Some property_kind_name
    | Typeconst | Const -> Some class_const_kind_name
    | LocalVar | Param -> None
  in
  match prefix, parent_class with
  | Some prefix, Some parent_class ->
      Some (Printf.sprintf "%s::%s::%s" prefix parent_class name)
  | Some prefix, None ->
      Some (Printf.sprintf "%s::%s" prefix name)
  | None, _ -> None
