(*
 * Copyright (c) 2019, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

(* This `.mli` file was generated automatically. It may include extra
definitions that should not actually be exposed to the caller. If you notice
that this interface file is a poor interface, please take a few minutes to
clean it up manually, and then delete this comment once the interface is in
shape. *)

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

and reactivity_attributes =
  | Rx
  | Shallow
  | Local
  | Nonreactive
  | OnlyRxIfImpl
  | AtMostRxAsArgs

and 'a t = {
  kind: kind;
  name: string;
  full_name: string;
  id: string option;
  pos: 'a Pos.pos;
  (* covers the span of just the identifier *)
  span: 'a Pos.pos;
  (* covers the span of the entire construct, including children *)
  modifiers: modifier list;
  children: 'a t list option;
  params: 'a t list option;
  docblock: string option;
  reactivity_attributes: reactivity_attributes list;
}

val to_absolute : Relative_path.t t -> string t

val to_relative : string t -> Relative_path.t t

val string_of_kind : kind -> string

val string_of_modifier : modifier -> string

val string_of_reactivity_attribute : reactivity_attributes -> string

val function_kind_name : string

val type_id_kind_name : string

val method_kind_name : string

val property_kind_name : string

val class_const_kind_name : string

val get_symbol_id : kind -> string option -> string -> string option
