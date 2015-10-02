(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

module Entry: sig

  type state = Undeclared | Declared | Initialized
  val string_of_state: state -> string

  type value_kind = Const | Let of implicit_let_kinds option | Var
  and implicit_let_kinds =
    | ClassNameBinding
    | CatchParamBinding
    | FunctionBinding

  val string_of_value_kind: value_kind -> string

  type value_binding = {
    kind: value_kind;
    value_state: state;
    value_loc: Loc.t;
    specific: Type.t;
    general: Type.t;
  }

  type type_binding = {
    type_state: state;
    type_loc: Loc.t;
    _type: Type.t;
  }

  type t =
  | Value of value_binding
  | Type of type_binding

  val new_var: loc:Loc.t -> ?state:state -> ?specific:Type.t -> Type.t -> t
  val new_let:
      loc:Loc.t
      -> ?state:state
      -> ?implicit:implicit_let_kinds
      -> Type.t
      -> t
  val new_const: loc:Loc.t -> ?state:state -> Type.t -> t
  val new_type: loc:Loc.t -> ?state:state -> Type.t -> t

  val loc: t -> Loc.t
  val actual_type: t -> Type.t
  val declared_type: t -> Type.t

  val string_of_kind: t -> string
  val havoc: ?name:string -> (Type.t -> Type.t) -> string -> t -> t

  val is_lex: t -> bool
end

module Key: sig
  type proj = Prop of string | Elem of t
  and t = string * proj list
  val string_of_key: t -> string
end

module KeySet: Set.S with type elt
= Key.t

module KeyMap: MapSig with type key
= Key.t

type function_kind = Ordinary | Async | Generator

type kind =
| VarScope of function_kind
| LexScope

type refi_binding = {
  refi_loc: Loc.t;
  refined: Type.t;
  original: Type.t;
}

type t = {
  kind: kind;
  mutable entries: Entry.t SMap.t;
  mutable refis: refi_binding KeyMap.t
}

val fresh: ?kind:function_kind -> unit -> t
val fresh_lex: unit -> t
val clone: t -> t

val iter_entries: (string -> Entry.t -> unit) -> t -> unit
val update_entries: (string -> Entry.t -> Entry.t) -> t -> unit
val add_entry: string -> Entry.t -> t -> unit
val remove_entry: string -> t -> unit
val get_entry: string -> t -> Entry.t option

val update_refis: (Key.t -> refi_binding -> refi_binding) -> t -> unit
val add_refi: Key.t -> refi_binding -> t -> unit
val remove_refi: Key.t -> t -> unit
val get_refi: Key.t -> t -> refi_binding option

val havoc: ?name: string -> ?make_specific: (Type.t -> Type.t) -> t -> unit

val is_lex: t -> bool
