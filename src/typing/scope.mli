(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module State :
  sig
    type t = Undeclared | Declared | MaybeInitialized | Initialized
    val to_string : t -> string
    val compare : t -> t -> int
  end
module Entry :
  sig
    type value_kind =
      | Const of const_binding_kind
      | Let of let_binding_kind
      | Var
    and const_binding_kind =
      | ConstImportBinding
      | ConstParamBinding
      | ConstVarBinding
    and let_binding_kind =
        LetVarBinding
      | ClassNameBinding
      | CatchParamBinding
      | FunctionBinding
      | ParamBinding
    val string_of_value_kind : value_kind -> string
    type value_binding = {
      kind : value_kind;
      value_state : State.t;
      value_declare_loc : Loc.t;
      value_assign_loc : Loc.t;
      specific : Type.t;
      general : Type.t;
    }
    type type_binding = {
      type_state : State.t;
      type_loc : Loc.t;
      _type : Type.t;
    }
    type t = Value of value_binding | Type of type_binding
    val new_value : value_kind -> State.t -> Type.t -> Type.t -> Loc.t -> t
    val new_const :
      loc:Loc.t -> ?state:State.t -> ?kind:const_binding_kind -> Type.t -> t
    val new_import :
      loc:Loc.t -> Type.t -> t
    val new_let :
      loc:Loc.t -> ?state:State.t -> ?kind:let_binding_kind -> Type.t -> t
    val new_var :
      loc:Loc.t -> ?state:State.t -> ?specific:Type.t -> Type.t -> t
    val new_type : loc:Loc.t -> ?state:State.t -> Type.t -> t
    val loc : t -> Loc.t
    val assign_loc : t -> Loc.t
    val declared_type : t -> Type.t
    val actual_type : t -> Type.t
    val string_of_kind : t -> string
    val kind_of_value : value_binding -> value_kind
    val general_of_value : value_binding -> Type.t
    val state_of_value : value_binding -> State.t
    val havoc : string -> t -> t
    val reset : Reason.t -> string -> t -> t
    val is_lex : t -> bool
  end
type var_scope_kind =
  | Ordinary
  | Async
  | Generator
  | AsyncGenerator
  | Module
  | Global
  | Predicate
val string_of_var_scope_kind : var_scope_kind -> string
type kind = VarScope of var_scope_kind | LexScope
val string_of_kind : kind -> string
type refi_binding = {
  refi_loc : Loc.t;
  refined : Type.t;
  original : Type.t;
}
type t = {
  id : int;
  kind : kind;
  mutable entries : Entry.t SMap.t;
  mutable refis : refi_binding Key_map.t;
}
val fresh_impl : kind -> t
val fresh : ?var_scope_kind:var_scope_kind -> unit -> t
val fresh_lex : unit -> t
val clone : t -> t
val iter_entries : (SMap.key -> Entry.t -> unit) -> t -> unit
val update_entries : (SMap.key -> Entry.t -> Entry.t) -> t -> unit
val add_entry : SMap.key -> Entry.t -> t -> unit
val remove_entry : SMap.key -> t -> unit
val get_entry : SMap.key -> t -> Entry.t option
val havoc_entry : SMap.key -> t -> unit
val update_refis : (Key_map.key -> refi_binding -> refi_binding) -> t -> unit
val add_refi : Key_map.key -> refi_binding -> t -> unit
val remove_refi : Key_map.key -> t -> unit
val get_refi : Key_map.key -> t -> refi_binding option
val havoc_refi : Key_map.key -> t -> unit
val filter_refis_using_propname : string -> 'a Key_map.t -> 'a Key_map.t
val havoc_refis : ?name:string -> t -> unit
val havoc : t -> unit
val reset : Reason.t -> t -> unit
val is_lex : t -> bool
val is_global : t -> bool
