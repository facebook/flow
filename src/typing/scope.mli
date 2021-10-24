(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module State : sig
  type t =
    | Undeclared
    | Declared
    | MaybeInitialized
    | Initialized

  val to_string : t -> string

  val compare : t -> t -> int
end

module Entry : sig
  type value_kind =
    | Const of const_binding_kind
    | Let of (let_binding_kind * non_const_specialization)
    | Var of non_const_specialization

  and const_binding_kind =
    | ConstImportBinding
    | ConstParamBinding
    | ConstVarBinding
    | EnumNameBinding

  and let_binding_kind =
    | LetVarBinding
    | ClassNameBinding
    | CatchParamBinding
    | FunctionBinding
    | DeclaredFunctionBinding of { predicate: bool }
    | ParamBinding

  and non_const_specialization =
    | Havocable
    | NotWrittenByClosure
    | ConstLike

  val string_of_let_binding_kind : let_binding_kind -> string

  val string_of_value_kind : value_kind -> string

  type value_binding = {
    kind: value_kind;
    value_state: State.t;
    value_declare_loc: ALoc.t;
    value_assign_loc: ALoc.t;
    specific: Type.t;
    general: Type.annotated_or_inferred;
    closure_writes: (Loc_collections.ALocSet.t * Type.t * Type.t option) option;
    provider: Type.t;
  }

  type type_binding_kind =
    | ImportTypeBinding
    | TypeBinding

  type type_binding = {
    type_binding_kind: type_binding_kind;
    type_state: State.t;
    type_loc: ALoc.t;
    type_: Type.t;
  }

  type t =
    | Value of value_binding
    | Type of type_binding
    | Class of Type.class_binding

  val new_class :
    ALoc.id ->
    Type.Properties.id ->
    Type.Properties.id ->
    Type.Properties.id ->
    Type.Properties.id ->
    t

  val new_const :
    loc:ALoc.t -> ?state:State.t -> ?kind:const_binding_kind -> Type.annotated_or_inferred -> t

  val new_import : loc:ALoc.t -> Type.t -> t

  val new_let :
    loc:ALoc.t ->
    ?state:State.t ->
    ?kind:let_binding_kind ->
    ?spec:non_const_specialization ->
    ?closure_writes:Loc_collections.ALocSet.t * Type.t * Type.t option ->
    provider:Type.t ->
    Type.annotated_or_inferred ->
    t

  val new_var :
    loc:ALoc.t ->
    ?state:State.t ->
    ?specific:Type.t ->
    ?closure_writes:Loc_collections.ALocSet.t * Type.t * Type.t option ->
    provider:Type.t ->
    ?spec:non_const_specialization ->
    Type.annotated_or_inferred ->
    t

  val new_type : loc:ALoc.t -> ?state:State.t -> Type.t -> t

  val new_import_type : loc:ALoc.t -> Type.t -> t

  val entry_loc : t -> ALoc.t

  val assign_loc : t -> ALoc.t

  val declared_type : t -> Type.t

  val actual_type : t -> Type.t

  val string_of_kind : t -> string

  val kind_of_value : value_binding -> value_kind

  val general_of_value : value_binding -> Type.t

  val state_of_value : value_binding -> State.t

  val havoc : ?on_call:(Type.t -> Type.t -> Type.t -> Type.t) -> Reason.name -> t -> t

  val reset : ALoc.t -> Reason.name -> t -> t

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
  | Ctor

val string_of_var_scope_kind : var_scope_kind -> string

type kind =
  | VarScope of var_scope_kind
  | LexScope

val string_of_kind : kind -> string

type refi_binding = {
  refi_loc: ALoc.t;
  refined: Type.t;
  original: Type.t;
}

type t = {
  id: int;
  kind: kind;
  mutable entries: Entry.t NameUtils.Map.t;
  mutable refis: refi_binding Key_map.t;
  mutable declare_func_annots: (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.annotation SMap.t;
}

val fresh_impl : kind -> t

val fresh : ?var_scope_kind:var_scope_kind -> unit -> t

val fresh_lex : unit -> t

val clone : t -> t

val iter_entries : (Reason.name -> Entry.t -> unit) -> t -> unit

val update_entries : (Reason.name -> Entry.t -> Entry.t) -> t -> unit

val add_entry : Reason.name -> Entry.t -> t -> unit

val remove_entry : Reason.name -> t -> unit

val get_entry : Reason.name -> t -> Entry.t option

val iter_refis : (Key_map.key -> refi_binding -> unit) -> t -> unit

val update_refis : (Key_map.key -> refi_binding -> refi_binding) -> t -> unit

val add_refi : Key_map.key -> refi_binding -> t -> unit

val remove_refi : Key_map.key -> t -> unit

val get_refi : Key_map.key -> t -> refi_binding option

val havoc_refi : Key_map.key -> t -> unit

val filter_refis_using_propname : private_:bool -> string -> 'a Key_map.t -> 'a Key_map.t

val havoc_refis : ?name:string -> private_:bool -> t -> unit

val havoc_all_refis : t -> unit

val havoc : t -> unit

val reset : ALoc.t -> t -> unit

val add_declare_func_annot :
  string -> (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.annotation -> t -> unit

val get_declare_func_annot :
  string -> t -> (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.annotation option

val is_lex : t -> bool

val is_global : t -> bool

val is_toplevel : t -> bool
