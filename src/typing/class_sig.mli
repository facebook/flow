(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Intermediate representation for classes and interfaces *)

type t

type set_asts =
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Function.body option *
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t option
  -> unit

type set_type = Type.t -> unit

and field =
  | Annot of Type.t
  | Infer of Func_sig.t * set_asts

type super =
  | Interface of {
      extends: typeapp list;
      callable: bool;
    }
  | Class of {
      extends: extends;
      mixins: typeapp list; (* declare class only *)
      implements: typeapp list
    }

and extends =
  | Explicit of typeapp
  | Implicit of { null: bool }

and typeapp = ALoc.t * Type.t * Type.t list option

(** 1. Constructors **)

(** Create signature with no elements. *)
val empty:
  int -> (* id *)
  Reason.t ->
  Type.typeparams ->
  Type.t SMap.t -> (* tparams_map *)
  super ->
  t

(** Add constructor to signature.

    Overwrites any existing constructor. This implements the behavior of
    classes, which permit duplicate definitions where latter definitions
    overwrite former ones. *)
val add_constructor:
  ALoc.t option ->
  Func_sig.t ->
  ?set_asts:set_asts ->
  ?set_type:set_type ->
  t -> t

val add_default_constructor: Reason.t -> t -> t

(** Add constructor override to signature.

    Does not overwrite existing constructors. This implements the behavior of
    interfaces, which interpret duplicate definitions as branches of a single
    overloaded constructor. *)
val append_constructor:
  ALoc.t option ->
  Func_sig.t ->
  ?set_asts:set_asts ->
  ?set_type:set_type ->
  t -> t

(** Add field to signature. *)
val add_field: static:bool -> string -> ALoc.t -> Type.polarity -> field -> t -> t

(** Add indexer to signature. *)
val add_indexer:
  static:bool ->
  Type.polarity ->
  key:(ALoc.t * Type.t) ->
  value:(ALoc.t * Type.t) ->
  t -> t

(** Add static `name` field. *)
val add_name_field: t -> t

(** Add proto field to signature. *)
val add_proto_field: string -> ALoc.t -> Type.polarity -> field -> t -> t

(** Add private field to signature. *)
val add_private_field: string -> ALoc.t -> Type.polarity -> field -> static:bool -> t -> t

(** Add method to signature.

    Overwrites any existing synonymous method. This implements the behavior of
    classes, which permit duplicate definitions where latter definitions
    overwrite former ones. *)
val add_method:
  static:bool ->
  string ->
  ALoc.t ->
  Func_sig.t ->
  ?set_asts:set_asts ->
  ?set_type:set_type ->
  t -> t

(** Add method override to signature.

    Does not overwrite existing synonymous methods. This implements the
    behavior of interfaces, which interpret duplicate definitions as branches
    of a single overloaded method. *)
val append_method:
  static:bool ->
  string ->
  ALoc.t ->
  Func_sig.t ->
  ?set_asts:set_asts ->
  ?set_type:set_type ->
  t -> t

val append_call: static:bool -> Type.t -> t -> t

val add_call_deprecated: static:bool -> Type.t -> t -> t

(** Add getter to signature. *)
val add_getter:
  static:bool ->
  string ->
  ALoc.t ->
  Func_sig.t ->
  ?set_asts:set_asts ->
  ?set_type:set_type ->
  t -> t

(** Add setter to signature. *)
val add_setter:
  static:bool ->
  string ->
  ALoc.t ->
  Func_sig.t ->
  ?set_asts:set_asts ->
  ?set_type:set_type ->
  t -> t

(** Check if this signature defines a given field *)
val mem_field: string -> static:bool -> t -> bool

(** Check if this signature defines a constructor *)
val mem_constructor: t -> bool

val add_this:
  Type.t -> (* self *)
  Context.t ->
  Reason.t ->
  Type.typeparams ->
  Type.t SMap.t -> (* tparams_map *)
  Type.t * Type.typeparams * Type.t SMap.t

(** 1. Manipulation *)

(** Emits constraints to ensure the signature is compatible with its declared
    interface implementations (classes) *)
val check_implements: Context.t -> Reason.reason -> t -> unit

(** Emits constraints to ensure the signature is compatible with its declared
    superclass (classes) or extends/mixins (interfaces) *)
val check_super: Context.t -> Reason.reason -> t -> unit

(** Invoke callback with type parameters substituted by upper/lower bounds. *)
val generate_tests: Context.t ->
  (t -> 'a) -> t -> 'a

(** Evaluate the class body. *)
val toplevels: Context.t ->
  decls:(Context.t -> (ALoc.t, ALoc.t) Flow_ast.Statement.t list -> unit) ->
  stmts:(Context.t -> (ALoc.t, ALoc.t) Flow_ast.Statement.t list ->
                      (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.t list) ->
  expr:(Context.t -> (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
                      (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t) ->
  t -> unit

(** 1. Type Conversion *)

val thistype: Context.t -> t -> Type.t

(* Create a (polymorphic) class type. *)
val classtype: Context.t ->
  ?check_polarity:bool ->
  t -> Type.t

module This: sig
  val is_bound_to_empty: t -> bool
  val in_class: (ALoc.t, ALoc.t) Flow_ast.Class.t -> bool
end

val with_typeparams: Context.t -> (unit -> 'a) -> t -> 'a
