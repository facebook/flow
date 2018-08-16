(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Intermediate representation for classes and interfaces *)

type t

type set_asts =
  (Loc.t, Loc.t * Type.t) Ast.Function.body option *
  (Loc.t, Loc.t * Type.t) Ast.Expression.t option
  -> unit

type field =
  | Annot of Type.t
  | Infer of Func_sig.t * set_asts

type super =
  | Interface of {
      extends: Type.t list;
      callable: bool;
    }
  | Class of {
      extends: extends;
      mixins: Type.t list; (* declare class only *)
      implements: Type.t list
    }

and extends =
  | Explicit of Type.t
  | Implicit of { null: bool }

(** 1. Constructors **)

(** Create signature with no elements. *)
val empty:
  int -> (* id *)
  Reason.t ->
  Type.typeparam list ->
  Type.t SMap.t -> (* tparams_map *)
  super ->
  t

(** Add constructor to signature.

    Overwrites any existing constructor. This implements the behavior of
    classes, which permit duplicate definitions where latter definitions
    overwrite former ones. *)
val add_constructor:
  Loc.t option ->
  Func_sig.t ->
  ?set_asts:set_asts ->
  t -> t

val add_default_constructor: Reason.t -> t -> t

(** Add constructor override to signature.

    Does not overwrite existing constructors. This implements the behavior of
    interfaces, which interpret duplicate definitions as branches of a single
    overloaded constructor. *)
val append_constructor:
  Loc.t option ->
  Func_sig.t ->
  ?set_asts:set_asts ->
  t -> t

(** Add field to signature. *)
val add_field: static:bool -> string -> Loc.t -> Type.polarity -> field -> t -> t

(** Add indexer to signature. *)
val add_indexer:
  static:bool ->
  Type.polarity ->
  key:(Loc.t * Type.t) ->
  value:(Loc.t * Type.t) ->
  t -> t

(** Add static `name` field. *)
val add_name_field: t -> t

(** Add proto field to signature. *)
val add_proto_field: string -> Loc.t -> Type.polarity -> field -> t -> t

(** Add private field to signature. *)
val add_private_field: string -> Loc.t -> Type.polarity -> field -> static:bool -> t -> t

(** Add method to signature.

    Overwrites any existing synonymous method. This implements the behavior of
    classes, which permit duplicate definitions where latter definitions
    overwrite former ones. *)
val add_method:
  static:bool ->
  string ->
  Loc.t ->
  Func_sig.t ->
  ?set_asts:set_asts ->
  t -> t

(** Add method override to signature.

    Does not overwrite existing synonymous methods. This implements the
    behavior of interfaces, which interpret duplicate definitions as branches
    of a single overloaded method. *)
val append_method:
  static:bool ->
  string ->
  Loc.t ->
  Func_sig.t ->
  ?set_asts:set_asts ->
  t -> t

val append_call: static:bool -> Type.t -> t -> t

val add_call_deprecated: static:bool -> Type.t -> t -> t

(** Add getter to signature. *)
val add_getter:
  static:bool ->
  string ->
  Loc.t ->
  Func_sig.t ->
  ?set_asts:set_asts ->
  t -> t

(** Add setter to signature. *)
val add_setter:
  static:bool ->
  string ->
  Loc.t ->
  Func_sig.t ->
  ?set_asts:set_asts ->
  t -> t

(** Check if this signature defines a given field *)
val mem_field: string -> static:bool -> t -> bool

(** Check if this signature defines a constructor *)
val mem_constructor: t -> bool

val add_this:
  Type.t -> (* self *)
  Context.t ->
  Reason.t ->
  Type.typeparam list ->
  Type.t SMap.t -> (* tparams_map *)
  Type.typeparam list * Type.t SMap.t

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
  decls:(Context.t -> (Loc.t, Loc.t) Ast.Statement.t list -> unit) ->
  stmts:(Context.t -> (Loc.t, Loc.t) Ast.Statement.t list ->
                      (Loc.t, Loc.t * Type.t) Ast.Statement.t list) ->
  expr:(Context.t -> (Loc.t, Loc.t) Ast.Expression.t ->
                      (Loc.t, Loc.t * Type.t) Ast.Expression.t) ->
  t -> unit

(** 1. Type Conversion *)

val thistype: Context.t -> t -> Type.t

(* Create a (polymorphic) class type. *)
val classtype: Context.t ->
  ?check_polarity:bool ->
  t -> Type.t

module This: sig
  val is_bound_to_empty: t -> bool
  val in_class: (Loc.t, Loc.t) Ast.Class.t -> bool
end

val with_typeparams: Context.t -> (unit -> 'a) -> t -> 'a
