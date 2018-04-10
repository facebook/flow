(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Intermediate representation for classes and interfaces *)

type t

type field = Loc.t option * Type.polarity * field'
and field' = Annot of Type.t | Infer of Func_sig.t

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
val add_constructor: Loc.t option -> Func_sig.t -> t -> t

(** Add constructor override to signature.

    Does not overwrite existing constructors. This implements the behavior of
    interfaces, which interpret duplicate definitions as branches of a single
    overloaded constructor. *)
val append_constructor: Loc.t option -> Func_sig.t -> t -> t

(** Add field to signature. *)
val add_field: static:bool -> string -> field -> t -> t

(** Add method to signature.

    Overwrites any existing synonymous method. This implements the behavior of
    classes, which permit duplicate definitions where latter definitions
    overwrite former ones. *)
val add_method: static:bool -> string -> Loc.t option -> Func_sig.t -> t -> t

(** Add method override to signature.

    Does not overwrite existing synonymous methods. This implements the
    behavior of interfaces, which interpret duplicate definitions as branches
    of a single overloaded method. *)
val append_method: static:bool -> string -> Loc.t option -> Func_sig.t -> t -> t

(** Add getter to signature. *)
val add_getter: static:bool -> string -> Loc.t option -> Func_sig.t -> t -> t

(** Add setter to signature. *)
val add_setter: static:bool -> string -> Loc.t option -> Func_sig.t -> t -> t

(** Create signature from class AST. *)
val mk: Context.t ->
  Loc.t ->
  Reason.t ->
  Type.t -> (* self *)
  expr:(Context.t -> Loc.t Ast.Expression.t -> Type.t) ->
  Loc.t Ast.Class.t ->
  t

(** Create signature from interface AST. *)
val of_interface: Context.t ->
  Reason.t ->
  Loc.t Ast.Statement.Interface.t ->
  (t * Type.t (* self *))

(** Create signature from DeclareClass AST. *)
val of_declare_class: Context.t ->
  Reason.t ->
  Loc.t Ast.Statement.DeclareClass.t ->
  (t * Type.t (* self *))

(** 1. Manipulation *)

(** Emits constraints to ensure the signature is compatible with its declared
    interface implementations (classes) *)
val check_implements: Context.t -> Reason.reason -> t -> unit

(** Emits constraints to ensure the signature is compatible with its declared
    superclass (classes) or extends/mixins (interfaces) *)
val check_super: Context.t -> Reason.reason -> t -> unit

(** Invoke callback with type parameters substituted by upper/lower bounds. *)
val generate_tests: Context.t ->
  (t -> unit) -> t -> unit

(** Evaluate the class body. *)
val toplevels: Context.t ->
  decls:(Context.t -> Loc.t Ast.Statement.t list -> unit) ->
  stmts:(Context.t -> Loc.t Ast.Statement.t list -> unit) ->
  expr:(Context.t -> Loc.t Ast.Expression.t -> Type.t) ->
  t -> unit

(** 1. Type Conversion *)

(* Create a (polymorphic) class type. *)
val classtype: Context.t ->
  ?check_polarity:bool ->
  t -> Type.t

module This: sig
  val is_bound_to_empty: t -> bool
  val in_class: Loc.t Ast.Class.t -> bool
end
