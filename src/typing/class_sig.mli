(** Intermediate representation for classes and interfaces *)

type t

type field = Type.t * Type.polarity * Ast.Expression.t option

type super =
  | Interface of {
      extends: Type.t list;
      callable: bool;
      static_callable: bool;
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
val add_constructor: Func_sig.t -> t -> t

(** Add constructor override to signature.

    Does not overwrite existing constructors. This implements the behavior of
    interfaces, which interpret duplicate definitions as branches of a single
    overloaded constructor. *)
val append_constructor: Func_sig.t -> t -> t

(** Add field to signature. *)
val add_field: string -> field -> static:bool -> t -> t

(** Add method to signature.

    Overwrites any existing synonymous method. This implements the behavior of
    classes, which permit duplicate definitions where latter definitions
    overwrite former ones. *)
val add_method: string -> Func_sig.t -> static:bool -> t -> t

(** Add method override to signature.

    Does not overwrite existing synonymous methods. This implements the
    behavior of interfaces, which interpret duplicate definitions as branches
    of a single overloaded method. *)
val append_method: string -> Func_sig.t -> static:bool -> t -> t

(** Add getter to signature. *)
val add_getter: string -> Func_sig.t -> static:bool -> t -> t

(** Add setter to signature. *)
val add_setter: string -> Func_sig.t -> static:bool -> t -> t

(** Create signature from class AST. *)
val mk: Context.t ->
  Loc.t ->
  Reason.t ->
  Type.t -> (* self *)
  expr:(Context.t -> Ast.Expression.t -> Type.t) ->
  Ast.Class.t ->
  t

(** Create signature from interface AST. *)
val mk_interface: Context.t ->
  Loc.t ->
  Reason.t ->
  bool -> (* structural *)
  Type.t -> (* self *)
  Ast.Statement.Interface.t ->
  t

(** 1. Manipulation *)

(** Emits constraints to ensure the signature is compatible with its declared
    interface implementations (classes) *)
val check_implements: Context.t -> t -> unit

(** Emits constraints to ensure the signature is compatible with its declared
    superclass (classes) or extends/mixins (interfaces) *)
val check_super: Context.t -> t -> unit

(** Invoke callback with type parameters substituted by upper/lower bounds. *)
val generate_tests: Context.t ->
  (t -> unit) -> t -> unit

(** Evaluate the class body. *)
val toplevels: Context.t ->
  decls:(Context.t -> Ast.Statement.t list -> unit) ->
  stmts:(Context.t -> Ast.Statement.t list -> unit) ->
  expr:(Context.t -> Ast.Expression.t -> Type.t) ->
  t -> unit

(** 1. Type Conversion *)

(* Create a (polymorphic) class type. *)
val classtype: Context.t ->
  ?check_polarity:bool ->
  t -> Type.t

module This: sig
  val is_bound_to_empty: t -> bool
  val in_class: Ast.Class.t -> bool
end
