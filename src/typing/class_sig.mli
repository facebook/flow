(** Intermediate representation for classes and interfaces *)

type t

type field = Type.t * Type.polarity * Spider_monkey_ast.Expression.t option

(** 1. Constructors **)

(** Create signature with no elements. *)
val empty:
  ?structural:bool ->
  int -> (* id *)
  Reason.t ->
  Type.typeparam list ->
  Type.t SMap.t -> (* tparams_map *)
  Type.t -> (* super *)
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
  expr:(Context.t -> Spider_monkey_ast.Expression.t -> Type.t) ->
  Spider_monkey_ast.Class.t ->
  t

(** Create signature from interface AST. *)
val mk_interface: Context.t ->
  Loc.t ->
  Reason.t ->
  bool -> (* structural *)
  Type.t -> (* self *)
  Spider_monkey_ast.Statement.Interface.t ->
  t

(** 1. Manipulation *)

(** Emits constraints to ensure the signature is compatible with its declared
    superclass (classes) or extends/mixins (interfaces) *)
val check_super: Context.t -> t -> unit

(** Invoke callback with type parameters substituted by upper/lower bounds. *)
val generate_tests: Context.t ->
  (t -> unit) -> t -> unit

(** Evaluate the class body. *)
val toplevels: Context.t ->
  decls:(Context.t -> Spider_monkey_ast.Statement.t list -> unit) ->
  stmts:(Context.t -> Spider_monkey_ast.Statement.t list -> unit) ->
  expr:(Context.t -> Spider_monkey_ast.Expression.t -> Type.t) ->
  t -> unit

(** 1. Type Conversion *)

(* Create a (polymorphic) class type. *)
val classtype: Context.t ->
  ?check_polarity:bool ->
  t -> Type.t
