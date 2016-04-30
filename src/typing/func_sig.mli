(** Intermediate representation for functions *)

type t

(** 1. Constructors *)

(** Create signature from function AST. *)
val mk_function: Context.t ->
  Type.t SMap.t -> (* type params map *)
  Reason_js.t ->
  Type.t -> (* this *)
  Spider_monkey_ast.Function.t ->
  t

val mk_method: Context.t ->
  Type.t SMap.t -> (* type params map *)
  Reason_js.t ->
  Type.t -> (* implicit `this` pseudo-parameter *)
  Spider_monkey_ast.Function.t ->
  t

(** Create function signature from function type AST. *)
(*TJP: This functionality could be achieved by pushing a "this" into
  `tparams_map` and using `~static:false`, but that's too magical. I'm tempted
  to remodel the AST....*)
val convert_function: Context.t ->
  Type.t SMap.t -> (* type params map *)
  Type.t -> (* this *)
  Loc.t ->
  Spider_monkey_ast.Type.Function.t ->
  t

(** Create method signature from function type AST. *)
val convert_method: Context.t ->
  Type.t SMap.t -> (* type params map *)
  ?static:bool ->
  Loc.t ->
  Spider_monkey_ast.Type.Function.t ->
  t

(** Create signature for a default constructor.

    Flow represents default constructors as empty functions, i.e., functions
    with no type parameters, no formal parameters, an empty body, and a void
    return type. *)
val default_constructor:
  Type.t SMap.t -> (* type params map *)
  Reason_js.t ->
  t

(** Create signature for a class field initializer.

    Field initializers are evaluated in the context of the class body.
    Representing the initializer as a function means we can reuse `toplevels`
    from this module to evaluate the initializer in the appropriate context,
    where `this` and `super` point to the appropriate types. *)
val field_initializer:
  Type.t SMap.t -> (* type params map *)
  Reason_js.t ->
  Spider_monkey_ast.Expression.t -> (* init *)
  Type.t -> (* return type *)
  t

(** 1. Manipulation *)

val this: t -> Type.t

(** Return a signature with types from provided map substituted.

    Note that this function does not substitute type parameters declared by the
    function itself, which may shadow the names of type parameters in the
    provided map.

    This signature's own type parameters will be subtituted by the
    `generate-tests` function. *)
val subst: Context.t ->
  Type.t SMap.t -> (* type params map *)
  t -> t

(** Invoke callback with type parameters substituted by upper/lower bounds. *)
val generate_tests: Context.t ->
  (t -> unit) -> t -> unit

(** Evaluate the function.

    This function creates a new scope, installs bindings for the function's
    parameters and internal bindings (e.g., this, yield), processes the
    statements in the function body, and provides an implicit return type if
    necessary *)
val toplevels:
  Spider_monkey_ast.Identifier.t option -> (* id *)
  Context.t ->
  Scope.Entry.t -> (* this *)
  Scope.Entry.t -> (* super *)
  decls:(
    Context.t ->
    Type.t SMap.t -> (* type params map *)
    Spider_monkey_ast.Statement.t list -> unit) ->
  stmts:(
    Context.t ->
    Type.t SMap.t -> (* type params map *)
    Spider_monkey_ast.Statement.t list -> unit) ->
  expr:(
    Context.t ->
    Type.t SMap.t -> (* type params map *)
    Spider_monkey_ast.Expression.t -> Type.t) ->
  t -> unit

(** 1. Type Conversion *)

(*TJP: Remove the following two, rolling their functionality into `mk` and
  `mk_class_method`--two functions in need of renaming.*)
(** Create a function type for function declarations/expressions. *)
val functiontype: Context.t -> t -> Type.t

(** Create a function type for class/interface methods. *)
val methodtype: t -> Type.t

(** Create a type of the return expression of a getter function.

    Note that this is a partial function. If the signature does not represent a
    getter, this function will raise an exception. *)
val gettertype: t -> Type.t

(** Create a type of the single parameter of a setter function.

    Note that this is a partial function. If the signature does not represent a
    setter, this function will raise an exception. *)
val settertype: t -> Type.t

val methodtype_DEPRECATED: t -> Type.t

(** 1. Util *)

(** The location of the return type for a function. *)
val return_loc: Spider_monkey_ast.Function.t -> Loc.t
