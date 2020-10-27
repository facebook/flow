(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type kind =
  | Ordinary
  | Async
  | Generator
  | AsyncGenerator
  | FieldInit of (ALoc.t, ALoc.t) Flow_ast.Expression.t
  | Predicate
  | Ctor

module type S = sig
  type func_params

  type func_params_tast

  type t = {
    reason: Reason.t;
    kind: kind;
    tparams: Type.typeparams;
    tparams_map: Type.t SMap.t;
    fparams: func_params;
    body: (ALoc.t, ALoc.t) Flow_ast.Function.body option;
    return_t: Type.annotated_or_inferred;
    knot: Type.t;
  }

  (** 1. Constructors *)

  val default_constructor : Reason.t -> t
  (** Create signature for a default constructor.

    Flow represents default constructors as empty functions, i.e., functions
    with no type parameters, no formal parameters, an empty body, and a void
    return type. *)

  val field_initializer :
    has_anno:bool ->
    Type.t SMap.t ->
    (* type params map *)
    Reason.t ->
    (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
    (* init *)
    Type.t ->
    (* return *)
    t
  (** Create signature for a class field initializer.

    Field initializers are evaluated in the context of the class body.
    Representing the initializer as a function means we can reuse `toplevels`
    from this module to evaluate the initializer in the appropriate context,
    where `this` and `super` point to the appropriate types. *)

  (** 1. Manipulation *)

  val subst : Context.t -> Type.t SMap.t -> (* type params map *)
                                            t -> t
  (** Return a signature with types from provided map substituted.

    Note that this function does not substitute type parameters declared by the
    function itself, which may shadow the names of type parameters in the
    provided map.

    This signature's own type parameters will be subtituted by the
    `generate-tests` function. *)

  val check_with_generics : Context.t -> (t -> 'a) -> t -> 'a
  (** Invoke callback with type parameters substituted by upper/lower bounds. *)

  val toplevels :
    (ALoc.t, ALoc.t) Flow_ast.Identifier.t option ->
    (* id *)
    Context.t ->
    Scope.Entry.t ->
    (* this *)
    Scope.Entry.t ->
    decls:((* super *)
           Context.t -> (ALoc.t, ALoc.t) Flow_ast.Statement.t list -> unit) ->
    stmts:
      (Context.t ->
      (ALoc.t, ALoc.t) Flow_ast.Statement.t list ->
      (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.t list) ->
    expr:
      (?cond:Type.cond_context ->
      Context.t ->
      annot:unit option ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t) ->
    t ->
    func_params_tast option
    * (ALoc.t, ALoc.t * Type.t) Flow_ast.Function.body option
    * (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t option
  (** Evaluate the function.

    This function creates a new scope, installs bindings for the function's
    parameters and internal bindings (e.g., this, yield), processes the
    statements in the function body, and provides an implicit return type if
    necessary. This is when the body of the function gets checked, so it also
    returns a typed AST of the function body. *)

  (** 1. Type Conversion *)

  val functiontype : Context.t -> Type.t -> (* this *)
                                            t -> Type.t
  (** Create a function type for function declarations/expressions. *)

  val methodtype : Context.t -> t -> Type.t
  (** Create a function type for class/interface methods. *)

  val gettertype : t -> Type.t
  (** Create a type of the return expression of a getter function.

    Note that this is a partial function. If the signature does not represent a
    getter, this function will raise an exception. *)

  val settertype : t -> Type.t
  (** Create a type of the single parameter of a setter function.

    Note that this is a partial function. If the signature does not represent a
    setter, this function will raise an exception. *)

  (** 1. Util *)

  val to_ctor_sig : t -> t
end
