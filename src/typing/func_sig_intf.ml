(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  module Config_types : Func_class_sig_types.Config.S

  module Config : Func_params_intf.Config with module Types := Config_types

  module Param : Func_params.S with module Config_types := Config_types and module Config := Config

  module Types :
    Func_class_sig_types.Func.S with module Config := Config_types and module Param := Param.Types

  open Types

  (** 1. Constructors *)

  (** Create signature for a default constructor.

      Flow represents default constructors as empty functions, i.e., functions
      with no type parameters, no formal parameters, an empty body, and a void
      return type. *)
  val default_constructor : Reason.t -> t

  (** Create signature for a class field initializer.

      Field initializers are evaluated in the context of the class body.
      Representing the initializer as a function means we can reuse `toplevels`
      from this module to evaluate the initializer in the appropriate context,
      where `this` and `super` point to the appropriate types. *)
  val field_initializer :
    Type.t Subst_name.Map.t ->
    (* type params map *)
    Reason.t ->
    (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
    (* init *)
    Type.annotated_or_inferred ->
    (* return *)
    t

  (** 1. Manipulation *)

  val toplevels :
    Context.t ->
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

  (** Create a function type for function declarations/expressions. *)
  val functiontype :
    Context.t ->
    arrow:bool ->
    (* function this loc *) ALoc.t option ->
    Type.t ->
    (* this *)
    t ->
    Type.t

  (** Create a function type for class/interface methods. *)
  val methodtype :
    Context.t -> (* method this loc *) ALoc.t option -> (* this *) Type.t -> t -> Type.t

  (** Create a type of the return expression of a getter function.

      Note that this is a partial function. If the signature does not represent a
      getter, this function will raise an exception. *)
  val gettertype : t -> Type.t

  (** Create a type of the single parameter of a setter function.

      Note that this is a partial function. If the signature does not represent a
      setter, this function will raise an exception. *)
  val settertype : t -> Type.t

  (** 1. Util *)

  val to_ctor_sig : t -> t

  val this_param : func_params -> Type.t option
end
