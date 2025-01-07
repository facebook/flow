(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module contains a collection of functions that operate on types, with the LTI assumption
 * the inspected type will never receive any additional future bounds. *)

module Ast = Flow_ast
open Reason
open FlowSymbol

module DistributeUnionIntersection : sig
  val distribute :
    Context.t ->
    ?use_op:Type.use_op ->
    break_up_union:(Context.t -> reason -> Type.t -> Type.t list) ->
    get_no_match_error_loc:(reason -> ALoc.t) ->
    check_base:(Context.t -> Type.t -> unit) ->
    Type.t ->
    unit

  val distribute_2 :
    Context.t ->
    ?use_op:Type.use_op ->
    break_up_union:(Context.t -> reason -> Type.t -> Type.t list) ->
    get_no_match_error_loc:(reason -> reason -> ALoc.t) ->
    check_base:(Context.t -> Type.t * Type.t -> unit) ->
    Type.t * Type.t ->
    unit
end

module Import_export : sig
  val concretize_module_type :
    Context.t -> Reason.t -> Type.t -> (Type.moduletype, Reason.reason * Type.any_source) result

  val get_module_t :
    Context.t ->
    ?perform_platform_validation:bool ->
    import_kind_for_untyped_import_validation:Type.import_kind option ->
    ALoc.t * string ->
    Type.t

  val import_named_specifier_type :
    Context.t ->
    reason ->
    Ast.Statement.ImportDeclaration.import_kind ->
    module_name:string ->
    source_module_t:Type.t ->
    remote_name:string ->
    local_name:string ->
    ALoc.t option * Type.t

  val get_module_namespace_type : Context.t -> reason -> namespace_symbol:symbol -> Type.t -> Type.t

  val assert_export_is_type : Context.t -> Reason.name -> Type.t -> Type.t

  val import_namespace_specifier_type :
    Context.t ->
    reason ->
    Ast.Statement.ImportDeclaration.import_kind ->
    module_name:string ->
    namespace_symbol:symbol ->
    source_module_t:Type.t ->
    local_loc:ALoc.t ->
    Type.t

  val import_default_specifier_type :
    Context.t ->
    reason ->
    Ast.Statement.ImportDeclaration.import_kind ->
    module_name:string ->
    source_module_t:Type.t ->
    local_name:string ->
    ALoc.t option * Type.t

  val cjs_require_type :
    Context.t ->
    reason ->
    namespace_symbol:symbol ->
    standard_cjs_esm_interop:bool ->
    legacy_interop:bool ->
    Type.t ->
    ALoc.t option * Type.t
end

module Operators : sig
  val arith : Context.t -> reason -> Type.ArithKind.t -> Type.t -> Type.t -> Type.t

  val check_comparator : Context.t -> Type.t -> Type.t -> unit

  val check_eq : Context.t -> Type.t * Type.t -> unit

  val check_strict_eq :
    cond_context:Type.cond_context option -> Context.t -> Type.t * Type.t -> unit

  val logical_and : Context.t -> reason -> Type.t -> Type.t -> Type.t

  val logical_or : Context.t -> reason -> Type.t -> Type.t -> Type.t

  val logical_nullish_coalesce : Context.t -> reason -> Type.t -> Type.t -> Type.t

  val unary_arith : Context.t -> reason -> Type.UnaryArithKind.t -> Type.t -> Type.t

  val unary_not : Context.t -> reason -> Type.t -> Type.t
end

module Promise : sig
  val await : Context.t -> reason -> Type.t -> Type.t
end

module TypeAssertions : sig
  val assert_binary_in_lhs : Context.t -> Type.t -> unit

  val assert_binary_in_rhs : Context.t -> Type.t -> unit

  val assert_for_in_rhs : Context.t -> Type.t -> unit

  val assert_non_component_like_base :
    Context.t -> ALoc.t -> ALoc.t virtual_reason -> Type.t -> unit

  val assert_instanceof_rhs : Context.t -> Type.t -> unit

  val assert_iterable :
    Context.t -> ALoc.t -> async:bool -> use_op:Type.use_op -> Type.t -> Type.t list -> unit

  val non_exhaustive : Context.t -> Type.t list -> bool
end
