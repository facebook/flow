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

module Operators : sig
  val arith : Context.t -> reason -> Type.ArithKind.t -> Type.t -> Type.t -> Type.t

  val check_comparator : Context.t -> Type.t -> Type.t -> unit

  val check_eq : Context.t -> Type.t * Type.t -> unit

  val check_strict_eq : encl_ctx:Type.enclosing_context -> Context.t -> Type.t * Type.t -> unit

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

  val assert_export_is_type : Context.t -> Reason.name -> Type.t -> Type.t

  val assert_for_in_rhs : Context.t -> Type.t -> unit

  val assert_non_component_like_base :
    Context.t -> ALoc.t -> ALoc.t virtual_reason -> Type.t -> unit

  val assert_instanceof_rhs : Context.t -> Type.t -> unit

  val assert_iterable :
    Context.t -> ALoc.t -> async:bool -> use_op:Type.use_op -> Type.t -> Type.t list -> unit

  val non_exhaustive : Context.t -> Type.t list -> bool
end
