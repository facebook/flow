(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

module type S = sig
  module Types : Func_class_sig_types.Config.S

  open Types

  val param_type : param -> string option * Type.t

  val rest_type : rest -> string option * ALoc.t * Type.t

  val this_type : this_param -> Type.t

  val subst_param : Context.t -> Type.t Subst_name.Map.t -> param -> param

  val subst_rest : Context.t -> Type.t Subst_name.Map.t -> rest -> rest

  val subst_this : Context.t -> Type.t Subst_name.Map.t -> this_param -> this_param

  val bind : Context.t -> string -> Type.annotated_or_inferred -> ALoc.t -> unit

  val destruct :
    Context.t ->
    use_op:Type.use_op ->
    name_loc:ALoc.t ->
    has_anno:bool ->
    string ->
    Type.t Default.t Base.Option.t ->
    Type.t ->
    Type.t

  val eval_default :
    Context.t ->
    annot_t:Type.t option ->
    (ALoc.t, ALoc.t) Ast.Expression.t option ->
    (ALoc.t, ALoc.t * Type.t) Ast.Expression.t option

  val eval_param : Context.t -> param -> (ALoc.t * Type.t) param_ast

  val eval_rest : Context.t -> rest -> (ALoc.t * Type.t) rest_ast

  val eval_this : Context.t -> this_param -> (ALoc.t * Type.t) this_ast
end
