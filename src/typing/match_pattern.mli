(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

val pattern :
  Context.t ->
  on_identifier:(Context.t -> ALoc.t Ast.Identifier.t' -> ALoc.t -> Type.t) ->
  on_expression:
    (Context.t -> (ALoc.t, ALoc.t) Ast.Expression.t -> (ALoc.t, ALoc.t * Type.t) Ast.Expression.t) ->
  on_binding:
    (use_op:Type.use_op -> name_loc:ALoc.t -> kind:Ast.Variable.kind -> string -> Type.t -> Type.t) ->
  (ALoc.t, ALoc.t) Ast.Expression.t ->
  (ALoc.t, ALoc.t) Ast.MatchPattern.t ->
  (ALoc.t, ALoc.t * Type.t) Ast.MatchPattern.t

val type_of_member_pattern :
  Context.t ->
  on_identifier:(Context.t -> ALoc.t Ast.Identifier.t' -> ALoc.t -> Type.t) ->
  on_expression:
    (Context.t -> (ALoc.t, ALoc.t) Ast.Expression.t -> (ALoc.t, ALoc.t * Type.t) Ast.Expression.t) ->
  (ALoc.t, ALoc.t) Ast.MatchPattern.MemberPattern.t ->
  Type.t
