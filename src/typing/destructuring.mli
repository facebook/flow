(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type state

type expr =
  Context.t ->
  (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t

type callback =
  use_op:Type.use_op ->
  ALoc.t ->
  string ->
  (ALoc.t, ALoc.t) Flow_ast.Expression.t Default.t option ->
  Type.t ->
  unit

val empty:
  ?init:(ALoc.t, ALoc.t) Flow_ast.Expression.t ->
  ?default:(ALoc.t, ALoc.t) Flow_ast.Expression.t Default.t ->
  Type.t ->
  state

val pattern:
  Context.t ->
  expr:expr ->
  f:callback ->
  state ->
  (ALoc.t, ALoc.t) Flow_ast.Pattern.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Pattern.t

val type_of_pattern:
  'a * (ALoc.t, ALoc.t) Flow_ast.Pattern.t' ->
  (ALoc.t, ALoc.t) Flow_ast.Type.annotation_or_hint

val assignment :
  Context.t ->
  expr:expr ->
  Type.t ->
  (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
  (ALoc.t, ALoc.t) Flow_ast.Pattern.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Pattern.t
