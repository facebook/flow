(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val destructuring :
  Context.t ->
  expr:(Context.t -> (ALoc.t, ALoc.t) Flow_ast.Expression.t -> (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t) ->
  f:(use_op:Type.use_op ->
    ALoc.t ->
    string ->
    (ALoc.t, ALoc.t) Flow_ast.Expression.t Default.t option ->
    Type.t -> unit) ->
  Type.t ->
  (ALoc.t, ALoc.t) Flow_ast.Expression.t option ->
  (ALoc.t, ALoc.t) Flow_ast.Expression.t Default.t option ->
  (ALoc.t, ALoc.t) Flow_ast.Pattern.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Pattern.t
val type_of_pattern :
  'a * (ALoc.t, ALoc.t) Flow_ast.Pattern.t' ->
  (ALoc.t, ALoc.t) Flow_ast.Type.annotation_or_hint
val destructuring_assignment :
  Context.t ->
  expr:(
    Context.t -> (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t
  ) ->
  Type.t ->
  (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
  (ALoc.t, ALoc.t) Flow_ast.Pattern.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Pattern.t
