(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val destructuring :
  Context.t ->
  expr:(Context.t -> (Loc.t, Loc.t) Ast.Expression.t -> (Loc.t, Loc.t * Type.t) Ast.Expression.t) ->
  f:(use_op:Type.use_op ->
    Loc.t ->
    string ->
    (Loc.t, Loc.t) Ast.Expression.t Default.t option ->
    Type.t -> unit) ->
  Type.t ->
  (Loc.t, Loc.t) Ast.Expression.t option ->
  (Loc.t, Loc.t) Ast.Expression.t Default.t option ->
  (Loc.t, Loc.t) Ast.Pattern.t ->
  (Loc.t, Loc.t * Type.t) Ast.Pattern.t
val type_of_pattern :
  'a * (Loc.t, Loc.t) Ast.Pattern.t' ->
  (Loc.t, Loc.t) Ast.Type.annotation option
val destructuring_assignment :
  Context.t ->
  expr:(
    Context.t -> (Loc.t, Loc.t) Ast.Expression.t ->
    (Loc.t, Loc.t * Type.t) Ast.Expression.t
  ) ->
  Type.t ->
  (Loc.t, Loc.t) Ast.Expression.t ->
  (Loc.t, Loc.t) Ast.Pattern.t ->
  (Loc.t, Loc.t * Type.t) Ast.Pattern.t
