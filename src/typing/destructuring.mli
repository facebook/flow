(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val destructuring :
  Context.t ->
  expr:(Context.t -> Loc.t Ast.Expression.t -> Type.t) ->
  f:(use_op:Type.use_op ->
    Loc.t ->
    string ->
    Loc.t Ast.Expression.t Default.t option ->
    Type.t -> unit) ->
  Type.t ->
  Loc.t Ast.Expression.t option ->
  Loc.t Ast.Expression.t Default.t option ->
  Loc.t Ast.Pattern.t -> unit
val type_of_pattern :
  'a * Loc.t Ast.Pattern.t' ->
  Loc.t Ast.Type.annotation option
val destructuring_assignment :
  Context.t ->
  expr:(Context.t -> Loc.t Ast.Expression.t -> Type.t) ->
  Type.t ->
  Loc.t Ast.Expression.t ->
  Loc.t Ast.Pattern.t -> unit
