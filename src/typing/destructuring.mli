(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val destructuring :
  Context.t ->
  expr:(Context.t -> Loc.t Ast.Expression.t -> Type.t) ->
  f:(Loc.t ->
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
