(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val extract_destructured_bindings :
  (Loc.t * string) list ->
  Ast.Pattern.t' ->
  (Loc.t * string) list

val destructuring :
  Context.t ->
  expr:(Context.t -> Ast.Expression.t -> Type.t) ->
  f:(Loc.t ->
    string ->
    Ast.Expression.t Default.t option ->
    Type.t -> unit) ->
  Type.t ->
  Ast.Expression.t option ->
  Ast.Expression.t Default.t option ->
  Ast.Pattern.t -> unit
val type_of_pattern :
  'a * Ast.Pattern.t' ->
  Ast.Type.annotation option
val destructuring_assignment :
  Context.t ->
  expr:(Context.t -> Ast.Expression.t -> Type.t) ->
  Type.t ->
  Ast.Expression.t ->
  Ast.Pattern.t -> unit
