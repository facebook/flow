(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val key: Loc.t Ast.Expression.t -> Key.t option
val get:
  Context.t ->
  Loc.t Ast.Expression.t ->
  Loc.t ->
  Type.t option
