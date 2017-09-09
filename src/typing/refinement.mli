(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val key: Loc.t Ast.Expression.t -> Key.t option
val get:
  Context.t ->
  Loc.t Ast.Expression.t ->
  Loc.t ->
  Type.t option
