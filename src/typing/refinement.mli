(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val key: Spider_monkey_ast.Expression.t -> Key.t option
val get:
  Context.t ->
  Spider_monkey_ast.Expression.t ->
  Reason.reason ->
  Type.t option
