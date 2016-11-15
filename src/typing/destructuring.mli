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
  Spider_monkey_ast.Pattern.t' ->
  (Loc.t * string) list

val destructuring :
  Context.t ->
  expr:(Context.t -> Spider_monkey_ast.Expression.t -> Type.t) ->
  f:(Loc.t ->
    string ->
    Spider_monkey_ast.Expression.t Default.t option ->
    Type.t -> unit) ->
  Type.t ->
  Spider_monkey_ast.Expression.t option ->
  Spider_monkey_ast.Expression.t Default.t option ->
  Spider_monkey_ast.Pattern.t -> unit
val type_of_pattern :
  'a * Spider_monkey_ast.Pattern.t' ->
  Spider_monkey_ast.Type.annotation option
val destructuring_assignment :
  Context.t ->
  expr:(Context.t -> Spider_monkey_ast.Expression.t -> Type.t) ->
  Type.t ->
  Spider_monkey_ast.Expression.t ->
  Spider_monkey_ast.Pattern.t -> unit
