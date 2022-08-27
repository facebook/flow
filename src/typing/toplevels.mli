(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val toplevels :
  (Context.t ->
  (ALoc.t, ALoc.t) Flow_ast.Statement.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.t
  ) ->
  Context.t ->
  (ALoc.t, ALoc.t) Flow_ast.Statement.t list ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.t list
