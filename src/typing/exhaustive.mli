(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val analyze :
  Context.t ->
  match_loc:ALoc.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.MatchPattern.t list ->
  Type.t ->
  unit
