(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val keywords_at_loc :
  component_syntax_enabled:bool ->
  pattern_matching_expressions_enabled:bool ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  Loc.t ->
  string list
