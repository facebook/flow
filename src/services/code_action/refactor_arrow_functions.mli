(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val add_or_remove_braces :
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  scope_info:Scope_api.With_Loc.info ->
  Loc.t ->
  ((Loc.t, Loc.t) Flow_ast.Program.t * string) option
