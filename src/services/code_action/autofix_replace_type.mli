(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val replace_type :
  f:((Loc.t, Loc.t) Flow_ast.Type.t' -> (Loc.t, Loc.t) Flow_ast.Type.t') ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  Loc.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t
