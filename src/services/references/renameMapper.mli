(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(**
Renames the target refs with the `new_name`
*)
val rename :
  global:bool ->
  targets:FindRefsTypes.ref_kind Loc_collections.LocMap.t ->
  new_name:string ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t
