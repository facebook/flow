(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type shorthandKind =
  | Obj
  | Import
  | Export

(**
Given a list of property locations, creates a map from each of those locations to the property kind.
If the property is not a shorthand or named import, then the property will not be added to the map
*)
val rename :
  targets:FindRefsTypes.ref_kind Loc_collections.LocMap.t ->
  new_name:string ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t
