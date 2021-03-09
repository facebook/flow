(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This variant is limited strictly to local variables, and does not attempt to find anything to do
 * with exports (the above will find some additional locations related to imports/exports even with
 * global:false). *)
val local_find_refs :
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  Loc.t ->
  (FindRefsTypes.find_refs_found * Loc.t) (* definition location *) option
