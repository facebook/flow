(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type search_result = {
  local_locs: Loc.t list;
  remote_locs: Loc.t list;
}

val search :
  options:Options.t ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  cx:Context.t ->
  file_sig:File_sig.t ->
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  Loc.t list ->
  search_result
