(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val find_local_refs :
  reader:State_reader.t ->
  options:Options.t ->
  File_key.t ->
  FindRefsUtils.ast_info ->
  Scope_api.With_Loc.info ->
  GetDefUtils.def_info ->
  (FindRefsTypes.find_refs_found, string) result
