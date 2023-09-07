(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val property_find_refs_in_file :
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  FindRefsUtils.ast_info ->
  Types_js_types.typecheck_artifacts ->
  File_key.t ->
  Get_def_types.property_def_info ->
  (FindRefsTypes.single_ref list, string) result

val find_local_refs :
  reader:State_reader.t ->
  File_key.t ->
  FindRefsUtils.ast_info ->
  Types_js_types.typecheck_artifacts ->
  Loc.t ->
  (FindRefsTypes.find_refs_found option, string) result
