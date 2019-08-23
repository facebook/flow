(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val find_refs:
  reader: State_reader.t ->
  ServerEnv.genv ->
  ServerEnv.env ref ->
  File_key.t ->
  FindRefsUtils.ast_info ->
  GetDefUtils.def_info ->
  global: bool ->
  multi_hop: bool ->
  (FindRefsTypes.find_refs_found * int option, string) result Lwt.t
