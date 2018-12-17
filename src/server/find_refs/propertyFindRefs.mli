(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val find_refs:
  reader: State_reader.t ->
  ServerEnv.genv ->
  ServerEnv.env ref ->
  content: string ->
  File_key.t ->
  (GetDefUtils.def_info option, string) Core_result.t ->
  global: bool ->
  multi_hop: bool ->
  ((FindRefsTypes.find_refs_found * int option) option, string) result Lwt.t
