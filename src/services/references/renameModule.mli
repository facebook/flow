(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val get_rename_edits :
  reader:State_reader.t ->
  options:Options.t ->
  old_haste_name:string ->
  new_haste_name:string ->
  File_key.t ->
  (Lsp.WorkspaceEdit.t, string) result
