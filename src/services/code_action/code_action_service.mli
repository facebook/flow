(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val code_actions_at_loc :
  reader:Parsing_heaps.Reader.reader ->
  options:Options.t ->
  env:ServerEnv.env ->
  profiling:Profiling_js.running ->
  params:Lsp.CodeActionRequest.params ->
  file_key:File_key.t ->
  file_contents:string ->
  loc:Loc.t ->
  (Lsp.CodeAction.command_or_action list, string) result Lwt.t
