(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val client_supports_quickfixes : Lsp.CodeActionRequest.params -> bool

type text_edits = {
  title: string;
  edits: Lsp.TextEdit.t list;
  from: string;
}

val text_edits_of_import :
  options:Options.t ->
  layout_options:Js_layout_generator.opts ->
  reader:State_reader.t ->
  src_dir:string option ->
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  Export_index.kind ->
  string ->
  Export_index.source ->
  text_edits option

val code_actions_at_loc :
  options:Options.t ->
  env:ServerEnv.env ->
  reader:Parsing_heaps.Reader.reader ->
  cx:Context.t ->
  file_sig:File_sig.With_Loc.t ->
  tolerable_errors:File_sig.With_Loc.tolerable_error list ->
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  parse_errors:(Loc.t * Parse_error.t) Base.List.t ->
  diagnostics:Lsp.PublishDiagnostics.diagnostic list ->
  uri:Lsp.DocumentUri.t ->
  loc:Loc.t ->
  (Lsp.CodeAction.command_or_action list, string) result Lwt.t

val autofix_exports :
  options:Options.t ->
  env:ServerEnv.env ->
  profiling:Profiling_js.running ->
  file_key:File_key.t ->
  file_content:string ->
  (Replacement_printer.patch * string list, string) result Lwt.t

val insert_type :
  options:Options.t ->
  env:ServerEnv.env ->
  profiling:Profiling_js.running ->
  file_key:File_key.t ->
  file_content:string ->
  target:Loc.t ->
  expand_aliases:bool ->
  omit_targ_defaults:bool ->
  location_is_strict:bool ->
  ambiguity_strategy:Autofix_options.ambiguity_strategy ->
  (Replacement_printer.patch, string) result Lwt.t

module For_tests : sig
  val path_of_modulename :
    node_resolver_dirnames:string list ->
    reader:State_reader.t ->
    string option ->
    Modulename.t ->
    string option
end
