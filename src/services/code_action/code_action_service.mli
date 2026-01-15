(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val kind_is_supported : Lsp.CodeActionKind.t list option -> bool

type ast_transform =
  cx:Context.t ->
  file_sig:File_sig.t ->
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  Loc.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t option

(** The confidence level will be used to create a fix-all command that can potentially fix multiple
  * issues at once. The command will:
  *
  * 1. Run all transforms tagged with SafeForRunningOnSave.
  * 2. If requested, run the first transform tagged with BestEffort, if there's any. *)
type quickfix_confidence =
  | WillFixErrorAndSafeForRunningOnSave
      (** e.g. user used some deprecated syntax when we have a modern alternative.
        * As the same suggest, transforms tagged with this level must be very safe.
        * At the very least, it should not cause any runtime changes. *)
  | BestEffort  (** e.g. fixing subtyping errors *)

type ast_transform_of_error = {
  title: string;
  diagnostic_title: string;
  transform: ast_transform;
  target_loc: Loc.t;
  confidence: quickfix_confidence;
}

val untyped_ast_transform :
  ((Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t -> (Loc.t, Loc.t) Flow_ast.Program.t) -> ast_transform

val ast_transforms_of_error :
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  ?loc:Loc.t ->
  Loc.t Error_message.t' ->
  ast_transform_of_error list

val code_actions_at_loc :
  options:Options.t ->
  lsp_init_params:Lsp.Initialize.params ->
  imports_ranked_usage:bool ->
  env:ServerEnv.env ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  get_ast_from_shared_mem:(File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option) ->
  get_type_sig:(File_key.t -> Type_sig_collections.Locs.index Packed_type_sig.Module.t option) ->
  module_system_info:Lsp_module_system_info.t ->
  cx:Context.t ->
  file_sig:File_sig.t ->
  tolerable_errors:File_sig.tolerable_error list ->
  file_contents:string ->
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  scope_info:Scope_api.With_Loc.info ->
  parse_errors:(Loc.t * Parse_error.t) Base.List.t ->
  diagnostics:Lsp.PublishDiagnostics.diagnostic list ->
  only:Lsp.CodeActionKind.t list option ->
  uri:Lsp.DocumentUri.t ->
  loc:Loc.t ->
  (Lsp.CodeAction.command_or_action list, string) result

val autofix_errors_cli :
  options:Options.t ->
  profiling:Profiling_js.running ->
  env:ServerEnv.env ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  get_ast_from_shared_mem:(File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option) ->
  module_system_info:Lsp_module_system_info.t ->
  get_type_sig:(File_key.t -> Type_sig_collections.Locs.index Packed_type_sig.Module.t option) ->
  include_best_effort_fix:bool ->
  file_key:File_key.t ->
  file_content:string ->
  (Replacement_printer.patch, string) result

val autofix_imports_cli :
  options:Options.t ->
  profiling:Profiling_js.running ->
  env:ServerEnv.env ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  module_system_info:Lsp_module_system_info.t ->
  file_key:File_key.t ->
  file_content:string ->
  (Replacement_printer.patch, string) result

val suggest_imports_cli :
  options:Options.t ->
  profiling:Profiling_js.running ->
  env:ServerEnv.env ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  module_system_info:Lsp_module_system_info.t ->
  file_key:File_key.t ->
  file_content:string ->
  (Lsp.CodeAction.command_or_action list SMap.t, string) result

val autofix_imports_lsp :
  options:Options.t ->
  env:ServerEnv.env ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  module_system_info:Lsp_module_system_info.t ->
  cx:Context.t ->
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  uri:Lsp.DocumentUri.t ->
  Lsp.TextEdit.t list

val autofix_exports :
  options:Options.t ->
  master_cx:Context.master_context ->
  profiling:Profiling_js.running ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  get_ast_from_shared_mem:(File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option) ->
  get_haste_module_info:(File_key.t -> Haste_module_info.t option) ->
  get_type_sig:(File_key.t -> Type_sig_collections.Locs.index Packed_type_sig.Module.t option) ->
  file_key:File_key.t ->
  file_content:string ->
  (Replacement_printer.patch * string list, string) result

val autofix_missing_local_annot :
  options:Options.t ->
  master_cx:Context.master_context ->
  profiling:Profiling_js.running ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  get_ast_from_shared_mem:(File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option) ->
  get_haste_module_info:(File_key.t -> Haste_module_info.t option) ->
  get_type_sig:(File_key.t -> Type_sig_collections.Locs.index Packed_type_sig.Module.t option) ->
  file_key:File_key.t ->
  file_content:string ->
  (Replacement_printer.patch, string) result

val insert_type :
  options:Options.t ->
  env:ServerEnv.env ->
  profiling:Profiling_js.running ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  get_ast_from_shared_mem:(File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option) ->
  get_haste_module_info:(File_key.t -> Haste_module_info.t option) ->
  get_type_sig:(File_key.t -> Type_sig_collections.Locs.index Packed_type_sig.Module.t option) ->
  file_key:File_key.t ->
  file_content:string ->
  target:Loc.t ->
  omit_targ_defaults:bool ->
  location_is_strict:bool ->
  (Replacement_printer.patch, string) result

val organize_imports :
  options:Options.t -> ast:(Loc.t, Loc.t) Flow_ast.Program.t -> Lsp.TextEdit.t list
