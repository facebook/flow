(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val post_merge_checks :
  Context.t ->
  File_sig.t ->
  (ALoc.t, ALoc.t) Flow_ast.Program.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  Context.metadata ->
  unit

val get_lint_severities :
  Context.metadata ->
  StrictModeSettings.t ->
  Severity.severity LintSettings.t ->
  Severity.severity LintSettings.t

val copy_into :
  Context.t ->
  Context.t ->
  (unit -> (Type.moduletype, Type.t) result) ->
  unit ->
  (Type.moduletype, Type.t) result

val merge_lib_files :
  project_opts:Flow_projects.options ->
  sig_opts:Type_sig_options.t ->
  (string option * (Loc.t, Loc.t) Flow_ast.Program.t) list ->
  Flow_error.ErrorSet.t * Context.master_context

val mk_builtins : Context.metadata -> Context.master_context -> Context.t -> Builtins.t
