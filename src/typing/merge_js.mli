(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val optimize_builtins : Context.t -> unit

val post_merge_checks : Context.t -> (ALoc.t, ALoc.t) Flow_ast.Program.t -> Context.metadata -> unit

val get_lint_severities :
  Context.metadata ->
  StrictModeSettings.t ->
  Severity.severity LintSettings.t ->
  Severity.severity LintSettings.t

val merge_lib_files :
  sig_opts:Type_sig_options.t ->
  ccx:Context.component_t ->
  metadata:Context.metadata ->
  (Loc.t, Loc.t) Flow_ast.Program.t list ->
  Type_sig_collections.Locs.index Packed_type_sig.Builtins.t * Context.t option
