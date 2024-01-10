(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val post_merge_checks :
  Context.t ->
  (ALoc.t, ALoc.t) Flow_ast.Program.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  Context.metadata ->
  unit

val get_lint_severities :
  Context.metadata ->
  StrictModeSettings.t ->
  Severity.severity LintSettings.t ->
  Severity.severity LintSettings.t

val copy_into : Context.t -> Context.t -> Type.t -> unit

val merge_lib_files :
  sig_opts:Type_sig_options.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t list ->
  Type_sig_collections.Locs.index Packed_type_sig.Builtins.t * Context.master_context

val mk_builtins : Context.metadata -> Context.master_context -> Context.t -> Builtins.t
