(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val optimize_builtins : Context.t -> unit

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
