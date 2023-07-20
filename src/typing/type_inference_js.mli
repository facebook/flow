(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val scan_for_suppressions :
  in_libdef:bool ->
  Severity.severity LintSettings.t ->
  (File_key.t * Loc.t Flow_ast.Comment.t list) list ->
  ExactCover.lint_severity_cover Utils_js.FilenameMap.t
  * Error_suppressions.t
  * ALoc.t Error_message.t' list

(* Lint suppressions are handled iff lint_severities is Some. *)
val infer_ast :
  lint_severities:Severity.severity LintSettings.t ->
  Context.t ->
  File_key.t ->
  Loc.t Flow_ast.Comment.t list ->
  (ALoc.t, ALoc.t) Flow_ast.Program.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t

(* Lint suppressions are handled iff lint_severities is Some. *)
val infer_lib_file :
  exclude_syms:NameUtils.Set.t ->
  lint_severities:Severity.severity LintSettings.t ->
  Context.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  Reason.name list
