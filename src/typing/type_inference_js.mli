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

val initialize_env :
  ?exclude_syms:SSet.t -> Context.t -> (ALoc.t, ALoc.t) Flow_ast.Program.t -> unit

(* Lint suppressions are handled iff lint_severities is Some. *)
val infer_ast :
  lint_severities:Severity.severity LintSettings.t ->
  Context.t ->
  File_key.t ->
  Context.metadata ->
  Loc.t Flow_ast.Comment.t list ->
  (ALoc.t, ALoc.t) Flow_ast.Program.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t

val infer_file :
  lint_severities:Severity.severity LintSettings.t ->
  Context.t ->
  File_key.t ->
  Context.metadata ->
  Loc.t Flow_ast.Comment.t list ->
  (ALoc.t, ALoc.t) Flow_ast.Program.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t
