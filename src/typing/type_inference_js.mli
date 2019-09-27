(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Lint suppressions are handled iff lint_severities is Some. *)
val infer_ast :
  lint_severities:Severity.severity LintSettings.t ->
  file_sig:File_sig.With_ALoc.t ->
  Context.t ->
  File_key.t ->
  Loc.t Flow_ast.Comment.t list ->
  (ALoc.t, ALoc.t) Flow_ast.program ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.program

(* Lint suppressions are handled iff lint_severities is Some. *)
val infer_lib_file :
  exclude_syms:SSet.t ->
  lint_severities:Severity.severity LintSettings.t ->
  file_sig:File_sig.With_ALoc.t ->
  Context.t ->
  (Loc.t, Loc.t) Flow_ast.program ->
  string list
