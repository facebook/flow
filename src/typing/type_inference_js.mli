(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Lint suppressions are handled iff lint_severities is Some. *)
val infer_ast:
  lint_severities: Severity.severity LintSettings.t ->
  file_sig: File_sig.t ->
  Context.t ->
  File_key.t ->
  Loc.t Ast.program ->
  unit
(* Lint suppressions are handled iff lint_severities is Some. *)
val infer_lib_file:
  metadata: Context.metadata ->
  exclude_syms: SSet.t ->
  lint_severities: Severity.severity LintSettings.t ->
  file_sig: File_sig.t ->
  File_key.t ->
  Loc.t Ast.program ->
  Context.t * string list
