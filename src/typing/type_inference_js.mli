(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Lint suppressions are handled iff lint_severities is Some. *)
val infer_ast:
  metadata: Context.metadata ->
  filename: Loc.filename ->
  lint_severities: Severity.severity LintSettings.t option ->
  Ast.program ->
  require_loc_map: Loc.t SMap.t ->
  Context.t
(* Lint suppressions are handled iff lint_severities is Some. *)
val infer_lib_file:
  metadata: Context.metadata ->
  exclude_syms: SSet.t ->
  lint_severities: Severity.severity LintSettings.t option ->
  Utils_js.filename ->
  Ast.program ->
  Context.t * string list
