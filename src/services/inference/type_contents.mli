(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val parse_contents :
  options:Options.t ->
  profiling:Profiling_js.running ->
  (* contents *)
  string ->
  (* fake file-/module name *)
  File_key.t ->
  (Types_js_types.parse_artifacts option * Flow_error.ErrorSet.t) Lwt.t

val ensure_checked_dependencies :
  options:Options.t ->
  reader:State_reader.t ->
  env:ServerEnv.env ->
  File_key.t ->
  File_sig.With_Loc.t ->
  unit Lwt.t

val type_parse_artifacts :
  options:Options.t ->
  env:ServerEnv.env ->
  profiling:Profiling_js.running ->
  (* fake file-/module name *)
  File_key.t ->
  Types_js_types.parse_artifacts option * Flow_error.ErrorSet.t ->
  (Types_js_types.file_artifacts, Flow_error.ErrorSet.t) result Lwt.t

val printable_errors_of_file_artifacts_result :
  options:Options.t ->
  env:ServerEnv.env ->
  (* fake file-/module name *)
  File_key.t ->
  (Types_js_types.file_artifacts, Flow_error.ErrorSet.t) result ->
  (* errors *)
  Errors.ConcreteLocPrintableErrorSet.t * (* warnings *)
                                          Errors.ConcreteLocPrintableErrorSet.t
