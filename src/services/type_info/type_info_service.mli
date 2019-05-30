(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val type_at_pos :
  options:Options.t ->
  env:ServerEnv.env ->
  profiling:Profiling_js.running ->
  expand_aliases:bool ->
  omit_targ_defaults:bool ->
  File_key.t ->
  string ->
  int ->
  int ->
  ((Loc.t * Ty.t option) * Hh_json.json option,
    string * Hh_json.json option) Core_result.t Lwt.t

val dump_types :
  options:Options.t ->
  env:ServerEnv.env ->
  profiling:Profiling_js.running ->
  File_key.t ->
  string ->
  ((Loc.t * string) list, string) Core_result.t Lwt.t

val coverage :
  options:Options.t ->
  env:ServerEnv.env ->
  profiling:Profiling_js.running ->
  force:bool ->
  trust:bool ->
  File_key.t ->
  string ->
  ((Loc.t * Coverage.expression_coverage) list, string) Core_result.t Lwt.t

val suggest :
  options:Options.t ->
  env:ServerEnv.env ->
  profiling:Profiling_js.running ->
  string -> string ->
  ((Errors.ConcreteLocPrintableErrorSet.t *   (* Typechecking errors *)
    Errors.ConcreteLocPrintableErrorSet.t *   (* Typechecking warnings *)
    Errors.ConcreteLocPrintableErrorSet.t *   (* Suggest-related warnings (normalization etc.) *)
    Replacement_printer.patch),   (* Annotated program *)
    Errors.ConcreteLocPrintableErrorSet.t     (* Parsing errors *)
  ) Core_result.t Lwt.t
