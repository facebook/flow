(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val type_at_pos :
  options:Options.t ->
  workers:MultiWorkerLwt.worker list option ->
  env:ServerEnv.env ref ->
  profiling:Profiling_js.running ->
  File_key.t ->
  string ->
  int ->
  int ->
  ((Loc.t * string option) * Hh_json.json option,
    string * Hh_json.json option) Core_result.t Lwt.t

val dump_types :
  options:Options.t ->
  workers:MultiWorkerLwt.worker list option ->
  env:ServerEnv.env ref ->
  profiling:Profiling_js.running ->
  File_key.t ->
  string ->
  ((Loc.t * string) list, string) Core_result.t Lwt.t

val coverage :
  options:Options.t ->
  workers:MultiWorkerLwt.worker list option ->
  env:ServerEnv.env ref ->
  profiling:Profiling_js.running ->
  force:bool ->
  File_key.t ->
  string -> ((Loc.t * bool) list, string) Core_result.t Lwt.t

val suggest :
  options:Options.t ->
  workers:MultiWorkerLwt.worker list option ->
  env:ServerEnv.env ref ->
  profiling:Profiling_js.running ->
  File_key.t ->
  string ->
  ((Errors.ErrorSet.t *   (* Typechecking errors *)
    Errors.ErrorSet.t *   (* Typechecking warnings *)
    Errors.ErrorSet.t *   (* Suggest-related warnings (normalization etc.) *)
    Loc.t Ast.program),   (* Annotated program *)
    Errors.ErrorSet.t     (* Parsing errors *)
  ) Core_result.t Lwt.t
