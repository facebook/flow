(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

val init:
  profiling:Profiling_js.t ->
  workers:Worker.t list option ->
  Options.t ->
  Profiling_js.t * FilenameSet.t * SSet.t * bool * ServerEnv.errors

val infer:
  options:Options.t ->
  profiling:Profiling_js.t ->
  workers:Worker.t list option ->
  suppressions:Error_suppressions.t FilenameMap.t ->
  lint_settings:SuppressionMap.t FilenameMap.t ->
  filename list ->
  Profiling_js.t * (
    Errors.ErrorSet.t FilenameMap.t *
    Error_suppressions.t FilenameMap.t *
    SuppressionMap.t FilenameMap.t
  )

val calc_deps:
  options:Options.t ->
  profiling:Profiling_js.t ->
  workers:Worker.t list option ->
  filename list ->
  Profiling_js.t * (FilenameSet.t FilenameMap.t * filename list FilenameMap.t)

(* incremental typecheck entry point *)
val recheck:
  options:Options.t ->
  workers:Worker.t list option ->
  updates:FilenameSet.t ->
  ServerEnv.env -> ServerEnv.env

(* initial (full) check *)
val full_check:
  profiling:Profiling_js.t ->
  options:Options.t ->
  workers:Worker.t list option ->
  focus_target:Loc.filename option ->
  should_merge:bool ->
  filename list ->
  ServerEnv.errors ->
  Profiling_js.t * Utils_js.FilenameSet.t * ServerEnv.errors

val typecheck_contents:
  options: Options.t ->
  workers: Worker.t list option ->
  env: ServerEnv.env ref ->
  ?check_syntax: bool ->
  string ->               (* contents *)
  filename ->             (* fake file-/module name *)
  Profiling_js.t *
    Context.t option *
    Errors.ErrorSet.t * (* errors *)
    Errors.ErrorSet.t * (* warnings *)
    Docblock.t
