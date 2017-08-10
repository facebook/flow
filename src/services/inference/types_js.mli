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
  profiling:Profiling_js.running ->
  workers:Worker.t list option ->
  Options.t ->
  FilenameSet.t * SSet.t * bool * ServerEnv.errors

val calc_deps:
  options:Options.t ->
  profiling:Profiling_js.running ->
  workers:Worker.t list option ->
  filename list ->
  FilenameSet.t FilenameMap.t * filename list FilenameMap.t

(* incremental typecheck entry point *)
val recheck:
  options:Options.t ->
  workers:Worker.t list option ->
  updates:FilenameSet.t ->
  ServerEnv.env -> ServerEnv.env

(* initial (full) check *)
val full_check:
  profiling:Profiling_js.running ->
  options:Options.t ->
  workers:Worker.t list option ->
  focus_targets:Loc.filename list ->
  should_merge:bool ->
  filename list ->
  ServerEnv.errors ->
  Utils_js.FilenameSet.t * ServerEnv.errors

val typecheck_contents:
  options: Options.t ->
  workers: Worker.t list option ->
  env: ServerEnv.env ref ->
  ?check_syntax: bool ->
  string ->               (* contents *)
  filename ->             (* fake file-/module name *)
  Profiling_js.finished *
    Context.t option *
    Errors.ErrorSet.t * (* errors *)
    Errors.ErrorSet.t * (* warnings *)
    Docblock.t
