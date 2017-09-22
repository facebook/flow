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
  File_key.t list ->
  FilenameSet.t FilenameMap.t * File_key.t list FilenameMap.t

(* incremental typecheck entry point *)
val recheck:
  options:Options.t ->
  workers:Worker.t list option ->
  updates:FilenameSet.t ->
  ServerEnv.env ->
  serve_ready_clients:(unit -> unit) ->
  ServerEnv.env

(* initial (full) check *)
val full_check:
  profiling:Profiling_js.running ->
  options:Options.t ->
  workers:Worker.t list option ->
  focus_targets:FilenameSet.t option ->
  should_merge:bool ->
  File_key.t list ->
  ServerEnv.errors ->
  CheckedSet.t * ServerEnv.errors

val basic_check_contents:
  options: Options.t ->
  workers: Worker.t list option ->
  env: ServerEnv.env ref ->
  string ->               (* contents *)
  File_key.t ->           (* fake file-/module name *)
  (Profiling_js.finished *
   Context.t *
   Docblock.t,
   string) result

val typecheck_contents:
  options: Options.t ->
  workers: Worker.t list option ->
  env: ServerEnv.env ref ->
  string ->               (* contents *)
  File_key.t ->           (* fake file-/module name *)
  Errors.ErrorSet.t *     (* errors *)
  Errors.ErrorSet.t       (* warnings *)
