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

(* incremental typecheck entry point *)
val recheck: ServerEnv.genv -> ServerEnv.env -> updates:FilenameSet.t -> ServerEnv.env

(* initial (full) check *)
val full_check:
  focus_target:Loc.filename option ->
  ServerEnv.genv -> Profiling_js.t * ServerEnv.env

val typecheck_contents:
  options: Options.t ->
  workers: Worker.t list option ->
  env: ServerEnv.env ref ->
  ?check_syntax: bool ->
  string ->               (* contents *)
  filename ->             (* fake file-/module name *)
  Profiling_js.t *
    Context.t option *
    Errors.ErrorSet.t *
    Docblock.t
