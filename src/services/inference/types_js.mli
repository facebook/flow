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

(* incremental typecheck entry point *)
val recheck: ServerEnv.genv -> ServerEnv.env -> FilenameSet.t -> ServerEnv.env

(* hh_server initial (full) check *)
val server_init:
  ServerEnv.genv -> Profiling_js.t * ServerEnv.env

val typecheck_contents:
  options: Options.t ->
  ?verbose: Verbose.t ->
  ?check_syntax: bool ->
  string ->               (* contents *)
  filename ->             (* fake file-/module name *)
  Profiling_js.t *
    Context.t option *
    Errors.ErrorSet.t *
    Docblock.t
