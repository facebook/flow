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
  ServerEnv.genv -> ServerEnv.env -> FlowEventLogger.Timing.t * ServerEnv.env

val get_errors: unit -> Errors_js.error list

val typecheck_contents:
  options: Options.options ->
  ?verbose: int ->
  string ->               (* contents *)
  filename ->             (* fake file-/module name *)
  FlowEventLogger.Timing.t *
    Context.t option *
    Errors_js.ErrorSet.t *
    (Spider_monkey_ast.program * Docblock.t, Errors_js.ErrorSet.t) ok_or_err
