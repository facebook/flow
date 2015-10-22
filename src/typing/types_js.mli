(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

val init_modes: Options.options -> unit

(* incremental typecheck entry point *)
val recheck: ServerEnv.genv -> ServerEnv.env -> FilenameSet.t -> ServerEnv.env

(* hh_server initial (full) check *)
val server_init:
  ServerEnv.genv -> ServerEnv.env -> ServerEnv.env

val get_errors: unit -> Errors_js.error list

val merge_strict_file: filename -> Context.t

val typecheck_contents:
  ?verbose: int ->
  string ->               (* contents *)
  filename ->             (* fake file-/module name *)
  Context.t option * Errors_js.ErrorSet.t
