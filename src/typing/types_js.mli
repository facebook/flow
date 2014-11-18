(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

type options = {
  opt_debug : bool;
  opt_all : bool;
  opt_weak : bool;
  opt_traces : bool;
  opt_newtraces : bool;
  opt_strict : bool;
  opt_console : bool;
  opt_json : bool;
  opt_show_all_errors : bool;
  opt_quiet : bool;
  opt_profile : bool;
  opt_strip_root : bool;
  opt_module: string;
  opt_lib: string option;
}

(* incremental typecheck entry point *)
val recheck: ServerEnv.genv -> ServerEnv.env -> SSet.t -> options ->
  ServerEnv.env

(* hh_server initial (full) check *)
val server_init: ServerEnv.genv -> ServerEnv.env -> options -> ServerEnv.env

(* hh_single_type_check entry point, probably to be moved *)
val single_main: string list -> options -> unit

val get_errors: unit -> Errors.t

val merge_strict_file: string -> Constraint_js.context

val typecheck_contents:
  string ->               (* contents *)
  string ->               (* fake file-/module name *)
  bool ->                 (* process autocomplete *)
  Constraint_js.context option * Errors_js.ErrorSet.t
