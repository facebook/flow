(**
 *  Copyright 2014 Facebook.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
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
  opt_quiet : bool;
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
