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

(* results of parse job, returned by parse and reparse *)
(* NOTE: same as Types_js.results, should factor to common upstream *)
type results =
  SSet.t *            (* successfully parsed files *)
  string list *             (* list of failed files *)
  Errors_js.ErrorSet.t list (* parallel list of error sets *)

(* initial parsing pass: success/failure info is returned,
 * asts are made available via get_ast_unsafe. *)
val parse:
  Worker.t list option ->   (* Some=parallel, None=serial *)
  (unit -> string list) ->  (* delivers buckets of filenames *)
  (unit -> unit) ->
  results                   (* job results, not asts *)

(* for non-initial passes: updates asts for passed file set. *)
val reparse:
  Worker.t list option ->   (* Some=parallel, None=serial *)
  SSet.t ->                 (* filenames to reparse *)
  (unit -> unit) ->
  results                   (* job results, not asts *)

(* after parsing, retrieves ast by filename (unsafe) *)
val get_ast_unsafe: string -> Spider_monkey_ast.program

(* remove asts for given file set. *)
val remove_asts: SSet.t -> unit

(* parse contents of a file *)
val do_parse:
  string ->                 (* contents of the file *)
  string ->                 (* filename *)
  (Spider_monkey_ast.program option * Errors_js.ErrorSet.t option)
