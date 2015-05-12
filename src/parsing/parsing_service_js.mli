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

(* true if file is in flow, i.e. is to be checked. CAUTION expensive *)
val in_flow: string -> string -> bool
