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

(* results of parse job, returned by parse and reparse *)
(* NOTE: same as Types_js.results, should factor to common upstream *)
type results =
  FilenameSet.t *           (* successfully parsed files *)
  filename list *           (* list of failed files *)
  Errors_js.ErrorSet.t list (* parallel list of error sets *)

(* initial parsing pass: success/failure info is returned,
 * asts are made available via get_ast_unsafe. *)
val parse:
  Worker.t list option ->       (* Some=parallel, None=serial *)
  (unit -> filename list) ->    (* delivers buckets of filenames *)
  (unit -> unit) ->
  results                       (* job results, not asts *)

(* for non-initial passes: updates asts for passed file set. *)
val reparse:
  Worker.t list option ->   (* Some=parallel, None=serial *)
  FilenameSet.t ->          (* filenames to reparse *)
  (unit -> unit) ->
  results                   (* job results, not asts *)

(* after parsing, retrieves ast by filename (unsafe) *)
val get_ast_unsafe: filename -> Spider_monkey_ast.program

(* remove asts for given file set. *)
val remove_asts: FilenameSet.t -> unit

(* Adds a hook called every time a file has been successfully parsed.
 * When a file is deleted, the hook is called with an empty Ast.
 *)
val call_on_success: (filename -> Spider_monkey_ast.program -> unit) -> unit

(* parse contents of a file *)
val do_parse:
  ?fail:bool ->
  string ->                 (* contents of the file *)
  filename ->               (* filename *)
  (Spider_monkey_ast.program, Errors_js.ErrorSet.t) ok_or_err

(* true if file is in flow, i.e. is to be checked. CAUTION expensive *)
val in_flow: string -> filename -> bool
val is_noflow: string -> bool
