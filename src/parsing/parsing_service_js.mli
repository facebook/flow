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

type types_mode =
  | TypesAllowed
  | TypesForbiddenByDefault

(* result of individual parse *)
type result =
  | Parse_ok of Spider_monkey_ast.program
  | Parse_err of Errors.ErrorSet.t
  | Parse_skip of parse_skip_reason

and parse_skip_reason =
  | Skip_resource_file
  | Skip_non_flow_file

(* results of parse job, returned by parse and reparse *)
type results = {
  parse_ok: FilenameSet.t;                   (* successfully parsed files *)
  parse_skips: (filename * Docblock.t) list; (* list of skipped files *)
  parse_fails: (filename * Docblock.t) list; (* list of failed files *)
  parse_errors: Errors.ErrorSet.t list;      (* parallel list of error sets *)
  parse_resource_files: FilenameSet.t;       (* resource files *)
}

(* initial parsing pass: success/failure info is returned,
 * asts are made available via get_ast_unsafe. *)
val parse:
  types_mode: types_mode ->
  use_strict: bool ->
  profile: bool ->
  max_header_tokens: int ->
  Worker.t list option ->       (* Some=parallel, None=serial *)
  (unit -> filename list) ->    (* delivers buckets of filenames *)
  results                       (* job results, not asts *)

(* for non-initial passes: updates asts for passed file set. *)
val reparse:
  types_mode: types_mode ->
  use_strict: bool ->
  profile: bool ->
  max_header_tokens: int ->
  Worker.t list option ->   (* Some=parallel, None=serial *)
  FilenameSet.t ->          (* filenames to reparse *)
  FilenameSet.t * results   (* modified files and job results *)

val has_ast: filename -> bool

(* after parsing, retrieves ast by filename (unsafe) *)
val get_ast_unsafe: filename -> Spider_monkey_ast.program
val get_ast_and_info_unsafe:
  filename -> Spider_monkey_ast.program * Docblock.t

(* remove asts for given file set. *)
val remove_asts: FilenameSet.t -> unit

(* Adds a hook that is called every time a file has been processed.
 * When a file fails to parse, is deleted or is skipped because it isn't a Flow
 * file, the AST is None.
 *)
val register_hook:
  (filename -> Spider_monkey_ast.program option -> unit) ->
  unit

val get_docblock:
  max_tokens:int -> (* how many tokens to check in the beginning of the file *)
  filename ->
  string ->
  Errors.ErrorSet.t option * Docblock.t

(* parse contents of a file *)
val do_parse:
  ?fail:bool ->
  types_mode: types_mode ->
  use_strict: bool ->
  info: Docblock.t ->
  string ->                 (* contents of the file *)
  filename ->               (* filename *)
  result
