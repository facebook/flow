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
  | Parse_ok of Ast.program
  | Parse_fail of parse_failure
  | Parse_skip of parse_skip_reason

and parse_skip_reason =
  | Skip_resource_file
  | Skip_non_flow_file

and parse_failure =
  | Docblock_errors of Docblock.error list
  | Parse_error of (Loc.t * Parse_error.t)

(* results of parse job, returned by parse and reparse *)
type results = {
  (* successfully parsed files *)
  parse_ok: FilenameSet.t;

  (* list of skipped files *)
  parse_skips: (filename * Docblock.t) list;

  (* list of failed files *)
  parse_fails: (filename * Docblock.t * parse_failure) list;
}

(* initial parsing pass: success/failure info is returned,
 * asts are made available via get_ast_unsafe. *)
val parse:
  types_mode: types_mode ->
  use_strict: bool ->
  profile: bool ->
  max_header_tokens: int ->
  lazy_mode: bool ->
  Worker.t list option ->       (* Some=parallel, None=serial *)
  filename list Bucket.next ->  (* delivers buckets of filenames *)
  results                       (* job results, not asts *)

(* Use default values for the various settings that parse takes. Each one can be overridden
individually *)
val parse_with_defaults:
  ?types_mode: types_mode ->
  ?use_strict: bool ->
  Options.t ->
  Worker.t list option ->
  filename list Bucket.next ->
  results

(* for non-initial passes: updates asts for passed file set. *)
val reparse:
  types_mode: types_mode ->
  use_strict: bool ->
  profile: bool ->
  max_header_tokens: int ->
  lazy_mode: bool ->
  options: Options.t ->
  Worker.t list option ->   (* Some=parallel, None=serial *)
  FilenameSet.t ->          (* filenames to reparse *)
  FilenameSet.t * results   (* modified files and job results *)

val reparse_with_defaults:
  ?types_mode: types_mode ->
  ?use_strict: bool ->
  Options.t ->
  Worker.t list option ->
  FilenameSet.t ->
  FilenameSet.t * results

val calc_requires:
  Ast.program ->
  default_jsx: bool ->
  Loc.t SMap.t

val has_ast: filename -> bool

val get_ast: filename -> Ast.program option

(* after parsing, retrieves ast and docblock by filename (unsafe) *)
val get_ast_unsafe: filename -> Ast.program
val get_docblock_unsafe: filename -> Docblock.t
val get_requires_unsafe: filename -> Loc.t SMap.t

(* remove asts and docblocks for given file set. *)
val remove_batch: FilenameSet.t -> unit

(* Adds a hook that is called every time a file has been processed.
 * When a file fails to parse, is deleted or is skipped because it isn't a Flow
 * file, the AST is None.
 *)
val register_hook:
  (filename -> Ast.program option -> unit) ->
  unit

val get_docblock:
  max_tokens:int -> (* how many tokens to check in the beginning of the file *)
  filename ->
  string ->
  Docblock.error list * Docblock.t

(* parse contents of a file *)
val do_parse:
  ?fail:bool ->
  types_mode: types_mode ->
  use_strict: bool ->
  info: Docblock.t ->
  string ->                 (* contents of the file *)
  filename ->               (* filename *)
  result

(* Utility to create the `next` parameter that `parse` requires *)
val next_of_filename_set:
  Worker.t list option ->
  FilenameSet.t ->
  filename list Bucket.next
