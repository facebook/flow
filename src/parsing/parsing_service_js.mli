(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type types_mode =
  | TypesAllowed
  | TypesForbiddenByDefault

(* result of individual parse *)
type result =
  | Parse_ok of Loc.t Ast.program * File_sig.t
  | Parse_fail of parse_failure
  | Parse_skip of parse_skip_reason

and parse_skip_reason =
  | Skip_resource_file
  | Skip_non_flow_file

and parse_failure =
  | Docblock_errors of docblock_error list
  | Parse_error of (Loc.t * Parse_error.t)
  | File_sig_error of File_sig.error

and docblock_error = Loc.t * docblock_error_kind
and docblock_error_kind =
  | MultipleFlowAttributes
  | MultipleProvidesModuleAttributes
  | MultipleJSXAttributes
  | InvalidJSXAttribute of string option

(* results of parse job, returned by parse and reparse *)
type results = {
  (* successfully parsed files *)
  parse_ok: (File_sig.tolerable_error list) FilenameMap.t;

  (* list of skipped files *)
  parse_skips: (File_key.t * Docblock.t) list;

  (* list of failed files *)
  parse_fails: (File_key.t * Docblock.t * parse_failure) list;

  (* set of unchanged files *)
  parse_unchanged: FilenameSet.t;
}

val docblock_max_tokens: int

val extract_docblock:
  max_tokens: int ->
  File_key.t ->
  string ->
  docblock_error list * Docblock.t

(* initial parsing pass: success/failure info is returned,
 * asts are made available via get_ast_unsafe. *)
val parse:
  types_mode: types_mode ->
  use_strict: bool ->
  profile: bool ->
  max_header_tokens: int ->
  noflow: (File_key.t -> bool) ->
  parse_unchanged: bool ->
  MultiWorkerLwt.worker list option ->       (* Some=parallel, None=serial *)
  File_key.t list Bucket.next ->  (* delivers buckets of filenames *)
  results Lwt.t                       (* job results, not asts *)

(* Use default values for the various settings that parse takes. Each one can be overridden
individually *)
val parse_with_defaults:
  ?types_mode: types_mode ->
  ?use_strict: bool ->
  Options.t ->
  MultiWorkerLwt.worker list option ->
  File_key.t list Bucket.next ->
  results Lwt.t

(* for non-initial passes: updates asts for passed file set. *)
val reparse:
  types_mode: types_mode ->
  use_strict: bool ->
  profile: bool ->
  max_header_tokens: int ->
  noflow: (File_key.t -> bool) ->
  parse_unchanged: bool ->
  ?with_progress: bool ->
  MultiWorkerLwt.worker list option ->   (* Some=parallel, None=serial *)
  FilenameSet.t ->          (* filenames to reparse *)
  (FilenameSet.t * results) Lwt.t   (* modified files and job results *)

val reparse_with_defaults:
  ?types_mode: types_mode ->
  ?use_strict: bool ->
  ?with_progress: bool ->
  Options.t ->
  MultiWorkerLwt.worker list option ->
  FilenameSet.t ->
  (FilenameSet.t * results) Lwt.t

val has_ast: File_key.t -> bool

val get_ast: File_key.t -> Loc.t Ast.program option
val get_docblock: File_key.t -> Docblock.t option
val get_file_sig: File_key.t -> File_sig.t option

(* after parsing, retrieves ast and docblock by filename (unsafe) *)
val get_ast_unsafe: File_key.t -> Loc.t Ast.program
val get_docblock_unsafe: File_key.t -> Docblock.t
val get_file_sig_unsafe: File_key.t -> File_sig.t

(* remove asts and docblocks for given file set. *)
val remove_batch: FilenameSet.t -> unit

val parse_docblock:
  max_tokens:int -> (* how many tokens to check in the beginning of the file *)
  File_key.t ->
  string ->
  docblock_error list * Docblock.t

(* parse contents of a file *)
val do_parse:
  ?fail:bool ->
  types_mode: types_mode ->
  use_strict: bool ->
  info: Docblock.t ->
  string ->                 (* contents of the file *)
  File_key.t ->               (* filename *)
  result

(* Utility to create the `next` parameter that `parse` requires *)
val next_of_filename_set:
  ?with_progress:bool ->
  MultiWorkerLwt.worker list option ->
  FilenameSet.t ->
  File_key.t list Bucket.next

(* APIs for loading saved state *)
val add_file_sig_from_saved_state: File_key.t -> File_sig.t -> unit
