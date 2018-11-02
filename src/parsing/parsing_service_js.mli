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
  | Parse_ok of (Loc.t, Loc.t) Flow_ast.program * File_sig.With_Loc.t
  | Parse_fail of parse_failure
  | Parse_skip of parse_skip_reason

and parse_skip_reason =
  | Skip_resource_file
  | Skip_non_flow_file

and parse_failure =
  | Docblock_errors of docblock_error list
  | Parse_error of (Loc.t * Parse_error.t)
  | File_sig_error of File_sig.With_Loc.error

and docblock_error = Loc.t * docblock_error_kind
and docblock_error_kind =
  | MultipleFlowAttributes
  | MultipleProvidesModuleAttributes
  | MultipleJSXAttributes
  | InvalidJSXAttribute of string option

(* results of parse job, returned by parse and reparse *)
type results = {
  (* successfully parsed files *)
  parse_ok: (File_sig.With_Loc.tolerable_error list) FilenameMap.t;

  (* list of skipped files *)
  parse_skips: (File_key.t * Docblock.t) list;

  (* list of files skipped due to an out of date hash *)
  parse_hash_mismatch_skips: FilenameSet.t;

  (* list of failed files *)
  parse_fails: (File_key.t * Docblock.t * parse_failure) list;

  (* set of unchanged files *)
  parse_unchanged: FilenameSet.t;
}

val docblock_max_tokens: int

(* Use default values for the various settings that parse takes. Each one can be overridden
individually *)
val parse_with_defaults:
  ?types_mode: types_mode ->
  ?use_strict: bool ->
  Options.t ->
  MultiWorkerLwt.worker list option ->
  File_key.t list Bucket.next ->
  results Lwt.t

val reparse_with_defaults:
  transaction: Transaction.t ->
  ?types_mode: types_mode ->
  ?use_strict: bool ->
  ?with_progress: bool ->
  workers: MultiWorkerLwt.worker list option ->
  modified: FilenameSet.t ->
  deleted: FilenameSet.t ->
  Options.t ->
  (FilenameSet.t * results) Lwt.t

val ensure_parsed:
  Options.t ->
  MultiWorkerLwt.worker list option ->
  FilenameSet.t ->
  FilenameSet.t Lwt.t

val parse_docblock:
  max_tokens:int -> (* how many tokens to check in the beginning of the file *)
  File_key.t ->
  string ->
  docblock_error list * Docblock.t

val parse_json_file :
  fail:bool ->
  string ->
  File_key.t ->
  Loc.t * (Loc.t * (Loc.t, Loc.t) Flow_ast.Statement.t') list * Loc.t Flow_ast.Comment.t list

(* parse contents of a file *)
val do_parse:
  ?fail:bool ->
  types_mode: types_mode ->
  use_strict: bool ->
  info: Docblock.t ->
  ?prevent_munge: bool ->
  module_ref_prefix: string option ->
  string ->                 (* contents of the file *)
  File_key.t ->               (* filename *)
  result

(* Utility to create the `next` parameter that `parse` requires *)
val next_of_filename_set:
  ?with_progress:bool ->
  MultiWorkerLwt.worker list option ->
  FilenameSet.t ->
  File_key.t list Bucket.next

val does_content_match_file_hash: File_key.t -> string -> bool
