(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type types_mode =
  | TypesAllowed
  | TypesForbiddenByDefault

type sig_extra = (ALoc.t, ALoc.t) Flow_ast.Program.t * File_sig.With_ALoc.t * ALoc.table option

(* result of individual parse *)
type result =
  | Parse_ok of {
      ast: (Loc.t, Loc.t) Flow_ast.Program.t;
      file_sig: File_sig.With_Loc.t;
      sig_extra: sig_extra option;
      tolerable_errors: File_sig.With_Loc.tolerable_error list;
      parse_errors: parse_error list;
    }
  | Parse_fail of parse_failure
  | Parse_skip of parse_skip_reason

and parse_skip_reason =
  | Skip_resource_file
  | Skip_non_flow_file

and parse_error = Loc.t * Parse_error.t

and parse_failure =
  | Docblock_errors of docblock_error list
  | Parse_error of parse_error
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
  parse_ok: File_sig.With_Loc.tolerable_error list FilenameMap.t;
  (* list of skipped files *)
  parse_skips: (File_key.t * Docblock.t) list;
  (* list of files skipped due to an out of date hash *)
  parse_hash_mismatch_skips: FilenameSet.t;
  (* list of failed files *)
  parse_fails: (File_key.t * Docblock.t * parse_failure) list;
  (* set of unchanged files *)
  parse_unchanged: FilenameSet.t;
}

type parse_options = {
  parse_fail: bool;
  parse_types_mode: types_mode;
  parse_use_strict: bool;
  parse_prevent_munge: bool;
  parse_module_ref_prefix: string option;
  parse_facebook_fbt: string option;
  parse_arch: Options.arch;
  parse_abstract_locations: bool;
}

val make_parse_options :
  ?fail:bool ->
  ?types_mode:types_mode ->
  ?use_strict:bool ->
  Docblock.t ->
  Options.t ->
  parse_options

val docblock_max_tokens : int

(* Use default values for the various settings that parse takes. Each one can be overridden
individually *)
val parse_with_defaults :
  ?types_mode:types_mode ->
  ?use_strict:bool ->
  reader:Mutator_state_reader.t ->
  Options.t ->
  MultiWorkerLwt.worker list option ->
  File_key.t list Bucket.next ->
  results Lwt.t

val reparse_with_defaults :
  transaction:Transaction.t ->
  reader:Mutator_state_reader.t ->
  ?types_mode:types_mode ->
  ?use_strict:bool ->
  ?with_progress:bool ->
  workers:MultiWorkerLwt.worker list option ->
  modified:FilenameSet.t ->
  deleted:FilenameSet.t ->
  Options.t ->
  (FilenameSet.t * results) Lwt.t

val ensure_parsed :
  reader:Mutator_state_reader.t ->
  Options.t ->
  MultiWorkerLwt.worker list option ->
  FilenameSet.t ->
  FilenameSet.t Lwt.t

val parse_docblock :
  max_tokens:int ->
  (* how many tokens to check in the beginning of the file *)
  File_key.t ->
  string ->
  docblock_error list * Docblock.t

val parse_json_file :
  fail:bool -> string -> File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t * parse_error list

val parse_source_file :
  fail:bool ->
  types:bool ->
  use_strict:bool ->
  string ->
  File_key.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t * parse_error list

(* parse contents of a file *)
val do_parse :
  parse_options:parse_options ->
  info:Docblock.t ->
  string ->
  (* contents of the file *)
  File_key.t ->
  (* filename *)
  result

(* Utility to create the `next` parameter that `parse` requires *)
val next_of_filename_set :
  ?with_progress:bool ->
  MultiWorkerLwt.worker list option ->
  FilenameSet.t ->
  File_key.t list Bucket.next

val does_content_match_file_hash : reader:State_reader.t -> File_key.t -> string -> bool
