(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
  | Parse_ok of {
      ast: (Loc.t, Loc.t) Flow_ast.Program.t;
      file_sig: File_sig.With_Loc.t;
      locs: Parsing_heaps.locs_tbl;
      type_sig: Parsing_heaps.type_sig;
      tolerable_errors: File_sig.With_Loc.tolerable_error list;
      exports: Exports.t;
      imports: Imports.t;
      cas_digest: Cas_digest.t option;
    }
  | Parse_recovered of {
      ast: (Loc.t, Loc.t) Flow_ast.Program.t;
      file_sig: File_sig.With_Loc.t;
      tolerable_errors: File_sig.With_Loc.tolerable_error list;
      parse_errors: parse_error Nel.t;
    }
  | Parse_exn of Exception.t
  | Parse_skip of parse_skip_reason

and parse_skip_reason =
  | Skip_resource_file
  | Skip_non_flow_file
  | Skip_package_json of (Package_json.t, parse_error) Result.t

and parse_error = Loc.t * Parse_error.t

and parse_failure =
  | Uncaught_exception of Exception.t
  | Docblock_errors of docblock_error list
  | Parse_error of parse_error

and docblock_error = Loc.t * docblock_error_kind

and docblock_error_kind =
  | MultipleFlowAttributes
  | InvalidFlowMode of string
  | MultipleProvidesModuleAttributes
  | MultipleJSXAttributes
  | InvalidJSXAttribute of string option
  | MultipleJSXRuntimeAttributes
  | InvalidJSXRuntimeAttribute

(* results of parse job, returned by parse and reparse *)
type results = {
  (* successfully parsed files *)
  parsed: FilenameSet.t;
  (* list of skipped files *)
  unparsed: FilenameSet.t;
  (* list of files skipped due to an out of date hash *)
  changed: FilenameSet.t;
  (* list of failed files *)
  failed: File_key.t list * parse_failure list;
  (* set of unchanged files *)
  unchanged: FilenameSet.t;
  (* set of files that were not found on disk *)
  not_found: FilenameSet.t;
  (* package.json files parsed *)
  package_json: File_key.t list * parse_error option list;
  (* set of modules that need to be committed *)
  dirty_modules: Modulename.Set.t;
}

type parse_options = {
  parse_types_mode: types_mode;
  parse_use_strict: bool;
  parse_prevent_munge: bool;
  parse_module_ref_prefix: string option;
  parse_facebook_fbt: string option;
  parse_suppress_types: SSet.t;
  parse_max_literal_len: int;
  parse_exact_by_default: bool;
  parse_enable_enums: bool;
  parse_enable_relay_integration: bool;
  parse_relay_integration_excludes: Str.regexp list;
  parse_relay_integration_module_prefix: string option;
  parse_relay_integration_module_prefix_includes: Str.regexp list;
  parse_node_main_fields: string list;
  parse_distributed: bool;
}

val make_parse_options :
  ?types_mode:types_mode -> ?use_strict:bool -> Docblock.t -> Options.t -> parse_options

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
  Options.t ->
  results Lwt.t

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

val parse_package_json_file :
  node_main_fields:string list -> string -> File_key.t -> (Package_json.t, parse_error) Result.t

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

val does_content_match_file_hash : reader:Abstract_state_reader.t -> File_key.t -> string -> bool
