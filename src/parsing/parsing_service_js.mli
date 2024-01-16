(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Docblock_parser

(* result of individual parse *)
type result =
  | Parse_ok of {
      ast: (Loc.t, Loc.t) Flow_ast.Program.t;
      requires: string array;
      file_sig: File_sig.t;
      locs: Parsing_heaps.locs_tbl;
      type_sig: Parsing_heaps.type_sig;
      tolerable_errors: File_sig.tolerable_error list;
      exports: Exports.t;
      imports: Imports.t;
    }
  | Parse_recovered of {
      ast: (Loc.t, Loc.t) Flow_ast.Program.t;
      requires: string array;
      file_sig: File_sig.t;
      tolerable_errors: File_sig.tolerable_error list;
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

(* Use default values for the various settings that parse takes. Each one can be overridden
   individually *)
val parse_with_defaults :
  ?locs_to_dirtify:Loc.t list ->
  reader:Mutator_state_reader.t ->
  Options.t ->
  MultiWorkerLwt.worker list option ->
  File_key.t list Bucket.next ->
  results Lwt.t

val reparse_with_defaults :
  transaction:Transaction.t ->
  reader:Mutator_state_reader.t ->
  ?locs_to_dirtify:Loc.t list ->
  with_progress:bool ->
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

val parse_package_json_file :
  options:Options.t -> string -> File_key.t -> (Package_json.t, parse_error) Result.t

val parse_file_sig :
  Options.t -> File_key.t -> Docblock.t -> (Loc.t, Loc.t) Flow_ast.Program.t -> File_sig.t

val parse_type_sig :
  Options.t ->
  Docblock.t ->
  Loc.t list ->
  File_key.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  Type_sig_collections.Locs.index Type_sig.error list
  * Parsing_heaps.locs_tbl
  * Parsing_heaps.type_sig

(* parse contents of a file *)
val do_parse :
  options:Options.t ->
  docblock:Docblock.t ->
  ?locs_to_dirtify:Loc.t list ->
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
