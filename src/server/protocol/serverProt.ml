(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Infer_type_options = struct
  type t = {
    input: File_input.t;
    line: int;
    char: int;
    verbose: Verbose.t option;
    omit_targ_defaults: bool;
    wait_for_recheck: bool option;
    verbose_normalizer: bool;
    max_depth: int;
    json: bool;
    strip_root: File_path.t option;
    expanded: bool;
    no_typed_ast_for_imports: bool;
  }
end

module Request = struct
  type command =
    | AUTOCOMPLETE of {
        input: File_input.t;
        cursor: int * int;
        trigger_character: string option;
        wait_for_recheck: bool option;
        imports: bool;  (** include auto-import suggestions *)
        imports_ranked_usage: bool;
        show_ranking_info: bool;
      }
    | AUTOFIX_EXPORTS of {
        input: File_input.t;
        verbose: Verbose.t option;
        wait_for_recheck: bool option;
      }
    | AUTOFIX_MISSING_LOCAL_ANNOT of {
        input: File_input.t;
        verbose: Verbose.t option;
        wait_for_recheck: bool option;
      }
    | CHECK_FILE of {
        input: File_input.t;
        verbose: Verbose.t option;
        force: bool;
        include_warnings: bool;
        wait_for_recheck: bool option;
      }
    | COVERAGE of {
        input: File_input.t;
        force: bool;
        wait_for_recheck: bool option;
      }
    | BATCH_COVERAGE of {
        batch: string list;
        wait_for_recheck: bool option;
      }
    | CYCLE of {
        filename: string;
        types_only: bool;
      }
    | DUMP_TYPES of {
        input: File_input.t;
        evaluate_type_destructors: bool;
        wait_for_recheck: bool option;
      }
    | FIND_MODULE of {
        moduleref: string;
        filename: string;
        wait_for_recheck: bool option;
      }
    | FORCE_RECHECK of {
        files: string list;
        focus: bool;
        missed_changes: bool;
        changed_mergebase: bool;
      }
    | GET_DEF of {
        input: File_input.t;
        line: int;
        char: int;
        wait_for_recheck: bool option;
      }
    | GRAPH_DEP_GRAPH of {
        root: string;
        strip_root: bool;
        outfile: string;
        types_only: bool;
      }
    | INFER_TYPE of Infer_type_options.t
    | INSERT_TYPE of {
        input: File_input.t;
        target: Loc.t;
        verbose: Verbose.t option;
        location_is_strict: bool;
        ambiguity_strategy: Autofix_options.ambiguity_strategy;
        wait_for_recheck: bool option;
        omit_targ_defaults: bool;
      }
    | RAGE of { files: string list }
    | SAVE_STATE of { out: [ `File of File_path.t | `Scm ] }
    | STATUS of { include_warnings: bool }

  let to_string = function
    | AUTOCOMPLETE
        {
          input;
          cursor = _;
          wait_for_recheck = _;
          trigger_character = _;
          imports = _;
          imports_ranked_usage = _;
          show_ranking_info = _;
        } ->
      Printf.sprintf "autocomplete %s" (File_input.filename_of_file_input input)
    | AUTOFIX_EXPORTS { input; _ } ->
      Printf.sprintf "autofix exports %s" (File_input.filename_of_file_input input)
    | AUTOFIX_MISSING_LOCAL_ANNOT { input; _ } ->
      Printf.sprintf "autofix missing-local-annot %s" (File_input.filename_of_file_input input)
    | CHECK_FILE { input; verbose = _; force = _; include_warnings = _; wait_for_recheck = _ } ->
      Printf.sprintf "check %s" (File_input.filename_of_file_input input)
    | BATCH_COVERAGE { batch = _; wait_for_recheck = _ } -> Printf.sprintf "%s" "batch-coverage"
    | COVERAGE { input; force = _; wait_for_recheck = _ } ->
      Printf.sprintf "coverage %s" (File_input.filename_of_file_input input)
    | CYCLE { filename; types_only } ->
      Printf.sprintf "cycle (types_only: %b) %s" types_only filename
    | GRAPH_DEP_GRAPH _ -> Printf.sprintf "dep-graph"
    | DUMP_TYPES { input; evaluate_type_destructors = _; wait_for_recheck = _ } ->
      Printf.sprintf "dump-types %s" (File_input.filename_of_file_input input)
    | FIND_MODULE { moduleref; filename; wait_for_recheck = _ } ->
      Printf.sprintf "find-module %s %s" moduleref filename
    | FORCE_RECHECK { files; focus; missed_changes; changed_mergebase } ->
      let parts =
        [
          (focus, Printf.sprintf "focus = %b" focus);
          (missed_changes, Printf.sprintf "missed_changes = %b" missed_changes);
          (changed_mergebase, Printf.sprintf "changed_mergebase = %b" changed_mergebase);
        ]
        |> Base.List.filter_map ~f:(fun (x, str) ->
               if x then
                 Some str
               else
                 None
           )
        |> String.concat "; "
      in
      Printf.sprintf "force-recheck %s (%s)" (String.concat " " files) parts
    | GET_DEF { input; line; char; wait_for_recheck = _ } ->
      Printf.sprintf "get-def %s:%d:%d" (File_input.filename_of_file_input input) line char
    | INFER_TYPE { Infer_type_options.input; line; char; _ } ->
      Printf.sprintf "type-at-pos %s:%d:%d" (File_input.filename_of_file_input input) line char
    | INSERT_TYPE { input; target; _ } ->
      Loc.(
        Printf.sprintf
          "autofix insert-type %s:%d:%d-%d:%d"
          (File_input.filename_of_file_input input)
          target.start.line
          target.start.column
          target._end.line
          target._end.column
      )
    | RAGE { files } -> Printf.sprintf "rage %s" (String.concat " " files)
    | STATUS { include_warnings = _ } -> "status"
    | SAVE_STATE { out } ->
      let out =
        match out with
        | `Scm -> "--scm"
        | `File file -> File_path.to_string file
      in
      Printf.sprintf "save-state %s" out
end

module Response = struct
  type lazy_stats = {
    lazy_mode: bool;
    checked_files: int;
    total_files: int;
  }

  (* Details about functions to be added in json output *)
  type func_param_result = {
    param_documentation: string option;
    param_name: string;
    param_ty: string;
  }

  type func_details_result = {
    func_documentation: string option;
    param_tys: func_param_result list;
    return_ty: string;
  }

  type textedit = Loc.t * string [@@deriving eq, show]

  type insert_replace_edit = {
    newText: string;
    insert: Loc.t;
    replace: Loc.t;
  }
  [@@deriving eq, show]

  module Completion = struct
    type completion_item = {
      kind: Lsp.Completion.completionItemKind option;
      name: string;
      labelDetail: string option;  (** LSP's CompletionItemLabelDetails.detail *)
      description: string option;  (** LSP's CompletionItemLabelDetails.description *)
      itemDetail: string option;  (** LSP's CompletionItem.detail *)
      text_edit: insert_replace_edit option;
      additional_text_edits: textedit list;
      sort_text: string option;
      preselect: bool;
      documentation: string option;
      tags: Lsp.CompletionItemTag.t list option;
      log_info: string;
      insert_text_format: Lsp.Completion.insertTextFormat;
    }
    [@@deriving eq, show]

    type t = {
      items: completion_item list;
      is_incomplete: bool;
    }
    [@@deriving eq, show]
  end

  (** Which "type" of autocomplete this was. e.g. identifier vs type vs member.
      See AutocompleteService_js.string_of_autocomplete_type *)
  type ac_type = string

  type autocomplete_response = (Completion.t * ac_type, string) result

  type autofix_exports_response = (Replacement_printer.patch * string list, string) result

  type autofix_missing_local_annot_response = (Replacement_printer.patch, string) result

  type coverage_response = ((Loc.t * Coverage.Kind.t) list, string) result

  type batch_coverage_response = ((File_key.t * Coverage.file_coverage) list, string) result

  type dump_types_response = ((Loc.t * string) list, string) result

  type get_def_response = (Loc.t list, string) result

  type infer_type_response_payload =
    | Infer_type_string of (string * (string * Loc.t) list option (* refs *)) option
    | Infer_type_JSON of Hh_json.json

  type infer_type_response_ok =
    | Infer_type_response of {
        loc: Loc.t;
        tys: infer_type_response_payload;
        documentation: string option;
      }

  type infer_type_response = (infer_type_response_ok, string) result

  type insert_type_response = (Replacement_printer.patch, string) result

  type rage_response = (string * string) list

  type graph_response = (graph_response_subgraph, string) result

  and graph_response_subgraph = (string * string list) list

  type status_response =
    | ERRORS of {
        errors: Flow_errors_utils.ConcreteLocPrintableErrorSet.t;
        warnings: Flow_errors_utils.ConcreteLocPrintableErrorSet.t;
        suppressed_errors: (Loc.t Flow_errors_utils.printable_error * Loc_collections.LocSet.t) list;
      }
    | NO_ERRORS
    | NOT_COVERED

  type check_file_response = status_response

  type find_module_response = File_key.t option

  type response =
    | AUTOCOMPLETE of autocomplete_response
    | AUTOFIX_EXPORTS of autofix_exports_response
    | AUTOFIX_MISSING_LOCAL_ANNOT of autofix_missing_local_annot_response
    | CHECK_FILE of check_file_response
    | COVERAGE of coverage_response
    | BATCH_COVERAGE of batch_coverage_response
    | CYCLE of graph_response
    | GRAPH_DEP_GRAPH of (unit, string) result
    | DUMP_TYPES of dump_types_response
    | FIND_MODULE of find_module_response
    | FORCE_RECHECK
    | GET_DEF of get_def_response
    | INFER_TYPE of infer_type_response
    | INSERT_TYPE of insert_type_response
    | RAGE of rage_response
    | STATUS of {
        status_response: status_response;
        lazy_stats: lazy_stats;
      }
    | SAVE_STATE of (string, string) result

  let to_string = function
    | AUTOCOMPLETE _ -> "autocomplete response"
    | AUTOFIX_EXPORTS _ -> "autofix exports response"
    | AUTOFIX_MISSING_LOCAL_ANNOT _ -> "autofix missing-local-annot response"
    | CHECK_FILE _ -> "check_file response"
    | COVERAGE _ -> "coverage response"
    | BATCH_COVERAGE _ -> "batch-coverage response"
    | CYCLE _ -> "cycle response"
    | GRAPH_DEP_GRAPH _ -> "dep-graph response"
    | DUMP_TYPES _ -> "dump_types response"
    | FIND_MODULE _ -> "find_module response"
    | FORCE_RECHECK -> "force_recheck response"
    | GET_DEF _ -> "get_def response"
    | INFER_TYPE _ -> "infer_type response"
    | INSERT_TYPE _ -> "insert_type response"
    | RAGE _ -> "rage response"
    | STATUS _ -> "status response"
    | SAVE_STATE _ -> "save_state response"
end
