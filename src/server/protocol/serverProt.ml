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
    debug_print_internal_repr: bool;
    no_typed_ast_for_imports: bool;
  }
end

module Inlay_hint_options = struct
  type t = {
    input: File_input.t;
    verbose: Verbose.t option;
    omit_targ_defaults: bool;
    wait_for_recheck: bool option;
    verbose_normalizer: bool;
    max_depth: int;
    no_typed_ast_for_imports: bool;
  }
end

module Type_of_name_options = struct
  type t = {
    input: File_input.t;
    name: string;
    verbose: Verbose.t option;
    wait_for_recheck: bool option;
    expand_component_props: bool;
    exact_match_only: bool;
    strip_root: File_path.t option;
  }
end

module Code_action = struct
  type t =
    | SourceAddMissingImports
    | SuggestImports

  let to_string = function
    | SourceAddMissingImports -> "source.addMissingImports"
    | SuggestImports -> "suggestImports"
end

module Request = struct
  type command =
    | APPLY_CODE_ACTION of {
        input: File_input.t;
        action: Code_action.t;
        wait_for_recheck: bool option;
      }
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
        for_tool: bool;
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
    | INLAY_HINT of Inlay_hint_options.t
    | TYPE_OF_NAME of Type_of_name_options.t
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
    | APPLY_CODE_ACTION { input; action; wait_for_recheck = _ } ->
      Printf.sprintf
        "apply-code-action source.addMissingImports %s %s"
        (File_input.filename_of_file_input input)
        (Code_action.to_string action)
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
    | DUMP_TYPES { input; evaluate_type_destructors = _; wait_for_recheck = _; for_tool = _ } ->
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
    | INLAY_HINT { Inlay_hint_options.input; _ } ->
      Printf.sprintf "inlay-hint %s" (File_input.filename_of_file_input input)
    | TYPE_OF_NAME { Type_of_name_options.input; name; _ } ->
      Printf.sprintf "type-of-name %s %s" (File_input.filename_of_file_input input) name
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

  type func_details_result =
    | SigHelpFunc of {
        func_documentation: string option;
        param_tys: func_param_result list;
        return_ty: string;
      }
    | SigHelpJsxAttr of {
        documentation: string option;
        name: string;
        ty: string;
        optional: bool;
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

  type apply_code_action_response = (Replacement_printer.patch, string) result

  type autofix_exports_response = (Replacement_printer.patch * string list, string) result

  type autofix_missing_local_annot_response = (Replacement_printer.patch, string) result

  type coverage_response = ((Loc.t * Coverage.Kind.t) list, string) result

  type batch_coverage_response = ((File_key.t * Coverage.file_coverage) list, string) result

  type dump_types_response = ((Loc.t * string) list, string) result

  type get_def_response = (Loc.t list, string) result

  module InferTypeOfName = struct
    type t = {
      loc: Loc.t;
      actual_name: string;
      type_: string;
      refs: (string * Loc.t) list option;
      documentation: string option;
    }
  end

  type infer_type_of_name_response = (InferTypeOfName.t, string) result

  module InferType = struct
    type friendly_response = {
      type_str: string;
      refs: (string * Loc.t) list option;
    }

    type payload =
      | Friendly of friendly_response option
      | JSON of Hh_json.json

    type t = {
      loc: Loc.t;
      tys: payload;
      refining_locs: Loc.t list;
      refinement_invalidated: (Loc.t * Refinement_invalidation.reason) list;
      documentation: string option;
    }
  end

  type infer_type_response = (InferType.t, string) result

  module InlayHint = struct
    type item = {
      cursor_loc: Loc.t;
      type_loc: Loc.t;
      tys: InferType.friendly_response option;
      refining_locs: Loc.t list;
      refinement_invalidated: (Loc.t * Refinement_invalidation.reason) list;
      documentation: string option;
    }

    type response = (item list, string) result
  end

  type insert_type_response = (Replacement_printer.patch, string) result

  type rage_response = (string * string) list

  type suggest_imports_response = (Lsp.CodeAction.command_or_action list SMap.t, string) result

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

  type find_module_response = File_key.t option * string list

  type response =
    | APPLY_CODE_ACTION of apply_code_action_response
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
    | INLAY_HINT of InlayHint.response
    | TYPE_OF_NAME of infer_type_of_name_response
    | INSERT_TYPE of insert_type_response
    | RAGE of rage_response
    | STATUS of {
        status_response: status_response;
        lazy_stats: lazy_stats;
      }
    | SAVE_STATE of (string, string) result
    | SUGGEST_IMPORTS of suggest_imports_response

  let to_string = function
    | APPLY_CODE_ACTION _ -> "apply-code-action response"
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
    | INLAY_HINT _ -> "inlay_hint response"
    | TYPE_OF_NAME _ -> "type_of_name response"
    | INSERT_TYPE _ -> "insert_type response"
    | RAGE _ -> "rage response"
    | STATUS _ -> "status response"
    | SAVE_STATE _ -> "save_state response"
    | SUGGEST_IMPORTS _ -> "suggest imports response"
end
