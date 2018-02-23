(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Request = struct
  type command =
  | AUTOCOMPLETE of File_input.t
  | CHECK_FILE of
      File_input.t *
      Verbose.t option *
      bool * (* force *)
      bool (* include_warnings *)
  | COVERAGE of File_input.t * bool (* force *)
  | CYCLE of string
  | DUMP_TYPES of File_input.t
  | FIND_MODULE of string * string
  | FIND_REFS of File_input.t * int * int * bool (* filename, line, char, global *)
  | GEN_FLOW_FILES of File_input.t list * bool (* include_warnings *)
  | GET_DEF of File_input.t * int * int (* filename, line, char *)
  | GET_IMPORTS of string list
  | INFER_TYPE of
      File_input.t * (* filename|content *)
      int * (* line *)
      int * (* char *)
      Verbose.t option
  | PORT of string list
  | STATUS of Path.t * bool (* include_warnings *)
  | FORCE_RECHECK of { files: string list; focus:bool; profile:bool }
  | SUGGEST of (string * string list) list

  let to_string = function
  | AUTOCOMPLETE fn ->
    Printf.sprintf "autocomplete %s" (File_input.filename_of_file_input fn)
  | CHECK_FILE (fn, _, _, _) ->
    Printf.sprintf "check %s" (File_input.filename_of_file_input fn)
  | COVERAGE (fn, _) ->
      Printf.sprintf "coverage %s" (File_input.filename_of_file_input fn)
  | CYCLE fn ->
      Printf.sprintf "cycle %s" fn
  | DUMP_TYPES (fn) ->
      Printf.sprintf "dump-types %s" (File_input.filename_of_file_input fn)
  | FIND_MODULE (moduleref, filename) ->
      Printf.sprintf "find-module %s %s" moduleref filename
  | FIND_REFS (fn, line, char, global) ->
      Printf.sprintf "find-refs %s:%d:%d:%B" (File_input.filename_of_file_input fn) line char global
  | FORCE_RECHECK {files; focus; profile=_} ->
      Printf.sprintf
        "force-recheck %s (focus = %b)" (String.concat " " files) focus
  | GEN_FLOW_FILES (files, _) ->
      Printf.sprintf "gen-flow-files %s"
        (files |> List.map File_input.filename_of_file_input |> String.concat " ")
  | GET_DEF (fn, line, char) ->
      Printf.sprintf "get-def %s:%d:%d"
        (File_input.filename_of_file_input fn) line char
  | GET_IMPORTS module_names ->
      Printf.sprintf "get-imports %s" (String.concat " " module_names)
  | INFER_TYPE (fn, line, char, _) ->
      Printf.sprintf "type-at-pos %s:%d:%d"
        (File_input.filename_of_file_input fn) line char
  | PORT (files) ->
      Printf.sprintf "port %s" (String.concat " " files)
  | STATUS (_, _) ->
      "status"
  | SUGGEST (_) ->
      "suggest"

  type command_with_context = {
    client_logging_context: FlowEventLogger.logging_context;
    command: command;
  }
end

module Response = struct

  (* Details about functions to be added in json output *)
  type func_param_result = {
      param_name     : string;
      param_ty       : string;
    }

  type func_details_result = {
      param_tys : func_param_result list;
      return_ty : string;
    }

  (* Results ready to be displayed to the user *)
  type complete_autocomplete_result = {
      res_loc      : Loc.t;
      res_ty       : string;
      res_name     : string;
      func_details : func_details_result option;
    }

  type autocomplete_response = (
    complete_autocomplete_result list,
    string
  ) result

  type coverage_response = (
    (Loc.t * bool) list,
    string
  ) result

  type dump_types_response = (
    (Loc.t * string) list,
    string
  ) result

  (* name of the symbol, locations where it appears, or None if no symbols were found *)
  type find_refs_success = (string * Loc.t list) option
  type find_refs_response = (find_refs_success, string) result

  type get_def_response = (Loc.t, string) result
  type get_imports_response = Loc.t Nel.t Modulename.Map.t SMap.t * SSet.t
  type infer_type_response = (
    Loc.t * string option,
    string
  ) result
  (* map of files to `Ok (line, col, annotation)` or `Error msg` *)
  type suggest_response = ((int * int * string) list, string) result SMap.t

  type cycle_response = (cycle_response_subgraph, string) result
  and cycle_response_subgraph = (string * string list) list

  type gen_flow_files_error =
    | GenFlowFiles_TypecheckError of {errors: Errors.ErrorSet.t; warnings: Errors.ErrorSet.t}
    | GenFlowFiles_UnexpectedError of string
  type gen_flow_files_result =
    | GenFlowFiles_FlowFile of string
    | GenFlowFiles_NonFlowFile
  type gen_flow_files_response =
    ((string * gen_flow_files_result) list, gen_flow_files_error) result

  type port_response = (string, exn) result SMap.t

  type directory_mismatch = {
    server: Path.t;
    client: Path.t;
  }

  type status_response =
  | DIRECTORY_MISMATCH of directory_mismatch
  | ERRORS of {errors: Errors.ErrorSet.t; warnings: Errors.ErrorSet.t}
  | NO_ERRORS
  | NOT_COVERED

  type lazy_stats = {
    lazy_mode: Options.lazy_mode option;
    checked_files: int;
    total_files: int;
  }

  type check_file_response = status_response

  type find_module_response = File_key.t option

  type response =
  | AUTOCOMPLETE of autocomplete_response
  | CHECK_FILE of check_file_response
  | COVERAGE of coverage_response
  | CYCLE of cycle_response
  | DUMP_TYPES of dump_types_response
  | FIND_MODULE of find_module_response
  | FIND_REFS of find_refs_response
  | GEN_FLOW_FILES of gen_flow_files_response
  | GET_DEF of get_def_response
  | GET_IMPORTS of get_imports_response
  | INFER_TYPE of infer_type_response
  | PORT of port_response
  | STATUS of { status_response: status_response; lazy_stats: lazy_stats }
  | FORCE_RECHECK of Profiling_js.finished option
  | SUGGEST of suggest_response

  let to_string = function
  | AUTOCOMPLETE _ -> "autocomplete response"
  | CHECK_FILE _ -> "check_file response"
  | COVERAGE _ -> "coverage response"
  | CYCLE _ -> "cycle reponse"
  | DUMP_TYPES _ -> "dump_types response"
  | FIND_MODULE _ -> "find_module response"
  | FIND_REFS _ -> "find_refs response"
  | GEN_FLOW_FILES _ -> "gen_flow_files response"
  | GET_DEF _ -> "get_def response"
  | GET_IMPORTS _ -> "get_imports response"
  | INFER_TYPE _ -> "infer_type response"
  | PORT _ -> "port response"
  | STATUS _ -> "status response"
  | FORCE_RECHECK _ -> "force_recheck response"
  | SUGGEST _ -> "suggest response"
end
