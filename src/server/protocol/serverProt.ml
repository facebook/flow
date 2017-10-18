(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let build_revision = match Build_id.build_revision with
  | "" -> Flow_version.version
  | x -> x

type command =
| AUTOCOMPLETE of File_input.t
| CHECK_FILE of
    File_input.t *
    Verbose.t option *
    bool * (* force *)
    bool (* include_warnings *)
| COVERAGE of File_input.t * bool (* force *)
| DUMP_TYPES of File_input.t
| FIND_MODULE of string * string
| FIND_REFS of File_input.t * int * int (* filename, line, char *)
| GEN_FLOW_FILES of File_input.t list * bool (* include_warnings *)
| GET_DEF of File_input.t * int * int (* filename, line, char *)
| GET_IMPORTS of string list
| INFER_TYPE of
    File_input.t * (* filename|content *)
    int * (* line *)
    int * (* char *)
    Verbose.t option
| KILL
| PORT of string list
| STATUS of Path.t * bool (* include_warnings *)
| FORCE_RECHECK of string list * bool (* focus *)
| SUGGEST of (string * string list) list
| CONNECT

let string_of_command = function
| AUTOCOMPLETE fn ->
  Printf.sprintf "autocomplete %s" (File_input.filename_of_file_input fn)
| CHECK_FILE (fn, _, _, _) ->
  Printf.sprintf "check %s" (File_input.filename_of_file_input fn)
| COVERAGE (fn, _) ->
    Printf.sprintf "coverage %s" (File_input.filename_of_file_input fn)
| DUMP_TYPES (fn) ->
    Printf.sprintf "dump-types %s" (File_input.filename_of_file_input fn)
| FIND_MODULE (moduleref, filename) ->
    Printf.sprintf "find-module %s %s" moduleref filename
| FIND_REFS (fn, line, char) ->
    Printf.sprintf "find-refs %s:%d:%d" (File_input.filename_of_file_input fn) line char
| FORCE_RECHECK (files, force_focus) ->
    Printf.sprintf
      "force-recheck %s (focus = %b)" (String.concat " " files) force_focus
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
| KILL ->
    "kill"
| PORT (files) ->
    Printf.sprintf "port %s" (String.concat " " files)
| STATUS (_, _) ->
    "status"
| SUGGEST (_) ->
    "suggest"
| CONNECT ->
    "connect"

type command_with_context = {
  client_logging_context: FlowEventLogger.logging_context;
  command: command;
}

type autocomplete_response = (
  AutocompleteService_js.complete_autocomplete_result list,
  string
) result
type coverage_response = (
  (Loc.t * bool) list,
  string
) result
type dump_types_response = (
  (Loc.t * string * string * Reason.t list) list,
  string
) result
type find_refs_response = (Loc.t list, string) result
type get_def_response = (Loc.t, string) result
type get_imports_response = (Modulename.Set.t * Loc.t SMap.t) SMap.t * SSet.t
type infer_type_response = (
  Loc.t * string option * Reason.t list,
  string
) result
(* map of files to `Ok (line, col, annotation)` or `Error msg` *)
type suggest_response = ((int * int * string) list, string) result SMap.t

type gen_flow_file_error =
  | GenFlowFile_TypecheckError of {errors: Errors.ErrorSet.t; warnings: Errors.ErrorSet.t}
  | GenFlowFile_UnexpectedError of string
type gen_flow_file_result =
  | GenFlowFile_FlowFile of string
  | GenFlowFile_NonFlowFile
type gen_flow_file_response =
  ((string * gen_flow_file_result) list, gen_flow_file_error) result
type port_response = (string, exn) result SMap.t
type stop_response = (unit, string) result

type directory_mismatch = {
  server: Path.t;
  client: Path.t;
}

type response =
| DIRECTORY_MISMATCH of directory_mismatch
| ERRORS of {errors: Errors.ErrorSet.t; warnings: Errors.ErrorSet.t}
| NO_ERRORS
| NOT_COVERED
