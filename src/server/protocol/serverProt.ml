(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let build_revision = match Build_id.build_revision with
  | "" -> Flow_version.version
  | x -> x

type command =
| AUTOCOMPLETE of File_input.t
| CHECK_FILE of
    File_input.t *
    Verbose.t option *
    bool * (* graphml *)
    bool (* force *)
| COVERAGE of File_input.t * bool (* force *)
| DUMP_TYPES of File_input.t * bool (* filename, include raw *) * (Path.t option) (* strip_root *)
| FIND_MODULE of string * string
| FIND_REFS of File_input.t * int * int (* filename, line, char *)
| GEN_FLOW_FILES of File_input.t list
| GET_DEF of File_input.t * int * int (* filename, line, char *)
| GET_IMPORTS of string list
| INFER_TYPE of
    File_input.t * (* filename|content *)
    int * (* line *)
    int * (* char *)
    Verbose.t option *
    bool (* include raw *)
| KILL
| PORT of string list
| STATUS of Path.t
| FORCE_RECHECK of string list
| SUGGEST of (string * string list) list
| CONNECT

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
  (Loc.t * string * string * string option * Reason.t list) list,
  string
) result
type find_refs_response = (Loc.t list, string) result
type get_def_response = (Loc.t, string) result
type infer_type_response = (
  Loc.t * string option * string option * Reason.t list,
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
