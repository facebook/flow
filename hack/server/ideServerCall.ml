(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

open IdeJson
open Core

type deferred_to_typechecker =
  | Find_refs_call of FindRefsService.action

type result =
  | Result of IdeJson.response_type
  | Deferred_to_typechecker of deferred_to_typechecker

let get_autocomplete_response content files_info =
  let res = ServerAutoComplete.auto_complete files_info content in
  Auto_complete_response ( Hh_json.JSON_Array (
    List.map res AutocompleteService.autocomplete_result_to_json
  ))

let get_identify_function_response content line char =
  Identify_function_response (
    ServerIdentifyFunction.go content line char
  )

let get_search_response s =
  let query_type = "" in (* no filters, search all types of identifiers *)
  let res = ServerSearch.go s query_type in
  Search_call_response ( Hh_json.JSON_Array (
    List.map res ServerSearch.result_to_json
  ))

let get_status_reponse errorl =
  let errorl = List.map errorl Errors.to_absolute in
  Status_response (ServerError.get_errorl_json errorl)

let get_colour_response tcopt files_info path =
  let check = fun () -> ServerIdeUtils.check_file_input
    tcopt files_info ServerUtils.(FileName path) in
  let coverage = ServerColorFile.get_level_list check in
  let file_contents = try Sys_utils.cat path with _ -> "" in
  let colors = ColorFile.go file_contents coverage in
  Colour_response (ServerColorFile.to_json colors)

let get_find_lvar_refs_response tcopt files_info content line column =
  let get_result = FindLocalsService.attach_hooks line column in
  ignore (ServerIdeUtils.check_file_input
    tcopt files_info (ServerUtils.FileContent content));
  FindLocalsService.detach_hooks ();
  Find_lvar_refs_response (get_result ())

let get_call_response id call tcopt files_info errorl =
  match call with
  | Auto_complete_call content ->
    Result (get_autocomplete_response content files_info)
  | Identify_function_call (content, line, char) ->
    Result (get_identify_function_response content line char)
  | Search_call s ->
    Result (get_search_response s)
  | Status_call ->
    Result (get_status_reponse errorl)
  | IdeJson.Find_refs_call s ->
    (* TODO: we can also lookup dependency table and fulfill requests with
     * only several dependencies in IDE process *)
    Deferred_to_typechecker (Find_refs_call s)
  | Colour_call path ->
    Result (get_colour_response tcopt files_info path)
  | Find_lvar_refs_call (content, line, column) ->
    Result (get_find_lvar_refs_response tcopt files_info content line column)
