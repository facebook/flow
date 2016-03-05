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

let get_autocomplete_response content files_info =
  let res = ServerAutoComplete.auto_complete files_info content in
  AutoCompleteResponse ( Hh_json.JSON_Array (
    List.map res AutocompleteService.autocomplete_result_to_json
  ))

let get_identify_function_response content line char =
  IdentifyFunctionResponse (
    ServerIdentifyFunction.go content line char
  )

let get_search_response s =
  let query_type = "" in (* no filters, search all types of identifiers *)
  let res = ServerSearch.go s query_type in
  SearchCallResponse ( Hh_json.JSON_Array (
    List.map res ServerSearch.result_to_json
  ))

let get_call_response id call files_info =
  let response = match call with
    | AutoCompleteCall content ->
      get_autocomplete_response content files_info
    | IdentifyFunctionCall (content, line, char) ->
      get_identify_function_response content line char
    | SearchCall s ->
      get_search_response s
  in
  IdeJsonUtils.json_string_of_response id response
