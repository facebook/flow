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

let get_autocomplete_response content files_info =
  let res = ServerAutoComplete.auto_complete files_info content in
  AutoCompleteResponse ( Hh_json.JSON_Array (
    List.map AutocompleteService.autocomplete_result_to_json res
  ))

let get_call_response id call files_info =
  let response = match call with
    | AutoCompleteCall content ->
      get_autocomplete_response content files_info in
  IdeJsonUtils.json_string_of_response id response
