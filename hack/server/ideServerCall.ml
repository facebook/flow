(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

open IdeEnv
open IdeJson
open Core

type deferred_to_typechecker =
  | Find_refs_call of FindRefsService.action
  | Status_call

type result =
  | Result of IdeJson.response_type
  | Deferred_to_typechecker of deferred_to_typechecker
  | Server_busy

let get_autocomplete_response tcopt content files_info =
  let res = ServerAutoComplete.auto_complete tcopt files_info content in
  Auto_complete_response ( Hh_json.JSON_Array (
    List.map res AutocompleteService.autocomplete_result_to_json
  ))

let get_identify_function_response content line char tcopt =
  let res = match ServerIdentifyFunction.go content line char tcopt with
    | Some result -> Utils.strip_ns result.IdentifySymbolService.name
    | _ -> ""
  in
  Identify_function_response res

let get_search_response s =
  let query_type = "" in (* no filters, search all types of identifiers *)
  let res = ServerSearch.go None s query_type in
  Search_call_response ( Hh_json.JSON_Array (
    List.map res ServerSearch.result_to_json
  ))

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

let get_type_at_pos_response tcopt files_info content line column =
  let get_result = InferAtPosService.attach_hooks line column in
  ignore (ServerIdeUtils.check_file_input
    tcopt files_info (ServerUtils.FileContent content));
  InferAtPosService.detach_hooks ();
  let type_reason_pos, type_name = get_result () in
  let type_reason_pos = Option.map type_reason_pos Pos.to_absolute in
  Type_at_pos_response (type_reason_pos, type_name)

let get_format_response content start end_ =
  let modes = [Some FileInfo.Mstrict; Some FileInfo.Mpartial] in
  Format_response (Format_hack.region modes Path.dummy_path start end_ content)

let get_method_name_response content line column tcopt =
  Get_method_name_response (
    ServerIdentifyFunction.go content line column tcopt
  )
let get_outline_call_response content =
  Outline_response (FileOutline.outline content)

let get_call_response_ id call env =
  match call with
  | Auto_complete_call content ->
    Result (get_autocomplete_response env.tcopt content env.files_info)
  | Identify_function_call (content, line, char) ->
    Result (get_identify_function_response content line char env.tcopt)
  | Search_call s ->
    Result (get_search_response s)
  | IdeJson.Status_call ->
    (* We need to go through the typechecker to reduce race conditions that
     * occur when editors issue "file save" and "status request" operations in
     * short succession, expecting that status will reflect the state of saved
     * file *)
    Deferred_to_typechecker (Status_call)
  | IdeJson.Find_refs_call s ->
    (* TODO: we can also lookup dependency table and fulfill requests with
     * only several dependencies in IDE process *)
    Deferred_to_typechecker (Find_refs_call s)
  | Colour_call path ->
    Result (get_colour_response env.tcopt env.files_info path)
  | Find_lvar_refs_call (content, line, column) ->
    Result (get_find_lvar_refs_response env.tcopt
      env.files_info content line column)
  | Type_at_pos_call (content, line, column) ->
    Result (get_type_at_pos_response env.tcopt
      env.files_info content line column)
  | Format_call (content, start, end_) ->
    Result (get_format_response content start end_)
  | Get_method_name_call (content, line, column) ->
    Result (get_method_name_response content line column env.tcopt)
  | Outline_call content ->
    Result (get_outline_call_response content)

let needs_typechecker_init = function
  | IdeJson.Status_call
  | IdeJson.Format_call _ -> false
  | _ -> true

let needs_shared_memory = function
  | IdeJson.Format_call _
  | IdeJson.Search_call _
  | IdeJson.Status_call -> false
  | _ -> true

let get_call_response id call env =
  if needs_typechecker_init call && (not env.typechecker_init_done) then
    Server_busy
  else if needs_shared_memory call then begin
    let response = SharedMem.try_lock_hashtable ~do_:begin fun () ->
     get_call_response_ id call env
    end in
    match response with
    | Some response -> response
    | None -> Server_busy
  end else
    get_call_response_ id call env
