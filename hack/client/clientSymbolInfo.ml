(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
open SymbolInfoService

let to_json fun_call_results =
  let open Hh_json in
  (* Map from 'fun_call_result list' to 'json JList' *)
  (* List.rev_map is used here for performance purpose(tail recursive) *)
  let result = List.rev_map begin fun item ->
    let item_type =
      match item.SymbolInfoService.type_ with
      | Function        -> "Function"
      | Method          -> "Method"
      | Constructor     -> "Constructor" in
    JAssoc [
      "name",           JString item.SymbolInfoService.name;
      "type",           JString item_type;
      "pos",            Pos.json item.SymbolInfoService.pos;
      "caller",         JString item.SymbolInfoService.caller;
    ]
  end fun_call_results in
  JAssoc [ "function_calls", JList result; ]

let go (files:string) ic oc expand_path =
  let file_list = match files with
  | "-" ->
      let content = ClientUtils.read_stdin_to_string () in
      Str.split (Str.regexp "\n") content
  | _ ->
      Str.split (Str.regexp ";") files
  in
  let expand_path_list file_list =
    List.rev_map begin fun file_path ->
      expand_path file_path
    end file_list in
  let command = ServerMsg.DUMP_SYMBOL_INFO (expand_path_list file_list) in
  ServerMsg.cmd_to_channel oc command;
  let result_json = to_json (Marshal.from_channel ic) in
  print_endline (Hh_json.json_to_string result_json)
