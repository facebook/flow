(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Hh_json

let fun_call_to_json fun_call_results =
  (* List.rev_map is used here for performance purpose(tail recursive) *)
  List.rev_map fun_call_results begin fun item ->
    let item_type =
      match item.SymbolFunCallService.type_ with
      | SymbolFunCallService.Function        -> "Function"
      | SymbolFunCallService.Method          -> "Method"
      | SymbolFunCallService.Constructor     -> "Constructor" in
    JSON_Object [
      "name",           JSON_String item.SymbolFunCallService.name;
      "type",           JSON_String item_type;
      "pos",            Pos.json item.SymbolFunCallService.pos;
      "caller",         JSON_String item.SymbolFunCallService.caller;
    ]
  end

let symbol_type_to_json symbol_type_results =
  List.rev_map symbol_type_results begin fun item ->
    JSON_Object [
      "pos",    Pos.json item.SymbolTypeService.pos;
      "type",   JSON_String item.SymbolTypeService.type_;
      "ident",  int_ item.SymbolTypeService.ident_;
    ]
  end

let to_json result =
  let fun_call_json = fun_call_to_json result.SymbolInfoService.fun_calls in
  let symbol_type_json =
    symbol_type_to_json result.SymbolInfoService.symbol_types in
  JSON_Object [
    "function_calls",   JSON_Array fun_call_json;
    "symbol_types",     JSON_Array symbol_type_json;
  ]

let go conn (files:string) expand_path =
  let file_list = match files with
  | "-" ->
      let content = Sys_utils.read_stdin_to_string () in
      Str.split (Str.regexp "\n") content
  | _ ->
      Str.split (Str.regexp ";") files
  in
  let expand_path_list file_list =
    List.rev_map file_list begin fun file_path ->
      expand_path file_path
    end in
  let command = ServerRpc.DUMP_SYMBOL_INFO (expand_path_list file_list) in
  let result = ServerCommand.rpc conn command in
  let result_json = to_json result in
  print_endline (Hh_json.json_to_string result_json)
