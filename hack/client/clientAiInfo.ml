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
  let open Ai.InfoService in
  (* List.rev_map is used here for performance purpose(tail recursive) *)
  List.rev_map fun_call_results (fun item ->
    let item_type =
      match item.type_ with
      | Function    -> "Function"
      | Method      -> "Method"
      | Constructor -> "Constructor" in
    JAssoc [
      "name",    JString item.name;
      "type",    JString item_type;
      "pos",     Pos.json item.pos;
      "caller",  JString item.caller;
      "callees", JList (List.map item.callees (fun n -> JString n));
    ]
  )

let throws_to_json throws_results =
  let open Ai.InfoService in
  List.rev_map throws_results (fun item ->
    JAssoc [
      "thrower", JString item.thrower;
      "filename", JString item.filename;
      "exceptions", JList (List.map item.exceptions (fun e -> JString e));
    ]
  )

let to_json result =
  let fun_call_json = fun_call_to_json result.Ai.InfoService.fun_calls in
  let throws_json = throws_to_json result.Ai.InfoService.throws in
  JAssoc [
    "function_calls",   JList fun_call_json;
    "throws", JList throws_json
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
  let command = ServerRpc.DUMP_AI_INFO (expand_path_list file_list) in
  let result = ServerCommand.rpc conn command in
  let result_json = to_json result in
  print_endline (Hh_json.json_to_string result_json)
