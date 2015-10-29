(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type file_input =
| FileName of string
| FileContent of string option * string (* filename, content *)

let path_of_input = function
| FileName f -> Some f
| FileContent (Some f, _) -> Some f
| _ -> None

let file_input_get_filename = function
  | FileName fn -> fn
  | FileContent (Some fn, _) -> fn
  | FileContent (None, _) -> "-"

let file_input_get_content = function
  | FileName fn -> Sys_utils.cat fn
  | FileContent (_, content) -> content

type command =
| AUTOCOMPLETE of file_input
| CHECK_FILE of file_input * int option (* verbose *)
| DUMP_TYPES of file_input * bool (* filename, include raw *)
| ERROR_OUT_OF_DATE
| FIND_MODULE of string * string
| GET_DEF of file_input * int * int (* filename, line, char *)
| GET_IMPORTERS of string list
| GET_IMPORTS of string list
| INFER_TYPE of file_input * int * int * bool (* filename|content, line,
                                                        char, include raw *)
| KILL
| PING
| PORT of string list
| STATUS of Path.t
| SEARCH of string
| SUGGEST of string list

type command_with_context = {
  client_logging_context: FlowEventLogger.logging_context;
  command: command;
}

let cmd_to_channel (oc:out_channel) (cmd:command): unit =
  let command = {
    client_logging_context = FlowEventLogger.get_context ();
    command = cmd;
  } in
  Printf.fprintf oc "%s\n" Build_id.build_id_ohai;
  Marshal.to_channel oc command [];
  flush oc

let cmd_from_channel (ic:in_channel): command_with_context =
  let s = input_line ic in
  if s <> Build_id.build_id_ohai
  then {
    client_logging_context = FlowEventLogger.get_context ();
    command = ERROR_OUT_OF_DATE;
  }
  else Marshal.from_channel ic

type directory_mismatch = {
  server: Path.t;
  client: Path.t;
}

type response =
| DIRECTORY_MISMATCH of directory_mismatch
| ERRORS of Errors_js.error list
| NO_ERRORS
| PONG
| SERVER_DYING
| SERVER_OUT_OF_DATE

let response_to_string = function
  | DIRECTORY_MISMATCH _ -> "Directory Mismatch"
  | ERRORS _ -> "Some Errors"
  | NO_ERRORS -> "No Errors"
  | PONG -> "Pong"
  | SERVER_DYING -> "Server Dying"
  | SERVER_OUT_OF_DATE -> "Server Out of Date"

let response_to_channel (oc:out_channel) (cmd:response): unit =
  Printf.fprintf oc "%s\n" Build_id.build_id_ohai;
  Marshal.to_channel oc cmd [];
  flush oc

let response_from_channel (ic:Timeout.in_channel): response =
  let s = Timeout.input_line ic in
  if s <> Build_id.build_id_ohai
  then SERVER_OUT_OF_DATE
  else Timeout.input_value ic
