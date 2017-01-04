(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Ide_parser_utils
open Ide_rpc_protocol_parser_types
open Result.Monad_infix

let string_to_json message =
  let open Hh_json in
  try
    Result.Ok (json_of_string message)
  with
    | Syntax_error e | Invalid_argument e -> Result.Error (Parse_error e)

let check_service_framework3_rpc_protocol = function
  | "service_framework3_rpc" -> Result.Ok Nuclide_rpc
  | x ->
    Result.Error (Invalid_request (Printf.sprintf "Unknown protocol: %s" x))

let check_jsonrpc_protocol = function
  | "2.0" -> Result.Ok JSON_RPC2
  | x -> Result.Error
    (Invalid_request (Printf.sprintf "Unknown JSON RPC protocol version: %s" x))

let get_protocol message =
  match get_string_field "jsonrpc" message with
  | Result.Ok protocol ->
    check_jsonrpc_protocol protocol
  | Result.Error _ ->
    get_string_field "protocol" message >>=
    check_service_framework3_rpc_protocol

let get_id message =
  maybe_get_int_field "id" message

let get_method message =
  get_string_field "method" message

let check_type_field message = function
  | "call" -> get_method message >>= fun s ->
    Result.Ok (Method_name s)
  | "unsubscribe" ->
    Result.Ok Unsubscribe_call
  | x ->
    Result.Error (Invalid_request
      (Printf.sprintf "Message type not recognized: %s" x))

let get_method_name message = function
  | Nuclide_rpc ->
    get_string_field "type" message >>= check_type_field message
  | JSON_RPC2 ->
    get_method message >>= fun s ->
    Result.Ok (Method_name s)

let get_params message = function
  | Nuclide_rpc -> maybe_get_obj_field "args" message
  | JSON_RPC2 -> maybe_get_obj_field "params" message

let result_of_error e = {
  protocol = Result.Error e;
  id = Result.Error e;
  result = Result.Error e;
}

let parse ~message ~version:_ =
  match string_to_json message with
  | Result.Error e -> result_of_error e
  | Result.Ok message ->
    let id = get_id message in
    let protocol = get_protocol message in
    let result =
      protocol >>= fun protocol ->
      get_method_name message protocol >>= fun method_name ->
      get_params message protocol >>= fun params ->
      Result.Ok { method_name; params; }
    in
    { id; protocol; result; }

let error_t_to_string = function
  | Parse_error s -> Printf.sprintf "Parsing error: %s" s
  | Invalid_request s -> Printf.sprintf "Invalid request: %s" s
  | Method_not_found s -> Printf.sprintf "Method not found: %s" s
  | Internal_error s -> Printf.sprintf "Internal error: %s" s
  | Invalid_params s -> Printf.sprintf "Invalid params: %s" s
  | Server_error s -> Printf.sprintf "Server error: %s" s

let error_t_to_code = function
  | Parse_error _ -> -32700
  | Invalid_request _ -> -32600
  | Method_not_found _ -> -32601
  | Invalid_params _ -> -32602
  | Internal_error _ -> -32603
  | Server_error _ -> -32000

let version_to_int = function
  | V0 -> 0
