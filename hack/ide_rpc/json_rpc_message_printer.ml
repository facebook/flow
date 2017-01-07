(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Hh_json
open Ide_rpc_protocol_parser

let response_to_json ~id ~result =
  let id = match id with
    | Some x -> JSON_Number (string_of_int x)
    | None -> JSON_Null
  in
  let result = match result with
    | `Result result -> ("result", result)
    | `Error e ->
      ("error", JSON_Object [
        "code", int_ (error_t_to_code e);
        "message", JSON_String (error_t_to_string e);
      ])
  in
  JSON_Object [
    "jsonrpc", JSON_String "2.0";
    "id", id;
    result;
  ]
