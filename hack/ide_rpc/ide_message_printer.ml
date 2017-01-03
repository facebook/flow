(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Ide_rpc_protocol_parser_types

let response_to_json_rpc id version response =
  let result = match version with
    | V0 -> Ide_rpc_V0_message_printer.to_json ~id ~response
  in
  Json_rpc_message_printer.response_to_json ~id ~result:(`Result result)

(* Delegate to the right printing module based on protocol and version *)
let get_print_fun version = function
  | Nuclide_rpc -> Nuclide_rpc_message_printer.to_message_json
  | JSON_RPC2 -> fun ~id ~response -> response_to_json_rpc id version response

let to_json ~id ~protocol ~version ~response =
  (get_print_fun version protocol) ~id ~response
