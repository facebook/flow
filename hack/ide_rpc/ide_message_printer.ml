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

(* Delegate to the right printing module based on protocol and version *)
let get_print_fun version protocol =
  match version, protocol with
  | _, Nuclide_rpc -> Nuclide_rpc_message_printer.to_json
  | V0, JSON_RPC2 -> raise @@ Failure "not implemented"

let to_json ~id ~protocol ~version ~response =
  (get_print_fun version protocol) ~id ~response
