(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Ide_message
open Hh_json

let init_respose_to_json { server_api_version } = JSON_Object [
  ("server_api_version", int_ server_api_version);
]

let autocomplete_response_to_json x =
  let param_to_json x = JSON_Object [
    ("name", JSON_String x.name);
    ("type", JSON_String x.type_);
  ] in

  let callable_details_to_json x = JSON_Object [
    ("return_type", JSON_String x.return_type);
    ("params", JSON_Array (List.map x.params ~f:param_to_json));
  ] in

  let callable_details_to_json = function
    | None -> []
    | Some x -> [("callable_details", callable_details_to_json x)] in

  let autocomplete_response_to_json x = JSON_Object ([
    ("name", JSON_String x.autocomplete_item_text);
    ("type", JSON_String x.autocomplete_item_type);
  ] @ (callable_details_to_json x.callable_details)) in

  JSON_Array (List.map x ~f:autocomplete_response_to_json)

let to_json ~id:_ ~response =
  match response with
  | Init_response x -> init_respose_to_json x
  | Autocomplete_response x -> autocomplete_response_to_json x
  (* Delegate unhandled messages to previous version of API *)
  | _ -> Nuclide_rpc_message_printer.to_json ~response
