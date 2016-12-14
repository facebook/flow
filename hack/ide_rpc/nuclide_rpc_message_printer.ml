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

(* There are fields that Nuclide doesn't use anymore, but the RPC framework
 * still requires them in responses. Stub them with some default values in the
 * meantime *)
let deprecated_pos_field = Pos.json (Pos.to_absolute Pos.none)
let deprecated_int_field = Hh_json.int_ 0
let deprecated_bool_field = JSON_Bool false

let autocomplete_response_to_json x =
  let param_to_json x = JSON_Object [
    ("name", JSON_String x.name);
    ("type", JSON_String x.type_);
    ("variadic", deprecated_bool_field);
  ] in

  let callable_details_to_json x = JSON_Object [
    ("return_type", JSON_String x.return_type);
    ("params", JSON_Array (List.map x.params ~f:param_to_json));
    ("min_arity", deprecated_int_field);
  ] in

  let callable_details_to_json = function
    | None -> []
    | Some x -> [("func_details", callable_details_to_json x)] in

  let autocomplete_response_to_json x = JSON_Object ([
    ("name", JSON_String x.autocomplete_item_text);
    ("type", JSON_String x.autocomplete_item_type);
    ("pos", deprecated_pos_field);
    ("expected_ty", deprecated_bool_field)
  ] @ (callable_details_to_json x.callable_details)) in

  JSON_Array (List.map x ~f:autocomplete_response_to_json)

let diagnostics_to_json x =
  JSON_Object [
    ("filename", JSON_String x.diagnostics_notification_filename);
    ("errors", JSON_Array (List.map x.diagnostics ~f:Errors.to_json));
  ]

let response_to_json id result =
  let id = match id with
    | Some x -> JSON_Number (string_of_int x)
    | None -> JSON_Null
  in
  JSON_Object [
    ("protocol", JSON_String "service_framework3_rpc");
    ("type", JSON_String "response");
    ("id", id);
    ("result", result);
  ]

let subscription_to_json id result=
   JSON_Object [
    ("protocol", JSON_String "service_framework3_rpc");
    ("type", JSON_String "next");
    ("id", int_ id);
    ("value", result);
  ]

let to_json ~id ~response =
  match response with
  | Autocomplete_response x ->
    response_to_json id (autocomplete_response_to_json x)
  | Diagnostics_notification x ->
    subscription_to_json x.subscription_id (diagnostics_to_json x)
