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
open Result.Monad_infix

let json_result_to_api_result = function
  | Result.Ok v ->
    Result.Ok (fst v)
  | Result.Error e ->
    Result.Error (Invalid_request (Hh_json.Access.access_failure_to_string e))

let missing_key_error_to_none = function
  | Result.Ok v -> Hh_json.Access.return (Some (fst v))
  | Result.Error (Hh_json.Access.Missing_key_error _) ->
    Hh_json.Access.return None
  | Result.Error e -> Result.Error e

let number_to_int s =
  try
    Result.Ok (int_of_string s)
  with Failure _ -> Result.Error (Invalid_request
    (Printf.sprintf "field must be an integer, got: %s" s))

let maybe_number_to_int = function
  | None -> Result.Ok None
  | Some s ->
   try
     Result.Ok (Some (int_of_string s))
   with Failure _ -> Result.Error (Invalid_request
     (Printf.sprintf "field must be an integer or null, got: %s" s))

let no_transform x = x

let get_field ~getter ~transform field_name message =
  let open Hh_json.Access in
  return message >>=
  getter field_name |>
  transform |>
  json_result_to_api_result

let maybe_get_number_field =
  get_field
    ~getter:Hh_json.Access.get_number
    ~transform:missing_key_error_to_none

let get_number_field =
  get_field
    ~getter:Hh_json.Access.get_number
    ~transform:no_transform

let get_int_field field_name message =
  get_field field_name message
    ~getter:Hh_json.Access.get_number
    ~transform:no_transform >>=
  number_to_int

let maybe_get_int_field field_name message =
  get_field field_name message
    ~getter:Hh_json.Access.get_number
    ~transform:missing_key_error_to_none >>=
  maybe_number_to_int

let maybe_get_obj_field =
  get_field
    ~getter:Hh_json.Access.get_obj
    ~transform:missing_key_error_to_none

let get_obj_field =
  get_field
    ~getter:Hh_json.Access.get_obj
    ~transform:no_transform

let get_string_field =
  get_field
    ~getter:Hh_json.Access.get_string
    ~transform:no_transform

let get_array_field =
  get_field
    ~getter:Hh_json.Access.get_array
    ~transform:no_transform

let not_implemented = Result.Error (Internal_error "Not implemented")
