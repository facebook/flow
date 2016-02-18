(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

open IdeJson
open Core
open Hh_json
open Result
open Result.Monad_infix

let server_busy_error_code = 1
let invalid_call_error_code = 2

(**
 * During transition from hh_client based to persistent connection based
 * operation we will initially just dump command line arguments into an "args"
 * array as is, so for example:
 *
 *   hh_client --auto-complete "some text"
 *
 * becomes:
 *
 * {
 *   "id" = <some_number>,
 *   "type" = "call",
 *   "args" = ["--auto-complete", "some text"]
 * }
 * This function translates those args into a call_type structure.
*)
let args_to_call = function
  | [JSON_String "--auto-complete"; JSON_String content] ->
    AutoCompleteCall content
  | _ -> raise Not_found

let call_of_string s =
  let get_object_fields s =
    try
      begin match json_of_string s with
        | JSON_Object fields -> Ok fields
        | _ ->  Error `Not_object
      end
    with Syntax_error e -> Error (`Syntax_error e) in

  let get_field fields field_name  =
    match List.find fields (fun (x, _) -> x = field_name) with
    | Some (_, x) -> Some x
    | None -> None in

  let get_id_field fields =
    match get_field fields "id" with
    | Some id -> begin match id with
      | JSON_Number i ->
        (try Ok (int_of_string i) with Failure _ -> Error `Id_not_int)
      | _ ->  Error `Id_not_int
    end
    | None -> Error `No_id in

  let get_type_field fields =
    match get_field fields "type" with
    | Some t -> begin match t with
      | JSON_String "call" -> Ok "call"
      | JSON_String _ -> Error `Message_type_not_recognized
      | _ -> Error `Message_type_not_string
    end
    | None -> Error `No_type in

  let get_call id fields =
    match get_field fields "args" with
    | Some (JSON_Array args) ->
      begin
        try
          Ok (Call (id, args_to_call args))
        with Not_found -> Error (`Call_not_recognized id)
      end
    | Some _ -> Error (`Args_not_an_array id)
    | _ -> Error (`No_args id) in

  match
    (get_object_fields s) >>= fun fields ->
    (get_id_field fields) >>= fun id ->
    (get_type_field fields) >>= fun type_ ->
    (get_call id fields)
  with
  | Ok x -> x
  | Error `Syntax_error e -> ParsingError ("Invalid JSON: " ^ e)
  | Error `Not_object -> ParsingError "Expected JSON object"
  | Error `No_id -> ParsingError "Request object must have id field"
  | Error `Id_not_int -> ParsingError "id field must be an integer"
  | Error `No_type -> ParsingError "Request object must have type field"
  | Error `Message_type_not_string ->
    ParsingError "Type field must be a string"
  | Error `Message_type_not_recognized ->
    ParsingError "Message type not recognized"
  | Error `No_args id ->
    InvalidCall (id, "Request object must have an args field")
  | Error `Args_not_an_array id ->
    InvalidCall (id, "Args field must be an array")
  | Error `Call_not_recognized id -> InvalidCall (id, "Call not recognized")

let json_string_of_response id = function
  | AutoCompleteResponse r ->
    json_to_string (JSON_Object [
      ("type", JSON_String "response");
      ("id", JSON_Number (string_of_int id));
      ("result", r);
    ])

let json_string_of_error id error_code error_message  =
  json_to_string (JSON_Object [
    ("type", JSON_String "response");
    ("id", JSON_Number (string_of_int id));
    ("error", JSON_Object [
       ("code", JSON_Number (string_of_int error_code));
       ("message", JSON_String error_message);
     ]);
  ])

let json_string_of_invalid_call id error_message =
  json_string_of_error id invalid_call_error_code error_message

let json_string_of_server_busy id =
  json_string_of_error id server_busy_error_code "Server busy"
