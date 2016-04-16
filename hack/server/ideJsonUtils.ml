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
open Utils

let server_busy_error_code = 1
let invalid_call_error_code = 2

let string_positions_to_ints line column =
  try
    int_of_string line, int_of_string column
  with Failure "int_of_string" -> raise Not_found

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
    Auto_complete_call content
  | [JSON_String "--identify-function"; JSON_String pos; JSON_String content] ->
    let tpos = Str.split (Str.regexp ":") pos in
    let line, char =
      try
         match tpos with
         | [line; char] ->
             int_of_string line, int_of_string char
         | _ -> raise Not_found
      with _ -> raise Not_found in
    Identify_function_call (content, line, char)
  | [JSON_String "--search"; JSON_String content] -> Search_call content
  | [] -> Status_call
  | [JSON_String "--find-refs"; JSON_String s] ->
    let open FindRefsService in
    begin match Str.split (Str.regexp "::") s with
      | class_name :: method_name :: _ ->
        Find_refs_call (Method (class_name, method_name))
      | function_name :: _ ->
        Find_refs_call (Function function_name)
      | _ -> raise Not_found
    end
  | [JSON_String "--find-class-refs"; JSON_String s] ->
    Find_refs_call (FindRefsService.Class s)
  | [JSON_String "--color"; JSON_String path]
  | [JSON_String "--colour"; JSON_String path] ->
    Colour_call path
  | [JSON_String "--find-lvar-refs";
     JSON_String content;
     JSON_Number line;
     JSON_Number column
    ] ->
      let line, column = string_positions_to_ints line column in
      Find_lvar_refs_call (content, line, column)
  | [JSON_String "--type-at-pos";
     JSON_String content;
     JSON_Number line;
     JSON_Number column
    ] ->
      let line, column = string_positions_to_ints line column in
      Type_at_pos_call (content, line, column)
  | [JSON_String "--format";
     JSON_String content;
     JSON_Number start;
     JSON_Number end_
    ] ->
      let start, end_ = string_positions_to_ints start end_ in
      Format_call (content, start, end_)
  | [JSON_String "--get-method-name";
     JSON_String content;
     JSON_Number line;
     JSON_Number column
    ] ->
      let line, column = string_positions_to_ints line column in
      Get_method_name_call (content, line, column)
  | [JSON_String "--outline"; JSON_String content] -> Outline_call content
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

  (* Client sometimes asks for JSON version, but in persistent connection mode
   * that's the only version, so filter it out *)
  let strip_json_args args = List.filter args begin function
    | JSON_String "--json" -> false
    | _ -> true end in

  let get_call id fields =
    match get_field fields "args" with
    | Some (JSON_Array args) ->
      begin
        try
          Ok (Call (id, args_to_call (strip_json_args args)))
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
  | Error `Syntax_error e -> Parsing_error ("Invalid JSON: " ^ e)
  | Error `Not_object -> Parsing_error "Expected JSON object"
  | Error `No_id -> Parsing_error "Request object must have id field"
  | Error `Id_not_int -> Parsing_error "id field must be an integer"
  | Error `No_type -> Parsing_error "Request object must have type field"
  | Error `Message_type_not_string ->
    Parsing_error "Type field must be a string"
  | Error `Message_type_not_recognized ->
    Parsing_error "Message type not recognized"
  | Error `No_args id ->
    Invalid_call (id, "Request object must have an args field")
  | Error `Args_not_an_array id ->
    Invalid_call (id, "Args field must be an array")
  | Error `Call_not_recognized id -> Invalid_call (id, "Call not recognized")

let build_response_json id result_field =
  JSON_Object [
    ("type", JSON_String "response");
    ("id", JSON_Number (string_of_int id));
    ("result", result_field);
  ]

let json_string_of_response id response =
  let result_field = match response with
    | Auto_complete_response r -> r
    | Identify_function_response s -> JSON_String s
    | Search_call_response r -> r
    | Status_response r -> r
    | Find_refs_response r -> ServerFindRefs.to_json r
    | Colour_response r -> r
    | Find_lvar_refs_response pos_list ->
      let res_list = List.map pos_list (compose Pos.json Pos.to_absolute) in
      JSON_Object [
        "positions",      JSON_Array res_list;
        "internal_error", JSON_Bool false
      ]
    | Type_at_pos_response (pos, ty) -> ServerInferType.to_json pos ty
    | Format_response result ->
      let error_text, result, is_error = match result with
        | Format_hack.Disabled_mode -> "Php_or_decl", "", false
        | Format_hack.Parsing_error _ -> "Parsing_error", "", false
        | Format_hack.Internal_error -> "", "", true
        | Format_hack.Success s -> "", s, false
      in
      JSON_Object [
        "error_message",  JSON_String error_text;
        "result",         JSON_String result;
        "internal_error", JSON_Bool is_error;
      ]
    | Get_method_name_response result ->
      begin match result with
      | Some res ->
        let result_type =
          match res.IdentifySymbolService.type_ with
          | IdentifySymbolService.Class -> "class"
          | IdentifySymbolService.Method _ -> "method"
          | IdentifySymbolService.Function -> "function"
          | IdentifySymbolService.LocalVar -> "local"
        in
        JSON_Object [
          "name",        JSON_String res.IdentifySymbolService.name;
          "result_type", JSON_String result_type;
          "pos",         Pos.json
                           (Pos.to_absolute res.IdentifySymbolService.pos);
        ]
      | _ -> JSON_Object []
      end
    | Outline_response result ->
      FileOutline.to_json result
  in
  json_to_string (build_response_json id result_field)

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
