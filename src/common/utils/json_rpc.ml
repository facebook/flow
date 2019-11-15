(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Hh_json

type t =
  (* method name, params, id (only for requests) *)
  | Obj of (string * json list * int option)
  | Malformed of string

exception Malformed_exn of string

let get_prop propname props =
  try List.assoc propname props
  with Not_found -> raise (Malformed_exn (propname ^ " property not found"))

let parse_unsafe str =
  let parsed = (try json_of_string str with Syntax_error msg -> raise (Malformed_exn msg)) in
  let props =
    match parsed with
    | JSON_Object props -> props
    | _ -> raise (Malformed_exn "Message is not a JSON Object")
  in
  let method_json = get_prop "method" props in
  let params_json = get_prop "params" props in
  let id_json = (try Some (List.assoc "id" props) with Not_found -> None) in
  let method_name =
    match method_json with
    | JSON_String str -> str
    | _ -> raise (Malformed_exn "Method name is not a string")
  in
  let params =
    match params_json with
    (* If you don't pass any props you just get a null here *)
    | JSON_Null -> []
    | JSON_Array lst -> lst
    | other -> [other]
  in
  let id =
    match id_json with
    | None -> None
    | Some (JSON_Number x) -> Some (int_of_string x)
    | Some _ -> raise (Malformed_exn "Unexpected id value")
  in
  Obj (method_name, params, id)

let parse_json_rpc_response str = (try parse_unsafe str with Malformed_exn msg -> Malformed msg)

let jsonrpcize_notification method_ params =
  Hh_json.(
    JSON_Object
      [
        ("jsonrpc", JSON_String "2.0");
        ("method", JSON_String method_);
        ("params", JSON_Array params);
      ])

let jsonrpcize_response id json =
  Hh_json.(
    JSON_Object
      [("jsonrpc", JSON_String "2.0"); ("id", JSON_Number (string_of_int id)); ("result", json)])
