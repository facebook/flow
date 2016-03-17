(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

(* Every message has a unique id, for debugging and pairing requests with
 * responses. *)
type call_id = int

type call_type =
  | Auto_complete_call of string
  | Identify_function_call of string * int * int
  | Search_call of string
  | Status_call
  | Find_refs_call of FindRefsService.action
  | Colour_call of string
  | Find_lvar_refs_call of string * int * int
  | Type_at_pos_call of string * int * int
  | Format_call of string * int * int
  | Get_method_name_call of string * int * int

type response_type =
  | Auto_complete_response of Hh_json.json
  | Identify_function_response of string
  | Search_call_response of Hh_json.json
  | Status_response of Hh_json.json
  | Find_refs_response of FindRefsService.result
  | Colour_response of Hh_json.json
  | Find_lvar_refs_response of Pos.t list
  | Type_at_pos_response of Pos.absolute option * string option
  | Format_response of string Format_hack.return
  | Get_method_name_response of IdentifySymbolService.find_symbol_result option

type parsing_result =
  (* Parsing_error means that message was unrecoverably mangled (eg. no ID, or
   * completely invalid JSON). We will just log it, but not send anything back
   * to the client. *)
  | Parsing_error of string
  (* Invalid_call will get an error response from the server. *)
  | Invalid_call of call_id * string
  | Call of call_id * call_type
