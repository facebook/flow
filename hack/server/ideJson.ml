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
  | AutoCompleteCall of string
  | IdentifyFunctionCall of string * int * int
  | SearchCall of string
  | StatusCall
  | FindRefsCall of FindRefsService.action
  | ColourCall of string

type response_type =
  | AutoCompleteResponse of Hh_json.json
  | IdentifyFunctionResponse of string
  | SearchCallResponse of Hh_json.json
  | StatusResponse of Hh_json.json
  | FindRefsResponse of FindRefsService.result
  | ColourResponse of Hh_json.json

type parsing_result =
  (* ParsingError means that message was unrecoverably mangled (eg. no ID, or
   * completely invalid JSON). We will just log it, but not send anything back
   * to the client. *)
  | ParsingError of string
  (* InvalidCall will get an error response from the server. *)
  | InvalidCall of call_id * string
  | Call of call_id * call_type
