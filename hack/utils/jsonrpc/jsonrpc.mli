(*
 * Copyright (c) 2017, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

type writer = Hh_json.json -> unit

type kind =
  | Request
  | Notification
  | Response

val kind_to_string : kind -> string

type message = {
  json: Hh_json.json;
  (* the json payload *)
  timestamp: float;
  (* time this message arrived at stdin *)
  (* Following fields are decompositions of 'json'... *)
  kind: kind;
  method_: string;
  (* mandatory for request+notification; empty otherwise *)
  id: Hh_json.json option;
  (* mandatory for request+response *)
  params: Hh_json.json option;
  (* optional for request+notification *)
  result: Hh_json.json option;
  (* optional for response *)
  error: Hh_json.json option; (* optional for response *)
}

val parse_message : json:Hh_json.json -> timestamp:float -> message

val message_to_short_string : message -> string

type queue

(* must call Daemon.entry_point at start of your main *)
val make_queue : unit -> queue

val get_read_fd : queue -> Unix.file_descr (* can be used for 'select' *)

val has_message : queue -> bool

val get_message :
  queue ->
  [> `Message of message
  | `Fatal_exception of Marshal_tools.remote_exception_data
  | `Recoverable_exception of Marshal_tools.remote_exception_data
  ]
  Lwt.t

(* 'respond to_this with_that' is for replying to a JsonRPC request. It will send either *)
(* a response or an error depending on whether 'with_that' has an error id in it.        *)
(* [powered_by] is our own non-standard extension to JsonRPC, which lets the
client know which back-end served the request. *)
val respond : writer -> ?powered_by:string -> message -> Hh_json.json -> unit

(* notify/request are for initiating JsonRPC messages *)
val notify : writer -> ?powered_by:string -> string -> Hh_json.json -> unit

val get_next_request_id : unit -> int

(* For logging purposes, you can get a copy of which JsonRPC message was last    *)
(* sent by this module - be it a response, notification, request or cancellation *)
val last_sent : unit -> Hh_json.json option

val clear_last_sent : unit -> unit
