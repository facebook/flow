(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Constructs just the "result" part of the message *)
val to_json :
  response:Ide_message.response ->
  Hh_json.json

(* Constructs the entire response message, ready to be sent to client *)
val to_message_json :
  id:int option ->
  response:Ide_message.response ->
  Hh_json.json
