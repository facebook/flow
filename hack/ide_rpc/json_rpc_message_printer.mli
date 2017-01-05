(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Hh_json

val response_to_json:
  id:int option ->
  result:[
    `Result of json |
    `Error of Ide_rpc_protocol_parser_types.error_t
  ] ->
  json
