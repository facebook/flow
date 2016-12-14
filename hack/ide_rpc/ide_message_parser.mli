(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(**
 * Parse a message sent from editor to server
 *)
val parse:
  message:string ->
  version:Ide_rpc_protocol_parser_types.version ->
  Ide_message_parser_types.result_t
