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

(**
 * Parse the protocol-specific part of the client message
 *)
val parse:
  message:string ->
  version:version ->
  result_t

val error_t_to_string: error_t -> string

val error_t_to_code: error_t -> int

val version_to_int: version -> int
