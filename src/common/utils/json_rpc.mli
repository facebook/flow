(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t =
  (* method name, params, id (only for requests) *)
  | Obj of (string * Hh_json.json list * int option)
  | Malformed of string

val parse_json_rpc_response: string -> t
