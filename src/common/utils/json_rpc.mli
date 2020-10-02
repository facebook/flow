(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  (* method name, params, id (only for requests) *)
  | Obj of (string * Hh_json.json list * int option)
  | Malformed of string

val parse_json_rpc_response : string -> t

val jsonrpcize_notification :
  (* method name *)
  string -> (* value to send *)
            Hh_json.json list -> Hh_json.json

val jsonrpcize_response : (* request id *)
                          int -> (* return value *)
                                 Hh_json.json -> Hh_json.json
