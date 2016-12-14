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
open Result.Monad_infix

let parse_method version rpc_parser_result =
  let result =
    rpc_parser_result.result >>= fun result ->
    rpc_parser_result.protocol >>= fun protocol ->
    Ide_rpc_method_parser.parse
      ~version
      ~protocol
      ~method_name:result.method_name
      ~params:result.params
  in
  {
    protocol = rpc_parser_result.protocol;
    id = rpc_parser_result.id;
    result
  }

let parse ~message ~version =
  (* Extract protocol-specific parts *)
  Ide_rpc_protocol_parser.parse ~message ~version |>
  (* Extract method-specific parts *)
  parse_method version
