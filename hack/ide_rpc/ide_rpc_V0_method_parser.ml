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

let delegate_to_previous_version method_name params =
  Nuclide_rpc_method_parser.parse
    ~method_name:(Method_name method_name)
    ~params

let parse_method method_name params = match method_name with
  | _ -> delegate_to_previous_version method_name params

let parse ~method_name ~params =
  match method_name with
  | Ide_rpc_protocol_parser_types.Unsubscribe_call -> Result.Error
    (* Should be impossible to get here *)
    (Internal_error "Unsubscribe_call is not part of jsonrpc protocol")
  | Method_name method_name -> parse_method method_name params
