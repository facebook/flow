(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Ide_message
open Ide_rpc_method_parser_utils
open Ide_rpc_protocol_parser_types
open Result.Monad_infix

let delegate_to_previous_version method_name params =
  Nuclide_rpc_method_parser.parse
    ~method_name:(Method_name method_name)
    ~params

let parse_did_open_file_params params =
  get_filename_filed params >>= fun did_open_file_filename ->
  get_text_field params >>= fun did_open_file_text ->
  Result.Ok { did_open_file_filename; did_open_file_text; }

let parse_did_open_file method_name params =
  assert_params_required method_name params >>=
  parse_did_open_file_params >>= fun params ->
  Result.Ok (Did_open_file params)

let parse_method method_name params = match method_name with
  | "didOpenFile" -> parse_did_open_file method_name params
  | "autocomplete" -> delegate_to_previous_version "getCompletions" params
  | _ -> delegate_to_previous_version method_name params

let parse ~method_name ~params =
  match method_name with
  | Ide_rpc_protocol_parser_types.Unsubscribe_call -> Result.Error
    (* Should be impossible to get here *)
    (Internal_error "Unsubscribe_call is not part of jsonrpc protocol")
  | Method_name method_name -> parse_method method_name params
