(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Ide_parser_utils
open Ide_rpc_protocol_parser_types

let assert_params_required method_name params =
  Result.of_option params
    ~error:(Invalid_params
      (Printf.sprintf "%s request requires params" method_name))

let get_text_field = get_string_field "text"

let get_filename_filed = get_string_field "filename"
