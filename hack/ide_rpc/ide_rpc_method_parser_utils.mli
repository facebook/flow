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
open Hh_json

val assert_params_required :
  string -> 'a option -> ('a, error_t) Result.t

val get_text_field :
  json -> (string, error_t) Result.t

val get_filename_filed :
  json -> (string, error_t) Result.t
