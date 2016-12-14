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
open Ide_rpc_protocol_parser_types

val get_string_field :
  string -> json -> (string, error_t) Result.t

val maybe_get_number_field :
  string -> json -> (string option, error_t) Result.t

val get_int_field :
  string -> json -> (int, error_t) Result.t

val maybe_get_int_field :
  string -> json -> (int option, error_t) Result.t

val maybe_get_obj_field :
  string ->  json -> (json option, error_t) Result.t

val get_obj_field :
  string -> json -> (json, error_t) Result.t

val get_number_field :
  string -> json -> (string, error_t) Result.t

val get_array_field :
  string -> json -> (json list, error_t) Result.t

val not_implemented :
  ('a, error_t) Result.t
