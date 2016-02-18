(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

open IdeJson

val call_of_string: string -> parsing_result
val json_string_of_response: call_id -> response_type -> string
val json_string_of_invalid_call: call_id -> string -> string
val json_string_of_server_busy: call_id -> string
