(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type env

val create_env: string -> env

val open_env: env -> unit

val close_env: env -> unit

val update_env: (string -> bool) -> env -> unit

val is_open_env: env -> bool

val last_line: env -> string

val get_lines: env -> string list

val set_lines: env -> string list -> unit

val is_tailing_current_file: env -> bool
