(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* The main entry point *)
(*****************************************************************************)

type options = {
  ai_mode          : Ai_options.prepared option;
  check_mode       : bool;
  json_mode        : bool;
  root             : Path.t;
  should_detach    : bool;
  convert          : Path.t option;
  no_load          : bool;
  save_filename    : (state_kind * string) option;
  waiting_client   : Handle.handle option;
}

and state_kind =
  | Complete
  | Mini

val parse_options: unit -> options
val default_options: root:string -> options

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

val ai_mode             : options -> Ai_options.prepared option
val check_mode          : options -> bool
val json_mode           : options -> bool
val root                : options -> Path.t
val should_detach       : options -> bool
val convert             : options -> Path.t option
val no_load             : options -> bool
val save_filename       : options -> (state_kind * string) option
val waiting_client      : options -> Handle.handle option
