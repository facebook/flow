(**
 * Copyright (c) 2014, Facebook, Inc.
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
  ai_mode          : string option;
  check_mode       : bool;
  json_mode        : bool;
  root             : Path.t;
  should_detach    : bool;
  convert          : Path.t option;
  no_load          : bool;
  save_filename    : string option;
  waiting_client   : Handle.handle option;
}

val parse_options: unit -> options
val default_options: root:string -> options

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

val ai_mode             : options -> string option
val check_mode          : options -> bool
val json_mode           : options -> bool
val root                : options -> Path.t
val should_detach       : options -> bool
val convert             : options -> Path.t option
val no_load             : options -> bool
val save_filename       : options -> string option
val waiting_client      : options -> Handle.handle option
