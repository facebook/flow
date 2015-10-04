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
  check_mode       : bool;
  json_mode        : bool;
  root             : Path.path;
  should_detach    : bool;
  convert          : Path.path option;
  no_load          : bool;
  save_filename    : string option;
}

val parse_options: unit -> options
val default_options: root:string -> options

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

val check_mode          : options -> bool
val json_mode           : options -> bool
val root                : options -> Path.path
val should_detach       : options -> bool
val convert             : options -> Path.path option
val no_load             : options -> bool
val save_filename       : options -> string option
