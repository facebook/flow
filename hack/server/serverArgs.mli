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
  load_save_opt    : env_store_action option;
  gc_control       : Gc.control; (* configures only the workers *)
  assume_php       : bool;
}

and env_store_action =
  | Load of string
  | Save of string

val parse_options: unit -> options
val default_options: root:string -> options

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

val check_mode    : options -> bool
val json_mode     : options -> bool
val root          : options -> Path.path
val should_detach : options -> bool
val convert       : options -> Path.path option
val load_save_opt : options -> env_store_action option
val gc_control    : options -> Gc.control
val assume_php    : options -> bool
