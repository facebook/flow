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
(* The options from the command line *)
(*****************************************************************************)

type options = {
  check_mode       : bool;
  json_mode        : bool;
  root             : Path.t;
  should_detach    : bool;
  convert          : Path.t option;
  no_load          : bool;
  save_filename    : string option;
  waiting_client   : int option;
  log_file         : Path.t;
}

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let check_mode options = options.check_mode
let json_mode options = options.json_mode
let root options = options.root
let should_detach options = options.should_detach
let convert options = options.convert
let no_load options = options.no_load
let save_filename options = options.save_filename
let waiting_client options = options.waiting_client
let log_file options = options.log_file
