(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
type modes = {
  mutable debug: bool;
  mutable all: bool;
  mutable weak_by_default: bool;
  mutable traces_enabled: bool;
  mutable newtraces_enabled: bool;
  mutable strict: bool;
  mutable console: bool;
  mutable json: bool;
  mutable show_all_errors: bool;
  mutable quiet : bool;
  mutable profile : bool;
}

let modes = {
  debug = false;
  all = false;
  weak_by_default = false;
  traces_enabled = false;
  newtraces_enabled = false;
  strict = false;
  console = false;
  json = false;
  show_all_errors = false;
  quiet = true;
  profile = false;
}

let debug_string f = if modes.debug then prerr_endline (f ()) else ()
