(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type color_mode = Always | Never | Auto

type modes = {
  mutable debug: bool;
  mutable verbose: bool;
  mutable all: bool;
  mutable weak_by_default: bool;
  mutable traces: int;
  mutable strict: bool;
  mutable console: bool;
  mutable json: bool;
  mutable show_all_errors: bool;
  mutable quiet : bool;
  mutable profile : bool;
  mutable no_flowlib: bool;
  mutable color: color_mode;
}

let modes = {
  debug = false;
  verbose = false;
  all = false;
  weak_by_default = false;
  traces = 0;
  strict = false;
  console = false;
  json = false;
  show_all_errors = false;
  quiet = true;
  profile = false;
  no_flowlib = false;
  color = Auto;
}

let debug_string f = if modes.debug then prerr_endline (f ())
let verbose_string f = if modes.verbose then prerr_endline (f ())
