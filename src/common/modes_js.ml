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
  mutable verbose: bool;
  mutable verbose_indent: bool;
  mutable all: bool;
  mutable weak_by_default: bool;
  mutable traces: int;
  mutable strict: bool;
  mutable json: bool;
  mutable strip_root: bool;
  mutable quiet : bool;
  mutable profile : bool;
  mutable no_flowlib: bool;
}

let modes = {
  debug = false;
  verbose = false;
  verbose_indent = false;
  all = false;
  weak_by_default = false;
  traces = 0;
  strict = false;
  json = false;
  strip_root = false;
  quiet = true;
  profile = false;
  no_flowlib = false;
}

let debug_string f = if modes.debug then prerr_endline (f ())
