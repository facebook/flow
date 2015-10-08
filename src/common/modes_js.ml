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
  mutable traces: int;
  mutable json: bool;
  mutable strip_root: bool;
  mutable quiet : bool;
  mutable profile : bool;
  mutable munge_underscores: bool;
}

let modes = {
  debug = false;
  traces = 0;
  json = false;
  strip_root = false;
  quiet = true;
  profile = false;
  munge_underscores = false;
}

let debug_string f = if modes.debug then prerr_endline (f ())
