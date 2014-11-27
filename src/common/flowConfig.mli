(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
type moduleSystem = Node | Haste

type options = {
  moduleSystem: moduleSystem;
}

type config = {
  excludes: (string * Str.regexp) list;
  includes: Path.path list;
  libs: Path.path list;
  options: options;
  root: Path.path;
}
val get: Path.path -> config
val fullpath: Path.path -> string

val init: Path.path -> string list -> unit
