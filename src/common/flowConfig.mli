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
  traces: int;
}

module PathMap : Utils.MapSig with type key = Path.path

type config = {
  (* file blacklist *)
  excludes: (string * Str.regexp) list;
  (* user-specified non-root include paths. may contain wildcards *)
  includes: Path.path list;
  (* stems extracted from includes *)
  include_stems: Path.path list;
  (* map from include_stems to list of (original path, regexified path) *)
  include_map: ((string * Str.regexp) list) PathMap.t;
  (* library paths. no wildcards *)
  libs: Path.path list;
  (* config options *)
  options: options;
  (* root path *)
  root: Path.path;
}
val get: Path.path -> config
val fullpath: Path.path -> string

val init: Path.path -> string list -> unit

val version: string



(* true if a file path matches an include path in config *)
val is_included: config -> string -> bool
