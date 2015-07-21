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
  munge_underscores: bool;
  moduleSystem: moduleSystem;
  module_name_mappers: (Str.regexp * string) list;
  suppress_comments: Str.regexp list;
  suppress_types: Utils.SSet.t;
  traces: int;
  strip_root: bool;
  log_file: Path.t;
}

val default_module_system: moduleSystem
val default_options: Path.t -> options

module PathMap : Utils.MapSig with type key = Path.t

type config = {
  (* file blacklist *)
  excludes: (string * Str.regexp) list;
  (* user-specified non-root include paths. may contain wildcards *)
  includes: Path.t list;
  (* stems extracted from includes *)
  include_stems: Path.t list;
  (* map from include_stems to list of (original path, regexified path) *)
  include_map: ((string * Str.regexp) list) PathMap.t;
  (* library paths. no wildcards *)
  libs: Path.t list;
  (* config options *)
  options: options;
  (* root path *)
  root: Path.t;
}
val get: Path.t -> config
val get_unsafe: unit -> config
val fullpath: Path.t -> string

val init: Path.t -> string list -> unit

val version: string

(* true if a file path matches an include path in config *)
val is_included: config -> string -> bool

(* true if a file path matches an exclude (ignore) entry in config *)
val is_excluded: config -> string -> bool
