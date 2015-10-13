(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
module PathMap : Utils.MapSig with type key = Path.t
module Opts : sig
  type experimental_feature_mode =
    | EXPERIMENTAL_IGNORE
    | EXPERIMENTAL_WARN
  type moduleSystem = Node | Haste
  type t = {
    enable_unsafe_getters_and_setters: bool;
    experimental_decorators: experimental_feature_mode;
    moduleSystem: moduleSystem;
    module_name_mappers: (Str.regexp * string) list;
    node_resolver_dirnames: string list;
    munge_underscores: bool;
    module_file_exts: string list;
    suppress_comments: Str.regexp list;
    suppress_types: Utils.SSet.t;
    traces: int;
    strip_root: bool;
    log_file: Path.t option;
    max_workers: int;
    temp_dir: Path.t;
  }
end

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
  options: Opts.t;
  (* root path *)
  root: Path.t;
}

val default_temp_dir: string

val get: Path.t -> config
val get_unsafe: unit -> config
val fullpath: Path.t -> string

val init_file: tmp_dir:string -> Path.t -> string
val lock_file: tmp_dir:string -> Path.t -> string
val pids_file: tmp_dir:string -> Path.t -> string
val socket_file: tmp_dir:string -> Path.t -> string
val log_file: tmp_dir:string -> Path.t -> Opts.t -> Path.t

val init: Path.t -> string list -> unit

val version: string

(* true if a file path matches an include path in config *)
val is_included: config -> string -> bool

(* true if a file path matches an exclude (ignore) entry in config *)
val is_excluded: config -> string -> bool
