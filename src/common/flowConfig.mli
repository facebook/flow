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
  type esproposal_feature_mode =
    | ESPROPOSAL_ENABLE
    | ESPROPOSAL_IGNORE
    | ESPROPOSAL_WARN
  type moduleSystem = Node | Haste
  type t = {
    enable_unsafe_getters_and_setters: bool;
    esproposal_class_instance_fields: esproposal_feature_mode;
    esproposal_class_static_fields: esproposal_feature_mode;
    esproposal_decorators: esproposal_feature_mode;
    ignore_non_literal_requires: bool;
    moduleSystem: moduleSystem;
    module_name_mappers: (Str.regexp * string) list;
    node_resolver_dirnames: string list;
    munge_underscores: bool;
    module_file_exts: Utils.SSet.t;
    suppress_comments: Str.regexp list;
    suppress_types: Utils.SSet.t;
    traces: int;
    strip_root: bool;
    log_file: Path.t option;
    max_workers: int;
    temp_dir: Path.t;
    shm_dir: Path.t;
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
val default_shm_dir: string

val get: Path.t -> config
val get_unsafe: unit -> config
val fullpath: Path.t -> string

val init_file: tmp_dir:string -> Path.t -> string
val recheck_file: tmp_dir:string -> Path.t -> string
val lock_file: tmp_dir:string -> Path.t -> string
val pids_file: tmp_dir:string -> Path.t -> string
val socket_file: tmp_dir:string -> Path.t -> string
val log_file: tmp_dir:string -> Path.t -> Opts.t -> Path.t

val init: Path.t -> string list -> unit

val version: string
val flow_ext: string

(* true if a file path matches an include path in config *)
val is_included: config -> string -> bool

(* true if a file path matches an exclude (ignore) entry in config *)
val is_excluded: config -> string -> bool

(**/**)
val restore: config -> unit
