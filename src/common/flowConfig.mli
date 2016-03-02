(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Opts : sig
  type esproposal_feature_mode =
    | ESPROPOSAL_ENABLE
    | ESPROPOSAL_IGNORE
    | ESPROPOSAL_WARN
  type moduleSystem = Node | Haste
  type t = {
    enable_const_params: bool;
    enable_unsafe_getters_and_setters: bool;
    esproposal_class_instance_fields: esproposal_feature_mode;
    esproposal_class_static_fields: esproposal_feature_mode;
    esproposal_decorators: esproposal_feature_mode;
    esproposal_export_star_as: esproposal_feature_mode;
    facebook_ignore_fbt: bool;
    ignore_non_literal_requires: bool;
    moduleSystem: moduleSystem;
    module_name_mappers: (Str.regexp * string) list;
    node_resolver_dirnames: string list;
    munge_underscores: bool;
    module_file_exts: SSet.t;
    suppress_comments: Str.regexp list;
    suppress_types: SSet.t;
    traces: int;
    strip_root: bool;
    log_file: Path.t option;
    max_workers: int;
    temp_dir: Path.t;
  }
end

type config = {
  (* file blacklist *)
  ignores: string list;
  (* non-root include paths *)
  includes: string list;
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
val recheck_file: tmp_dir:string -> Path.t -> string
val lock_file: tmp_dir:string -> Path.t -> string
val pids_file: tmp_dir:string -> Path.t -> string
val socket_file: tmp_dir:string -> Path.t -> string
val log_file: tmp_dir:string -> Path.t -> Opts.t -> Path.t

val init: Path.t -> string list -> string list -> string list -> unit

val version: string
