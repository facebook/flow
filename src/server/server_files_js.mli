(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val config_file: Path.t -> string
val dfind_log_file: tmp_dir:string -> Path.t -> string
val gc_file: tmp_dir:string -> Path.t -> string
val init_file: tmp_dir:string -> Path.t -> string
val lock_file: tmp_dir:string -> Path.t -> string
val log_file: tmp_dir:string -> Path.t -> FlowConfig.Opts.t -> Path.t
val pids_file: tmp_dir:string -> Path.t -> string
val recheck_file: tmp_dir:string -> Path.t -> string
val socket_file: tmp_dir:string -> Path.t -> string
