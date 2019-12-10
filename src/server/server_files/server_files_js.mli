(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val default_flowconfig_name : string

val config_file : string -> Path.t -> string

val log_file : flowconfig_name:string -> tmp_dir:string -> Path.t -> string

val monitor_log_file : flowconfig_name:string -> tmp_dir:string -> Path.t -> string

val lock_file : flowconfig_name:string -> tmp_dir:string -> Path.t -> string

val pids_file : flowconfig_name:string -> tmp_dir:string -> Path.t -> string

val socket_file : flowconfig_name:string -> tmp_dir:string -> Path.t -> string

val legacy2_socket_file : flowconfig_name:string -> tmp_dir:string -> Path.t -> string

val legacy1_socket_file : flowconfig_name:string -> tmp_dir:string -> Path.t -> string

val recheck_stats_file : flowconfig_name:string -> tmp_dir:string -> Path.t -> string
