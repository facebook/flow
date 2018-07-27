(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val default_flowconfig_name: string

val file_of_root: string -> flowconfig_name:string -> tmp_dir:string -> Path.t -> string

val config_file: string -> Path.t -> string
val dfind_log_file: flowconfig_name:string -> tmp_dir:string -> Path.t -> string
val lock_file: flowconfig_name:string -> tmp_dir:string -> Path.t -> string
val pids_file: flowconfig_name:string -> tmp_dir:string -> Path.t -> string
val socket_file: flowconfig_name:string -> tmp_dir:string -> Path.t -> string
val legacy2_socket_file: flowconfig_name:string -> tmp_dir:string -> Path.t -> string
val legacy1_socket_file: flowconfig_name:string -> tmp_dir:string -> Path.t -> string
