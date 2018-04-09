(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val file_of_root: string -> tmp_dir:string -> Path.t -> string

val config_file: Path.t -> string
val dfind_log_file: tmp_dir:string -> Path.t -> string
val lock_file: tmp_dir:string -> Path.t -> string
val pids_file: tmp_dir:string -> Path.t -> string
val socket_file: tmp_dir:string -> Path.t -> string
val legacy_socket_file: tmp_dir:string -> Path.t -> string
