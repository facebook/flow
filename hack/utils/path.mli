(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


type path

val dummy_path: path
val mk_path: string -> path
val string_of_path: path -> string
val equal: path -> path -> bool
val file_exists: path -> bool
val is_directory: path -> bool
val concat: path -> string -> path
val remove: path -> unit
val parent: path -> path

val slash_escaped_string_of_path: path -> string
val path_of_slash_escaped_string: string -> path
