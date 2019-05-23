(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)


type t = private string

val dummy_path: t
val make: string -> t
val make_unsafe: string -> t
val to_string: t -> string
val file_exists: t -> bool
val is_directory: t -> bool
val compare: t -> t -> int
val concat: t -> string -> t
val chdir: t -> unit
val dirname: t -> t
val basename: t -> string
val getcwd: unit -> t
val output: out_channel -> t -> unit
val remove: t -> unit
val parent: t -> t
val executable_name: t
val cat: t -> string

val slash_escaped_string_of_path: t -> string
val path_of_slash_escaped_string: string -> t
