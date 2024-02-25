(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception NotADirectory of string

exception No_such_file_or_directory of string

exception Rename_target_already_exists of string

exception Rename_target_dir_not_empty of string

val cat : string -> string

val write_file : file:string -> contents:string -> unit

val file_exists : string -> bool

val mkdir_p : string -> unit

(* Delete the given path - if it is a directory, delete recursively. *)
val rm_dir_tree : string -> unit

val is_directory : string -> bool

val getcwd : unit -> string

val chdir : string -> unit

val mkdir : string -> int -> unit

(* Return the names of all files present in the given directory. *)
val readdir : string -> string array

(* Rename from old path to new path. *)
val rename : string -> string -> unit
