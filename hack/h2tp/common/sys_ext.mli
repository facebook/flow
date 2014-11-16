(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

exception NotADirectory of string

include (module type of Sys)

(* Copies a given directory to the destination matching permissions.
   If the destination already exists, it matches the permissions.
   If the destination is not a directory, it deletes it and replaces it with a directory
   This is not recursive *)
val copy_dir : string -> string -> unit

(* Given a source and a destination directory,
    recursively searches through the source directory and provides pairs of
  the source directory and the corresponding destination directory *)
val recursive_file_pairs : string -> string -> (string * string) list

(* Checks if a file has a given extension *)
val has_extension : string -> string -> bool

(* Sets the extension for a file path *)
val set_extension : string -> string -> string

val copy_file : string -> string -> unit

(* Creates a directory, and all parent directories that must be created *)
val mkdir_p : string -> unit

val write_file : string (* content *) -> string (* filename *) -> unit

(* ends in an error state after displaying the error names and
  messages on stderr *)
val die : (string * string) list -> unit

(* removes the trailing / in a path *)
val chop_dirsymbol : string -> string

(* if this is a regular file *)
val is_file : string -> bool

(* if this is a symlink *)
val is_symlink : string -> bool
