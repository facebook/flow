(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type position = { line : int; column : int; offset : int; }
type filename =
    LibFile of string
  | SourceFile of string
  | JsonFile of string
  | ResourceFile of string
  | Builtins
type t = { source : filename option; start : position; _end : position; }
val none : t
val from_lb : filename option -> Lexing.lexbuf -> t
val from_curr_lb : filename option -> Lexing.lexbuf -> t
val btwn : t -> t -> t
val btwn_exclusive : t -> t -> t
val char_before : t -> t
val contains : t -> t -> bool
val filename_map : (string -> string) -> filename -> filename
val string_of_filename : filename -> string
val compare : t -> t -> int
val to_string : ?include_source:bool -> t -> string
val source : t -> filename option
val source_is_lib_file : filename -> bool
val check_suffix : filename -> string -> bool
val chop_suffix : filename -> string -> filename
val with_suffix : filename -> string -> filename
module FilenameKey :
  sig
    type t = filename
    val to_string : filename -> string
    val compare : 'a -> 'a -> int
  end
