(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type position = { line : int; column : int; offset : int; } [@@deriving show]
type t = { source : File_key.t option; start : position; _end : position; } [@@deriving show]
val none : t
val btwn : t -> t -> t
val btwn_exclusive : t -> t -> t
val char_before : t -> t
val first_char: t -> t
val contains : t -> t -> bool
val lines_intersect : t -> t -> bool
val pos_cmp : position -> position -> int
val span_compare : t -> t -> int
val compare : t -> t -> int
val equal : t -> t -> bool
val to_string : ?include_source:bool -> t -> string
val source : t -> File_key.t option
(* filename, line, column. produces a Loc.t at the given location, with stubbed out offsets *)
val make: File_key.t -> int -> int -> t
