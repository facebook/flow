(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type position = {
  line: int;
  column: int;
}
[@@deriving show]

type t = {
  source: File_key.t option;
  start: position;
  _end: position;
}
[@@deriving show]

val none : t

val btwn : t -> t -> t

val char_before : t -> t

val first_char : t -> t

val contains : t -> t -> bool
(** [contains loc1 loc2] returns true if [loc1] entirely overlaps [loc2] *)

val intersects : t -> t -> bool
(** [intersects loc1 loc2] returns true if [loc1] intersects [loc2] at all *)

val lines_intersect : t -> t -> bool
(** [lines_intersect loc1 loc2] returns true if [loc1] and [loc2] cover any part of
    the same line, even if they don't actually intersect.

    For example, if [loc1] ends and then [loc2] begins later on the same line,
    [intersects loc1 loc2] is false, but [lines_intersect loc1 loc2] is true. *)

val pos_cmp : position -> position -> int

val span_compare : t -> t -> int

val compare : t -> t -> int

val equal : t -> t -> bool

val debug_to_string : ?include_source:bool -> t -> string

(* Relatively compact; suitable for use as a unique string identifier *)
val to_string_no_source : t -> string

val source : t -> File_key.t option

val cursor : File_key.t option -> int -> int -> t
(** Produces a zero-width Loc.t, where start = end *)

(* Produces a location at the start of the input location *)
val start_loc : t -> t

(* Produces a location at the end of the input location *)
val end_loc : t -> t
