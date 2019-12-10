(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type position = {
  line: int;
  (* 1-based *)
  column: int; (* 1-based *)
}

type range = {
  st: position;
  ed: position;
}

type text_edit = {
  range: range option;
  text: string;
}

val edit_file : string -> text_edit list -> (string, string * Utils.callstack) result

val edit_file_unsafe : string -> text_edit list -> string

(* NOTE: If you need two offsets, use `get_offsets` below instead. *)
val get_offset : string -> position -> int

(* May raise Invalid_argument "out of bounds" if out of bounds *)
val get_offsets : string -> position * position -> int * int

val offset_to_position : string -> int -> position

val get_char : string -> int -> char
