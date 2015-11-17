(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t

(* compatible with Pervasives.compare *)
val compare : t -> t -> int

(* compatible with Lexing.dummy_pos
   and is always smaller than any valid position *)
val dummy : t

val is_dummy : t -> bool

(* line 1, column 0, offset 0 *)
val beg_of_file : t

val of_line_column_offset : line:int -> column:int -> offset:int -> t

val of_lexing_pos : Lexing.position -> t

val offset : t -> int

val line : t -> int

val column : t -> int

val beg_of_line : t -> int

val line_beg : t -> int * int

val line_column : t -> int * int

val line_column_beg : t -> int * int * int

val line_column_offset : t -> int * int * int

val line_beg_offset : t -> int * int * int

val to_lexing_pos : string -> t -> Lexing.position
