(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

(* Note: While Pos.string prints out positions as closed intervals, pos_start
 * and pos_end actually form a half-open interval (i.e. pos_end points to the
 * character *after* the last character of the relevant lexeme.) *)
type 'a pos

type t = Relative_path.t pos

type absolute = string pos

val none : t

val filename : 'a pos -> 'a

val start_cnum : 'a pos -> int

val line : 'a pos -> int

val end_line : 'a pos -> int

val info_pos : 'a pos -> int * int * int

val info_raw : 'a pos -> int * int

val length : 'a pos -> int

val string : absolute -> string

val json : absolute -> Hh_json.json

val inside : 'a pos -> int -> int -> bool

val contains : 'a pos -> 'a pos -> bool

val make : 'a -> Lexing.lexbuf -> 'a pos

val make_from : 'a -> 'a pos

val btw : 'a pos -> 'a pos -> 'a pos

val to_absolute : t -> absolute

val to_relative_string : t -> string pos

(* Compare by filename, then tie-break by start position, and finally by the
 * end position *)
val compare : 'a pos -> 'a pos -> int

(* XXX deprecated: do not use! Talk to @jezng if you are not hack_sgrep and
 * you feel a need to use this. *)
val pos_start : 'a pos -> File_pos.t
val pos_end : 'a pos -> File_pos.t

val destruct_range : 'a pos -> (int * int * int * int)

(* XXX deprecated: should only be used by Flow *)
val make_from_file_pos :
  pos_file:Relative_path.t -> pos_start:File_pos.t ->
    pos_end:File_pos.t -> t

module Map : MapSig with type key = t
