(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Lexing
open Utils

(* Note: While Pos.string prints out positions as closed intervals, pos_start
 * and pos_end actually form a half-open interval (i.e. pos_end points to the
 * character *after* the last character of the relevant lexeme.) *)
type 'a pos = {
  pos_file: 'a ;
  pos_start: File_pos.t ;
  pos_end: File_pos.t ;
}

type t = Relative_path.t pos

type absolute = string pos

let none = {
  pos_file = Relative_path.default ;
  pos_start = File_pos.dummy ;
  pos_end = File_pos.dummy ;
}

let filename p = p.pos_file

let info_pos t =
  let line, start_minus1, bol = File_pos.line_column_beg t.pos_start in
  let start = start_minus1 + 1 in
  let end_offset = File_pos.offset t.pos_end in
  let end_ = end_offset - bol in
  line, start, end_

let info_raw t = File_pos.offset t.pos_start, File_pos.offset t.pos_end
let length t = File_pos.offset t.pos_end - File_pos.offset t.pos_start

let start_cnum t = File_pos.offset t.pos_start
let line t = File_pos.line t.pos_start
let end_line t = File_pos.line t.pos_end

let string t =
  let line, start, end_ = info_pos t in
  Printf.sprintf "File %S, line %d, characters %d-%d:"
    (String.trim t.pos_file) line start end_

let json pos =
  let line, start, end_ = info_pos pos in
  let fn = filename pos in
  Hh_json.JSON_Object [
    "filename",   Hh_json.JSON_String fn;
    "line",       Hh_json.int_ line;
    "char_start", Hh_json.int_ start;
    "char_end",   Hh_json.int_ end_;
  ]

let inside p line char_pos =
  let first_line, first_col = File_pos.line_column p.pos_start in
  let last_line, last_col = File_pos.line_column p.pos_end in
  if first_line = last_line then
    first_line = line && first_col + 1 <= char_pos && char_pos <= last_col
  else
    if line = first_line then char_pos > first_col
    else if line = last_line then char_pos <= last_col
    else line > first_line && line < last_line

let contains pos_container pos =
  filename pos_container = filename pos &&
    pos.pos_start >= pos_container.pos_start &&
    pos.pos_end <= pos_container.pos_end

let make file (lb:Lexing.lexbuf) =
  let pos_start = lexeme_start_p lb in
  let pos_end = lexeme_end_p lb in
  { pos_file = file;
    pos_start = File_pos.of_lexing_pos pos_start;
    pos_end = File_pos.of_lexing_pos pos_end;
  }

let make_from file =
  let pos = File_pos.dummy in
  { pos_file = file;
    pos_start = pos;
    pos_end = pos;
  }

let btw x1 x2 =
  if x1.pos_file <> x2.pos_file
  then failwith "Position in separate files" ;
  if File_pos.compare x1.pos_end x2.pos_end > 0
  then failwith "Invalid positions Pos.btw" ;
  { x1 with pos_end = x2.pos_end }

let to_absolute p = { p with pos_file = Relative_path.to_absolute (p.pos_file) }

let to_relative_string p =
  { p with pos_file = Relative_path.suffix (p.pos_file) }

(* Compare by filename, then tie-break by start position, and finally by the
 * end position
 *
 * Using Pervasives.compare on Pos.t is still correct but a least twice slower
 * on average
 *
 * This version is **about** 20% faster than the previous one that used a list
 * of closures
 *)
let compare x y =
  let r = Pervasives.compare x.pos_file y.pos_file in
  if r <> 0 then r
  else let r = File_pos.compare x.pos_start y.pos_start in
    if r <> 0 then r
    else File_pos.compare x.pos_end y.pos_end

let pos_start p = p.pos_start
let pos_end p = p.pos_end

let destruct_range (p : 'a pos) : (int * int * int * int) =
  let line_start, col_start_minus1 = File_pos.line_column p.pos_start in
  let line_end,   col_end_minus1   = File_pos.line_column p.pos_end in
  line_start, col_start_minus1 + 1,
  line_end,   col_end_minus1 + 1

let make_from_file_pos ~pos_file ~pos_start ~pos_end =
  { pos_file; pos_start; pos_end }

module Map = MyMap (struct
  type path = t
  (* The definition below needs to refer to the t in the outer scope, but MyMap
   * expects a module with a type of name t, so we define t in a second step *)
  type t = path
  let compare = compare
end)
