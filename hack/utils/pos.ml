(**
 * Copyright (c) 2014, Facebook, Inc.
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
  pos_start: Lexing.position ;
  pos_end: Lexing.position ;
}

type t = Relative_path.t pos

type absolute = string pos

let none = {
  pos_file = Relative_path.default ;
  pos_start = dummy_pos ;
  pos_end = dummy_pos ;
}

let filename p = p.pos_file

let info_pos t =
  let line = t.pos_start.pos_lnum in
  let start = t.pos_start.pos_cnum - t.pos_start.pos_bol + 1 in
  let end_ = t.pos_end.pos_cnum - t.pos_start.pos_bol in
  line, start, end_

let info_raw t = t.pos_start.pos_cnum, t.pos_end.pos_cnum
let length t = t.pos_end.pos_cnum - t.pos_start.pos_cnum

let string t =
  let line, start, end_ = info_pos t in
  Printf.sprintf "File %S, line %d, characters %d-%d:"
    t.pos_file line start end_

let json pos =
    let line, start, end_ = info_pos pos in
    let fn = filename pos in
    Hh_json.JAssoc [
      "filename",   Hh_json.JString fn;
      "line",       Hh_json.JInt line;
      "char_start", Hh_json.JInt start;
      "char_end",   Hh_json.JInt end_;
    ]

let inside p line char_pos =
  let first_line = p.pos_start.pos_lnum in
  let last_line = p.pos_end.pos_lnum in
  if first_line = last_line then
    let _, start, end_ = info_pos p in
    first_line = line && start <= char_pos && char_pos <= end_
  else
    if line = first_line then char_pos > (p.pos_start.pos_cnum - p.pos_start.pos_bol)
    else if line = last_line then char_pos <= (p.pos_end.pos_cnum - p.pos_end.pos_bol)
    else line > first_line && line < last_line

let contains pos_container pos =
  filename pos_container = filename pos &&
    pos.pos_start.pos_cnum >= pos_container.pos_start.pos_cnum &&
    pos.pos_end.pos_cnum <= pos_container.pos_end.pos_cnum

let make file (lb:Lexing.lexbuf) =
  let pos_start = lexeme_start_p lb in
  let pos_end = lexeme_end_p lb in
  { pos_file = file;
    pos_start = pos_start;
    pos_end = pos_end;
  }

let make_from file =
  let pos = Lexing.dummy_pos in
  { pos_file = file;
    pos_start = pos;
    pos_end = pos;
  }

let btw x1 x2 =
  if x1.pos_file <> x2.pos_file
  then failwith "Position in separate files" ;
  if x1.pos_end > x2.pos_end
  then failwith "Invalid positions Pos.btw" ;
  { x1 with pos_end = x2.pos_end }

let set_line pos value =
  let pos_start = pos.pos_start in
  let pos_end = pos.pos_end in
  let pos_start = { pos_start with pos_lnum = value } in
  let pos_end = { pos_end with pos_lnum = value } in
  { pos with pos_start; pos_end }

let to_absolute p = { p with pos_file = Relative_path.to_absolute (p.pos_file) }

(* Compare by filename, then tie-break by start position, and finally by the
 * end position *)
let compare x y =
  let rec seq = function
    | [] -> 0
    | f :: rl ->
        let result = f x y in
        if result <> 0 then result else seq rl
  in
  seq [(fun x y -> compare x.pos_file y.pos_file);
       (fun x y -> compare x.pos_start.pos_lnum y.pos_start.pos_lnum);
       (fun x y -> compare x.pos_start.pos_cnum y.pos_start.pos_cnum);
       (fun x y -> compare x.pos_end.pos_cnum y.pos_end.pos_cnum)]

module Map = MyMap (struct
  type path = t
  (* The definition below needs to refer to the t in the outer scope, but MyMap
   * expects a module with a type of name t, so we define t in a second step *)
  type t = path
  let compare = compare
end)
