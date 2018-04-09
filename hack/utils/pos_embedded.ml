(** Positions embedded with full position info inside its
 * data structure. See "type 'a pos"
 *
 * Note: This module can only be used with the Lexbuf Pos_source
 * module, as it will not compile with other Pos_source modules. So,
 * when choosing a different Pos module, you must also choose its
 * compatible Pos_source module. *)

open Lexing

type b = Pos_source.t

(* Note: While Pos.string and Pos.info_pos return positions as closed intervals,
 * pos_start and pos_end actually form a half-open interval (i.e. pos_end points
 * to the character *after* the last character of the relevant lexeme.) *)
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

let pp fmt pos =
  if pos = none then
    Format.pp_print_string fmt "[Pos.none]"
  else begin
    Format.pp_print_string fmt "[";
    File_pos.pp fmt pos.pos_start;
    Format.pp_print_string fmt "-";
    File_pos.pp fmt pos.pos_end;
    Format.pp_print_string fmt "]";
  end

let filename p = p.pos_file

(* This returns a closed interval that's incorrect for multi-line spans. *)
let info_pos t =
  let line, start_minus1, bol = File_pos.line_column_beg t.pos_start in
  let start = start_minus1 + 1 in
  let end_offset = File_pos.offset t.pos_end in
  let end_ = end_offset - bol in
  line, start, end_

(* This returns a closed interval. *)
let info_pos_extended t =
  let line_begin, start, end_ = info_pos t in
  let line_end, _, _ = File_pos.line_column_beg t.pos_end in
  line_begin, line_end, start, end_

let info_raw t = File_pos.offset t.pos_start, File_pos.offset t.pos_end
let length t = File_pos.offset t.pos_end - File_pos.offset t.pos_start

let start_cnum t = File_pos.offset t.pos_start
let end_cnum t = File_pos.offset t.pos_end
let line t = File_pos.line t.pos_start
let end_line t = File_pos.line t.pos_end

(* This returns a closed interval. *)
let string t =
  let line, start, end_ = info_pos t in
  Printf.sprintf "File %S, line %d, characters %d-%d:"
    (String.trim t.pos_file) line start end_

(* Some positions, like those in buffers sent by IDE/created by unit tests might
 * not have a file specified.
 * This returns a closed interval. *)
let string_no_file t =
  let line, start, end_ = info_pos t in
  Printf.sprintf "line %d, characters %d-%d" line start end_

(* This returns a closed interval. *)
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

let exactly_matches_range p ~start_line ~start_col ~end_line ~end_col =
  let p_start_line, p_start_col = File_pos.line_column p.pos_start in
  let p_end_line, p_end_col = File_pos.line_column p.pos_end in
  p_start_line = start_line &&
  p_start_col = start_col - 1 &&
  p_end_line = end_line &&
  p_end_col = end_col - 1

let contains pos_container pos =
  filename pos_container = filename pos &&
    pos.pos_start >= pos_container.pos_start &&
    pos.pos_end <= pos_container.pos_end

let make file (lb: b) =
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

(* This returns a half-open interval. *)
let destruct_range (p : 'a pos) : (int * int * int * int) =
  let line_start, col_start_minus1 = File_pos.line_column p.pos_start in
  let line_end,   col_end_minus1   = File_pos.line_column p.pos_end in
  line_start, col_start_minus1 + 1,
  line_end,   col_end_minus1 + 1

(* This returns a half-open interval. *)
let multiline_string t =
  let line_start, char_start, line_end, char_end = destruct_range t in
  Printf.sprintf "File %S, line %d, character %d - line %d, character %d:"
    (String.trim t.pos_file) line_start char_start line_end (char_end - 1)

(* This returns a half-open interval. *)
let multiline_string_no_file t =
  let line_start, char_start, line_end, char_end = destruct_range t in
  Printf.sprintf "line %d, character %d - line %d, character %d"
    line_start char_start line_end (char_end - 1)

(* This returns a half-open interval. *)
let multiline_json t =
  let line_start, char_start, line_end, char_end = destruct_range t in
  let fn = filename t in
  Hh_json.JSON_Object [
    "filename",   Hh_json.JSON_String fn;
    "line_start", Hh_json.int_ line_start;
    "char_start", Hh_json.int_ char_start;
    "line_end",   Hh_json.int_ line_end;
    "char_end",   Hh_json.int_ (char_end - 1);
  ]

let make_from_file_pos ~pos_file ~pos_start ~pos_end =
  { pos_file; pos_start; pos_end }

let set_file pos_file pos =
  { pos with pos_file }

let print_verbose_absolute p =
  let a, b, c = File_pos.line_beg_offset p.pos_start in
  let d, e, f = File_pos.line_beg_offset p.pos_end in
  Printf.sprintf "Pos('%s', <%d,%d,%d>, <%d,%d,%d>)" p.pos_file a b c d e f

let print_verbose_relative p = print_verbose_absolute (to_absolute p)

module Map = MyMap.Make (struct
  type path = t
  (* The definition below needs to refer to the t in the outer scope, but MyMap
   * expects a module with a type of name t, so we define t in a second step *)
  type t = path
  let compare = compare
end)

module AbsolutePosMap = MyMap.Make (struct
  type t = absolute
  let compare = compare
end)
