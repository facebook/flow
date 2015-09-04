(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type position = {
  line: int;
  column: int;
  offset: int;
}

type t = {
  source: string option;
  start: position;
  _end: position;
}

let none = {
  source = None;
  start = { line = 0; column = 0; offset = 0; };
  _end = { line = 0; column = 0; offset = 0; };
}

let from_lb_p start _end = Lexing.(
  {
    source = if start.pos_fname = ""
      then None
      else Some (start.pos_fname);
    start = {
      line = start.pos_lnum;
      column = start.pos_cnum - start.pos_bol;
      offset = start.pos_cnum;
    };
    _end = {
      line = _end.pos_lnum;
      column = max 0 (_end.pos_cnum - _end.pos_bol);
      offset = _end.pos_cnum;
    }
  }
)

(* Returns the position for the token that was just lexed *)
let from_lb lb = Lexing.(
  let start = lexeme_start_p lb in
  let _end = lexeme_end_p lb in
  from_lb_p start _end
)

(* Returns the position that the lexer is currently about to lex *)
let from_curr_lb lb = Lexing.(
  let curr = lb.lex_curr_p in
  from_lb_p curr curr
)

let btwn loc1 loc2 = {
  source = loc1.source;
  start = loc1.start;
  _end = loc2._end;
}

let string loc =
  let source = match loc.source with
  | None -> ""
  | Some source -> source in
  if loc.start.line = loc._end.line
  then Printf.sprintf "File %S, line %d, column %d-%d:"
    source loc.start.line loc.start.column loc._end.column
  else Printf.sprintf "File %S, line %d column %d - line %d column %d:"
    source loc.start.line loc.start.column loc._end.line loc._end.column

let compare loc1 loc2 =
  Pervasives.compare
    (loc1.source, loc1.start.line, loc1.start.column,
     loc1._end.line, loc1._end.column)
    (loc2.source, loc2.start.line, loc2.start.column,
     loc2._end.line, loc2._end.column)

let source loc = loc.source
