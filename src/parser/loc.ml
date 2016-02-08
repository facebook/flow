(**
 * Copyright (c) 2013-present, Facebook, Inc.
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

type filename =
  | LibFile of string
  | SourceFile of string
  | JsonFile of string
  | Builtins

type t = {
  source: filename option;
  start: position;
  _end: position;
}

let none = {
  source = None;
  start = { line = 0; column = 0; offset = 0; };
  _end = { line = 0; column = 0; offset = 0; };
}

let from_lb_p source start _end = Lexing.(
  {
    source;
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
let from_lb source lb = Lexing.(
  let start = lexeme_start_p lb in
  let _end = lexeme_end_p lb in
  from_lb_p source start _end
)

(* Returns the position that the lexer is currently about to lex *)
let from_curr_lb source lb = Lexing.(
  let curr = lb.lex_curr_p in
  from_lb_p source curr curr
)

let btwn loc1 loc2 = {
  source = loc1.source;
  start = loc1.start;
  _end = loc2._end;
}

(* Returns true if loc1 entirely overlaps loc2 *)
let contains loc1 loc2 =
  loc1.source = loc2.source &&
  loc1.start.offset <= loc2.start.offset &&
  loc1._end.offset >= loc2._end.offset

let string_of_filename = function
  | LibFile x | SourceFile x | JsonFile x -> x
  | Builtins -> "(global)"

let compare loc1 loc2 =
  Pervasives.compare
    (loc1.source, loc1.start.line, loc1.start.column,
     loc1._end.line, loc1._end.column)
    (loc2.source, loc2.start.line, loc2.start.column,
     loc2._end.line, loc2._end.column)

let source loc = loc.source

let source_is_lib_file = function
| LibFile _ -> true
| Builtins -> true
| SourceFile _ -> false
| JsonFile _ -> false

let filename_map f = function
  | LibFile filename -> LibFile (f filename)
  | SourceFile filename -> SourceFile (f filename)
  | JsonFile filename -> JsonFile (f filename)
  | Builtins -> Builtins

let filename_exists f = function
  | LibFile filename
  | SourceFile filename
  | JsonFile filename -> f filename
  | Builtins -> false

let check_suffix filename suffix =
  filename_exists (fun fn -> Filename.check_suffix fn suffix) filename

let chop_suffix filename suffix =
  filename_map (fun fn -> Filename.chop_suffix fn suffix) filename

let with_suffix filename suffix =
  filename_map (fun fn -> fn ^ suffix) filename

(* implements OrderedType and SharedMem.UserKeyType *)
module FilenameKey = struct
  type t = filename
  let to_string = string_of_filename
  let compare = Pervasives.compare
end
