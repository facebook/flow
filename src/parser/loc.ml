(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type position = {
  line: int;
  column: int;
  offset: int;
} [@@deriving show]

type t = {
  source: File_key.t option;
  start: position;
  _end: position;
} [@@deriving show]

let none = {
  source = None;
  start = { line = 0; column = 0; offset = 0; };
  _end = { line = 0; column = 0; offset = 0; };
}

let btwn loc1 loc2 = {
  source = loc1.source;
  start = loc1.start;
  _end = loc2._end;
}

let btwn_exclusive loc1 loc2 = {
  source = loc1.source;
  start = loc1._end;
  _end = loc2.start;
}

(* Returns the position immediately before the start of the given loc. If the
   given loc is at the beginning of a line, return the position of the first
   char on the same line. *)
let char_before loc =
  let start =
    let { line; column; offset } = loc.start in
    let column, offset = if column > 0
    then column - 1, offset - 1
    else column, offset in
    { line; column; offset }
  in
  let _end = loc.start in
  { loc with start; _end }

(* Returns the location of the first character in the given loc. Not accurate if the
 * first line is a newline character, but is still consistent with loc orderings. *)
let first_char loc =
  let start = loc.start in
  let _end = {start with column = start.column + 1; offset = start.offset + 1} in
  {loc with _end}

let pos_cmp a b =
  let k = a.line - b.line in if k = 0 then a.column - b.column else k

(**
 * If `a` spans (completely contains) `b`, then returns 0.
 * If `b` starts before `a` (even if it ends inside), returns < 0.
 * If `b` ends after `a` (even if it starts inside), returns > 0.
 *)
let span_compare a b =
  let k = File_key.compare_opt a.source b.source in
  if k = 0 then
    let k = pos_cmp a.start b.start in
    if k <= 0 then
      let k = pos_cmp a._end b._end in
      if k >= 0 then 0 else -1
    else 1
  else k

(* Returns true if loc1 entirely overlaps loc2 *)
let contains loc1 loc2 = span_compare loc1 loc2 = 0

(* Returns true if loc1 intersects loc2 at all *)
let lines_intersect loc1 loc2 =
  File_key.compare_opt loc1.source loc2.source = 0 && not (
    (loc1._end.line < loc2.start.line) ||
    (loc1.start.line > loc2._end.line)
  )

let compare loc1 loc2 =
  let k = File_key.compare_opt loc1.source loc2.source in
  if k = 0 then
    let k = pos_cmp loc1.start loc2.start in
    if k = 0 then pos_cmp loc1._end loc2._end
    else k
  else k

let equal loc1 loc2 = compare loc1 loc2 = 0

(**
 * This is mostly useful for debugging purposes.
 * Please don't dead-code delete this!
 *)
let to_string ?(include_source=false) loc =
  let source =
    if include_source
    then Printf.sprintf "%S: " (
      match loc.source with
      | Some src -> File_key.to_string src
      | None -> "<NONE>"
    ) else ""
  in
  let pos = Printf.sprintf "(%d, %d) to (%d, %d)"
    loc.start.line
    loc.start.column
    loc._end.line
    loc._end.column
  in
  source ^ pos

let source loc = loc.source

let make file line col =
  {
    source = Some file;
    start = { line; column = col; offset = 0; };
    _end = { line; column = col + 1; offset = 0; };
  }
