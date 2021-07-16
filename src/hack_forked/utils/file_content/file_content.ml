(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Hh_core

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

(* UTF-8 encoding character lengths.
 *
 * NOTE: at the moment, edit commands are the only place where we count
 * UTF-8 encoded characters as opposed to ASCII bytes - in all of the other
 * places (column numbers in errors, positions in IDE commands) we still use the
 * latter.
 *
 * We make an exception here because that's the way Nuclide counts characters,
 * and the consequences of mishandling it are much more dire than in other
 * places - we'll not only fail the current single request, but diverge
 * the synchronized state forever.
 *)
let get_char_length c =
  let c = Char.code c in
  if c lsr 7 = 0b0 then
    1
  else if c lsr 5 = 0b110 then
    2
  else if c lsr 4 = 0b1110 then
    3
  else if c lsr 3 = 0b11110 then
    4
  else
    raise (Failure (Printf.sprintf "Invalid UTF-8 leading byte: %d" c))

let is_target t line column = t.line = line && t.column = column

let get_char content offset =
  (* sentinel newline to make things easier *)
  if offset = String.length content then
    '\n'
  else
    content.[offset]

let rec get_offsets content queries line column offset acc =
  match acc with
  | (Some _, Some _) -> acc
  | (None, r2) when is_target (fst queries) line column ->
    get_offsets content queries line column offset (Some offset, r2)
  | ((Some _ as r1), None) when is_target (snd queries) line column ->
    get_offsets content queries line column offset (r1, Some offset)
  | acc ->
    let (line, column, offset) =
      match get_char content offset with
      | '\n' -> (line + 1, 1, offset + 1)
      | c -> (line, column + 1, offset + get_char_length c)
    in
    get_offsets content queries line column offset acc

let invalid_position p =
  raise (Failure (Printf.sprintf "Invalid position: {line: %d; column: %d}" p.line p.column))

(* this returns 0-based offsets *)
let get_offsets (content : string) (queries : position * position) : int * int =
  match get_offsets content queries 1 1 0 (None, None) with
  | (Some r1, Some r2) -> (r1, r2)
  | (None, _) -> invalid_position (fst queries)
  | (_, None) -> invalid_position (snd queries)

(* This returns a 0-based offset. If you need to get two offsets, use
   `get_offsets` instead. *)
let get_offset (content : string) (position : position) : int =
  fst (get_offsets content (position, position))

(* This takes 0-based offsets and returns 1-based positions.                  *)
(* It gives the position of the character *immediately after* this offset,    *)
(* e.g. "offset_to_position s 0" gives the 1-based position {line=1,col=1}.   *)
(* It sounds confusing but is natural when you work with half-open ranges!    *)
(* It is okay to ask for the position of the offset of the end of the file.   *)
(* In case of multi-byte characters, if you give an offset inside a character,*)
(* it still gives the position immediately after.                             *)
let offset_to_position (content : string) (offset : int) : position =
  let rec helper ~(line : int) ~(column : int) ~(index : int) =
    if index >= offset then
      { line; column }
    else
      let c = get_char content index in
      let clen = get_char_length c in
      if c = '\n' then
        helper (line + 1) 1 (index + clen)
      else
        helper line (column + 1) (index + clen)
  in
  if offset > String.length content then
    raise (Failure (Printf.sprintf "Invalid offset: %d" offset))
  else
    helper ~line:1 ~column:1 ~index:0

let apply_edit content { range; text } =
  match range with
  | None -> text
  | Some { st; ed } ->
    let (start_offset, end_offset) = get_offsets content (st, ed) in
    let prefix = Str.string_before content start_offset in
    let suffix = Str.string_after content end_offset in
    prefix ^ text ^ suffix

let print_edit b edit =
  let range =
    match edit.range with
    | None -> "None"
    | Some range ->
      Printf.sprintf "%d:%d - %d:%d" range.st.line range.st.column range.ed.line range.ed.column
  in
  Printf.bprintf b "range = %s\n text = \n%s\n" range edit.text

let edit_file content (edits : text_edit list) : (string, string * Utils.callstack) result =
  try Ok (List.fold ~init:content ~f:apply_edit edits) with
  | e ->
    let stack = Printexc.get_backtrace () in
    let b = Buffer.create 1024 in
    Printf.bprintf b "Invalid edit: %s\n" (Printexc.to_string e);
    Printf.bprintf b "Original content:\n%s\n" content;
    Printf.bprintf b "Edits:\n";
    List.iter edits ~f:(print_edit b);
    Error (Buffer.contents b, Utils.Callstack stack)

let edit_file_unsafe fc edits =
  match edit_file fc edits with
  | Ok r -> r
  | Error (e, _stack) ->
    Printf.eprintf "%s" e;
    failwith e
