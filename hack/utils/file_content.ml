(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
open Core
open Ide_api_types

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
  if c lsr 7 = 0b0 then 1
  else if c lsr 5 = 0b110 then 2
  else if c lsr 4 = 0b1110 then 3
  else if c lsr 3 = 0b11110 then 4
  else raise (Failure (Printf.sprintf "Invalid UTF-8 leading byte: %d" c))

let is_target t line column = t.line = line && t.column = column

let get_char content offset =
  (* sentinel newline to make things easier *)
  if offset = String.length content then '\n'
  else content.[offset]

let rec get_offsets content queries line column offset acc =
  match acc with
  | (Some _, Some _) -> acc
  | (None, r2) when is_target (fst queries) line column ->
    get_offsets content queries line column offset (Some offset, r2)
  | (Some _ as r1, None) when is_target (snd queries) line column ->
    get_offsets content queries line column offset (r1, Some offset)
  | acc ->
    let line, column, offset = match get_char content offset with
      | '\n' -> line + 1, 1, offset + 1
      | c -> line, column + 1, offset + (get_char_length c)
    in
    get_offsets content queries line column offset acc

let invalid_position p =
  raise (Failure (Printf.sprintf
    "Invalid position: {line: %d; column: %d}" p.line p.column))

let get_offsets content queries =
  match get_offsets content queries 1 1 0 (None, None) with
  | Some r1, Some r2 -> r1, r2
  | None, _ -> invalid_position (fst queries)
  | _, None -> invalid_position (snd queries)

let apply_edit = fun content {range; text} ->
  match range with
  | None -> text
  | Some {st; ed} ->
    let start_offset, end_offset = get_offsets content (st, ed) in
    let prefix = Str.string_before content start_offset in
    let suffix = Str.string_after content end_offset in
    prefix ^ text ^ suffix

let print_edit b edit =
  let range = match edit.range with
    | None -> "None"
    | Some range -> Printf.sprintf "%d:%d - %d:%d"
        range.st.line range.st.column range.ed.line range.ed.column
  in
  Printf.bprintf b "range = %s\n text = \n%s\n" range edit.text

let edit_file content (edits: text_edit list) =
  try
    Result.Ok (List.fold ~init:content ~f:apply_edit edits)
  with e ->
    let b = Buffer.create 1024 in
    Printf.bprintf b "Invalid edit: %s\n" (Printexc.to_string e);
    Printf.bprintf b "Original content:\n%s\n" content;
    Printf.bprintf b "Edits:\n";
    List.iter edits ~f:(print_edit b);
    Result.Error (Buffer.contents b)

let edit_file_unsafe fc edits =
  match edit_file fc edits with
  | Result.Ok r -> r
  | Result.Error e ->
      Printf.eprintf "%s" e;
      assert false
