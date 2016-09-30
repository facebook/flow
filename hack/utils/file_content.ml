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

type t = {
  time : float;
  content : string;
}

type content_pos = {
  line : int;
  column : int;
}

type content_range = {
  st : content_pos;
  ed : content_pos;
}

type code_edit = {
  range : content_range option;
  text : string;
}

let of_content ~content = {
  time = Unix.gettimeofday ();
  content;
}

let get_content t = t.content

let get_line_lengths text limit =
  let text = text ^ "\n" in (* sentinel to make things easier *)
  Str.bounded_split_delim (Str.regexp_string "\n") text (limit+1) |>
    List.map ~f:String.length

let get_offset line column line_lengths =

  let line = line - 1 in
  let column = column -1 in

  assert (line < (List.length line_lengths));
  List.foldi line_lengths
    ~init:0
    ~f:begin fun i acc len ->
      if i < line then acc + len + 1
      else if i > line then acc
      else begin
        assert (column <= len);
        acc + column
      end
    end

let apply_edit = fun fc {range; text} ->
  match range with
  | None -> of_content text
  | Some {st; ed} ->
    let content = get_content fc in
    let line_lengths = get_line_lengths content (ed.line+1) in
    let start_offset = get_offset st.line st.column  line_lengths in
    let end_offset = get_offset ed.line ed.column line_lengths in

    let prefix = Str.string_before content start_offset in
    let suffix = Str.string_after content end_offset in
    of_content ~content:(prefix ^ text ^ suffix)

let print_edit b edit =
  let range = match edit.range with
    | None -> "None"
    | Some range -> Printf.sprintf "%d:%d - %d:%d"
        range.st.line range.st.column range.ed.line range.ed.column
  in
  Printf.bprintf b "range = %s\n text = \n%s\n" range edit.text

let edit_file fc (edits: code_edit list) =
  try
    Result.Ok (List.fold ~init:fc ~f:apply_edit edits)
  with e ->
    let b = Buffer.create 1024 in
    Printf.bprintf b "Invalid edit: %s\n" (Printexc.to_string e);
    Printf.bprintf b "Original content:\n%s\n" fc.content;
    Printf.bprintf b "Edits:\n";
    List.iter edits ~f:(print_edit b);
    Result.Error (Buffer.contents b)

let edit_file_unsafe fc edits =
  match edit_file fc edits with
  | Result.Ok r -> r
  | Result.Error e ->
      Printf.eprintf "%s" e;
      assert false
