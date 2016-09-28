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

let nth_line lines n =
  match List.nth lines n with
  | Some s -> s
  | None -> ""

let apply_edit = fun fc {range; text} ->
  match range with
  | None -> of_content text
  | Some {st; ed} ->
    let content = get_content fc in
    let lines = if content = "" then [] else match content.[0] with
    | '\n' ->
      "" :: Str.bounded_split (Str.regexp_string "\n") content (ed.line)
    | _ ->
      Str.bounded_split (Str.regexp_string "\n") content (ed.line+1) in
    let hd = ref "" in
    let tl = ref "" in
    for i = 0 to List.length lines - 1 do
      let line = nth_line lines i in
      let length = String.length line in
      if i < st.line - 1
        then hd := !hd ^ line ^ "\n"
      else if i >= ed.line
        then tl := !tl ^ line
      else if i == st.line - 1
        then hd := !hd ^ (String.sub line 0 (st.column - 1));
      if i == ed.line - 1 && length - (ed.column - 1) > 0
        then tl := (String.sub line (ed.column - 1)
          (length - ed.column + 1)) ^ "\n" ^ !tl
      else if i == ed.line - 1
        then tl := "\n" ^ !tl
    done;
    of_content (!hd ^ text ^ !tl)

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
