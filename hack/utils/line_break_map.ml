(*
 * Copyright (c) 2017, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

type t = int array [@@deriving show]

let last_offset = ref 0

let curr_index = ref 0

let reset_global_state () =
  last_offset := 0;
  curr_index := 0

let make text =
  reset_global_state ();

  (* Clever Tricks Warning
   * ---------------------
   * We prepend 0, so as to make the invariant hold that there is always a
   * perceived line break at the start of the file. We use this in translating
   * offsets to line numbers.
   *
   * Similarly, whether there's a line break at the end or not, the line break
   * map will always end with the length of the original string. This solves
   * off-by-one issues.
   *)
  let len = String.length text in
  let newline_list =
    let result = ref [] in
    for i = 1 to len do
      let prev = text.[i - 1] in
      if (prev = '\r' && text.[i] != '\n') || prev = '\n' then
        result := i :: !result
    done;
    (match !result with
    | r :: _ as rs when r <> len -> result := len :: rs
    | _ -> ());
    0 :: List.rev !result
  in
  Array.of_list newline_list

let offset_to_file_pos_triple bolmap offset =
  let len = Array.length bolmap in
  if !curr_index >= len then curr_index := len - 1;
  let rec forward_search i =
    let offset_at_i = Array.unsafe_get bolmap i in
    if offset < offset_at_i then
      i - 1
    else if i + 1 >= len then
      len - 1
    else
      forward_search (i + 1)
  in
  let rec backward_search i =
    let offset_at_i = Array.unsafe_get bolmap i in
    if offset >= offset_at_i then
      i
    else if i = 0 then
      0
    else
      backward_search (i - 1)
  in
  let index =
    if !last_offset < offset && !curr_index <> len - 1 then
      forward_search (!curr_index + 1)
    else if !last_offset > offset then
      backward_search !curr_index
    else
      !curr_index
  in
  let line_start = bolmap.(index) in
  curr_index := index;
  last_offset := offset;
  (index + 1, line_start, offset)

let offset_to_position bolmap offset =
  let (index, line_start, offset) = offset_to_file_pos_triple bolmap offset in
  (index, offset - line_start + 1)

let position_to_offset ?(existing = false) bolmap (line, column) =
  let len = Array.length bolmap in
  let file_line = line in
  (* Treat all file_line errors the same: Not_found *)
  let line_start = (try bolmap.(file_line - 1) with _ -> raise Not_found) in
  let offset = line_start + column - 1 in
  if
    (not existing)
    || (offset >= 0 && offset <= bolmap.(min (len - 1) file_line))
  then
    offset
  else
    raise Not_found

let offset_to_line_start_offset bolmap offset =
  offset - snd (offset_to_position bolmap offset) + 1
