(**
 * Copyright (c) 2017, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t = int array

let make text =
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
      let prev = text.[i-1] in
      if prev = '\r' && text.[i] != '\n' || prev = '\n'
      then result := i :: !result;
    done;
    (match !result with
    | (r :: _) as rs when r <> len -> result := len :: rs
    | _ -> ()
    );
    0 :: List.rev !result
  in
  Array.of_list newline_list

let offset_to_file_pos_triple ?(cyclic_index=false) bolmap offset =
  let len = Array.length bolmap in
  let offset =
    if cyclic_index (* Normalise and/or cycle around the length of the text. *)
    then
      max 0 (if offset < 0 then Array.get bolmap (len - 1) + offset else offset)
    else offset
  in
  let rec binary_search lower upper =
    if lower >= upper then lower - 1 else begin
      let i = (upper - lower) / 2 + lower in
      let offset_at_i = Array.get bolmap i in
      let l, u = if offset_at_i > offset then lower, i else (i + 1), upper in
      binary_search l u
    end
  in
  let index = binary_search 0 len in
  let line_start = Array.get bolmap index in
  index + 1, line_start, offset

let offset_to_position ?(cyclic_index=false) bolmap offset =
  let index, line_start, offset =
    offset_to_file_pos_triple ~cyclic_index bolmap offset
  in
  (index, offset - line_start + 1)

let position_to_offset ?(cyclic_index = false) ?(existing = false)
    bolmap (line, column) =
  let len = Array.length bolmap in
  let file_line =
    if cyclic_index && line < 1
    then max 1 (len + line - 1)
    else line
  in

  let line_start = Array.get bolmap (file_line - 1) in
  let offset = line_start + column - 1 in

  if not existing
  || offset >= 0 && offset <= Array.get bolmap (min (len-1) file_line)
  then offset
  else raise Not_found

let offset_to_line_start_offset ?(cyclic_index = false) bolmap offset =
  offset - snd (offset_to_position ~cyclic_index bolmap offset) + 1
