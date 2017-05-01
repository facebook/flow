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
   * By invoking `scan_breaks` on a newline, the first offset at which a line
   * break occurs is *always* 0. We use this in translating offsets to line
   * numbers.
   *
   * Similarly, whether there's a line break at the end or not, the line break
   * map will always end with the length of the original string. This solves
   * off-by-one issues.
   *)
  let len = String.length text in
  let newline_list =
    let rec scan_breaks i prev =
      if i >= len then [len] else begin
        let ch = String.get text i in
        if prev = '\r' && ch != '\n' || prev = '\n'
        then i :: scan_breaks (i + 1) ch
        else      scan_breaks (i + 1) ch
      end
    in
    scan_breaks 0 '\n'
  in
  Array.of_list newline_list

let offset_to_position ?(cyclic_index=false) lbmap offset =
  let len = Array.length lbmap in
  let offset =
    if cyclic_index (* Normalise and/or cycle around the length of the text. *)
    then
      max 0 (if offset < 0 then Array.get lbmap (len - 1) + offset else offset)
    else offset
  in
  let rec binary_search lower upper =
    if lower >= upper then lower - 1 else begin
      let i = (upper - lower) / 2 + lower in
      let offset_at_i = Array.get lbmap i in
      let l, u = if offset_at_i > offset then lower, i else (i + 1), upper in
      binary_search l u
    end
  in
  let index = binary_search 0 len in
  let line_start = Array.get lbmap index in
  (index + 1, offset - line_start + 1)

let position_to_offset ?(cyclic_index = false) ?(existing = false)
    lbmap (line, column) =
  let len = Array.length lbmap in
  let file_line =
    if cyclic_index && line < 1
    then max 1 (len + line - 1)
    else line
  in

  let line_start = Array.get lbmap (file_line - 1) in
  let offset = line_start + column - 1 in

  if not existing
  || offset >= 0 && offset <= Array.get lbmap (min (len-1) file_line)
  then offset
  else raise Not_found

let offset_to_line_start_offset ?(cyclic_index = false) lbmap offset =
  offset - snd (offset_to_position ~cyclic_index lbmap offset) + 1
