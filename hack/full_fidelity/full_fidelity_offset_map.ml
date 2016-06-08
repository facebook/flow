(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This data structure takes a string and produces a map which can convert
   absolute offsets into line / column pairs, or vice-versa. *)

(* The map is currently implemented as a monotone decreasing list of
   (offset, line) pairs. *)

(* TODO: Come up with a better data structure than this linear search. *)

type t = (int * int) list

let make text =
  let len = String.length text in
  let rec aux acc offset line last =
    if offset >= len then
      acc
    else
      let ch = String.get text offset in
      if last = '\r' && ch != '\n' || last = '\n' then
        aux ((offset, line) :: acc) (offset + 1) (line + 1) ch
      else
        aux acc (offset + 1) line ch in
  aux [] 0 1 '\n'

(* Take a zero-based offset, produce a one-based (line, char) pair. *)
let rec offset_to_position offset_map offset =
  match offset_map with
  | [] -> (1, 1) (* This can only happen if the file is empty. *)
  | (o, l) :: t ->
    if o <= offset then (l, offset - o + 1)
    else offset_to_position t offset

(* Take a one-based (line, char) pair, produce a zero-based offset *)
let rec position_to_offset offset_map (line, character) =
  match offset_map with
  | [] -> 0 (* This can only happen if the file is empty. *)
  | (o, l) :: t ->
    if l < line then failwith "invalid line passed to pos_to_offset"
    else if l = line then o + character - 1
    else position_to_offset t (line, character)

let offset_to_line_start_offset offset_map offset =
  let (_, c) = offset_to_position offset_map offset in
  offset - c + 1
