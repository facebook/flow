(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Line Separator (0xE2 0x80 0xA8) or Paragraph Separator (0xE2 0x80 0xA9) *)
let is_ls_or_ps =
  let c1 = Char.chr 0xE2 in
  let c2 = Char.chr 0x80 in
  let c3ls = Char.chr 0xA8 in
  let c3ps = Char.chr 0xA9 in
  fun str len i ->
    str.[i] = c1 && i + 2 < len && str.[i + 1] = c2 && (str.[i + 2] = c3ls || str.[i + 2] = c3ps)

let length_of_line_terminator str len i =
  if str.[i] = '\n' then
    1
  else if str.[i] = '\r' then
    if i + 1 < len && str.[i + 1] = '\n' then
      2
    else
      1
  else if is_ls_or_ps str len i then
    3
  else
    0

(* Finds the index of the first character of the nth line (0-based).

   Assumes a UTF-8 encoding, and treats \n, \r, U+2028 (line separator) and
   U+2029 (paragraph separator) as line terminators, per the ECMAscript spec:
   https://tc39.es/ecma262/#sec-line-terminators

   If the line doesn't exist, including if the string ends with a line terminator
   for the (n-1)th line, then returns [None] (e.g. "foo\n" for n=1, i=0 returns `None`
   because the index is the end of the string. *)
let rec nth_line_opt n str len i =
  if i >= len then
    None
  else if n = 0 then
    Some i
  else
    let x = length_of_line_terminator str len i in
    if x > 0 then
      nth_line_opt (n - 1) str len (i + x)
    else
      nth_line_opt n str len (i + 1)

let split_nth s n =
  let len = String.length s in
  match nth_line_opt n s len 0 with
  | Some i ->
    let j =
      match nth_line_opt 1 s len i with
      | Some j -> j
      | None -> len
    in
    Some String.(sub s 0 i, sub s i (j - i), sub s j (len - j))
  | None -> None

let transform_nth s n f =
  match split_nth s n with
  | Some (pre, s, post) -> pre ^ f s ^ post
  | None -> s

let position_of_offset =
  let rec iter acc str max i =
    if i = max then
      acc
    else
      let (line, column) = acc in
      let eol = length_of_line_terminator str max i in
      let (i, line, column) =
        if eol > 0 then
          (i + eol, line + 1, 0)
        else
          (i + 1, line, column + 1)
      in
      iter (line, column) str max i
  in
  fun str offset ->
    let len = String.length str in
    if offset >= len then
      raise Not_found
    else
      iter (1, 0) str offset 0
