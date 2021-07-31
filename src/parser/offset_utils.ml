(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* table from 0-based line number and 0-based column number to the offset at that point *)
type t = int array array

type offset_kind =
  | Utf8
  | JavaScript

(* Classify each codepoint. We care about how many bytes each codepoint takes, in order to
   compute offsets in terms of bytes instead of codepoints. We also care about various kinds of
   newlines. To reduce memory, it is important that this is a basic variant with no parameters
   (so, don't make it `Chars of int`). *)
type kind =
  (* Char has a codepoint greater than or equal to 0x0 but less than 0x80 *)
  | Chars_0x0
  (* Char has a codepoint greater than or equal to 0x80 but less than 0x800 *)
  | Chars_0x80
  | Chars_0x800
  | Chars_0x10000
  | Malformed
  | Cr
  | Nl
  | Ls

(* Gives the size in bytes of the character's UTF-8 encoding *)
let utf8_size_of_kind = function
  | Chars_0x0 -> 1
  | Chars_0x80 -> 2
  | Chars_0x800 -> 3
  | Chars_0x10000 -> 4
  | Malformed -> 1
  | Cr -> 1
  | Nl -> 1
  | Ls -> 3

(* Gives the size in code units (16-bit blocks) of the character's UTF-16 encoding *)
let js_size_of_kind = function
  | Chars_0x0
  | Chars_0x80
  | Chars_0x800 ->
    1
  | Chars_0x10000 -> 2
  | Malformed -> 1
  | Cr -> 1
  | Nl -> 1
  | Ls -> 1

let make =
  (* Using Wtf8 allows us to properly track multi-byte characters, so that we increment the column
   * by 1 for a multi-byte character, but increment the offset by the number of bytes in the
   * character. It also keeps us from incrementing the line number if a multi-byte character happens
   * to include e.g. the codepoint for '\n' as a second-fourth byte. *)
  let fold_codepoints acc _offset chr =
    let kind =
      match chr with
      | Wtf8.Point code ->
        if code == 0x2028 || code == 0x2029 then
          Ls
        else if code == 0xA then
          Nl
        else if code == 0xD then
          Cr
        else if code >= 0x10000 then
          Chars_0x10000
        else if code >= 0x800 then
          Chars_0x800
        else if code >= 0x80 then
          Chars_0x80
        else
          Chars_0x0
      | Wtf8.Malformed -> Malformed
    in
    kind :: acc
  in
  (* Traverses a `kind list`, breaking it up into an `int array array`, where each `int array`
     contains the offsets at each character (aka codepoint) of a line. *)
  let rec build_table size_of_kind (offset, rev_line, acc) = function
    | [] -> Array.of_list (List.rev acc)
    | Cr :: Nl :: rest ->
      (* https://www.ecma-international.org/ecma-262/5.1/#sec-7.3 says that "\r\n" should be treated
         like a single line terminator, even though both '\r' and '\n' are line terminators in their
         own right. *)
      let line = Array.of_list (List.rev (offset :: rev_line)) in
      build_table size_of_kind (offset + 2, [], line :: acc) rest
    | ((Cr | Nl | Ls) as kind) :: rest ->
      let line = Array.of_list (List.rev (offset :: rev_line)) in
      build_table size_of_kind (offset + size_of_kind kind, [], line :: acc) rest
    | ((Chars_0x0 | Chars_0x80 | Chars_0x800 | Chars_0x10000 | Malformed) as kind) :: rest ->
      build_table size_of_kind (offset + size_of_kind kind, offset :: rev_line, acc) rest
  in
  fun ~kind text ->
    let rev_kinds = Wtf8.fold_wtf_8 fold_codepoints [] text in
    (* Add a phantom line at the end of the file. Since end positions are reported exclusively, it
     * is possible for the lexer to output an end position with a line number one higher than the
     * last line, to indicate something such as "the entire last line." For this purpose, we can
     * return the offset that is one higher than the last legitimate offset, since it could only be
     * correctly used as an exclusive index. *)
    let rev_kinds = Nl :: rev_kinds in
    let size_of_kind =
      match kind with
      | Utf8 -> utf8_size_of_kind
      | JavaScript -> js_size_of_kind
    in
    build_table size_of_kind (0, [], []) (List.rev rev_kinds)

exception Offset_lookup_failed of Loc.position * string

let lookup arr i pos context_string =
  try arr.(i) with
  | Invalid_argument _ ->
    let msg =
      Printf.sprintf
        "Failure while looking up %s. Index: %d. Length: %d."
        context_string
        i
        (Array.length arr)
    in
    raise (Offset_lookup_failed (pos, msg))

let offset table pos =
  Loc.(
    (* Special-case `Loc.none` so we don't try to look up line -1. *)
    if pos.line = 0 && pos.column = 0 then
      (* Loc.none sets the offset as 0, so that's what we'll return here. *)
      0
    else
      (* lines are 1-indexed, columns are zero-indexed *)
      let line_table = lookup table (pos.line - 1) pos "line" in
      lookup line_table pos.column pos "column")

let debug_string table =
  let buf = Buffer.create 4096 in
  Array.iteri
    (fun line_num line ->
      Printf.bprintf buf "%6d: " line_num;
      Array.iter (fun offset -> Printf.bprintf buf "%8d " offset) line;
      Buffer.add_char buf '\n')
    table;
  Buffer.contents buf

let line_lengths table =
  Array.fold_left
    (fun (prev_line_end, lengths_rev) line ->
      let line_end = line.(Array.length line - 1) in
      (line_end, (line_end - prev_line_end) :: lengths_rev))
    (-1, [])
    table
  |> snd
  |> List.rev

let contains_multibyte_character table =
  let exception FoundMultibyte in
  try
    Array.iter
      (fun line ->
        Array.iteri
          (fun i offset ->
            if i > 0 then
              let offset_before = line.(i - 1) in
              if offset - offset_before > 1 then raise FoundMultibyte)
          line)
      table;
    false
  with
  | FoundMultibyte -> true
