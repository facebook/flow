(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* table from 0-based line number and 0-based column number to the offset at that point *)
type t = int array array

(* https://www.ecma-international.org/ecma-262/5.1/#sec-7.3 *)
let is_newline codepoint =
  codepoint = Char.code '\n' ||
  codepoint = Char.code '\r' ||
  (* Unicode line separator *)
  codepoint = 0x2028 ||
  (* Unicode line separator *)
  codepoint = 0x2029

(* The spec (linked above) says that "\r\n" should be treated like a single line terminator, even
 * though both '\r' and '\n' are line terminators in their own right. So, if we just saw '\r', and
 * now we see '\n', we just skip. *)
let should_skip last_codepoint codepoint =
  last_codepoint = Some (Char.code '\r') &&
  codepoint = Char.code '\n'

let make text =
  let line = ref 0 in
  let column = ref 0 in
  let folder =
    (* Used to properly handle Windows line endings ("\r\n") -- see `should_skip` function. *)
    let last_codepoint = ref None in
    fun acc offset chr ->
      let open Wtf8 in
      (* Because we update acc before checking the current character, we will add an entry for a
       * phantom column at the end of each line. This is good, because end positions produced by the
       * parser are exclusive so they can refer to the column after the last actual column. *)
      let acc = (!line, !column, offset)::acc in
      begin match chr with
      | Point codepoint ->
        if should_skip !last_codepoint codepoint then begin
          ()
        end else if is_newline codepoint then begin
          line := (!line + 1);
          column := 0;
        end else begin
          column := !column + 1
        end;
        last_codepoint := Some codepoint
      | Malformed ->
        column := !column + 1;
        last_codepoint := None
      end;
      acc
  in
  (* Using Wtf8 allows us to properly track multi-byte characters, so that we increment the column
   * by 1 for a multi-byte character, but increment the offset by the number of bytes in the
   * character. It also keeps us from incrementing the line number if a multi-byte character happens
   * to include e.g. the codepoint for '\n' as a second-fourth byte. *)
  let tuples, num_lines =
    (* The tuples are built up in reverse order *)
    let rev = Wtf8.fold_wtf_8 folder [] text in
    (* As described in the `folder` function, we need to make sure we have an entry after the last
     * column. If the final line did not have a terminating newline character, we won't end up with
     * one, so let's add one here. If it did, one more entry won't hurt. *)
    let (last_actual_line, _, last_offset) as extra_column_entry = match rev with
    | [] -> (0, 0, 0)
    | (line, col, offset)::_ -> (line, col + 1, offset + 1)
    in
    let last_line = last_actual_line + 1 in
    let num_lines = last_line + 1 in
    (* For a similar reason that we add a phantom column at the end of each line, we also want to
     * add a phantom line at the end of the file. Since end positions are reported exclusively, it
     * is possible for the lexer to output an end position with a line number one higher than the
     * last line, to indicate something such as "the entire last line." For this purpose, we can
     * return the offset that is one higher than the last legitimate offset, since it could only be
     * correctly used as an exclusive index. *)
    let extra_line_entry = (last_line, 0, last_offset) in
    (List.rev (extra_line_entry::extra_column_entry::rev), num_lines)
  in
  (* Build up the table, a 2D array indexed by line, then column. The top level array needs to have
   * the same number of entries as the number of lines, and the second-level arrays need to have one
   * entry for each column in that line. Here we do the work to build the array. Next we populate
   * it. *)
  let table =
    let columns_per_line = Array.make num_lines 0 in
    let rec find_cols_per_line = function
    (* If the next entry is for a different line, this is the last column on the current line, so we
     * know the length. *)
    | (line1, col1, _)::(((line2, _, _)::_) as rst) when line1 <> line2 ->
      columns_per_line.(line1) <- col1 + 1;
      find_cols_per_line rst
    (* If this is the last entry, we know the length of the last line. *)
    | [(line, col, _)] -> columns_per_line.(line) <- col + 1
    (* Something else, so recurse *)
    | _::rst -> find_cols_per_line rst
    | [] -> ()
    in
    find_cols_per_line tuples;
    Array.init num_lines (fun i -> Array.make columns_per_line.(i) 0)
  in
  List.iter (fun (line, col, offset) -> table.(line).(col) <- offset) tuples;
  table

exception Offset_lookup_failed of Loc.position * string

let lookup arr i pos context_string =
  try
    arr.(i)
  with Invalid_argument _ ->
    let msg =
      Printf.sprintf "Failure while looking up %s. Index: %d. Length: %d."
        context_string
        i
        (Array.length arr)
    in
    raise (Offset_lookup_failed (pos, msg))

let offset table pos =
  let open Loc in
  (* Special-case `Loc.none` so we don't try to look up line -1. *)
  if pos.line = 0 && pos.column = 0 then
    (* Loc.none sets the offset as 0, so that's what we'll return here. *)
    0
  else
    (* lines are 1-indexed, columns are zero-indexed *)
    let line_table = lookup table (pos.line - 1) pos "line" in
    lookup line_table pos.column pos "column"
