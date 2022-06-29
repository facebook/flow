(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Autocomplete relies on a "magic symbol" to make the file parse properly
  even though you haven't typed a token yet. For example, [foo.| + 1] (where
  [|] signifies your cursor) would be a parse error.

  We insert [AUTO332] (arbitrary, unlikely you'd type that yourself) at the
  cursor location: [foo.AUTO332 + 1] parses as you'd expect.

  A sigil is a symbol with magical powers :)
 *)
let sigil = "AUTO332"

let sigil_len = String.length sigil

let add contents line column =
  let line = line - 1 in
  let contents_with_sigil =
    Line.transform_nth contents line (fun line_str ->
        let length = String.length line_str in
        if length >= column then
          let start = String.sub line_str 0 column in
          let end_ = String.sub line_str column (length - column) in
          start ^ sigil ^ end_
        else
          line_str
    )
  in
  let f (_, x, _) = x in
  let default = "" in
  ( contents_with_sigil,
    Base.Option.value_map ~f ~default (Line.split_nth contents_with_sigil (line - 1))
    ^ Base.Option.value_map ~f ~default (Line.split_nth contents_with_sigil line)
    ^ Base.Option.value_map ~f ~default (Line.split_nth contents_with_sigil (line + 1))
  )

(**
 * the autocomplete sigil inserts `sigil_len` characters, which are included
 * in `ac_loc` returned by `Autocomplete_js`. They need to be removed before
 * showing `ac_loc` to the client.
 * Sometimes `ac_loc` ends on a different line than it starts.
 * This happens mostly in two situations:
 * with unclosed string literals: {|
 *   foo["
 *        ^
 * |}
 * and with unclosed bracket-syntax members:
 *   foo[
 *       ^
 * In these situations, the code is malformed so the parser usually doesn't
 * realize to close the AST node until a future line, so the `ac_loc` we get
 * can end on a later line than it started.
 * In these situations, moving the end position backwards by sigil_len can
 * result in an end position with an invalid (negative) column number.
 * It's also invalid for a completionItem's textEdit to have a target range
 * that spans multiple lines.
 * So when `ac_loc` ends on a different line than it starts, we just replace
 * the end position with the start position. *)
let remove_from_loc loc =
  let open Loc in
  if loc.start.line = loc._end.line then
    { loc with _end = { loc._end with column = loc._end.column - sigil_len } }
  else
    { loc with _end = loc.start }

let remove_opt =
  let regexp = Str.regexp_string sigil in
  fun str ->
    match Str.bounded_split_delim regexp str 2 with
    | [] -> None
    | [_] -> None
    | before :: after :: _ -> Some (before, after)

let remove str =
  match remove_opt str with
  | None -> (str, "")
  | Some split -> split

(** Finds the first occurrence of the sigil and returns the contents with the
    sigil removed, along with the (line, column) cursor position. *)
let extract_cursor contents =
  match remove_opt contents with
  | None -> None
  | Some (before, after) ->
    let offset = String.length before in
    let cursor = Line.position_of_offset contents offset in
    let contents = before ^ after in
    Some (contents, cursor)
