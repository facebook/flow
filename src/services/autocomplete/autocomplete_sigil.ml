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

module Canonical = struct
  type token = {
    cursor: Loc.t;
    prefix_and_suffix: (string * string) option;
  }

  let cursor { cursor; _ } = cursor

  let mk ~span ~prefix ~suffix =
    { cursor = Loc.start_loc span; prefix_and_suffix = Some (prefix, suffix) }

  let to_relative_token ~canon token =
    match canon with
    | Some { prefix_and_suffix = Some (prefix, suffix); _ } -> prefix ^ token ^ suffix
    | _ -> token
end

(*
 * Let's assume that the input file's contents include `foo.bar| + 1`, where `|`
 * denotes the position of the cursor, this function returns new contents where
 * the above segment has been replaced with `foo.AUTO332 + 1`. Notice that we
 * have removed the prefix `bar` before adding the sigil. In fact, all input
 * contents `foo.|`, `foo.b|`, `foo.ba|`, etc. produce the same canonical output
 * content `foo.AUTO332`. The reason for this is that for the purposes of type
 * inference the canonical output is equivalent to including the various
 * prefixes before the sigil. Having a single form to represent all these forms
 * enables caching of typing artifacts for autocomplete.
 *
 * When this function succeeds in adding a canonical form for the token under
 * cursor, it will return a [canon_token] structure. This structure includes
 * information used in typechecking the canonical form of the contents, and also
 * in restoring the results of `Autocomplete_js.process_location` to a form that
 * can be further processed by AutocompleteServices_js.
 *)
let add source contents loc_line column =
  let line = loc_line - 1 in
  let (contents_with_sigil, canon_token) =
    match Line.split_nth contents line with
    | None -> (contents, None)
    | Some (pre, line_str, post) ->
      let length = String.length line_str in
      let line_str =
        if length >= column then
          let start = String.sub line_str 0 column in
          let end_ = String.sub line_str column (length - column) in
          start ^ sigil ^ end_
        else
          line_str
      in
      let (line_str, canon_token) =
        match
          Parser_flow.find_ident ~predicate:(Base.String.is_substring ~substring:sigil) line_str
        with
        | Some ({ Loc.start; source = _; _end }, pattern) ->
          (* Since we parsed this as a single line, we need to adjust line and source. *)
          let index_of_pattern = Base.String.substr_index_exn pattern ~pattern:sigil in
          let prefix = Base.String.prefix pattern index_of_pattern in
          let suffix =
            Base.String.drop_prefix pattern index_of_pattern
            |> Base.String.chop_prefix ~prefix:sigil
            |> Base.Option.value_exn
          in
          let loc =
            {
              Loc.start = { start with Loc.line = loc_line };
              source;
              _end = { _end with Loc.line = loc_line };
            }
          in
          ( Base.String.substr_replace_first ~pattern ~with_:sigil line_str,
            Some (Canonical.mk ~prefix ~suffix ~span:loc)
          )
        | None -> (line_str, None)
      in
      (pre ^ line_str ^ post, canon_token)
  in
  let broader_context =
    let f (_, x, _) = x in
    let default = "" in
    Base.Option.value_map ~f ~default (Line.split_nth contents_with_sigil (line - 1))
    ^ Base.Option.value_map ~f ~default (Line.split_nth contents_with_sigil line)
    ^ Base.Option.value_map ~f ~default (Line.split_nth contents_with_sigil (line + 1))
  in
  (contents_with_sigil, broader_context, canon_token)

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
let remove_from_loc ~canon loc =
  let open Loc in
  if loc.start.line = loc._end.line then
    let pad_len =
      match canon with
      | Some { Canonical.prefix_and_suffix = Some (prefix, suffix); _ } ->
        String.length prefix + String.length suffix
      | _ -> 0
    in
    { loc with _end = { loc._end with column = loc._end.column - sigil_len + pad_len } }
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

(** [split_opt str] splits [str] at the autocomplete sigil.

    Returns [Some (before_sigil, sigil_and_after)]. Like [remove_opt], but includes the sigil *)
let split_opt str =
  remove_opt str |> Base.Option.map ~f:(fun (before, after) -> (before, sigil ^ after))

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
