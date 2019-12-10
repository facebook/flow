(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*
 * Process jsx text normalize via the following rules:
 * - Each line of the JSXText is trimmed of whitespace
 * - Empty or pure whitespace lines are removed
 * - Tabs are replaces with spaces
 * - The lines are joined together with spaces
 *
 * This is hard for two main reasons:
 *  1. We can't use Str
 *  2. It's not enough to trim the text, we also need to figure out the line and
 *     column for the start and end of the text
 *)
let trim_jsx_text =
  (* Removes all the spaces from the beginning of the string *)
  let prefix_trim =
    let rec trimmer str len idx =
      if idx >= len then
        ""
      else if str.[idx] = ' ' then
        trimmer str len (idx + 1)
      else
        String.sub str idx (len - idx)
    in
    (fun str -> trimmer str (String.length str) 0)
  in
  (* Removes all the spaces from the end of the string *)
  let suffix_trim =
    let rec trimmer str idx =
      if idx < 0 then
        ""
      else if str.[idx] = ' ' then
        trimmer str (idx - 1)
      else
        String.sub str 0 (idx + 1)
    in
    (fun str -> trimmer str (String.length str - 1))
  in
  fun loc value ->
    (* Tabs get turned into spaces *)
    let value = String_utils.replace_char '\t' ' ' value in
    (* The algorithm is line based, so split the string into lines *)
    let lines = String_utils.split_into_lines value in
    let last_line = List.length lines - 1 in
    let trimmed_lines =
      List.mapi
        (fun idx line ->
          (* Remove the leading whitespace from every line but the first *)
          let line =
            if idx <> 0 then
              prefix_trim line
            else
              line
          in
          (* Remove the trailing whitespace from every line but the last *)
          if idx <> last_line then
            suffix_trim line
          else
            line)
        lines
    in
    (* Figure out the first and last non-empty line, if there are any *)
    let (_, first_and_last_non_empty) =
      List.fold_left
        (fun (idx, first_and_last) line ->
          let first_and_last =
            if line <> "" then
              match first_and_last with
              | None -> Some (idx, idx)
              | Some (first, _) -> Some (first, idx)
            else
              first_and_last
          in
          (idx + 1, first_and_last))
        (0, None)
        trimmed_lines
    in
    match first_and_last_non_empty with
    | None -> None
    | Some (first_line, last_line) ->
      (* Filter out empty lines and turn newlines into spaces *)
      let trimmed = trimmed_lines |> List.filter (fun line -> line <> "") |> String.concat " " in
      Loc.(
        let start_line = loc.start.line + first_line in
        let end_line = loc.start.line + last_line in
        (* We want to know the column and offset for the first and last
         * non-whitespace characters. We can do that by figuring out what those
         * characters are and using String.index and String.rindex to search for
         * them *)
        let first_trimmed_line = List.nth trimmed_lines first_line in
        let last_trimmed_line = List.nth trimmed_lines last_line in
        let first_char = first_trimmed_line.[0] in
        let last_char = last_trimmed_line.[String.length last_trimmed_line - 1] in
        (* For column we just do a search within the line *)
        let start_column = String.index (List.nth lines first_line) first_char in
        let end_column = String.rindex (List.nth lines last_line) last_char + 1 in
        (* If we're on the first line, then we need to see on which column the line
         starts *)
        let start_column =
          if first_line = 0 then
            start_column + loc.start.column
          else
            start_column
        in
        let end_column =
          if last_line = 0 then
            end_column + loc.start.column
          else
            end_column
        in
        let loc =
          {
            loc with
            start = { line = start_line; column = start_column };
            _end = { line = end_line; column = end_column };
          }
        in
        Some (loc, trimmed))
