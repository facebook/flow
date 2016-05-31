(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(**
 * Positioned trivia
 *
 * A positioned trivia stores the original source text, the offset of the
 * start of the trivia, and its width.  From all this information we can
 * rapidly compute the absolute position of the trivia, or obtain its text.
 *
 *)

module TriviaKind = Full_fidelity_trivia_kind
module SourceText = Full_fidelity_source_text
module MinimalTrivia = Full_fidelity_minimal_trivia

type t = {
  kind: TriviaKind.t;
  source_text : SourceText.t;
  offset : int;
  width : int
}

let make_whitespace source_text offset width =
  { kind = TriviaKind.WhiteSpace; source_text; offset; width }

let make_eol source_text offset width =
  { kind = TriviaKind.EndOfLine; source_text; offset; width }

let make_single_line_comment source_text offset width =
  { kind = TriviaKind.SingleLineComment; source_text; offset; width }

let make_delimited_comment source_text offset width =
  { kind = TriviaKind.DelimitedComment; source_text; offset; width }

let width trivia =
  trivia.width

let kind trivia =
  trivia.kind

let start_offset trivia =
  trivia.offset

let end_offset trivia =
  trivia.offset + trivia.width - 1

let source_text trivia =
  trivia.source_text

let text trivia =
  SourceText.sub (source_text trivia) (start_offset trivia) (width trivia)

let start_position trivia =
  SourceText.offset_to_position (source_text trivia) (start_offset trivia)

let end_position trivia =
  SourceText.offset_to_position (source_text trivia) (end_offset trivia)

let from_minimal source_text minimal_trivia offset =
  let width = MinimalTrivia.width minimal_trivia in
  match MinimalTrivia.kind minimal_trivia with
  | TriviaKind.WhiteSpace -> make_whitespace source_text offset width
  | TriviaKind.EndOfLine -> make_eol source_text offset width
  | TriviaKind.SingleLineComment ->
    make_single_line_comment source_text offset width
  | TriviaKind.DelimitedComment ->
    make_delimited_comment source_text offset width

let from_minimal_list source_text ts offset =
  let rec aux acc ts offset =
    match ts with
    | [] -> acc
    | h :: t ->
      let et = from_minimal source_text h offset in
      aux (et :: acc) t (offset + (MinimalTrivia.width h)) in
  List.rev (aux [] ts offset)
