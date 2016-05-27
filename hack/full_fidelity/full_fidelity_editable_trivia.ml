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
 * An editable trivia contains the text of the trivia; these trivia are not
 * backed by a source text, like the minimal trivia are.
 *)

module TriviaKind = Full_fidelity_trivia_kind
module MinimalTrivia = Full_fidelity_minimal_trivia
module SourceText = Full_fidelity_source_text

type t = {
  kind: TriviaKind.t;
  text: string
}

let make_whitespace text =
  { kind = TriviaKind.WhiteSpace; text }

let make_eol text =
  { kind = TriviaKind.EndOfLine; text }

let make_single_line_comment text =
  { kind = TriviaKind.SingleLineComment; text }

let make_delimited_comment text =
  { kind = TriviaKind.DelimitedComment; text }

let width trivia =
  String.length trivia.text

let kind trivia =
  trivia.kind

let text trivia =
  trivia.text

let with_text trivia text =
  { trivia with text }

let text_from_trivia_list trivia_list =
  (* TODO: Better way to accumulate a string? *)
  let folder str trivia =
    str ^ (text trivia) in
  List.fold_left folder "" trivia_list

let from_minimal source_text minimal_trivia offset =
  let width = MinimalTrivia.width minimal_trivia in
  let text = SourceText.sub source_text offset width in
  match MinimalTrivia.kind minimal_trivia with
  | TriviaKind.WhiteSpace -> make_whitespace text
  | TriviaKind.EndOfLine -> make_eol text
  | TriviaKind.SingleLineComment -> make_single_line_comment text
  | TriviaKind.DelimitedComment -> make_delimited_comment text

let from_minimal_list source_text ts offset =
  let rec aux acc ts offset =
    match ts with
    | [] -> acc
    | h :: t ->
      let et = from_minimal source_text h offset in
      aux (et :: acc) t (offset + (MinimalTrivia.width h)) in
  List.rev (aux [] ts offset)
