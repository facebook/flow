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
 * An editable token contains the text of the token; these tokens are not
 * backed by a source text, like the minimal tokens are.
 *)

module MinimalToken = Full_fidelity_minimal_token
module EditableTrivia = Full_fidelity_editable_trivia
module SourceText = Full_fidelity_source_text
module TokenKind = Full_fidelity_token_kind

type t = {
  kind: TokenKind.t;
  text: string;
  leading: EditableTrivia.t list;
  trailing: EditableTrivia.t list
}

let make kind text leading trailing =
  { kind; text; leading; trailing }

let leading_width token =
  let folder sum t = sum + (EditableTrivia.width t) in
  List.fold_left folder 0 token.leading

let trailing_width token =
  let folder sum t = sum + (EditableTrivia.width t) in
  List.fold_left folder 0 token.trailing

let width token =
  String.length token.text

let full_width token =
  (leading_width token) + (width token) + (trailing_width token)

let kind token =
  token.kind

let leading token =
  token.leading

let trailing token =
  token.trailing

let text token =
  token.text

let with_text token text =
  { token with text }

let leading_text token =
  EditableTrivia.text_from_trivia_list (leading token)

let trailing_text token =
  EditableTrivia.text_from_trivia_list (trailing token)

let full_text token =
  (leading_text token) ^ (text token) ^ (trailing_text token)

let from_minimal source_text minimal_token offset =
  let lw = MinimalToken.leading_width minimal_token in
  let w = MinimalToken.width minimal_token in
  let leading = EditableTrivia.from_minimal_list source_text
    (MinimalToken.leading minimal_token) offset in
  let text = SourceText.sub source_text (offset + lw) w in
  let trailing = EditableTrivia.from_minimal_list source_text
    (MinimalToken.trailing minimal_token) (offset + lw + w) in
  make (MinimalToken.kind minimal_token) text leading trailing
