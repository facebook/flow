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
 * Positioned token
 *
 * A positioned token stores the original source text,
 * the offset of the leading trivia, the width of the leading trivia,
 * node proper, and trailing trivia. From all this information we can
 * rapidly compute the absolute position of any portion of the node,
 * or the text.
 *
 *)

module MinimalToken = Full_fidelity_minimal_token
module PositionedTrivia = Full_fidelity_positioned_trivia
module SourceText = Full_fidelity_source_text
module TokenKind = Full_fidelity_token_kind

type t = {
  kind: TokenKind.t;
  source_text: SourceText.t;
  offset: int; (* Beginning of first trivia *)
  leading_width: int;
  width: int; (* Width of actual token, not counting trivia *)
  trailing_width: int;
  leading: PositionedTrivia.t list;
  trailing: PositionedTrivia.t list
}

let make kind source_text offset width leading trailing =
  let folder sum trivia =
    sum + (PositionedTrivia.width trivia) in
  let leading_width = List.fold_left folder 0 leading in
  let trailing_width = List.fold_left folder 0 trailing in
  { kind; source_text; offset; leading_width; width; trailing_width;
    leading; trailing }

let kind token =
  token.kind

let source_text token =
  token.source_text

let leading_width token =
  token.leading_width

let width token =
  token.width

let trailing_width token =
  token.trailing_width

let full_width token =
  (leading_width token) + (width token) + (trailing_width token)

let leading token =
  token.leading

let trailing token =
  token.trailing

let leading_start_offset token =
  token.offset

let leading_end_offset token =
  let w = (leading_width token) - 1 in
  let w = if w < 0 then 0 else w in
  (leading_start_offset token) + w

let start_offset token =
  (leading_start_offset token) + (leading_width token)

let end_offset token =
  let w = (width token) - 1 in
  let w = if w < 0 then 0 else w in
  (start_offset token) + w

let trailing_start_offset token =
  (leading_start_offset token) + (leading_width token) + (width token)

let trailing_end_offset token =
  let w = (full_width token) - 1 in
  let w = if w < 0 then 0 else w in
  (leading_start_offset token) + w

let leading_start_position token =
  SourceText.offset_to_position (source_text token) (leading_start_offset token)

let leading_end_position token =
  SourceText.offset_to_position (source_text token) (leading_end_offset token)

let start_position token =
  SourceText.offset_to_position (source_text token) (start_offset token)

let end_position token =
  SourceText.offset_to_position (source_text token) (end_offset token)

let trailing_start_position token =
  SourceText.offset_to_position
    (source_text token) (trailing_start_offset token)

let trailing_end_position token =
  SourceText.offset_to_position (source_text token) (trailing_end_offset token)

let leading_span token =
  ((leading_start_position token), (leading_end_position token))

let span token =
  ((start_position token), (end_position token))

let trailing_span token =
  ((trailing_start_position token), (trailing_end_position token))

let full_span token =
  ((leading_start_position token), (trailing_end_position token))

let full_text token =
  SourceText.sub
    (source_text token) (leading_start_offset token) (full_width token)

let leading_text token =
  SourceText.sub (source_text token) (start_offset token) (leading_width token)

let trailing_text token =
  SourceText.sub
    (source_text token) ((end_offset token) + 1) (trailing_width token)

let text token =
  SourceText.sub (source_text token) (start_offset token) (width token)

let from_minimal source_text minimal_token offset =
  let kind = MinimalToken.kind minimal_token in
  let leading_width = MinimalToken.leading_width minimal_token in
  let width = MinimalToken.width minimal_token in
  let leading = PositionedTrivia.from_minimal_list source_text
    (MinimalToken.leading minimal_token) offset in
  let trailing = PositionedTrivia.from_minimal_list source_text
    (MinimalToken.trailing minimal_token) (offset + leading_width + width) in
  make kind source_text offset width leading trailing
