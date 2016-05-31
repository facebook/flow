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
 * Positioned syntax tree
 *
 * A positioned syntax tree stores the original source text,
 * the offset of the leading trivia, the width of the leading trivia,
 * node proper, and trailing trivia. From all this information we can
 * rapidly compute the absolute position of any portion of the node,
 * or the text.
 *
 *)

module SyntaxTree = Full_fidelity_syntax_tree
module SourceText = Full_fidelity_source_text
module MinimalSyntax = Full_fidelity_minimal_syntax
module PositionedToken = Full_fidelity_positioned_token

module SyntaxWithPositionedToken =
  Full_fidelity_syntax.WithToken(PositionedToken)

module PositionedSyntaxValue = struct
  type t = {
    source_text: SourceText.t;
    offset: int; (* Beginning of first trivia *)
    leading_width: int;
    width: int; (* Width of node, not counting trivia *)
    trailing_width: int;
  }

  let make source_text offset leading_width width trailing_width =
    { source_text; offset; leading_width; width; trailing_width }

  let source_text value =
    value.source_text

  let start_offset value =
    value.offset

  let leading_width value =
    value.leading_width

  let width value =
    value.width

  let trailing_width value =
    value.trailing_width

end

include SyntaxWithPositionedToken.WithSyntaxValue(PositionedSyntaxValue)

let rec from_minimal source_text minimal_node offset =
  match MinimalSyntax.syntax minimal_node with
  | MinimalSyntax.Token token ->
    let positioned_token =
      PositionedToken.from_minimal source_text token offset in
    let syntax = Token positioned_token in
    let leading_width = PositionedToken.leading_width positioned_token in
    let width = PositionedToken.width positioned_token in
    let trailing_width = PositionedToken.trailing_width positioned_token in
    let value = PositionedSyntaxValue.make
      source_text offset leading_width width trailing_width in
    make syntax value
  | _ ->
    let folder (acc, offset) child =
      let new_child = from_minimal source_text child offset in
      let w = MinimalSyntax.full_width child in
      (new_child :: acc, offset + w) in
    let kind = MinimalSyntax.kind minimal_node in
    let minimals = MinimalSyntax.children minimal_node in
    let (positioned, _) = List.fold_left folder ([], offset) minimals in
    let positioned = List.rev positioned in
    let syntax = syntax_from_children kind positioned in
    let leading_width = MinimalSyntax.leading_width minimal_node in
    let width = MinimalSyntax.width minimal_node in
    let trailing_width = MinimalSyntax.trailing_width minimal_node in
    let value = PositionedSyntaxValue.make
      source_text offset leading_width width trailing_width in
    make syntax value

let from_tree tree =
  from_minimal (SyntaxTree.text tree) (SyntaxTree.root tree) 0

let source_text node =
  PositionedSyntaxValue.source_text (value node)

let leading_width node =
  PositionedSyntaxValue.leading_width (value node)

let width node =
  PositionedSyntaxValue.width (value node)

let trailing_width node =
  PositionedSyntaxValue.trailing_width (value node)

let full_width node =
  (leading_width node) + (width node) + (trailing_width node)

let leading_start_offset node =
  PositionedSyntaxValue.start_offset (value node)

let leading_end_offset node =
  let w = (leading_width node) - 1 in
  let w = if w < 0 then 0 else w in
  (leading_start_offset node) + w

let start_offset node =
  (leading_start_offset node) + (leading_width node)

let end_offset node =
  let w = (width node) - 1 in
  let w = if w < 0 then 0 else w in
  (start_offset node) + w

let trailing_start_offset node =
  (leading_start_offset node) + (leading_width node) + (width node)

let trailing_end_offset node =
  let w = (full_width node) - 1 in
  let w = if w < 0 then 0 else w in
  (leading_start_offset node) + w

let leading_start_position node =
  SourceText.offset_to_position (source_text node) (leading_start_offset node)

let leading_end_position node =
  SourceText.offset_to_position (source_text node) (leading_end_offset node)

let start_position node =
  SourceText.offset_to_position (source_text node) (start_offset node)

let end_position node =
  SourceText.offset_to_position (source_text node) (end_offset node)

let trailing_start_position node =
  SourceText.offset_to_position (source_text node) (trailing_start_offset node)

let trailing_end_position node =
  SourceText.offset_to_position (source_text node) (trailing_end_offset node)

let leading_span node =
  ((leading_start_position node), (leading_end_position node))

let span node =
  ((start_position node), (end_position node))

let trailing_span node =
  ((trailing_start_position node), (trailing_end_position node))

let full_span node =
  ((leading_start_position node), (trailing_end_position node))

let full_text node =
  SourceText.sub
    (source_text node) (leading_start_offset node) (full_width node)

let leading_text node =
  SourceText.sub (source_text node) (start_offset node) (leading_width node)

let trailing_text node =
  SourceText.sub
    (source_text node) ((end_offset node) + 1) (trailing_width node)

let text node =
  SourceText.sub (source_text node) (start_offset node) (width node)
