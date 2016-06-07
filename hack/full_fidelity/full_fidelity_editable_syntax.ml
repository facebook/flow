(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(** Editable syntax tree
 *
 *  Every token and trivia in the tree knows its text. Therefore we can add
 *  new nodes without having to back them with a source text.
 *)

module SyntaxTree = Full_fidelity_syntax_tree
module EditableToken = Full_fidelity_editable_token
module MinimalSyntax = Full_fidelity_minimal_syntax
module SyntaxWithEditableToken = Full_fidelity_syntax.WithToken(EditableToken)

(**
 * Ironically, an editable syntax tree needs even less per-node information
 * than the "minimal" syntax tree, which needs to know the width of the node.
 **)

module EditableSyntaxValue = struct
  type t = NoValue
end

module EditableSyntax =
  SyntaxWithEditableToken.WithSyntaxValue(EditableSyntaxValue)

module EditableValueBuilder = struct
  let value_from_children _ _ =
    EditableSyntaxValue.NoValue

  let value_from_token _ =
    EditableSyntaxValue.NoValue
end

include EditableSyntax
include EditableSyntax.WithValueBuilder(EditableValueBuilder)

let rec from_minimal text minimal_node offset =
  match MinimalSyntax.syntax minimal_node with
  | MinimalSyntax.Token token ->
    let editable_token = EditableToken.from_minimal text token offset in
    let syntax = Token editable_token in
    make syntax EditableSyntaxValue.NoValue
  | _ ->
    let folder (acc, offset) child =
      let new_child = from_minimal text child offset in
      let w = MinimalSyntax.full_width child in
      (new_child :: acc, offset + w) in
    let kind = MinimalSyntax.kind minimal_node in
    let minimals = MinimalSyntax.children minimal_node in
    let (editables, _) = List.fold_left folder ([], offset) minimals in
    let editables = List.rev editables in
    let syntax = syntax_from_children kind editables in
    make syntax EditableSyntaxValue.NoValue

let from_tree tree =
  from_minimal (SyntaxTree.text tree) (SyntaxTree.root tree) 0

let text node =
  let buffer = Buffer.create 100 in
  let aux token =
    Buffer.add_string buffer (EditableToken.full_text token) in
  List.iter aux (all_tokens node);
  Buffer.contents buffer

let leading_trivia node =
  let token = leading_token node in
  match token with
  | None -> []
  | Some t -> EditableToken.leading t

let trailing_trivia node =
  let token = trailing_token node in
  match token with
  | None -> []
  | Some t -> EditableToken.trailing t
