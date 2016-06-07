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
  * Minimal syntax tree
  *
  * Every node in the tree knows its full width, and can compute its leading
  * trivia width, trailing trivia width, and width without trivia.
  *)

module MinimalToken = Full_fidelity_minimal_token

module SyntaxWithMinimalToken =
  Full_fidelity_syntax.WithToken(MinimalToken)

module MinimalSyntaxValue = struct
  type t = { full_width: int }
  let make w = { full_width = w }
  let full_width n = n.full_width
end

module MinimalSyntax =
  SyntaxWithMinimalToken.WithSyntaxValue(MinimalSyntaxValue)

module MinimalValueBuilder = struct
  let value_from_children kind nodes =
    let folder sum node =
      let v = MinimalSyntax.value node in
      let w = MinimalSyntaxValue.full_width v in
      sum + w in
    let width = List.fold_left folder 0 nodes in
    MinimalSyntaxValue.make width

  let value_from_token token =
    MinimalSyntaxValue.make (MinimalToken.full_width token)
end

include MinimalSyntax
include MinimalSyntax.WithValueBuilder(MinimalValueBuilder)

let full_width node =
  MinimalSyntaxValue.full_width (value node)

let leading_width node =
  match leading_token node with
  | None -> 0
  | Some token -> MinimalToken.leading_width token

let trailing_width node =
  match trailing_token node with
  | None -> 0
  | Some token -> MinimalToken.trailing_width token

let width node =
  (full_width node) - (leading_width node) - (trailing_width node)
