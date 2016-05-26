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
 * Minimal token
 *
 * A minimal token knows its kind, its width (without trivia), and its
 * associated trivia. It does not know its text.
 *)

module TokenKind = Full_fidelity_token_kind
module MinimalTrivia = Full_fidelity_minimal_trivia

type t = {
  kind: TokenKind.t;
  width: int;
  leading: MinimalTrivia.t list;
  trailing: MinimalTrivia.t list
}

let make kind width leading trailing =
  { kind; width; leading; trailing }

let leading_width token =
  let folder sum t = sum + (MinimalTrivia.width t) in
  List.fold_left folder 0 token.leading

let trailing_width token =
  let folder sum t = sum + (MinimalTrivia.width t) in
  List.fold_left folder 0 token.trailing

let width token =
  token.width

let full_width token =
  (leading_width token) + (width token) + (trailing_width token)

let kind token =
  token.kind

let leading token =
  token.leading

let trailing token =
  token.trailing
