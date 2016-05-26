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
 * Minimal trivia
 *
 * A minimal trivia knows its kind and width. It does not know its text.
 *
 *)

open Full_fidelity_trivia_kind

type t = {
  kind: Full_fidelity_trivia_kind.t;
  width: int
}

let make_whitespace width =
  { kind = WhiteSpace; width }

let make_eol width =
  { kind = EndOfLine; width }

let make_single_line_comment width =
  { kind = SingleLineComment; width }

let make_delimited_comment width =
  { kind = DelimitedComment; width }

let width trivia =
  trivia.width

let kind trivia =
  trivia.kind
