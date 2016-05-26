(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t =
| WhiteSpace
| EndOfLine
| SingleLineComment
| DelimitedComment

let to_string kind =
  match kind with
  | WhiteSpace -> "whitespace"
  | EndOfLine -> "end_of_line"
  | SingleLineComment -> "single_line_comment"
  | DelimitedComment -> "delimited_comment"
