(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type comment =
  | CmtLine of string
  | CmtBlock of string
[@@deriving eq, show]

let is_line_comment = function
  | CmtLine _ -> true
  | _ -> false

let string_of_comment = function
  | CmtLine s
  | CmtBlock s ->
    s
