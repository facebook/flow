(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type comment =
  | CmtLine of string
  | CmtBlock of string
[@@deriving eq, show]

val is_line_comment : comment -> bool

val string_of_comment : comment -> string
