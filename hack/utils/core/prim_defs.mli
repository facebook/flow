(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 **
 *
 * "Primitive" definitions; fighting the dependency web, this module is a leaf
 * on the dependency tree. It may only depend on external libraries and not on
 * a single module inside the repository.
 *
 *)

type comment =
  | CmtLine of string
  | CmtBlock of string
  | CmtMarkup of string
[@@deriving show]

val is_line_comment : comment -> bool

val string_of_comment : comment -> string
