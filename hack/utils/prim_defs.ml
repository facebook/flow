(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 **
 *
 * "Primitive" definitions; fighting the dependency web, this module is a leaf
 * on the dependency tree. It may only depend on external libraries and not on
 * a single module inside the repository.
 *
 *)

type comment =
  | CmtLine   of string
  | CmtBlock  of string
  | CmtMarkup of string

let is_line_comment = function | CmtLine _ -> true | _ -> false
let string_of_comment = function | CmtLine s | CmtBlock s | CmtMarkup s -> s
