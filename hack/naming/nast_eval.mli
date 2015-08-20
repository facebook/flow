(**
 *  Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type eval_error =
  | Not_static of Pos.t
  | Type_error (* type errors are already handled by naming / typing code,
                * so the caller can usually ignore them *)

val static_string : Nast.class_ option -> Nast.expr ->
  (Pos.t * string, eval_error) Result.t
val static_string_no_consts : Nast.expr ->
  (Pos.t * string, eval_error) Result.t
