(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Main type for code *)
type t = {
  stmt : Loc.t Ast.Statement.t;
  stmt_deps : t list
};;

(* This is mainly used for expressions. Eventually this will be turned
 * into Code.t. The purpose for this type is to carry dependecies for
 * expressions.
*)
type t' = {
  expr : Loc.t Ast.Expression.t';
  expr_deps : t list
};;
