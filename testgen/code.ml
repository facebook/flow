(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Main type for code *)
type t = {
  stmt : (Loc.t, Loc.t) Ast.Statement.t;
  stmt_deps : t list
};;

(* This is mainly used for expressions. Eventually this will be turned
 * into Code.t. The purpose for this type is to carry dependecies for
 * expressions.
*)
type t' = {
  expr : (Loc.t, Loc.t) Ast.Expression.t';
  expr_deps : t list
};;
