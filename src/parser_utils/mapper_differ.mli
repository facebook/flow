(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type node =
  | Statement of Loc.t Ast.Statement.t * Loc.t Ast.Statement.t
  | Expression of Loc.t Ast.Expression.t * Loc.t Ast.Expression.t
  | ClassElement of Loc.t Ast.Class.Body.element * Loc.t Ast.Class.Body.element
  | Type of Loc.t Ast.Type.t * Loc.t Ast.Type.t

(*
 * A map of (old node, new node)
 *)

type t = node Utils_js.LocMap.t

val diff : Flow_ast_mapper.mapper -> Loc.t Ast.program -> t * Loc.t Ast.program

(*
 * Combines two diffs and merges the old value with the add value
 * The combination keeps the old node of orig and the new node of add
 *)

val combine : orig:t -> add:t -> t
