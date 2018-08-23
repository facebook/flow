(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type node =
  | Statement of (Loc.t, Loc.t) Flow_ast.Statement.t * (Loc.t, Loc.t) Flow_ast.Statement.t
  | Expression of (Loc.t, Loc.t) Flow_ast.Expression.t * (Loc.t, Loc.t) Flow_ast.Expression.t
  | ClassElement of (Loc.t, Loc.t) Flow_ast.Class.Body.element * (Loc.t, Loc.t) Flow_ast.Class.Body.element
  | Type of (Loc.t, Loc.t) Flow_ast.Type.t * (Loc.t, Loc.t) Flow_ast.Type.t

(*
 * A map of (old node, new node)
 *)

type t = node Utils_js.LocMap.t

val diff : Flow_ast_mapper.mapper -> (Loc.t, Loc.t) Flow_ast.program -> t * (Loc.t, Loc.t) Flow_ast.program

(*
 * Combines two diffs and merges the old value with the add value
 * The combination keeps the old node of orig and the new node of add
 *)

val combine : orig:t -> add:t -> t
