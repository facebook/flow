(**
 * Copyright (c) 2014, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'a change' =
  | Replace of 'a * 'a

type 'a change = (Loc.t * 'a change')

type node =
  | Statement of (Loc.t, Loc.t) Flow_ast.Statement.t
  | Program of (Loc.t, Loc.t) Flow_ast.program
  | Expression of (Loc.t, Loc.t) Flow_ast.Expression.t
  | Identifier of Loc.t Flow_ast.Identifier.t

(* Diffs the given ASTs using referential equality to determine whether two nodes are different.
 * This works well for transformations based on Flow_ast_mapper, which preserves identity, but it
 * does not work well for e.g. parsing two programs and determining their differences. *)
val program: (Loc.t, Loc.t) Flow_ast.program -> (Loc.t, Loc.t) Flow_ast.program -> node change list
