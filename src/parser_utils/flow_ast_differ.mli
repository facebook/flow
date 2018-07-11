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
  | Statement of Loc.t Ast.Statement.t
  | Program of Loc.t Ast.program
  | Expression of Loc.t Ast.Expression.t

val program: Loc.t Ast.program -> Loc.t Ast.program -> node change list
