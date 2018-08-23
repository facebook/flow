(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type binding = Loc.t * string
type ident = Loc.t * string
type source = Loc.t * string

val bindings_of_pattern:
  binding list ->
  (Loc.t, Loc.t) Ast.Pattern.t' ->
  binding list

val bindings_of_variable_declarations:
  (Loc.t, Loc.t) Ast.Statement.VariableDeclaration.Declarator.t list ->
  binding list

val partition_directives:
  (Loc.t, Loc.t) Ast.Statement.t list ->
  (Loc.t, Loc.t) Ast.Statement.t list * (Loc.t, Loc.t) Ast.Statement.t list

val negate_number_literal:
  float * string ->
  float * string

val loc_of_expression:
  ('a, 'a) Ast.Expression.t -> 'a

val loc_of_statement:
  ('a, 'a) Ast.Statement.t -> 'a
