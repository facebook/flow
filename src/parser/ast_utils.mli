(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type binding = Loc.t * string

val bindings_of_pattern:
  binding list ->
  Loc.t Ast.Pattern.t' ->
  binding list

val bindings_of_variable_declarations:
  Loc.t Ast.Statement.VariableDeclaration.Declarator.t list ->
  binding list

val bindings_of_export_specifiers:
  Loc.t Ast.Statement.ExportNamedDeclaration.ExportSpecifier.t list ->
  binding list
