(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type import_specifier =
  (Loc.t * Loc.t Flow_ast.StringLiteral.t)
  * (Loc.t, Loc.t) Flow_ast.Statement.ImportDeclaration.named_specifier

type hard_coded_type_ast = {
  tast_type: (Loc.t, Loc.t) Flow_ast.Type.t;
  tast_imports: import_specifier list;
}

let expr_to_type_ast (_expr : ('loc, 'loc) Flow_ast.Expression.t) : hard_coded_type_ast option =
  None
