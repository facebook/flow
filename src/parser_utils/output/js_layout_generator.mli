(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type expression_context = {
  left: expression_context_left;
  group: expression_context_group;
}
and expression_context_left =
  | Normal_left
  | In_expression_statement
  | In_tagged_template
  | In_plus_op
  | In_minus_op
and expression_context_group =
  | Normal_group
  | In_arrow_func
  | In_for_init

val normal_context: expression_context

val program: preserve_docblock:bool -> checksum:string option -> Loc.t Ast.program -> Layout.layout_node
val expression: ?ctxt:expression_context -> Loc.t Ast.Expression.t -> Layout.layout_node
val statement:
  ?allow_empty:bool ->
  ?pretty_semicolon:bool ->
  Loc.t Ast.Statement.t -> Layout.layout_node
val object_property: Loc.t Ast.Expression.Object.property -> Layout.layout_node

val better_quote: string -> string
val utf8_escape: quote:string -> string -> string
val wrap_in_parens: Layout.layout_node -> Layout.layout_node
val with_semicolon: Layout.layout_node -> Layout.layout_node
