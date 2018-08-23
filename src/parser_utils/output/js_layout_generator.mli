(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module LocMap = Utils_js.LocMap

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

type comment_attach =
    | Preceding
    | Enclosing
    | Following

type comment_map =
  (comment_attach * (Loc.t, Loc.t) Flow_ast.Statement.t * Loc.t Flow_ast.Comment.t)
  list LocMap.t

val normal_context: expression_context

val with_attached_comments: comment_map option ref

val program:
  preserve_docblock:bool ->
  checksum:string option ->
  (Loc.t, Loc.t) Flow_ast.program -> Layout.layout_node
val program_simple:
  (Loc.t, Loc.t) Flow_ast.program -> Layout.layout_node
val expression: ?ctxt:expression_context -> (Loc.t, Loc.t) Flow_ast.Expression.t -> Layout.layout_node
val statement:
  ?allow_empty:bool ->
  ?pretty_semicolon:bool ->
  (Loc.t, Loc.t) Flow_ast.Statement.t -> Layout.layout_node
val object_property: (Loc.t, Loc.t) Flow_ast.Expression.Object.property -> Layout.layout_node
val class_method: (Loc.t, Loc.t) Flow_ast.Class.Method.t -> Layout.layout_node
val class_property: (Loc.t, Loc.t) Flow_ast.Class.Property.t -> Layout.layout_node
val class_private_field: (Loc.t, Loc.t) Flow_ast.Class.PrivateField.t -> Layout.layout_node
val type_: (Loc.t, Loc.t) Flow_ast.Type.t -> Layout.layout_node
val identifier: Loc.t Flow_ast.Identifier.t -> Layout.layout_node

val better_quote: string -> string
val utf8_escape: quote:string -> string -> string
val wrap_in_parens: Layout.layout_node -> Layout.layout_node
val with_semicolon: Layout.layout_node -> Layout.layout_node
