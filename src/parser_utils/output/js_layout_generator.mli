(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type opts = {
  preserve_formatting: bool;
  bracket_spacing: bool;
}

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

val default_opts : opts

val normal_context : expression_context

val program :
  ?opts:opts ->
  preserve_docblock:bool ->
  checksum:string option ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  Layout.layout_node

val program_simple : ?opts:opts -> (Loc.t, Loc.t) Flow_ast.Program.t -> Layout.layout_node

val literal : opts:opts -> Loc.t -> Loc.t Flow_ast.Literal.t -> Layout.layout_node

val number_literal_type : Loc.t -> Loc.t Flow_ast.NumberLiteral.t -> Layout.layout_node

val string_literal_type : Loc.t -> Loc.t Flow_ast.StringLiteral.t -> Layout.layout_node

val bigint_literal_type : Loc.t -> Loc.t Flow_ast.BigIntLiteral.t -> Layout.layout_node

val boolean_literal_type : Loc.t -> Loc.t Flow_ast.BooleanLiteral.t -> Layout.layout_node

val expression :
  ?ctxt:expression_context ->
  opts:opts ->
  (Loc.t, Loc.t) Flow_ast.Expression.t ->
  Layout.layout_node

val statement :
  ?pretty_semicolon:bool -> opts:opts -> (Loc.t, Loc.t) Flow_ast.Statement.t -> Layout.layout_node

val statement_list :
  ?pretty_semicolon:bool ->
  opts:opts ->
  (Loc.t, Loc.t) Flow_ast.Statement.t list ->
  Layout.layout_node list

val object_property :
  opts:opts -> (Loc.t, Loc.t) Flow_ast.Expression.Object.property -> Layout.layout_node

val class_method : opts:opts -> (Loc.t, Loc.t) Flow_ast.Class.Method.t -> Layout.layout_node

val class_property : opts:opts -> (Loc.t, Loc.t) Flow_ast.Class.Property.t -> Layout.layout_node

val class_private_field :
  opts:opts -> (Loc.t, Loc.t) Flow_ast.Class.PrivateField.t -> Layout.layout_node

val type_ : opts:opts -> (Loc.t, Loc.t) Flow_ast.Type.t -> Layout.layout_node

val variance : Loc.t Flow_ast.Variance.t -> Layout.layout_node

val type_param : opts:opts -> (Loc.t, Loc.t) Flow_ast.Type.TypeParam.t -> Layout.layout_node

val type_annotation :
  ?parens:bool -> opts:opts -> (Loc.t, Loc.t) Flow_ast.Type.annotation -> Layout.layout_node

val identifier : (Loc.t, Loc.t) Flow_ast.Identifier.t -> Layout.layout_node

val pattern :
  ?ctxt:expression_context -> opts:opts -> (Loc.t, Loc.t) Flow_ast.Pattern.t -> Layout.layout_node

val comment : Loc.t Flow_ast.Comment.t -> Layout.layout_node

val template_literal :
  opts:opts -> (Loc.t, Loc.t) Flow_ast.Expression.TemplateLiteral.t -> Layout.layout_node

val jsx_identifier : (Loc.t, Loc.t) Flow_ast.JSX.Identifier.t -> Layout.layout_node

val jsx_child :
  opts:opts -> (Loc.t, Loc.t) Flow_ast.JSX.child -> (Loc.t * Layout.layout_node) option

val function_params :
  ?ctxt:expression_context ->
  opts:opts ->
  (Loc.t, Loc.t) Flow_ast.Function.Params.t ->
  Layout.layout_node

val better_quote : string -> string

val utf8_escape : quote:string -> string -> string

val wrap_in_parens : ?with_break:bool -> Layout.layout_node -> Layout.layout_node

val with_semicolon : Layout.layout_node -> Layout.layout_node
