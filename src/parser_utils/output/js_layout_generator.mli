(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Trailing_commas : sig
  type t =
    | All  (** Wherever possible (including function arguments). *)
    | ES5  (** Where valid in ES5 (objects, arrays, etc.) *)
    | Off  (** No trailing commas *)
end

type opts = {
  bracket_spacing: bool;
  preserve_formatting: bool;
  single_quotes: bool;
  trailing_commas: Trailing_commas.t;
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

val number_literal : opts:opts -> Loc.t -> Loc.t Flow_ast.NumberLiteral.t -> Layout.layout_node

val string_literal : opts:opts -> Loc.t -> Loc.t Flow_ast.StringLiteral.t -> Layout.layout_node

val bigint_literal : Loc.t -> Loc.t Flow_ast.BigIntLiteral.t -> Layout.layout_node

val boolean_literal : Loc.t -> Loc.t Flow_ast.BooleanLiteral.t -> Layout.layout_node

val regexp_literal : opts:opts -> Loc.t -> Loc.t Flow_ast.RegExpLiteral.t -> Layout.layout_node

val module_ref_literal : Loc.t -> (Loc.t, Loc.t) Flow_ast.ModuleRefLiteral.t -> Layout.layout_node

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

val import_named_specifier :
  (Loc.t, Loc.t) Flow_ast.Statement.ImportDeclaration.named_specifier -> Layout.layout_node

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

val type_guard :
  opts:opts -> needs_parens:bool -> (Loc.t, Loc.t) Flow_ast.Type.TypeGuard.t -> Layout.layout_node

val type_guard_annotation :
  opts:opts ->
  needs_parens:bool ->
  (Loc.t, Loc.t) Flow_ast.Type.type_guard_annotation ->
  Layout.layout_node

val identifier : (Loc.t, Loc.t) Flow_ast.Identifier.t -> Layout.layout_node

val pattern :
  ?ctxt:expression_context -> opts:opts -> (Loc.t, Loc.t) Flow_ast.Pattern.t -> Layout.layout_node

val comment : Loc.t Flow_ast.Comment.t -> Layout.layout_node

val template_literal :
  opts:opts -> (Loc.t, Loc.t) Flow_ast.Expression.TemplateLiteral.t -> Layout.layout_node

val jsx_identifier : (Loc.t, Loc.t) Flow_ast.JSX.Identifier.t -> Layout.layout_node

val jsx_child :
  opts:opts -> (Loc.t, Loc.t) Flow_ast.JSX.child -> (Loc.t * Layout.layout_node) option

val jsx_closing : (Loc.t, Loc.t) Flow_ast.JSX.Closing.t -> Layout.layout_node

val jsx_opening_attr :
  opts:opts -> (Loc.t, Loc.t) Flow_ast.JSX.Opening.attribute -> Layout.layout_node

val function_params :
  ?ctxt:expression_context ->
  opts:opts ->
  (Loc.t, Loc.t) Flow_ast.Function.Params.t ->
  Layout.layout_node

val function_params_and_return :
  opts:opts -> Loc.t * (Loc.t, Loc.t) Flow_ast.Function.t -> Layout.layout_node

val match_pattern : opts:opts -> (Loc.t, Loc.t) Flow_ast.MatchPattern.t -> Layout.layout_node

val match_object_pattern_property :
  opts:opts -> (Loc.t, Loc.t) Flow_ast.MatchPattern.ObjectPattern.Property.t -> Layout.layout_node

val better_quote : prefer_single_quotes:bool -> string -> string

val utf8_escape : quote:string -> string -> string

val quote_string : prefer_single_quotes:bool -> string -> string

val wrap_in_parens : ?with_break:bool -> Layout.layout_node -> Layout.layout_node

val with_semicolon : Layout.layout_node -> Layout.layout_node
