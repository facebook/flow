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
  (Loc.t, Loc.t) Flow_ast.Pattern.t' ->
  binding list

val bindings_of_variable_declarations:
  (Loc.t, Loc.t) Flow_ast.Statement.VariableDeclaration.Declarator.t list ->
  binding list

val partition_directives:
  (Loc.t, Loc.t) Flow_ast.Statement.t list ->
  (Loc.t, Loc.t) Flow_ast.Statement.t list * (Loc.t, Loc.t) Flow_ast.Statement.t list

val negate_number_literal:
  float * string ->
  float * string

val loc_of_expression:
  ('a, 'a) Flow_ast.Expression.t -> 'a

val loc_of_statement:
  ('a, 'a) Flow_ast.Statement.t -> 'a

val loc_of_pattern:
  ('a, 'a) Flow_ast.Pattern.t -> 'a

module ExpressionSort: sig
  type t =
    | Array
    | ArrowFunction
    | Assignment
    | Binary
    | Call
    | Class
    | Comprehension
    | Conditional
    | Function
    | Generator
    | Identifier
    | Import
    | JSXElement
    | JSXFragment
    | Literal
    | Logical
    | Member
    | MetaProperty
    | New
    | Object
    | OptionalCall
    | OptionalMember
    | Sequence
    | Super
    | TaggedTemplate
    | TemplateLiteral
    | This
    | TypeCast
    | Unary
    | Update
    | Yield

  val to_string: t -> string
end

val string_of_binary_operator:
  Flow_ast.Expression.Binary.operator -> string
