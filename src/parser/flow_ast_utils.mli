(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'loc binding = 'loc * string

type 'loc ident = 'loc * string [@@deriving show]

type 'loc source = 'loc * string [@@deriving show]

val fold_bindings_of_pattern :
  ('a -> ('m, 't) Flow_ast.Identifier.t -> 'a) -> 'a -> ('m, 't) Flow_ast.Pattern.t -> 'a

val fold_bindings_of_variable_declarations :
  (bool -> 'a -> ('m, 't) Flow_ast.Identifier.t -> 'a) ->
  'a ->
  ('m, 't) Flow_ast.Statement.VariableDeclaration.Declarator.t list ->
  'a

val pattern_has_binding : ('m, 't) Flow_ast.Pattern.t -> bool

val partition_directives :
  (Loc.t, Loc.t) Flow_ast.Statement.t list ->
  (Loc.t, Loc.t) Flow_ast.Statement.t list * (Loc.t, Loc.t) Flow_ast.Statement.t list

val hoist_function_and_component_declarations :
  ('a, 'b) Flow_ast.Statement.t list -> ('a, 'b) Flow_ast.Statement.t list

val is_call_to_invariant : ('a, 'b) Flow_ast.Expression.t -> bool

val is_call_to_is_array : ('a, 'b) Flow_ast.Expression.t -> bool

val is_call_to_object_dot_freeze : ('a, 'b) Flow_ast.Expression.t -> bool

val is_call_to_object_static_method : ('a, 'b) Flow_ast.Expression.t -> bool

val is_super_member_access : ('a, 'b) Flow_ast.Expression.Member.t -> bool

val negate_number_literal : float * string -> float * string

val negate_bigint_literal : int64 option * string -> int64 option * string

val loc_of_expression : ('a, 'a) Flow_ast.Expression.t -> 'a

val loc_of_statement : ('a, 'a) Flow_ast.Statement.t -> 'a

val loc_of_pattern : ('a, 'a) Flow_ast.Pattern.t -> 'a

val loc_of_ident : ('a, 'a) Flow_ast.Identifier.t -> 'a

val name_of_ident : ('loc, 'a) Flow_ast.Identifier.t -> string

val source_of_ident : ('a, 'a) Flow_ast.Identifier.t -> 'a source

val ident_of_source :
  ?comments:('a, unit) Flow_ast.Syntax.t -> 'a source -> ('a, 'a) Flow_ast.Identifier.t

val mk_comments :
  ?leading:'loc Flow_ast.Comment.t list ->
  ?trailing:'loc Flow_ast.Comment.t list ->
  'a ->
  ('loc, 'a) Flow_ast.Syntax.t

val mk_comments_opt :
  ?leading:'loc Flow_ast.Comment.t list ->
  ?trailing:'loc Flow_ast.Comment.t list ->
  unit ->
  ('loc, unit) Flow_ast.Syntax.t option

val mk_comments_with_internal_opt :
  ?leading:'loc Flow_ast.Comment.t list ->
  ?trailing:'loc Flow_ast.Comment.t list ->
  internal:'loc Flow_ast.Comment.t list ->
  unit ->
  ('loc, 'loc Flow_ast.Comment.t list) Flow_ast.Syntax.t option

val merge_comments :
  inner:('M, unit) Flow_ast.Syntax.t option ->
  outer:('M, unit) Flow_ast.Syntax.t option ->
  ('M, unit) Flow_ast.Syntax.t option

val merge_comments_with_internal :
  inner:('M, 'loc Flow_ast.Comment.t list) Flow_ast.Syntax.t option ->
  outer:('M, 'a) Flow_ast.Syntax.t option ->
  ('M, 'loc Flow_ast.Comment.t list) Flow_ast.Syntax.t option

val split_comments :
  ('loc, unit) Flow_ast.Syntax.t option ->
  ('loc, unit) Flow_ast.Syntax.t option * ('loc, unit) Flow_ast.Syntax.t option

module ExpressionSort : sig
  type t =
    | Array
    | ArrowFunction
    | Assignment
    | Binary
    | Call
    | Class
    | Conditional
    | Function
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
  [@@deriving show]

  val to_string : t -> string
end

val string_of_assignment_operator : Flow_ast.Expression.Assignment.operator -> string

val string_of_binary_operator : Flow_ast.Expression.Binary.operator -> string

val loc_of_annotation_or_hint : ('loc, 'loc) Flow_ast.Type.annotation_or_hint -> 'loc

val loc_of_return_annot : ('loc, 'loc) Flow_ast.Function.ReturnAnnot.t -> 'loc

val push_toplevel_type :
  't -> ('loc, 'loc * 't) Flow_ast.Expression.t -> ('loc, 'loc * 't) Flow_ast.Expression.t

val hook_function : ('a, 'b) Flow_ast.Function.t -> 'b option

val hook_call : ('loc, 'loc) Flow_ast.Expression.Call.t -> bool
