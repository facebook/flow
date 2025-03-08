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

val match_pattern_has_binding : ('m, 't) Flow_ast.MatchPattern.t -> bool

val string_of_variable_kind : Flow_ast.Variable.kind -> string

val partition_directives :
  (Loc.t, Loc.t) Flow_ast.Statement.t list ->
  (Loc.t, Loc.t) Flow_ast.Statement.t list * (Loc.t, Loc.t) Flow_ast.Statement.t list

val hoist_function_and_component_declarations :
  ('a, 'b) Flow_ast.Statement.t list -> ('a, 'b) Flow_ast.Statement.t list

val is_call_to_invariant : ('a, 'b) Flow_ast.Expression.t -> bool

val is_call_to_require : ('a, 'b) Flow_ast.Expression.t -> bool

val is_call_to_is_array : ('a, 'b) Flow_ast.Expression.t -> bool

val is_call_to_object_dot_freeze : ('a, 'b) Flow_ast.Expression.t -> bool

val get_call_to_object_dot_freeze_arg :
  ('a, 'b) Flow_ast.Expression.t ->
  ('a, 'b) Flow_ast.Expression.CallTypeArgs.t option ->
  ('a, 'b) Flow_ast.Expression.ArgList.t ->
  ('b * ('a, 'b) Flow_ast.Expression.Object.t) option

val is_call_to_object_static_method : ('a, 'b) Flow_ast.Expression.t -> bool

val get_call_to_jest_module_mocking_fn :
  ('loc, 'annot) Flow_ast.Expression.t ->
  ('loc, 'annot) Flow_ast.Expression.ArgList.t ->
  ('annot * 'annot * string) option

val is_super_member_access : ('a, 'b) Flow_ast.Expression.Member.t -> bool

(* Returns Ok () for such statement, and Error kind_of_statement otherwise. *)
val acceptable_statement_in_declaration_context :
  in_declare_namespace:bool -> ('a, 'b) Flow_ast.Statement.t' -> (unit, string) result

val is_type_only_declaration_statement : ('a, 'b) Flow_ast.Statement.t -> bool

val negate_number_literal : float * string -> float * string

val negate_bigint_literal : int64 option * string -> int64 option * string

val is_number_literal : ('a, 'b) Flow_ast.Expression.t' -> bool

val extract_number_literal : ('a, 'b) Flow_ast.Expression.t' -> (float * string) option

val is_bigint_literal : ('a, 'b) Flow_ast.Expression.t' -> bool

val extract_bigint_literal : ('a, 'b) Flow_ast.Expression.t' -> (int64 option * string) option

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
    | Match
    | Member
    | MetaProperty
    | New
    | Object
    | OptionalCall
    | OptionalMember
    | Satisfies
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

val hook_call : ('a, 'b) Flow_ast.Expression.Call.t -> bool

val hook_name : string -> bool

val match_root_name : string

val match_root_ident : 'loc -> ('loc, 'loc) Flow_ast.Identifier.t

val expression_of_match_member_pattern :
  visit_expression:(('loc, 'loc) Flow_ast.Expression.t -> unit) ->
  ('loc, 'loc) Flow_ast.MatchPattern.MemberPattern.t ->
  ('loc, 'loc) Flow_ast.Expression.t * ('loc, 'loc) Flow_ast.Identifier.t

val get_inferred_type_guard_candidate :
  ('l, 't) Flow_ast.Function.Params.t ->
  ('l, 't) Flow_ast.Function.body ->
  ('l, 't) Flow_ast.Function.ReturnAnnot.t ->
  ('t * string) option
