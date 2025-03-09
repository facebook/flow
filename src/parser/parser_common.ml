(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Parser_env
open Flow_ast

type pattern_errors = {
  if_expr: (Loc.t * Parse_error.t) list;
  if_patt: (Loc.t * Parse_error.t) list;
}

type pattern_cover =
  | Cover_expr of (Loc.t, Loc.t) Expression.t
  | Cover_patt of (Loc.t, Loc.t) Expression.t * pattern_errors

module type PARSER = sig
  val program : env -> (Loc.t, Loc.t) Program.t

  val statement : ?allow_sequence:bool -> env -> (Loc.t, Loc.t) Statement.t

  val statement_list_item :
    ?decorators:(Loc.t, Loc.t) Class.Decorator.t list -> env -> (Loc.t, Loc.t) Statement.t

  val statement_list : term_fn:(Token.t -> bool) -> env -> (Loc.t, Loc.t) Statement.t list

  val statement_list_with_directives :
    term_fn:(Token.t -> bool) -> env -> (Loc.t, Loc.t) Statement.t list * bool

  val module_body : term_fn:(Token.t -> bool) -> env -> (Loc.t, Loc.t) Statement.t list

  val expression : env -> (Loc.t, Loc.t) Expression.t

  val expression_or_pattern : env -> pattern_cover

  val conditional : env -> (Loc.t, Loc.t) Expression.t

  val assignment : env -> (Loc.t, Loc.t) Expression.t

  val left_hand_side : env -> (Loc.t, Loc.t) Expression.t

  val object_initializer : env -> Loc.t * (Loc.t, Loc.t) Expression.Object.t * pattern_errors

  val identifier : ?restricted_error:Parse_error.t -> env -> (Loc.t, Loc.t) Identifier.t

  val identifier_with_type :
    env -> ?no_optional:bool -> Parse_error.t -> Loc.t * (Loc.t, Loc.t) Pattern.Identifier.t

  val block_body : env -> Loc.t * (Loc.t, Loc.t) Statement.Block.t

  val function_block_body :
    expression:bool -> env -> (Loc.t * (Loc.t, Loc.t) Statement.Block.t) * bool

  val jsx_element_or_fragment :
    env ->
    Loc.t * [ `Element of (Loc.t, Loc.t) JSX.element | `Fragment of (Loc.t, Loc.t) JSX.fragment ]

  val pattern : env -> Parse_error.t -> (Loc.t, Loc.t) Pattern.t

  val pattern_from_expr : env -> (Loc.t, Loc.t) Expression.t -> (Loc.t, Loc.t) Pattern.t

  val object_key : ?class_body:bool -> env -> Loc.t * (Loc.t, Loc.t) Expression.Object.Property.key

  val class_declaration : env -> (Loc.t, Loc.t) Class.Decorator.t list -> (Loc.t, Loc.t) Statement.t

  val class_expression : env -> (Loc.t, Loc.t) Expression.t

  val is_assignable_lhs : (Loc.t, Loc.t) Expression.t -> bool

  val number : env -> Token.number_type -> string -> float

  val annot : env -> (Loc.t, Loc.t) Type.annotation

  val bigint : env -> Token.bigint_type -> string -> int64 option

  val match_pattern : env -> (Loc.t, Loc.t) MatchPattern.t
end

module type TYPE = sig
  val _type : env -> (Loc.t, Loc.t) Type.t

  val type_identifier : env -> (Loc.t, Loc.t) Identifier.t

  val type_params : env -> (Loc.t, Loc.t) Type.TypeParams.t option

  val type_args : env -> (Loc.t, Loc.t) Type.TypeArgs.t option

  val generic : env -> Loc.t * (Loc.t, Loc.t) Type.Generic.t

  val _object : is_class:bool -> env -> Loc.t * (Loc.t, Loc.t) Type.Object.t

  val interface_helper :
    env -> (Loc.t * (Loc.t, Loc.t) Type.Generic.t) list * (Loc.t * (Loc.t, Loc.t) Type.Object.t)

  val function_param_list : env -> (Loc.t, Loc.t) Type.Function.Params.t

  val component_param_list : env -> (Loc.t, Loc.t) Type.Component.Params.t

  val annotation : env -> (Loc.t, Loc.t) Type.annotation

  val annotation_opt : env -> (Loc.t, Loc.t) Type.annotation_or_hint

  val renders_annotation_opt : env -> (Loc.t, Loc.t) Type.component_renders_annotation

  val function_return_annotation_opt : env -> (Loc.t, Loc.t) Function.ReturnAnnot.t

  val predicate_opt : env -> (Loc.t, Loc.t) Type.Predicate.t option

  val function_return_annotation_and_predicate_opt :
    env -> (Loc.t, Loc.t) Function.ReturnAnnot.t * (Loc.t, Loc.t) Type.Predicate.t option

  val type_guard : env -> (Loc.t, Loc.t) Type.TypeGuard.t
end

module type COVER = sig
  val as_expression : env -> pattern_cover -> (Loc.t, Loc.t) Expression.t

  val as_pattern : ?err:Parse_error.t -> env -> pattern_cover -> (Loc.t, Loc.t) Pattern.t

  val empty_errors : pattern_errors

  val cons_error : Loc.t * Parse_error.t -> pattern_errors -> pattern_errors

  val rev_append_errors : pattern_errors -> pattern_errors -> pattern_errors

  val rev_errors : pattern_errors -> pattern_errors
end

module type PATTERN = sig
  val from_expr : Parser_env.env -> (Loc.t, Loc.t) Expression.t -> (Loc.t, Loc.t) Pattern.t

  val pattern : Parser_env.env -> Parse_error.t -> (Loc.t, Loc.t) Pattern.t
end

module type OBJECT = sig
  val key : ?class_body:bool -> env -> Loc.t * (Loc.t, Loc.t) Expression.Object.Property.key

  val _initializer : env -> Loc.t * (Loc.t, Loc.t) Expression.Object.t * pattern_errors

  val class_declaration : env -> (Loc.t, Loc.t) Class.Decorator.t list -> (Loc.t, Loc.t) Statement.t

  val class_expression : env -> (Loc.t, Loc.t) Expression.t

  val class_implements : env -> attach_leading:bool -> (Loc.t, Loc.t) Class.Implements.t

  val decorator_list : env -> (Loc.t, Loc.t) Class.Decorator.t list
end

module type JSX = sig
  val element_or_fragment :
    parent_opening_name:(Loc.t, Loc.t) JSX.name option ->
    env ->
    Loc.t * [ `Element of (Loc.t, Loc.t) JSX.element | `Fragment of (Loc.t, Loc.t) JSX.fragment ]
end

module type EXPRESSION = sig
  val arguments : env -> (Loc.t, Loc.t) Expression.ArgList.t

  val assignment : env -> (Loc.t, Loc.t) Expression.t

  val assignment_cover : env -> pattern_cover

  val conditional : env -> (Loc.t, Loc.t) Expression.t

  val is_assignable_lhs : (Loc.t, Loc.t) Expression.t -> bool

  val left_hand_side : env -> (Loc.t, Loc.t) Expression.t

  val number : env -> Token.number_type -> string -> float

  val bigint : env -> Token.bigint_type -> string -> int64 option

  val sequence :
    env -> start_loc:Loc.t -> (Loc.t, Loc.t) Expression.t list -> (Loc.t, Loc.t) Expression.t

  val call_type_args : env -> (Loc.t, Loc.t) Expression.CallTypeArgs.t option

  val call_cover :
    ?allow_optional_chain:bool ->
    ?in_optional_chain:bool ->
    env ->
    Loc.t ->
    pattern_cover ->
    pattern_cover
end

module type STATEMENT = sig
  val for_ : env -> (Loc.t, Loc.t) Statement.t

  val if_ : env -> (Loc.t, Loc.t) Statement.t

  val let_ : env -> (Loc.t, Loc.t) Statement.t

  val try_ : env -> (Loc.t, Loc.t) Statement.t

  val while_ : env -> (Loc.t, Loc.t) Statement.t

  val with_ : env -> (Loc.t, Loc.t) Statement.t

  val block : env -> (Loc.t, Loc.t) Statement.t

  val break : env -> (Loc.t, Loc.t) Statement.t

  val continue : env -> (Loc.t, Loc.t) Statement.t

  val debugger : env -> (Loc.t, Loc.t) Statement.t

  val declare : ?in_module_or_namespace:bool -> env -> (Loc.t, Loc.t) Statement.t

  val declare_export_declaration : env -> (Loc.t, Loc.t) Statement.t

  val declare_opaque_type : env -> (Loc.t, Loc.t) Statement.t

  val do_while : env -> (Loc.t, Loc.t) Statement.t

  val empty : env -> (Loc.t, Loc.t) Statement.t

  val export_declaration :
    decorators:(Loc.t, Loc.t) Class.Decorator.t list -> env -> (Loc.t, Loc.t) Statement.t

  val expression : ?allow_sequence:bool -> env -> (Loc.t, Loc.t) Statement.t

  val import_declaration : env -> (Loc.t, Loc.t) Statement.t

  val interface : env -> (Loc.t, Loc.t) Statement.t

  val match_statement : env -> (Loc.t, Loc.t) Statement.t

  val maybe_labeled : env -> (Loc.t, Loc.t) Statement.t

  val opaque_type : env -> (Loc.t, Loc.t) Statement.t

  val return : env -> (Loc.t, Loc.t) Statement.t

  val switch : env -> (Loc.t, Loc.t) Statement.t

  val throw : env -> (Loc.t, Loc.t) Statement.t

  val type_alias : env -> (Loc.t, Loc.t) Statement.t

  val var : env -> (Loc.t, Loc.t) Statement.t

  val const : env -> (Loc.t, Loc.t) Statement.t
end

module type DECLARATION = sig
  val async : env -> bool * Loc.t Comment.t list

  val generator : env -> bool * Loc.t Comment.t list

  val variance : env -> parse_readonly:bool -> bool -> bool -> Loc.t Variance.t option

  val function_params : await:bool -> yield:bool -> env -> (Loc.t, Loc.t) Function.Params.t

  val function_body :
    env ->
    async:bool ->
    generator:bool ->
    expression:bool ->
    simple_params:bool ->
    (Loc.t, Loc.t) Function.body * bool

  val check_unique_formal_parameters : env -> (Loc.t, Loc.t) Function.Params.t -> unit

  val check_unique_component_formal_parameters :
    env -> (Loc.t, Loc.t) Statement.ComponentDeclaration.Params.t -> unit

  val strict_function_post_check :
    env ->
    contains_use_strict:bool ->
    (Loc.t, Loc.t) Identifier.t option ->
    (Loc.t, Loc.t) Function.Params.t ->
    unit

  val strict_component_post_check :
    env ->
    contains_use_strict:bool ->
    (Loc.t, Loc.t) Identifier.t ->
    (Loc.t, Loc.t) Statement.ComponentDeclaration.Params.t ->
    unit

  val let_ :
    env ->
    (Loc.t, Loc.t) Statement.VariableDeclaration.Declarator.t list
    * Loc.t Comment.t list
    * (Loc.t * Parse_error.t) list

  val const :
    env ->
    (Loc.t, Loc.t) Statement.VariableDeclaration.Declarator.t list
    * Loc.t Comment.t list
    * (Loc.t * Parse_error.t) list

  val var :
    env ->
    (Loc.t, Loc.t) Statement.VariableDeclaration.Declarator.t list
    * Loc.t Comment.t list
    * (Loc.t * Parse_error.t) list

  val _function : env -> (Loc.t, Loc.t) Statement.t

  val enum_declaration : ?leading:Loc.t Comment.t list -> env -> (Loc.t, Loc.t) Statement.t

  val component : env -> (Loc.t, Loc.t) Statement.t
end

module type MATCH_PATTERN = sig
  val match_pattern : env -> (Loc.t, Loc.t) MatchPattern.t
end

let identifier_name_raw env =
  let open Token in
  let name =
    match Peek.token env with
    (* obviously, Identifier is a valid IdentifierName *)
    | T_IDENTIFIER { value; _ } -> value
    (* keywords are also IdentifierNames *)
    | T_AWAIT -> "await"
    | T_BREAK -> "break"
    | T_CASE -> "case"
    | T_CATCH -> "catch"
    | T_CLASS -> "class"
    | T_CONST -> "const"
    | T_CONTINUE -> "continue"
    | T_DEBUGGER -> "debugger"
    | T_DEFAULT -> "default"
    | T_DELETE -> "delete"
    | T_DO -> "do"
    | T_ELSE -> "else"
    | T_EXPORT -> "export"
    | T_EXTENDS -> "extends"
    | T_FINALLY -> "finally"
    | T_FOR -> "for"
    | T_FUNCTION -> "function"
    | T_IF -> "if"
    | T_IMPORT -> "import"
    | T_IN -> "in"
    | T_INSTANCEOF -> "instanceof"
    | T_NEW -> "new"
    | T_RETURN -> "return"
    | T_SUPER -> "super"
    | T_SWITCH -> "switch"
    | T_THIS -> "this"
    | T_THROW -> "throw"
    | T_TRY -> "try"
    | T_TYPEOF -> "typeof"
    | T_VAR -> "var"
    | T_VOID -> "void"
    | T_WHILE -> "while"
    | T_WITH -> "with"
    | T_YIELD -> "yield"
    (* FutureReservedWord *)
    | T_ENUM -> "enum"
    | T_LET -> "let"
    | T_STATIC -> "static"
    | T_INTERFACE -> "interface"
    | T_IMPLEMENTS -> "implements"
    | T_PACKAGE -> "package"
    | T_PRIVATE -> "private"
    | T_PROTECTED -> "protected"
    | T_PUBLIC -> "public"
    (* NullLiteral *)
    | T_NULL -> "null"
    (* BooleanLiteral *)
    | T_TRUE -> "true"
    | T_FALSE -> "false"
    (* Flow-specific stuff *)
    | T_ASSERTS -> "asserts"
    | T_IMPLIES -> "implies"
    | T_IS -> "is"
    | T_DECLARE -> "declare"
    | T_TYPE -> "type"
    | T_OPAQUE -> "opaque"
    | T_ANY_TYPE -> "any"
    | T_MATCH -> "match"
    | T_MIXED_TYPE -> "mixed"
    | T_EMPTY_TYPE -> "empty"
    | T_BOOLEAN_TYPE BOOL -> "bool"
    | T_BOOLEAN_TYPE BOOLEAN -> "boolean"
    | T_NUMBER_TYPE -> "number"
    | T_BIGINT_TYPE -> "bigint"
    | T_STRING_TYPE -> "string"
    | T_VOID_TYPE -> "void"
    | T_SYMBOL_TYPE -> "symbol"
    | T_UNKNOWN_TYPE -> "unknown"
    | T_NEVER_TYPE -> "never"
    | T_UNDEFINED_TYPE -> "undefined"
    | T_KEYOF -> "keyof"
    | T_READONLY -> "readonly"
    (* Contextual stuff *)
    | T_OF -> "of"
    | T_ASYNC -> "async"
    (* punctuators, types, literals, etc are not identifiers *)
    | _ ->
      error_unexpected ~expected:"an identifier" env;
      ""
  in
  Eat.token env;
  name

(* IdentifierName - https://tc39.github.io/ecma262/#prod-IdentifierName *)
let identifier_name env =
  let loc = Peek.loc env in
  let leading = Peek.comments env in
  let name = identifier_name_raw env in
  let trailing = Eat.trailing_comments env in
  let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
  (loc, { Identifier.name; comments })

(** PrivateIdentifier - https://tc39.es/ecma262/#prod-PrivateIdentifier

    N.B.: whitespace, line terminators, and comments are not allowed
    between the # and IdentifierName because PrivateIdentifier is a
    CommonToken which is considered a single token. See also
    https://tc39.es/ecma262/#prod-InputElementDiv *)
let private_identifier env =
  let start_loc = Peek.loc env in
  let leading = Peek.comments env in
  Expect.token env Token.T_POUND;
  let name_loc = Peek.loc env in
  let name = identifier_name_raw env in
  let trailing = Eat.trailing_comments env in
  let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
  let loc = Loc.btwn start_loc name_loc in
  if not (Loc.equal_position start_loc.Loc._end name_loc.Loc.start) then
    error_at env (loc, Parse_error.WhitespaceInPrivateName);
  (loc, { PrivateName.name; comments })

(** The operation IsSimpleParamterList
    https://tc39.es/ecma262/#sec-static-semantics-issimpleparameterlist *)
let is_simple_parameter_list =
  let is_simple_param = function
    | (_, { Flow_ast.Function.Param.argument = (_, Pattern.Identifier _); default = None }) -> true
    | _ -> false
  in
  fun (_, { Flow_ast.Function.Params.params; rest; comments = _; this_ = _ }) ->
    rest = None && List.for_all is_simple_param params

(**
 * The abstract operation IsLabelledFunction
 *
 * https://tc39.github.io/ecma262/#sec-islabelledfunction
 *)
let rec is_labelled_function = function
  | (_, Flow_ast.Statement.Labeled { Flow_ast.Statement.Labeled.body; _ }) -> begin
    match body with
    | (_, Flow_ast.Statement.FunctionDeclaration _) -> true
    | _ -> is_labelled_function body
  end
  | _ -> false

(** https://tc39.es/ecma262/#sec-exports-static-semantics-early-errors *)
let assert_identifier_name_is_identifier
    ?restricted_error env (loc, { Flow_ast.Identifier.name; comments = _ }) =
  match name with
  | "let" when no_let env ->
    error_at env (loc, Parse_error.Unexpected (Token.quote_token_value name))
  | "await" ->
    (* `allow_await` means that `await` is allowed to be a keyword,
       which makes it illegal to use as an identifier.
       https://tc39.github.io/ecma262/#sec-identifiers-static-semantics-early-errors *)
    if allow_await env then error_at env (loc, Parse_error.AwaitAsIdentifierReference)
  | "yield" ->
    (* `allow_yield` means that `yield` is allowed to be a keyword,
       which makes it illegal to use as an identifier.
       https://tc39.github.io/ecma262/#sec-identifiers-static-semantics-early-errors *)
    if allow_yield env then
      error_at env (loc, Parse_error.UnexpectedReserved)
    else
      strict_error_at env (loc, Parse_error.StrictReservedWord)
  | _ when is_strict_reserved name -> strict_error_at env (loc, Parse_error.StrictReservedWord)
  | _ when is_reserved name -> error_at env (loc, Parse_error.UnexpectedReserved)
  | _ -> begin
    match restricted_error with
    | Some err when is_restricted name -> strict_error_at env (loc, err)
    | _ -> ()
  end

let with_loc ?start_loc fn env =
  let start_loc =
    match start_loc with
    | Some x -> x
    | None -> Peek.loc env
  in
  let result = fn env in
  let loc =
    match last_loc env with
    | Some end_loc -> Loc.btwn start_loc end_loc
    | None -> start_loc
  in
  (loc, result)

let with_loc_opt ?start_loc fn env =
  match with_loc ?start_loc fn env with
  | (loc, Some x) -> Some (loc, x)
  | (_, None) -> None

let with_loc_extra ?start_loc fn env =
  let (loc, (x, extra)) = with_loc ?start_loc fn env in
  ((loc, x), extra)

let is_start_of_type_guard env =
  let open Token in
  (* Parse the identifier part as normal code, since this can be any name that
   * a parameter can be. *)
  Eat.push_lex_mode env Lex_mode.NORMAL;
  let token_1 = Peek.token env in
  Eat.pop_lex_mode env;
  let token_2 = Peek.ith_token ~i:1 env in
  match (token_1, token_2) with
  | (T_IDENTIFIER { raw = "asserts"; _ }, (T_IDENTIFIER _ | T_THIS))
  | (T_IDENTIFIER { raw = "implies"; _ }, (T_IDENTIFIER _ | T_THIS))
  | ((T_IDENTIFIER _ | T_THIS), (T_IS | T_IDENTIFIER { raw = "is"; _ })) ->
    true
  | _ -> false

let reparse_arguments_as_match_argument env (args_loc, args) =
  let { Expression.ArgList.arguments; _ } = args in
  if List.is_empty arguments then Parser_env.error_at env (args_loc, Parse_error.MatchEmptyArgument);
  let filtered_args =
    List.filter_map
      (function
        | Expression.Spread (loc, _) ->
          Parser_env.error_at env (loc, Parse_error.MatchSpreadArgument);
          None
        | Expression.Expression e -> Some e)
      arguments
  in
  match filtered_args with
  | [expr] -> expr
  | expressions ->
    (args_loc, Expression.Sequence { Expression.Sequence.expressions; comments = None })
