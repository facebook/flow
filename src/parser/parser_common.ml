(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Parser_env
open Ast
module Error = Parse_error

type object_cover =
  | Cover_expr of Expression.t
  | Cover_patt of Expression.t * Loc.t list

module type PARSER = sig
  val program : env -> program
  val statement : env -> Statement.t
  val statement_list_item : ?decorators:Expression.t list -> env -> Statement.t
  val statement_list : term_fn:(Token.t -> bool) -> env -> Statement.t list
  val statement_list_with_directives : term_fn:(Token.t -> bool) -> env -> Statement.t list * bool
  val module_body : term_fn:(Token.t -> bool) -> env -> Statement.t list
  val expression : env -> Expression.t
  val expression_or_pattern : env -> object_cover
  val conditional : env -> Expression.t
  val assignment : env -> Expression.t
  val left_hand_side : env -> Expression.t
  val object_initializer : env -> Loc.t * Expression.Object.t * Loc.t list
  val identifier : ?restricted_error:Error.t -> env -> Identifier.t
  val identifier_or_reserved_keyword : env -> (Identifier.t * (Loc.t * Error.t) option)
  val identifier_with_type : env -> ?no_optional:bool -> Error.t -> Loc.t * Pattern.Identifier.t
  val block_body : env -> Loc.t * Statement.Block.t
  val function_block_body : env -> Loc.t * Statement.Block.t * bool
  val jsx_element : env -> Loc.t * JSX.element
  val pattern : env -> Error.t -> Pattern.t
  val pattern_from_expr : env -> Expression.t -> Pattern.t
  val object_key : ?class_body: bool -> env -> Loc.t * Expression.Object.Property.key
  val class_declaration : env -> Expression.t list -> Statement.t
  val class_expression : env -> Expression.t
  val is_assignable_lhs : Expression.t -> bool
end

(* IdentifierName - https://tc39.github.io/ecma262/#prod-IdentifierName *)
let identifier_name env =
  let open Token in
  let loc = Peek.loc env in
  let name = match Peek.token env with
  (* obviously, Identifier is a valid IdentifierName *)
  | T_IDENTIFIER id -> id
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
  | T_DECLARE -> "declare"
  | T_TYPE -> "type"
  | T_OPAQUE -> "opaque"
  | T_ANY_TYPE -> "any"
  | T_MIXED_TYPE -> "mixed"
  | T_EMPTY_TYPE -> "empty"
  | T_BOOLEAN_TYPE BOOL -> "bool"
  | T_BOOLEAN_TYPE BOOLEAN -> "boolean"
  | T_NUMBER_TYPE -> "number"
  | T_STRING_TYPE -> "string"
  | T_VOID_TYPE -> "void"
  (* Contextual stuff *)
  | T_OF -> "of"
  | T_ASYNC -> "async"
  (* punctuators, types, literals, etc are not identifiers *)
  | _ -> error_unexpected env; ""
  in
  Eat.token env;
  loc, name

(**
 * The abstract operation IsLabelledFunction
 *
 * https://tc39.github.io/ecma262/#sec-islabelledfunction
 *)
let rec is_labelled_function = function
  | _, Ast.Statement.Labeled { Ast.Statement.Labeled.body; _ } ->
    begin match body with
    | _, Ast.Statement.FunctionDeclaration _ -> true
    | _ -> is_labelled_function body
    end
  | _ ->
    false

let with_loc fn env =
  let start_loc = Peek.loc env in
  let result = fn env in
  let loc = match last_loc env with
  | Some end_loc -> Loc.btwn start_loc end_loc
  | None -> start_loc
  in
  loc, result
