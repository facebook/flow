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

(* IdentifierName: https://tc39.github.io/ecma262/#prod-IdentifierName *)
let identifier_name token =
  let open Token in
  match token with
  (* obviously, Identifier is a valid IdentifierName *)
  | T_IDENTIFIER id -> Some id
  (* keywords are also IdentifierNames *)
  | T_AWAIT -> Some "await"
  | T_BREAK -> Some "break"
  | T_CASE -> Some "case"
  | T_CATCH -> Some "catch"
  | T_CLASS -> Some "class"
  | T_CONST -> Some "const"
  | T_CONTINUE -> Some "continue"
  | T_DEBUGGER -> Some "debugger"
  | T_DEFAULT -> Some "default"
  | T_DELETE -> Some "delete"
  | T_DO -> Some "do"
  | T_ELSE -> Some "else"
  | T_EXPORT -> Some "export"
  | T_EXTENDS -> Some "extends"
  | T_FINALLY -> Some "finally"
  | T_FOR -> Some "for"
  | T_FUNCTION -> Some "function"
  | T_IF -> Some "if"
  | T_IMPORT -> Some "import"
  | T_IN -> Some "in"
  | T_INSTANCEOF -> Some "instanceof"
  | T_NEW -> Some "new"
  | T_RETURN -> Some "return"
  | T_SUPER -> Some "super"
  | T_SWITCH -> Some "switch"
  | T_THIS -> Some "this"
  | T_THROW -> Some "throw"
  | T_TRY -> Some "try"
  | T_TYPEOF -> Some "typeof"
  | T_VAR -> Some "var"
  | T_VOID -> Some "void"
  | T_WHILE -> Some "while"
  | T_WITH -> Some "with"
  | T_YIELD -> Some "yield"
  (* FutureReservedWord *)
  | T_ENUM -> Some "enum"
  | T_LET -> Some "let"
  | T_STATIC -> Some "static"
  | T_INTERFACE -> Some "interface"
  | T_IMPLEMENTS -> Some "implements"
  | T_PACKAGE -> Some "package"
  | T_PRIVATE -> Some "private"
  | T_PROTECTED -> Some "protected"
  | T_PUBLIC -> Some "public"
  (* NullLiteral *)
  | T_NULL -> Some "null"
  (* BooleanLiteral *)
  | T_TRUE -> Some "true"
  | T_FALSE -> Some "false"
  (* Flow-specific stuff *)
  | T_DECLARE -> Some "declare"
  | T_TYPE -> Some "type"
  | T_OPAQUE -> Some "opaque"
  | T_ANY_TYPE -> Some "any"
  | T_MIXED_TYPE -> Some "mixed"
  | T_EMPTY_TYPE -> Some "empty"
  | T_BOOLEAN_TYPE BOOL -> Some "bool"
  | T_BOOLEAN_TYPE BOOLEAN -> Some "boolean"
  | T_NUMBER_TYPE -> Some "number"
  | T_STRING_TYPE -> Some "string"
  | T_VOID_TYPE -> Some "void"
  (* Contextual stuff *)
  | T_OF -> Some "of"
  | T_ASYNC -> Some "async"
  (* punctuators, types, literals, etc are not identifiers *)
  | _ -> None

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
