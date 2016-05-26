(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Token = Full_fidelity_minimal_token
module Syntax = Full_fidelity_minimal_syntax
module SyntaxKind = Full_fidelity_syntax_kind
module TokenKind = Full_fidelity_token_kind
module SourceText = Full_fidelity_source_text
module SyntaxError = Full_fidelity_syntax_error
module Lexer = Full_fidelity_lexer
module Operator = Full_fidelity_operator
module TypeParser = Full_fidelity_type_parser
module rec ExpressionParser :
  Full_fidelity_expression_parser_type.ExpressionParserType =
  Full_fidelity_expression_parser.WithStatementParser(StatementParser)
and StatementParser :
  Full_fidelity_statement_parser_type.StatementParserType =
  Full_fidelity_statement_parser.WithExpressionParser(ExpressionParser)


open TokenKind
open Syntax

type t = {
  lexer : Lexer.t;
  errors : SyntaxError.t list
}

let make text =
  { lexer = Lexer.make text; errors = [] }

let errors parser =
  parser.errors @ (Lexer.errors parser.lexer)

let with_error parser message =
  (* TODO: Should be able to express errors on whole syntax node. *)
  (* TODO: Is this even right? Won't this put the error on the trivia? *)
  let start_offset = Lexer.start_offset parser.lexer in
  let end_offset = Lexer.end_offset parser.lexer in
  let error = SyntaxError.make start_offset end_offset message in
  { parser with errors = error :: parser.errors }

let next_token parser =
  let (lexer, token) = Lexer.next_token parser.lexer in
  let parser = { parser with lexer } in
  (parser, token)

(* let skip_token parser =
  let (lexer, _) = Lexer.next_token parser.lexer in
  let parser = { parser with lexer } in
  parser *)

(* let next_token_as_name parser =
  let (lexer, token) = Lexer.next_token_as_name parser.lexer in
  let parser = { parser with lexer } in
  (parser, token) *)

let peek_token parser =
  let (_, token) = Lexer.next_token parser.lexer in
  token

let optional_token parser kind =
  let (parser1, token) = next_token parser in
  if (Token.kind token) = kind then
    (parser1, make_token token)
  else
    (parser, make_missing())

let expect_token parser kind error =
  let (parser1, token) = next_token parser in
  if (Token.kind token) = kind then
    (parser1, make_token token)
  else
    (* ERROR RECOVERY: Create a missing token for the expected token,
       and continue on from the current token. Don't skip it. *)
    (with_error parser error, make_missing())

(* Parsing *)

(* Types *)

let parse_type_specifier parser =
  let type_parser = TypeParser.make parser.lexer parser.errors in
  let (type_parser, node) = TypeParser.parse_type_specifier type_parser in
  let lexer = TypeParser.lexer type_parser in
  let errors = TypeParser.errors type_parser in
  let parser = { lexer; errors } in
  (parser, node)

(* Expressions *)

let parse_expression parser =
  let expression_parser = ExpressionParser.make parser.lexer parser.errors in
  let (expression_parser, node) =
    ExpressionParser.parse_expression expression_parser in
  let lexer = ExpressionParser.lexer expression_parser in
  let errors = ExpressionParser.errors expression_parser in
  let parser = { lexer; errors } in
  (parser, node)


(* Statements *)

let parse_compound_statement parser =
  let statement_parser = StatementParser.make parser.lexer parser.errors in
  let (statement_parser, node) =
    StatementParser.parse_compound_statement statement_parser in
  let lexer = StatementParser.lexer statement_parser in
  let errors = StatementParser.errors statement_parser in
  let parser = { lexer; errors } in
  (parser, node)


(* Declarations *)

let rec parse_require_multiple_directive parser =
  (* TODO *)
  let (parser, token) = next_token parser in
  (parser, make_error [make_token token])

and parse_require_once_directive parser =
  (* TODO *)
  let (parser, token) = next_token parser in
  (parser, make_error [make_token token])

and parse_type_alias_declaration parser =
  (* TODO *)
  let (parser, token) = next_token parser in
  (parser, make_error [make_token token])

and parse_newtype_alias_declaration parser =
  (* TODO *)
  let (parser, token) = next_token parser in
  (parser, make_error [make_token token])

and parse_enum_declaration parser =
  (* TODO *)
  let (parser, token) = next_token parser in
  (parser, make_error [make_token token])

and parse_interface_declaration parser =
  (* TODO *)
  let (parser, token) = next_token parser in
  (parser, make_error [make_token token])

and parse_namespace_declaration parser =
  (* TODO *)
  let (parser, token) = next_token parser in
  (parser, make_error [make_token token])

and parse_namespace_use_declaration parser =
  (* TODO *)
  let (parser, token) = next_token parser in
  (parser, make_error [make_token token])

and parse_trait_declaration parser =
  (* TODO *)
  let (parser, token) = next_token parser in
  (parser, make_error [make_token token])

and parse_class_declaration parser =
  (* TODO *)
  let (parser, token) = next_token parser in
  (parser, make_error [make_token token])

and parse_attribute_specification_opt parser =
  let (parser1, token) = next_token parser in
  if (Token.kind token) = LessThanLessThan then
    (* TODO *)
    (parser1, make_error [make_token token])
  else
    (parser, make_missing())

and parse_generic_type_parameter_list_opt parser =
  let (parser1, token) = next_token parser in
  if (Token.kind token) = LessThan then
    (* TODO *)
    (parser1, make_error [make_token token])
  else
    (parser, make_missing())

and parse_return_type parser =
  let (parser1, token) = next_token parser in
  if (Token.kind token) = Noreturn then
    (parser1, make_token token)
  else
    parse_type_specifier parser

and parse_return_type_hint_opt parser =
  let (parser1, colon_token) = next_token parser in
  if (Token.kind colon_token) = Colon then
    let (parser2, return_type) = parse_return_type parser1 in
    (parser2, make_token colon_token, return_type)
  else
    (parser, make_missing(), make_missing())

 (* SPEC
    parameter-list:
        ...
        parameter-declaration-list
        parameter-declaration-list  ,  ...

      parameter-declaration-list:
        parameter-declaration
        parameter-declaration-list  ,  parameter-declaration
*)
and parse_parameter_list parser =
  let rec aux parser parameters =
    let (parser, parameter) = parse_parameter parser in
    let parameters = parameter :: parameters in
    let (parser1, token) = next_token parser in
    match (Token.kind token) with
    | Comma ->
      aux parser1 ((make_token token) :: parameters )
    | RightParen ->
      (parser, parameters)
    | EndOfFile
    | _ ->
      (* ERROR RECOVERY TODO: How to recover? *)
      let parser = with_error parser SyntaxError.error1009 in
      (parser, parameters) in
  let (parser, parameters) = aux parser [] in
  (parser, make_list (List.rev parameters))

and parse_parameter_list_opt parser =
  let token = peek_token parser in
  if (Token.kind token) = RightParen then (parser, make_missing())
  else parse_parameter_list parser

and parse_parameter parser =
  let (parser1, token) = next_token parser in
  match (Token.kind token) with
  | DotDotDot ->
    (parser1, make_token token)
  | Comma ->
    (* ERROR RECOVERY *)
    (parser, make_missing())
  | _ ->
    parse_parameter_declaration parser

(* SPEC
  parameter-declaration:
    attribute-specificationopt  type-specifier  variable-name \
    default-argument-specifieropt
*)
and parse_parameter_declaration parser =
  (* TODO: The type specifier is required in strict mode. *)
  let (parser, attrs) = parse_attribute_specification_opt parser in
  let token = peek_token parser in
  let (parser, type_specifier) =
    if (Token.kind token) = Variable then (parser, make_missing())
    else parse_type_specifier parser in
  let (parser, variable_name) =
    expect_token parser Variable SyntaxError.error1008 in
  let (parser, default) = parse_default_argument_specifier_opt parser in
  let syntax =
    make_parameter_declaration attrs type_specifier variable_name default in
  (parser, syntax)

(* SPEC
          default-argument-specifier:
            =  const-expression
*)
and parse_default_argument_specifier_opt parser =
  let (parser1, token) = next_token parser in
  match (Token.kind token) with
  | Equal ->
    (* TODO: Detect if expression is not const *)
    let (parser, default_value) = parse_expression parser1 in
    (parser, make_default_argument_specifier (make_token token) default_value)
  | _ -> (parser, make_missing())

and parse_function_declaration parser =
  let (parser, attribute_specification) =
    parse_attribute_specification_opt parser in
  let (parser, async_token) = optional_token parser Async in
  let (parser, function_token) =
    expect_token parser Function SyntaxError.error1003 in
  let (parser, name) =
    expect_token parser Name SyntaxError.error1004 in
  let (parser, generic_type_parameter_list) =
    parse_generic_type_parameter_list_opt parser in
  let (parser, left_paren_token) =
    expect_token parser LeftParen SyntaxError.error1004 in
  let (parser, parameter_list) = parse_parameter_list_opt parser in
  let (parser, right_paren_token) =
    expect_token parser RightParen SyntaxError.error1004 in
  (* TODO: required in strict mode *)
  let (parser, colon_token, return_type) =
    parse_return_type_hint_opt parser in
  let (parser, body) = parse_compound_statement parser in
  let syntax = make_function attribute_specification async_token
    function_token name generic_type_parameter_list left_paren_token
    parameter_list right_paren_token colon_token return_type body in
  (parser, syntax)

let parse_class_function_interface_or_trait_declaration parser =
  (* TODO *)
  let (parser, token) = next_token parser in
  (parser, make_error [make_token token])

let parse_declaration parser =
  let (parser1, token) = next_token parser in
  match (Token.kind token) with
  | Require -> parse_require_multiple_directive parser
  | Require_once -> parse_require_once_directive parser
  | Type -> parse_type_alias_declaration parser
  | Newtype -> parse_newtype_alias_declaration parser
  | Enum -> parse_enum_declaration parser
  | Interface -> parse_interface_declaration parser
  | Namespace -> parse_namespace_declaration parser
  | Use -> parse_namespace_use_declaration parser
  | Trait -> parse_trait_declaration parser
  | Abstract
  | Final
  | Class -> parse_class_declaration parser
  | Async
  | Function -> parse_function_declaration parser
  | LessThanLessThan ->
    parse_class_function_interface_or_trait_declaration parser
  | _ ->
    (* ERROR RECOVERY: Skip the token, try again. *)
    (* TODO: Better policy would be to skip ahead to
       the first token that makes a legal declaration. *)
    let parser = with_error parser1 SyntaxError.error1002 in
    (parser, make_error [make_token token])

let parse_declarations parser =
  let rec aux parser declarations =
    let token = peek_token parser in
    match (Token.kind token) with
    | EndOfFile -> (parser, declarations)
    (* TODO: ?> tokens *)
    | _ ->
      let (parser, declaration) = parse_declaration parser in
      aux parser (declaration :: declarations) in
  let (parser, declarations) = aux parser [] in
  let syntax = make_list (List.rev declarations) in
  (parser, syntax)

let parse_script_header parser =
  (* TODO: Detect if there is trivia before or after any token. *)
  (* TODO: Detect if there is a strict comment *)
  (* TODO: Detect the language *)

  let (parser1, less_than) = next_token parser in
  let (parser2, question) = next_token parser1 in
  let (parser3, language) = next_token parser2 in
  let valid = (Token.kind less_than) == LessThan &&
              (Token.kind question) == Question &&
              (Token.kind language) == Name in
  if valid then
    let less_than = make_token less_than in
    let question = make_token question in
    let language = make_token language in
    let script_header = make_script_header less_than question language in
    (parser3, script_header)
  else
    (* TODO: Report an error *)
    (* ERROR RECOVERY *)
    (* Make no progress; try parsing the file without a header *)
    let parser = with_error parser SyntaxError.error1001 in
    let less_than = make_token (Token.make LessThan 0 [] []) in
    let question = make_token (Token.make Question 0 [] []) in
    let language = make_token (Token.make Name 0 [] []) in
    let script_header = make_script_header less_than question language in
    (parser, script_header )

let parse_script parser =
  let (parser, script_header) = parse_script_header parser in
  let (parser, declarations) = parse_declarations parser in
  (parser, make_script script_header declarations)
