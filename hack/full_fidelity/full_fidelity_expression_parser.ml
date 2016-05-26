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

open TokenKind
open Syntax

module WithStatementParser
  (StatementParser : Full_fidelity_statement_parser_type.StatementParserType) :
  Full_fidelity_expression_parser_type.ExpressionParserType = struct

  type t = {
    lexer : Lexer.t;
    precedence : int;
    errors : SyntaxError.t list
  }

  let make lexer errors =
    { lexer; errors; precedence = 0 }

  let errors parser =
    parser.errors

  let lexer parser =
    parser.lexer

  let with_error parser message =
    (* TODO: Should be able to express errors on whole syntax node. *)
    (* TODO: Is this even right? Won't this put the error on the trivia? *)
    let start_offset = Lexer.start_offset parser.lexer in
    let end_offset = Lexer.end_offset parser.lexer in
    let error = SyntaxError.make start_offset end_offset message in
    { parser with errors = error :: parser.errors }

  let with_precedence parser precedence =
    { parser with precedence }

  let next_token parser =
    let (lexer, token) = Lexer.next_token parser.lexer in
    let parser = { parser with lexer } in
    (parser, token)

  let peek_token parser =
    let (_, token) = Lexer.next_token parser.lexer in
    token

  let expect_token parser kind error =
    let (parser1, token) = next_token parser in
    if (Token.kind token) = kind then
      (parser1, make_token token)
    else
      (* ERROR RECOVERY: Create a missing token for the expected token,
         and continue on from the current token. Don't skip it. *)
      (with_error parser error, (make_missing()))

  let rec parse_expression parser =
    let (parser, term) = parse_term parser in
    parse_remaining_expression parser term

  and parse_term parser =
    let (parser1, token) = next_token parser in
    match (Token.kind token) with
    | DecimalLiteral
    | OctalLiteral
    | HexadecimalLiteral
    | BinaryLiteral
    | FloatingLiteral
    | SingleQuotedStringLiteral
    | HeredocStringLiteral (*TODO: Special? *)
    | NowdocStringLiteral (* TODO: Special? *)
    | BooleanLiteral -> (parser1, make_literal_expression (make_token token))
    | NullLiteral ->
      (* TODO: Something special about null *)
      (parser1, make_literal_expression (make_token token))

    | DoubleQuotedStringLiteral
      (* TODO: Parse interior *)
      -> (parser1, make_literal_expression (make_token token))

    | Variable ->
        (parser1, make_variable_expression (make_token token))

    | Name
    | QualifiedName ->
        (parser1, make_qualified_name_expression (make_token token))

    | Exclamation
    | PlusPlus
    | MinusMinus
    | Tilde
    | Minus
    | Plus
    | Ampersand
    | Await
    | Yield
    | Clone
    | At ->
      parse_prefix_unary_expression parser

    | LeftParen ->
      parse_parenthesized_or_lambda_expression parser

    | LessThan -> (* TODO: XHP *)
      (parser1, make_token token)
    | Class -> (* TODO When is this legal? *)
      (parser1, make_token token)


    | Shape (* TODO: Parse shape *)
    | New  (* TODO: Parse object creation *)
    | Async (* TODO: Parse lambda *)
    | Function  (* TODO: Parse local function *)


    | LeftBracket (* TODO: ? *)
    | Dollar (* TODO: ? *)
    | DollarDollar (* TODO: ? *)


    (* TODO: Array *)
    (* TODO: Collections *)
    (* TODO: List *)
    (* TODO: imports *)
    (* TODO: non-lowercased true false null array *)
    (* TODO: Unsafe *)
    (* TODO: What keywords are legal as names? *)

    | Final | Abstract | Interface | Trait ->
      (* TODO: Error *)
     (parser1, make_token token)
    | EndOfFile
    | _ ->
      (* TODO: Error, expected expression *)
      (parser1, make_token token)

  and parse_remaining_expression parser term =
    let (parser1, token) = next_token parser in
    match (Token.kind token) with
    (* Binary operators *)
    | Plus
    | Minus
    | Star
    | Slash
    | StarStar
    | Equal
    | BarEqual
    | PlusEqual
    | StarEqual
    | SlashEqual
    | DotEqual
    | MinusEqual
    | PercentEqual
    | CaratEqual
    | AmpersandEqual
    | LessThanLessThanEqual
    | GreaterThanGreaterThanEqual
    | EqualEqualEqual
    | GreaterThan
    | Percent
    | Dot
    | EqualEqual
    | AmpersandAmpersand
    | BarBar
    | ExclamationEqual
    | LessThan
    | ExclamationEqualEqual
    | LessThanEqual
    | GreaterThanEqual
    | Ampersand
    | Bar
    | LessThanLessThan
    | GreaterThanGreaterThan
    | Carat
    | BarGreaterThan
    | MinusGreaterThan
    | QuestionMinusGreaterThan
    | ColonColon
    | QuestionQuestion
    | Instanceof ->
    (* TODO: "and" "or" "xor" *)
      parse_remaining_binary_operator parser term

    | EqualEqualGreaterThan ->
      (* TODO parse lambda *)
      (parser1, make_token token)
    | PlusPlus
    | MinusMinus ->
      parse_postfix_unary parser term

    | LeftParen (* TODO: Parse call *)
    | LeftBracket
    | LeftBrace -> (* TODO indexers *) (* TODO: Produce an error for brace *)
    (parser1, make_token token)
    | Question -> (* TODO conditional expression *)
      (parser1, make_token token)
    | _ -> (parser, term)

  and parse_parenthesized_or_lambda_expression parser =
    (*TODO: Does not yet deal with lambdas *)
    let (parser, left_paren) = next_token parser in
    let precedence = parser.precedence in
    let parser = with_precedence parser 0 in
    let (parser, expression) = parse_expression parser in
    let (parser, right_paren) =
      expect_token parser RightParen SyntaxError.error1011 in
    let parser = with_precedence parser precedence in
    let syntax =
      make_parenthesized_expression
        (make_token left_paren) expression right_paren in
    (parser, syntax)

  and parse_postfix_unary parser term =
    let (parser, token) = next_token parser in
    let term = make_postfix_unary_operator (make_token token) term in
    parse_remaining_expression parser term

  and parse_prefix_unary_expression parser =
    (* TODO: Operand to ++ and -- must be an lvalue. *)
    let (parser1, token) = next_token parser in
    let operator = Operator.prefix_unary_from_token (Token.kind token) in
    let precedence = Operator.precedence operator in
    let parser2 = with_precedence parser1 precedence in
    let (parser3, expression) = parse_expression parser2 in
    let syntax = make_prefix_unary_operator (make_token token) expression in
    let parser4 = with_precedence parser3 parser.precedence in
    (parser4, syntax)

  and parse_remaining_binary_operator parser left_term =
    (* We have a left term. If we get here then we know that
     * we have a binary operator to its right.
     *
     * Here's how this works.  Suppose we have something like
     *
     *     A x B y C
     *
     * where A, B and C are terms, and x and y are operators.
     * We must determine whether this parses as
     *
     *     (A x B) y C
     *
     * or
     *
     *     A x (B y C)
     *
     * We have the former if either x is higher precedence than y,
     * or x and y are the same precedence and x is left associative.
     * Otherwise, if x is lower precedence than y, or x is right
     * associative, then we have the latter.
     *
     * How are we going to figure this out?
     *
     * We have the term A in hand; the precedence is zero.
     * We see that x follows A.
     * We obtain the precedence of x. It is greater than zero,
     * so we obtain B, and then we call a helper method that
     * collects together everything to the right of B that is
     * of higher precedence than x. (Or equal, and right-associative.)
     *
     * So, if x is of lower precedence than y (or equal and right-assoc)
     * then the helper will construct (B y C) as the right term, and then
     * we'll make A x (B y C), and we're done.  Otherwise, the helper
     * will simply return B, we'll construct (A x B) and recurse with that
     * as the left term.
     *)

      let (parser1, token) = next_token parser in
      let operator = Operator.trailing_from_token (Token.kind token) in
      let precedence = Operator.precedence operator in
      if precedence < parser.precedence then
        (parser, left_term)
      else
        let (parser2, right_term) = parse_term parser1 in
        let (parser2, right_term) = parse_remaining_binary_operator_helper
          parser2 right_term precedence in
        let term =
          make_binary_operator left_term (make_token token) right_term in
        parse_remaining_expression parser2 term

  and parse_remaining_binary_operator_helper
      parser right_term left_precedence =
    (* This gathers up terms to the right of an operator that are
       operands of operators of higher precedence than the
       operator to the left. For instance, if we have
       A + B * C / D + E and we just parsed A +, then we want to
       gather up B * C / D into the right side of the +.
       In this case "right term" would be B and "left precedence"
       would be the precedence of +.
       See comments above for more details. *)
    let token = peek_token parser in
    if Operator.is_trailing_operator_token (Token.kind token) then
      let right_operator = Operator.trailing_from_token (Token.kind token) in
      let right_precedence = Operator.precedence right_operator in
      let associativity = Operator.associativity right_operator in
      if right_precedence > left_precedence ||
        (associativity = Operator.RightAssociative &&
          right_precedence = left_precedence ) then
        let parser1 = with_precedence parser right_precedence in
        let (parser2, right_term) =
          parse_remaining_expression parser1 right_term in
        let parser3 = with_precedence parser2 parser.precedence in
        parse_remaining_binary_operator_helper
          parser3 right_term left_precedence
      else
        (parser, right_term)
    else
      (parser, right_term)

end
