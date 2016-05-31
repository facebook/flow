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

open TokenKind
open Syntax

module WithExpressionParser(ExpressionParser :
  Full_fidelity_expression_parser_type.ExpressionParserType) :
  Full_fidelity_statement_parser_type.StatementParserType = struct

  type t = {
    lexer : Lexer.t;
    errors : SyntaxError.t list
  }

  let make lexer errors =
    { lexer; errors }

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
      (with_error parser error, (make_missing()))

  let assert_token parser kind =
    let (parser, token) = next_token parser in
    assert ((Token.kind token) = kind);
    (parser, make_token token)


  let rec parse_statement parser =
    let token = peek_token parser in
    match (Token.kind token) with
    | For -> parse_for_statement parser
    | Foreach -> parse_foreach_statement parser
    | Do -> parse_do_statement parser
    | While -> parse_while_statement parser
    | If -> parse_if_statement parser
    | Switch -> parse_switch_statement parser
    | Try -> parse_try_statement parser
    | Break -> parse_break_statement parser
    | Continue -> parse_continue_statement parser
    | Return -> parse_return_statement parser
    | Throw -> parse_throw_statement parser
    (* TODO: Only valid inside switch *)
    | Default -> parse_default_label_statement parser
    (* TODO: Only valid inside switch *)
    | Case -> parse_case_label_statement parser
    | LeftBrace -> parse_compound_statement parser
    | Static -> parse_function_static_declaration parser
    | _ -> parse_expression_statement parser

  (* Helper: parses ( expr ) *)
  and parse_paren_expr parser =
    let (parser, left_paren) =
      expect_token parser LeftParen SyntaxError.error1019 in
    let (parser, expr_syntax) = parse_expression parser in
    let (parser, right_paren) =
      expect_token parser RightParen SyntaxError.error1011 in
    (parser, left_paren, expr_syntax, right_paren)

  and parse_for_statement parser =
    (* TODO *)
    let (parser, token) = next_token parser in
    (parser, make_error [make_token token])

  and parse_foreach_statement parser =
    (* TODO *)
    let (parser, token) = next_token parser in
    (parser, make_error [make_token token])

  and parse_do_statement parser =
    let (parser, do_keyword_token) =
      assert_token parser Do in
    let (parser, statement_node) =
      parse_statement parser in
    let (parser, do_while_keyword_token) =
      expect_token parser While SyntaxError.error1018 in
    let (parser, left_paren_token, expr_node, right_paren_token) =
      parse_paren_expr parser in
    let (parser, do_semicolon_token) =
      expect_token parser Semicolon SyntaxError.error1010 in
    let syntax = make_do_statement do_keyword_token statement_node
      do_while_keyword_token left_paren_token expr_node right_paren_token
      do_semicolon_token in
    (parser, syntax)

  and parse_while_statement parser =
    let (parser, while_keyword_token) =
      assert_token parser While in
    let (parser, left_paren_token, expr_node, right_paren_token) =
      parse_paren_expr parser in
    let (parser, statement_node) =
      parse_statement parser in
    let syntax = make_while_statement while_keyword_token left_paren_token
      expr_node right_paren_token statement_node in
    (parser, syntax)

  and parse_if_statement parser =
    (* parses the "( expr ) statement" segment of If, Elseif or Else clauses.
     * Return a tuple of 5 elements, the first one being the resultant parser
     *)
    let parse_if_body_helper parser_body =
      let (parser_body, left_paren_token, expr_node, right_paren_token) =
        parse_paren_expr parser_body in
      let (parser_body, statement_node) = parse_statement parser_body in
        (parser_body, left_paren_token, expr_node, right_paren_token,
        statement_node)
    in
    (* Parse a elseif clause. Do not eat token and return Missing if the
     * first keyword is not elseif
     *)
    let parse_elseif_opt parser_elseif =
      let (parser_elseif, elseif_token) = optional_token parser_elseif Elseif in
      match syntax elseif_token with
      |Missing -> (parser_elseif, elseif_token)  (* return original parser *)
      |_ ->
        let (parser_elseif, elseif_left_paren, elseif_condition_expr,
          elseif_right_paren, elseif_statement) =
          parse_if_body_helper parser_elseif in
        let elseif_syntax = make_elseif_clause elseif_token elseif_left_paren
          elseif_condition_expr elseif_right_paren elseif_statement in
        (parser_elseif, elseif_syntax)
    in
    (* do not eat token and return Missing if first token is not Else *)
    let parse_else_opt parser_else =
      let (parser_else, else_token) = optional_token parser_else Else in
      match syntax else_token with
      |Missing ->(parser_else, else_token)
      |_ ->
        let (parser_else, else_consequence) = parse_statement parser_else in
        let else_syntax = make_else_clause else_token else_consequence in
        (parser_else, else_syntax)
    in
    (* keep parsing until there is no else if clause
     * return the new parser and a syntax list of all else if statements
     * return Missing if there is no Elseif, with parser unchanged
     *)
    let parse_elseif_clauses parser_elseif =
      let rec parse_clauses_helper acc parser_elseif =
        let (parser_elseif, elseif_syntax) = parse_elseif_opt parser_elseif in
        match (syntax elseif_syntax, acc) with
        |Missing, [] -> (parser_elseif, elseif_syntax)
        |Missing, _ -> (parser_elseif, make_list (List.rev acc))
        |_, _ -> parse_clauses_helper (elseif_syntax :: acc) parser_elseif
      in
      parse_clauses_helper [] parser_elseif
    in
    let (parser, if_keyword_token) = assert_token parser If in
    let (parser, if_left_paren, if_expr, if_right_paren, if_consequence) =
      parse_if_body_helper parser in
    let (parser, elseif_syntax) = parse_elseif_clauses parser in
    let (parser, else_syntax) = parse_else_opt parser in
    let syntax = make_if_statement if_keyword_token if_left_paren if_expr
      if_right_paren if_consequence elseif_syntax else_syntax in
    (parser, syntax)
  and parse_switch_statement parser =
    let (parser, switch_keyword_token) =
      assert_token parser Switch in
    let (parser, left_paren_token, expr_node, right_paren_token) =
      parse_paren_expr parser in
    let (parser, statement_node) =
      parse_compound_statement parser in
    let syntax = make_switch_statement switch_keyword_token left_paren_token
      expr_node right_paren_token statement_node in
    (parser, syntax)

  and parse_try_statement parser =
    (* TODO *)
    let (parser, token) = next_token parser in
    (parser, make_error [make_token token])

  and parse_break_statement parser =
    (* TODO *)
    let (parser, token) = next_token parser in
    (parser, make_error [make_token token])

  and parse_continue_statement parser =
    (* TODO *)
    let (parser, token) = next_token parser in
    (parser, make_error [make_token token])

  and parse_return_statement parser =
    (* TODO *)
    let (parser, token) = next_token parser in
    (parser, make_error [make_token token])

  and parse_throw_statement parser =
    (* TODO *)
    let (parser, token) = next_token parser in
    (parser, make_error [make_token token])

  and parse_default_label_statement parser =
    (* TODO *)
    let (parser, token) = next_token parser in
    (parser, make_error [make_token token])

  and parse_case_label_statement parser =
    (* TODO *)
    let (parser, token) = next_token parser in
    (parser, make_error [make_token token])

  and parse_function_static_declaration parser =
    (* TODO *)
    let (parser, token) = next_token parser in
    (parser, make_error [make_token token])

  and parse_expression_statement parser =
    let (parser, expression) = parse_expression parser in
    let (parser, token) = expect_token parser Semicolon SyntaxError.error1010 in
    (parser, make_expression_statement expression token)

  and parse_statement_list_opt parser =
     let rec aux parser acc =
       let token = peek_token parser in
       match (Token.kind token) with
       | RightBrace
       | EndOfFile -> (parser, acc)
       | _ ->
         let (parser, statement) = parse_statement parser in
         aux parser (statement :: acc) in
     let (parser, statements) = aux parser [] in
     let statements = List.rev statements in
     (parser, make_list statements)

  and parse_compound_statement parser =
    let (parser, left_brace_token) =
      expect_token parser LeftBrace SyntaxError.error1005 in
    let (parser, statement_list) = parse_statement_list_opt parser in
    let (parser, right_brace_token) =
      expect_token parser RightBrace SyntaxError.error1006 in
    let syntax = make_compound_statement
      left_brace_token statement_list right_brace_token in
    (parser, syntax)

  and parse_expression parser =
    let expression_parser = ExpressionParser.make parser.lexer parser.errors in
    let (expression_parser, node) =
      ExpressionParser.parse_expression expression_parser in
    let lexer = ExpressionParser.lexer expression_parser in
    let errors = ExpressionParser.errors expression_parser in
    let parser = make lexer errors in
    (parser, node)

end
