module Ast = Spider_monkey_ast

open Token

type t = {
  lex_token: Token.t;
  lex_loc: Loc.t;
  lex_value: string;
  lex_errors: (Loc.t * Parse_error.t) list;
  lex_comments: Ast.Comment.t list;
}

let token result = result.lex_token
let loc result = result.lex_loc
let value result = result.lex_value
let comments result = result.lex_comments
let errors result = result.lex_errors

let debug_string_of_lex_result lex_result =
  Printf.sprintf
    "{\n  \
      lex_token = %s\n  \
      lex_value = %S\n  \
      lex_errors = (length = %d)\n  \
      lex_comments = (length = %d)\n\
    }"
  (token_to_string lex_result.lex_token)
  lex_result.lex_value
  (List.length lex_result.lex_errors)
  (List.length lex_result.lex_comments)
