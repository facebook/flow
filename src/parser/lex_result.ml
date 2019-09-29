(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  lex_token: Token.t;
  lex_loc: Loc.t;
  lex_errors: (Loc.t * Parse_error.t) list;
  lex_comments: Loc.t Flow_ast.Comment.t list;
}

let token result = result.lex_token

let loc result = result.lex_loc

let comments result = result.lex_comments

let errors result = result.lex_errors

let debug_string_of_lex_result lex_result =
  Printf.sprintf
    "{\n  lex_token = %s\n  lex_value = %S\n  lex_errors = (length = %d)\n  lex_comments = (length = %d)\n}"
    (Token.token_to_string lex_result.lex_token)
    (Token.value_of_token lex_result.lex_token)
    (List.length lex_result.lex_errors)
    (List.length lex_result.lex_comments)
