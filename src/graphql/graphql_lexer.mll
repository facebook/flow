{
module Token = struct
  type t =
    (* punctuators *)
    | T_BANG
    | T_DOLLAR
    | T_LPAREN
    | T_RPAREN
    | T_ELLIPSIS
    | T_COLON
    | T_EQUAL
    | T_AT
    | T_LBRACKET
    | T_RBRACKET
    | T_LCURLY
    | T_PIPE
    | T_RCURLY

    | T_NAME
    | T_INT
    | T_FLOAT
    | T_STRING of string (* value *)

    | T_ILLEGAL

    | T_EXPR of Spider_monkey_ast.Expression.t

    | T_EOF

  let string_of_token = function
    | T_BANG -> "T_BANG"
    | T_DOLLAR -> "T_DOLLAR"
    | T_LPAREN -> "T_LPAREN"
    | T_RPAREN -> "T_RPAREN"
    | T_ELLIPSIS -> "T_ELLIPSIS"
    | T_COLON -> "T_COLON"
    | T_EQUAL -> "T_EQUAL"
    | T_AT -> "T_AT"
    | T_LBRACKET -> "T_LBRACKET"
    | T_RBRACKET -> "T_RBRACKET"
    | T_LCURLY -> "T_LCURLY"
    | T_PIPE -> "T_PIPE"
    | T_RCURLY -> "T_RCURLY"

    | T_NAME -> "T_NAME"
    | T_INT -> "T_INT"
    | T_FLOAT -> "T_FLOAT"
    | T_STRING _ -> "T_STRING"

    | T_ILLEGAL -> "T_ILLEGAL"

    | T_EXPR _ -> "T_EXPR"

    | T_EOF -> "T_EOF"
end

module Lex_result = struct
  type t = {
    lex_token: Token.t;
    lex_loc: Loc.t;
    lex_value: string;
  }

  let token result = result.lex_token
  let loc result = result.lex_loc
  let value result = result.lex_value
end

open Token
}

let white = [' ' '\t' ',']+
let comment = '#' [^ '\n']*
let newline = '\n'

let int = ('-'? '0') | ('-'? ['1'-'9'] ['0'-'9']*)

let float_fract = '.' ['0'-'9']+
let float_exp = ['e''E'] ['-''+']? ['0'-'9']+
let float = (int float_fract) | (int float_exp) | (int float_fract float_exp)

rule read =
  parse
  | white | comment { read lexbuf }
  | newline {
      Lexing.new_line lexbuf;
      read lexbuf
    }

  | '!' { T_BANG }
  | '$' { T_DOLLAR }
  | '(' { T_LPAREN }
  | ')' { T_LPAREN }
  | "..." { T_ELLIPSIS }
  | ':' { T_COLON }
  | '=' { T_EQUAL }
  | '@' { T_AT }
  | '[' { T_LBRACKET }
  | ']' { T_RBRACKET }
  | '{' { T_LCURLY }
  | '|' { T_PIPE }
  | '}' { T_RCURLY }

  | ['_' 'A'-'Z' 'a'-'z'] ['_' '0'-'9' 'A'-'Z' 'a'-'z']* { T_NAME }

  (* TODO: handle unicode and escaped *)
  | '"' ([^ '"' '\n']* as value) '"' { T_STRING value }

  | eof { T_EOF }

  | _ { T_ILLEGAL }

{
let read lexbuf loc =
  let add_pos loc_pos lex_pos =
    let col_start = if lex_pos.Lexing.pos_lnum = 0 then loc_pos.Loc.column else 0 in
    Loc.{
      line = loc_pos.line + lex_pos.Lexing.pos_lnum - 1;
      column = lex_pos.Lexing.pos_cnum - lex_pos.Lexing.pos_bol + col_start;
      offset = loc_pos.offset + lex_pos.Lexing.pos_cnum;
    }
  in

  let token = read lexbuf in

  {
    Lex_result.lex_token = token;
    lex_loc = Loc.{
      source = loc.source;
      start = add_pos loc.start lexbuf.Lexing.lex_start_p;
      _end = add_pos loc.start lexbuf.Lexing.lex_curr_p;
    };
    lex_value = Lexing.lexeme lexbuf;
  }
}
