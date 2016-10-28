{
open Lexing
open Graphql_parser
}

let white = [' ' '\t' ',']+
let comment = '#' [^ '\n' '\r']*
let newline = '\r' | '\n' | "\r\n"

let digit = ['0'-'9']
let int = '-'? '0' | '-'? ['1'-'9'] digit*
let fract_part = '.' digit+
let exp_part = ['e' 'E'] ['+' '-']? digit+
let float = int fract_part | int exp_part | int fract_part exp_part

let name = ['_' 'A'-'Z' 'a'-'z'] ['_' '0'-'9' 'A'-'Z' 'a'-'z']*

rule token = parse
  | white { token lexbuf }
  | comment { token lexbuf }
  | newline { new_line lexbuf; token lexbuf }

  | int { INT (lexeme lexbuf) }
  | float { FLOAT (lexeme lexbuf) }
  | '"' { read_string (Buffer.create 17) lexbuf }

  | "enum" { ENUM }
  | "false" { FALSE }
  | "fragment" { FRAGMENT }
  | "implements" { IMPLEMENTS }
  | "input" { INPUT }
  | "interface" { INTERFACE }
  | "mutation" { MUTATION }
  | "null" { NULL }
  | "on" { ON }
  | "query" { QUERY }
  | "scalar" { SCALAR }
  | "schema" { SCHEMA }
  | "subscription" { SUBSCRIPTION }
  | "true" { TRUE }
  | "type" { TYPE }
  | "union" { UNION }
  | name { NAME (lexeme lexbuf) }

  | '(' { LBRACE }
  | ')' { RBRACE }
  | '{' { LCURLY }
  | '}' { RCURLY }
  | ':' { COLON }
  | '=' { EQUAL }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '!' { BANG }
  | '|' { PIPE }
  | "..." { ELLIPSIS }
  | '$' { DOLLAR }
  | '@' { AT }
  | eof { EOF }

and read_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' '"' { Buffer.add_char buf '"'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' '/' { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' 'b' { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f' { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r' { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't' { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\' '\n' '\r']+
    {
      Buffer.add_string buf (lexeme lexbuf);
      read_string buf lexbuf
    }
