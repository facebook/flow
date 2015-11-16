(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

{
  module Ast = Spider_monkey_ast

  type token =
    | T_NUMBER of number_type
    | T_STRING of (Loc.t * string * string * bool) (* loc, value, raw, octal *)
    | T_TEMPLATE_PART of (Ast.Expression.TemplateLiteral.Element.t * string)
    | T_IDENTIFIER
    | T_REGEXP of (Loc.t * string * string) (* /pattern/flags *)
    (* Syntax *)
    | T_LCURLY
    | T_RCURLY
    | T_LPAREN
    | T_RPAREN
    | T_LBRACKET
    | T_RBRACKET
    | T_SEMICOLON
    | T_COMMA
    | T_PERIOD
    | T_ARROW
    | T_ELLIPSIS
    | T_AT
    (* Keywords *)
    | T_FUNCTION
    | T_IF
    | T_IN
    | T_INSTANCEOF
    | T_RETURN
    | T_SWITCH
    | T_THIS
    | T_THROW
    | T_TRY
    | T_VAR
    | T_WHILE
    | T_WITH
    | T_CONST
    | T_LET
    | T_NULL
    | T_FALSE
    | T_TRUE
    | T_BREAK
    | T_CASE
    | T_CATCH
    | T_CONTINUE
    | T_DEFAULT
    | T_DO
    | T_FINALLY
    | T_FOR
    | T_CLASS
    | T_EXTENDS
    | T_STATIC
    | T_ELSE
    | T_NEW
    | T_DELETE
    | T_TYPEOF
    | T_VOID
    | T_ENUM
    | T_EXPORT
    | T_IMPORT
    | T_SUPER
    | T_IMPLEMENTS
    | T_INTERFACE
    | T_PACKAGE
    | T_PRIVATE
    | T_PROTECTED
    | T_PUBLIC
    | T_YIELD
    | T_DEBUGGER
    | T_DECLARE
    | T_TYPE
    | T_OF
    | T_ASYNC
    | T_AWAIT
    (* Operators *)
    | T_RSHIFT3_ASSIGN
    | T_RSHIFT_ASSIGN
    | T_LSHIFT_ASSIGN
    | T_BIT_XOR_ASSIGN
    | T_BIT_OR_ASSIGN
    | T_BIT_AND_ASSIGN
    | T_MOD_ASSIGN
    | T_DIV_ASSIGN
    | T_MULT_ASSIGN
    | T_MINUS_ASSIGN
    | T_PLUS_ASSIGN
    | T_ASSIGN
    | T_PLING
    | T_COLON
    | T_OR
    | T_AND
    | T_BIT_OR
    | T_BIT_XOR
    | T_BIT_AND
    | T_EQUAL
    | T_NOT_EQUAL
    | T_STRICT_EQUAL
    | T_STRICT_NOT_EQUAL
    | T_LESS_THAN_EQUAL
    | T_GREATER_THAN_EQUAL
    | T_LESS_THAN
    | T_GREATER_THAN
    | T_LSHIFT
    | T_RSHIFT
    | T_RSHIFT3
    | T_PLUS
    | T_MINUS
    | T_DIV
    | T_MULT
    | T_MOD
    | T_NOT
    | T_BIT_NOT
    | T_INCR
    | T_DECR
    (* Extra tokens *)
    | T_ERROR
    | T_EOF
    (* JSX *)
    | T_JSX_IDENTIFIER
    | T_JSX_TEXT of (Loc.t * string * string) (* loc, value, raw *)
    (* Type primitives *)
    | T_ANY_TYPE
    | T_BOOLEAN_TYPE
    | T_NUMBER_TYPE
    | T_NUMBER_SINGLETON_TYPE of number_type * float
    | T_STRING_TYPE
    | T_VOID_TYPE

  and number_type =
    | BINARY
    | LEGACY_OCTAL
    | OCTAL
    | NORMAL

(*****************************************************************************)
(* Backtracking. *)
(*****************************************************************************)
  let yyback n lexbuf =
    Lexing.(
      lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - n;
      let currp = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <-
        { currp with pos_cnum = currp.pos_cnum - n }
    )

  let back lb =
    let n = Lexing.lexeme_end lb - Lexing.lexeme_start lb in
    yyback n lb

(*****************************************************************************)
(* Pretty printer (pretty?) *)
(*****************************************************************************)
  let token_to_string = function
    | T_NUMBER _ -> "T_NUMBER"
    | T_STRING _ -> "T_STRING"
    | T_TEMPLATE_PART _ -> "T_TEMPLATE_PART"
    | T_IDENTIFIER -> "T_IDENTIFIER"
    | T_REGEXP _ -> "T_REGEXP"
    | T_FUNCTION -> "T_FUNCTION"
    | T_IF -> "T_IF"
    | T_IN -> "T_IN"
    | T_INSTANCEOF -> "T_INSTANCEOF"
    | T_RETURN -> "T_RETURN"
    | T_SWITCH -> "T_SWITCH"
    | T_THIS -> "T_THIS"
    | T_THROW -> "T_THROW"
    | T_TRY -> "T_TRY"
    | T_VAR -> "T_VAR"
    | T_WHILE -> "T_WHILE"
    | T_WITH -> "T_WITH"
    | T_CONST -> "T_CONST"
    | T_LET  -> "T_LET"
    | T_NULL -> "T_NULL"
    | T_FALSE -> "T_FALSE"
    | T_TRUE -> "T_TRUE"
    | T_BREAK -> "T_BREAK"
    | T_CASE -> "T_CASE"
    | T_CATCH -> "T_CATCH"
    | T_CONTINUE -> "T_CONTINUE"
    | T_DEFAULT -> "T_DEFAULT"
    | T_DO -> "T_DO"
    | T_FINALLY -> "T_FINALLY"
    | T_FOR -> "T_FOR"
    | T_CLASS -> "T_CLASS"
    | T_EXTENDS -> "T_EXTENDS"
    | T_STATIC -> "T_STATIC"
    | T_ELSE -> "T_ELSE"
    | T_NEW -> "T_NEW"
    | T_DELETE -> "T_DELETE"
    | T_TYPEOF -> "T_TYPEOF"
    | T_VOID -> "T_VOID"
    | T_ENUM -> "T_ENUM"
    | T_EXPORT  -> "T_EXPORT"
    | T_IMPORT -> "T_IMPORT"
    | T_SUPER  -> "T_SUPER"
    | T_IMPLEMENTS -> "T_IMPLEMENTS"
    | T_INTERFACE -> "T_INTERFACE"
    | T_PACKAGE -> "T_PACKAGE"
    | T_PRIVATE -> "T_PRIVATE"
    | T_PROTECTED -> "T_PROTECTED"
    | T_PUBLIC -> "T_PUBLIC"
    | T_YIELD -> "T_YIELD"
    | T_DEBUGGER -> "T_DEBUGGER"
    | T_DECLARE -> "T_DECLARE"
    | T_TYPE -> "T_TYPE"
    | T_OF -> "T_OF"
    | T_ASYNC -> "T_ASYNC"
    | T_AWAIT -> "T_AWAIT"
    | T_LCURLY -> "T_LCURLY"
    | T_RCURLY -> "T_RCURLY"
    | T_LPAREN -> "T_LPAREN"
    | T_RPAREN -> "T_RPAREN"
    | T_LBRACKET -> "T_LBRACKET"
    | T_RBRACKET -> "T_RBRACKET"
    | T_SEMICOLON -> "T_SEMICOLON"
    | T_COMMA -> "T_COMMA"
    | T_PERIOD -> "T_PERIOD"
    | T_ARROW -> "T_ARROW"
    | T_ELLIPSIS -> "T_ELLIPSIS"
    | T_AT -> "T_AT"
    | T_RSHIFT3_ASSIGN -> "T_RSHIFT3_ASSIGN"
    | T_RSHIFT_ASSIGN -> "T_RSHIFT_ASSIGN"
    | T_LSHIFT_ASSIGN -> "T_LSHIFT_ASSIGN"
    | T_BIT_XOR_ASSIGN -> "T_BIT_XOR_ASSIGN"
    | T_BIT_OR_ASSIGN -> "T_BIT_OR_ASSIGN"
    | T_BIT_AND_ASSIGN -> "T_BIT_AND_ASSIGN"
    | T_MOD_ASSIGN -> "T_MOD_ASSIGN"
    | T_DIV_ASSIGN -> "T_DIV_ASSIGN"
    | T_MULT_ASSIGN -> "T_MULT_ASSIGN"
    | T_MINUS_ASSIGN -> "T_MINUS_ASSIGN"
    | T_PLUS_ASSIGN -> "T_PLUS_ASSIGN"
    | T_ASSIGN -> "T_ASSIGN"
    | T_PLING -> "T_PLING"
    | T_COLON -> "T_COLON"
    | T_OR -> "T_OR"
    | T_AND -> "T_AND"
    | T_BIT_OR -> "T_BIT_OR"
    | T_BIT_XOR -> "T_BIT_XOR"
    | T_BIT_AND -> "T_BIT_AND"
    | T_EQUAL -> "T_EQUAL"
    | T_NOT_EQUAL -> "T_NOT_EQUAL"
    | T_STRICT_EQUAL -> "T_STRICT_EQUAL"
    | T_STRICT_NOT_EQUAL -> "T_STRICT_NOT_EQUAL"
    | T_LESS_THAN_EQUAL -> "T_LESS_THAN_EQUAL"
    | T_GREATER_THAN_EQUAL -> "T_GREATER_THAN_EQUAL"
    | T_LESS_THAN -> "T_LESS_THAN"
    | T_GREATER_THAN -> "T_GREATER_THAN"
    | T_LSHIFT -> "T_LSHIFT"
    | T_RSHIFT -> "T_RSHIFT"
    | T_RSHIFT3 -> "T_RSHIFT3"
    | T_PLUS -> "T_PLUS"
    | T_MINUS -> "T_MINUS"
    | T_DIV -> "T_DIV"
    | T_MULT -> "T_MULT"
    | T_MOD -> "T_MOD"
    | T_NOT -> "T_NOT"
    | T_BIT_NOT -> "T_BIT_NOT"
    | T_INCR -> "T_INCR"
    | T_DECR -> "T_DECR"
    (* Extra tokens *)
    | T_ERROR -> "T_ERROR"
    | T_EOF -> "T_EOF"
    | T_JSX_IDENTIFIER -> "T_JSX_IDENTIFIER"
    | T_JSX_TEXT _ -> "T_JSX_TEXT"
    (* Type primitives *)
    | T_ANY_TYPE -> "T_ANY_TYPE"
    | T_BOOLEAN_TYPE -> "T_BOOLEAN_TYPE"
    | T_NUMBER_TYPE -> "T_NUMBER_TYPE"
    | T_NUMBER_SINGLETON_TYPE _ -> "T_NUMBER_SINGLETON_TYPE"
    | T_STRING_TYPE -> "T_STRING_TYPE"
    | T_VOID_TYPE -> "T_VOID_TYPE"

(*****************************************************************************)
(* Errors *)
(*****************************************************************************)
  type lex_env = {
    lex_source            : Loc.filename option;
    lex_lb                : Lexing.lexbuf;
    lex_in_comment_syntax : bool;
    lex_state             : lex_state ref;
  }

  and lex_state = {
    lex_errors_acc: (Loc.t * Parse_error.t) list;
    lex_comments_acc: Ast.Comment.t list;
  }

  let empty_lex_state = {
    lex_errors_acc = [];
    lex_comments_acc = [];
  }

  let new_lex_env lex_source lex_lb = {
    lex_source;
    lex_lb;
    lex_in_comment_syntax = false;
    lex_state = ref empty_lex_state;
  }

  type lex_result = {
    lex_token: token;
    lex_loc: Loc.t;
    lex_value: string;
    lex_errors: (Loc.t * Parse_error.t) list;
    lex_comments: Ast.Comment.t list;
    lex_lb_curr_p: Lexing.position;
  }

  let lb_to_loc = Loc.from_lb

  let get_result_and_clear_state (env, lex_token) =
    let state = !(env.lex_state) in
    env.lex_state := empty_lex_state;
    let (lex_loc, lex_value) = match lex_token with
    | T_STRING (loc, _, raw, _) ->
        loc, raw
    | T_JSX_TEXT (loc, _, raw) -> loc, raw
    | T_TEMPLATE_PART ((loc, _), raw_text) ->
        loc, raw_text
    | T_REGEXP (loc, pattern, flags) -> loc, "/" ^ pattern ^ "/" ^ flags
    | _ -> lb_to_loc env.lex_source env.lex_lb, Lexing.lexeme env.lex_lb in
    env, {
      lex_token;
      lex_loc;
      lex_value;
      lex_errors = List.rev state.lex_errors_acc;
      lex_comments = List.rev state.lex_comments_acc;
      lex_lb_curr_p = env.lex_lb.Lexing.lex_curr_p;
    }

  let lex_error env loc err: unit =
    let lex_errors_acc = (loc, err)::(!(env.lex_state).lex_errors_acc) in
    env.lex_state := { !(env.lex_state) with lex_errors_acc; }

  let unexpected_error env loc value =
    lex_error env loc (Parse_error.UnexpectedToken value)

  let unexpected_error_w_suggest env loc value suggest =
    lex_error env loc (Parse_error.UnexpectedTokenWithSuggestion (value, suggest))

  let illegal env loc = lex_error env loc (Parse_error.UnexpectedToken "ILLEGAL")

  let illegal_number env lexbuf word token =
    let loc = lb_to_loc env.lex_source lexbuf in
    yyback (String.length word) lexbuf;
    illegal env loc;
    env, token

  let save_comment env start _end buf multiline = Ast.Comment.(
    let loc = Loc.btwn start _end in
    let s = Buffer.contents buf in
    let c = if multiline then Block s else Line s in
    let lex_comments_acc = (loc, c) :: !(env.lex_state).lex_comments_acc in
    env.lex_state := { !(env.lex_state) with lex_comments_acc; }
  )

  let unicode_fix_cols lb =
    let rec count start stop acc =
      if start = stop then acc
      else
        let c = Char.code (lb.Lexing.lex_buffer.[start]) in
        let acc = if (c land 0xC0) = 0x80
          then acc + 1
          else acc in
        count (start+1) stop acc
    in
      Lexing.(
        let bytes = count lb.lex_start_pos lb.lex_curr_pos 0 in
        let new_bol = lb.lex_curr_p.pos_bol + bytes in
        lb.lex_curr_p <- {
          lb.lex_curr_p with pos_bol = new_bol;
        }
      )

  let oct_to_int = function
    | '0'..'7' as x -> Char.code x - Char.code '0'
    | _ -> assert false

  let hexa_to_int = function
    | '0'..'9' as x -> Char.code x - Char.code '0'
    | 'a'..'f' as x -> Char.code x - Char.code 'a' + 10
    | 'A'..'F' as x -> Char.code x - Char.code 'A' + 10
    | _ -> assert false

  let utf16to8 code =
    if code >= 0x10000
    then
    (* 4 bytes *)
      [
        Char.chr (0xf0 lor (code lsr 18));
        Char.chr (0x80 lor ((code lsr 12) land 0x3f));
        Char.chr (0x80 lor ((code lsr 6) land 0x3f));
        Char.chr (0x80 lor (code land 0x3f));
      ]
    else if code >= 0x800
    then
    (* 3 bytes *)
      [
        Char.chr (0xe0 lor (code lsr 12));
        Char.chr (0x80 lor ((code lsr 6) land 0x3f));
        Char.chr (0x80 lor (code land 0x3f));
      ]
    else if code >= 0x80
    then
    (* 2 bytes *)
      [
        Char.chr (0xc0 lor (code lsr 6));
        Char.chr (0x80 lor (code land 0x3f));
      ]
    else
    (* 1 byte *)
      [
        Char.chr code;
      ]

  let mk_num_singleton number_type num neg =
    (* convert singleton number type into a float *)
    let value = match number_type with
    | LEGACY_OCTAL ->
      float (int_of_string ("0o"^num))
    | BINARY
    | OCTAL ->
      float (int_of_string num)
    | NORMAL ->
      Sys_utils.float_of_string num
    in
    let value = if neg = "" then value else ~-.value in
    T_NUMBER_SINGLETON_TYPE (number_type, value)

  type jsx_text_mode =
    | JSX_SINGLE_QUOTED_TEXT
    | JSX_DOUBLE_QUOTED_TEXT
    | JSX_CHILD_TEXT

  let lex_template_part env template_part =
    let start = lb_to_loc env.lex_source env.lex_lb in
    let cooked = Buffer.create 127 in
    let uncooked = Buffer.create 127 in
    let raw = Buffer.create 127 in
    Buffer.add_string raw (Lexing.lexeme env.lex_lb);
    let _end, tail = template_part env cooked uncooked raw env.lex_lb in
    let part = Ast.Expression.TemplateLiteral.({
      Element.value = {
        Element.cooked = Buffer.contents cooked;
        raw = Buffer.contents uncooked;
      };
      tail;
    }) in
    env, T_TEMPLATE_PART ((Loc.btwn start _end, part), Buffer.contents raw)

  let keywords = Hashtbl.create 53
  let type_keywords = Hashtbl.create 53
  let _ = List.iter (fun (key, token) -> Hashtbl.add keywords key token)
    [
      "function", T_FUNCTION;
      "if", T_IF;
      "in", T_IN;
      "instanceof", T_INSTANCEOF;
      "return", T_RETURN;
      "switch", T_SWITCH;
      "this", T_THIS;
      "throw", T_THROW;
      "try", T_TRY;
      "var", T_VAR;
      "while", T_WHILE;
      "with", T_WITH;
      "const", T_CONST;
      "let", T_LET ;
      "null", T_NULL;
      "false", T_FALSE;
      "true", T_TRUE;
      "break", T_BREAK;
      "case", T_CASE;
      "catch", T_CATCH;
      "continue", T_CONTINUE;
      "default", T_DEFAULT;
      "do", T_DO;
      "finally", T_FINALLY;
      "for", T_FOR;
      "class", T_CLASS;
      "extends", T_EXTENDS;
      "static", T_STATIC;
      "else", T_ELSE;
      "new", T_NEW;
      "delete", T_DELETE;
      "typeof", T_TYPEOF;
      "void", T_VOID;
      "enum", T_ENUM;
      "export", T_EXPORT ;
      "import", T_IMPORT;
      "super", T_SUPER ;
      "implements", T_IMPLEMENTS;
      "interface", T_INTERFACE;
      "package", T_PACKAGE;
      "private", T_PRIVATE;
      "protected", T_PROTECTED;
      "public", T_PUBLIC;
      "yield", T_YIELD;
      "debugger", T_DEBUGGER;
      "declare", T_DECLARE;
      "type", T_TYPE;
      "of", T_OF;
      "async", T_ASYNC;
      "await", T_AWAIT;
    ]
  let _ = List.iter (fun (key, token) -> Hashtbl.add type_keywords key token)
    [
      "static",  T_STATIC;
      "typeof",  T_TYPEOF;
      "any",     T_ANY_TYPE;
      "bool",    T_BOOLEAN_TYPE;
      "boolean", T_BOOLEAN_TYPE;
      "true",    T_TRUE;
      "false",   T_FALSE;
      "number",  T_NUMBER_TYPE;
      "string",  T_STRING_TYPE;
      "void",    T_VOID_TYPE;
      "null",    T_NULL;
    ]
}

let hex = ['0'-'9''a'-'f''A'-'F']
let unicode_whitespace =
  ('\xE1''\x9A''\x80')|('\xE1''\xA0''\x8E')|('\xE2''\x80''\x80')|
  ('\xE2''\x80''\x81')|('\xE2''\x80''\x82')|('\xE2''\x80''\x83')|
  ('\xE2''\x80''\x84')|('\xE2''\x80''\x85')|('\xE2''\x80''\x86')|
  ('\xE2''\x80''\x87')|('\xE2''\x80''\x88')|('\xE2''\x80''\x89')|
  ('\xE2''\x80''\x8A')|('\xE2''\x80''\xAF')|('\xE2''\x81''\x9F')|
  ('\xE3''\x80''\x80')|('\xEF''\xBB''\xBF')
let whitespace = [' ' '\t' '\r' '\x0c'] | unicode_whitespace

(* Different ways you can write a number *)
let binnumber = '0' ['B''b'] ['0''1']+
let hexnumber = '0' ['X''x'] hex+
let octnumber = '0' ['O''o'] ['0'-'7']+
let legacyoctnumber = '0' ['0'-'7']+
let scinumber = ['0'-'9']*'.'?['0'-'9']+['e''E']['-''+']?['0'-'9']+
let wholenumber = ['0'-'9']+'.'?
let floatnumber = ['0'-'9']*'.'['0'-'9']+

let digit = ['0'-'9']
let non_ascii_unicode =
  ['\xC2'-'\xDF']['\x80'-'\xBF']|('\xE0'['\xA0'-'\xBF']|['\xE1'-'\xEF']['\x80'-'\xBF'])['\x80'-'\xBF']
let explicit_unicode =
  '\\'('u'['0'-'9']['0'-'9']['0'-'9']['0'-'9']|'x'['0'-'9']['0'-'9'])

(* these definitions include unicode in legal id and hex literal chars. *)
(* let letter = ['a'-'z''A'-'Z''_''$'] | non_ascii_unicode | explicit_unicode *)
(* let non_hex_letter = ['g'-'z''G'-'Z''_''$'] | non_ascii_unicode | explicit_unicode *)

(* we only allow unicode in string literals *)
let letter = ['a'-'z''A'-'Z''_''$']
let non_hex_letter = ['g'-'z''G'-'Z''_''$']

let alphanumeric = digit | letter
let word = letter alphanumeric*

let line_terminator = '\n'|'\r'
let line_terminator_sequence = line_terminator | "\r\n"

let single_escape_character = ['\'''"''\\''b''f''n''r''t''v']

(* 2-8 alphanumeric characters. I could match them directly, but this leads to
 * ~5k more lines of generated lexer
let htmlentity = "quot" | "amp" | "apos" | "lt" | "gt" | "nbsp" | "iexcl" | "cent" | "pound" | "curren" | "yen" | "brvbar" | "sect" | "uml" | "copy" | "ordf" | "laquo" | "not" | "shy" | "reg" | "macr" | "deg" | "plusmn" | "sup2" | "sup3" | "acute" | "micro" | "para" | "middot" | "cedil" | "sup1" | "ordm" | "raquo" | "frac14" | "frac12" | "frac34" | "iquest" | "Agrave" | "Aacute" | "Acirc" | "Atilde" | "Auml" | "Aring" | "AElig" | "Ccedil" | "Egrave" | "Eacute" | "Ecirc" | "Euml" | "Igrave" | "Iacute" | "Icirc" | "Iuml" | "ETH" | "Ntilde" | "Ograve" | "Oacute" | "Ocirc" | "Otilde" | "Ouml" | "times" | "Oslash" | "Ugrave" | "Uacute" | "Ucirc" | "Uuml" | "Yacute" | "THORN" | "szlig" | "agrave" | "aacute" | "acirc" | "atilde" | "auml" | "aring" | "aelig" | "ccedil" | "egrave" | "eacute" | "ecirc" | "euml" | "igrave" | "iacute" | "icirc" | "iuml" | "eth" | "ntilde" | "ograve" | "oacute" | "ocirc" | "otilde" | "ouml" | "divide" | "oslash" | "ugrave" | "uacute" | "ucirc" | "uuml" | "yacute" | "thorn" | "yuml" | "OElig" | "oelig" | "Scaron" | "scaron" | "Yuml" | "fnof" | "circ" | "tilde" | "Alpha" | "Beta" | "Gamma" | "Delta" | "Epsilon" | "Zeta" | "Eta" | "Theta" | "Iota" | "Kappa" | "Lambda" | "Mu" | "Nu" | "Xi" | "Omicron" | "Pi" | "Rho" | "Sigma" | "Tau" | "Upsilon" | "Phi" | "Chi" | "Psi" | "Omega" | "alpha" | "beta" | "gamma" | "delta" | "epsilon" | "zeta" | "eta" | "theta" | "iota" | "kappa" | "lambda" | "mu" | "nu" | "xi" | "omicron" | "pi" | "rho" | "sigmaf" | "sigma" | "tau" | "upsilon" | "phi" | "chi" | "psi" | "omega" | "thetasym" | "upsih" | "piv" | "ensp" | "emsp" | "thinsp" | "zwnj" | "zwj" | "lrm" | "rlm" | "ndash" | "mdash" | "lsquo" | "rsquo" | "sbquo" | "ldquo" | "rdquo" | "bdquo" | "dagger" | "Dagger" | "bull" | "hellip" | "permil" | "prime" | "Prime" | "lsaquo" | "rsaquo" | "oline" | "frasl" | "euro" | "image" | "weierp" | "real" | "trade" | "alefsym" | "larr" | "uarr" | "rarr" | "darr" | "harr" | "crarr" | "lArr" | "uArr" | "rArr" | "dArr" | "hArr" | "forall" | "part" | "exist" | "empty" | "nabla" | "isin" | "notin" | "ni" | "prod" | "sum" | "minus" | "lowast" | "radic" | "prop" | "infin" | "ang" | "and" | "or" | "cap" | "cup" | "'int'" | "there4" | "sim" | "cong" | "asymp" | "ne" | "equiv" | "le" | "ge" | "sub" | "sup" | "nsub" | "sube" | "supe" | "oplus" | "otimes" | "perp" | "sdot" | "lceil" | "rceil" | "lfloor" | "rfloor" | "lang" | "rang" | "loz" | "spades" | "clubs" | "hearts" | "diams"
*)
let htmlentity = alphanumeric alphanumeric alphanumeric? alphanumeric? alphanumeric? alphanumeric? alphanumeric? alphanumeric?

(* minus sign in front of negative numbers
   (only for types! regular numbers use T_MINUS!) *)
let neg = '-' whitespace*

rule token env = parse
  (* Ignored *)
  | '\n'               {
                         Lexing.new_line lexbuf;
                         token env lexbuf
                       }
  | '\\'               { illegal env (lb_to_loc env.lex_source lexbuf);
                         token env lexbuf }
  | whitespace+        {
                         unicode_fix_cols lexbuf;
                         token env lexbuf }
  | "/*"               {
                            let start = lb_to_loc env.lex_source lexbuf in
                            let buf = Buffer.create 127 in
                            let _end = comment env buf lexbuf in
                            save_comment env start _end buf true;
                         token env lexbuf
                       }
  | "/*" whitespace* (":" | "::" | "flow-include" as escape_type) as pattern
                       {
                         let () = if env.lex_in_comment_syntax then
                           let loc = lb_to_loc env.lex_source lexbuf in
                           unexpected_error env loc pattern
                         in
                         let env = { env with lex_in_comment_syntax = true } in
                         match escape_type with
                         | ":" -> env, T_COLON
                         | _ -> token env lexbuf
                       }
  | "*/"               {
                         let () = if not env.lex_in_comment_syntax then
                           let loc = lb_to_loc env.lex_source lexbuf in
                           unexpected_error env loc "*/"
                         in
                         let env = { env with lex_in_comment_syntax = false } in
                         token env lexbuf
                       }
  | "//"               {
                         let start = lb_to_loc env.lex_source lexbuf in
                         let buf = Buffer.create 127 in
                         let _end = line_comment env buf lexbuf in
                         save_comment env start _end buf false;
                         token env lexbuf
                       }
  (* Support for the shebang at the beginning of a file. It is treated like a
   * comment at the beginning or an error elsewhere *)
  | "#!"               { if lexbuf.Lexing.lex_start_pos = 0
                         then begin
                           let _ = line_comment env (Buffer.create 127) lexbuf in
                           token env lexbuf
                          end else env, T_ERROR
                       }
  (* Values *)
  | ("'"|'"') as quote {
                         let start = lb_to_loc env.lex_source lexbuf in
                         let buf = Buffer.create 127 in
                         let raw = Buffer.create 127 in
                         Buffer.add_char raw quote;
                         let octal = false in
                         let _end, octal = string_quote env quote buf raw octal lexbuf in
                         env, T_STRING (Loc.btwn start _end, Buffer.contents buf, Buffer.contents raw, octal)
                       }
  | '`'                { lex_template_part env template_part }
  (* Numbers cannot be immediately followed by words *)
  | binnumber ((letter | ['2'-'9']) alphanumeric* as w)
                       { illegal_number env lexbuf w (T_NUMBER BINARY) }
  | binnumber          { env, T_NUMBER BINARY }
  | octnumber ((letter | ['8'-'9']) alphanumeric* as w)
                       { illegal_number env lexbuf w (T_NUMBER OCTAL) }
  | octnumber          { env, T_NUMBER OCTAL }
  | legacyoctnumber ((letter | ['8'-'9']) alphanumeric* as w)
                       { illegal_number env lexbuf w (T_NUMBER LEGACY_OCTAL) }
  | legacyoctnumber    { env, T_NUMBER LEGACY_OCTAL }
  | hexnumber (non_hex_letter alphanumeric* as w)
                       { illegal_number env lexbuf w (T_NUMBER NORMAL) }
  | hexnumber          { env, T_NUMBER NORMAL }
  | scinumber (word as w)
                       { illegal_number env lexbuf w (T_NUMBER NORMAL) }
  | scinumber          { env, T_NUMBER NORMAL }
  | (wholenumber | floatnumber) (word as w)
                       { illegal_number env lexbuf w (T_NUMBER NORMAL) }
  | wholenumber
  | floatnumber        { env, T_NUMBER NORMAL }

  (* Keyword or Identifier *)
  (* TODO: Use [Symbol.iterator] instead of @@iterator. *)
  | ("@@"? word) as word
                       {
                         unicode_fix_cols lexbuf;
                         try env, Hashtbl.find keywords word
                         with Not_found -> env, T_IDENTIFIER
                       }
  (* Syntax *)
  | "{"                { env, T_LCURLY }
  | "}"                { env, T_RCURLY }
  | "("                { env, T_LPAREN }
  | ")"                { env, T_RPAREN }
  | "["                { env, T_LBRACKET }
  | "]"                { env, T_RBRACKET }
  | "..."              { env, T_ELLIPSIS }
  | "."                { env, T_PERIOD }
  | ";"                { env, T_SEMICOLON }
  | ","                { env, T_COMMA }
  | ":"                { env, T_COLON }
  | "?"                { env, T_PLING }
  | "&&"               { env, T_AND }
  | "||"               { env, T_OR }
  | "==="              { env, T_STRICT_EQUAL }
  | "!=="              { env, T_STRICT_NOT_EQUAL }
  | "<="               { env, T_LESS_THAN_EQUAL }
  | ">="               { env, T_GREATER_THAN_EQUAL }
  | "=="               { env, T_EQUAL }
  | "!="               { env, T_NOT_EQUAL }
  | "++"               { env, T_INCR }
  | "--"               { env, T_DECR }
  | "<<="              { env, T_LSHIFT_ASSIGN }
  | "<<"               { env, T_LSHIFT }
  | ">>="              { env, T_RSHIFT_ASSIGN }
  | ">>>="             { env, T_RSHIFT3_ASSIGN }
  | ">>>"              { env, T_RSHIFT3 }
  | ">>"               { env, T_RSHIFT }
  | "+="               { env, T_PLUS_ASSIGN }
  | "-="               { env, T_MINUS_ASSIGN }
  | "*="               { env, T_MULT_ASSIGN }
  | "%="               { env, T_MOD_ASSIGN }
  | "&="               { env, T_BIT_AND_ASSIGN }
  | "|="               { env, T_BIT_OR_ASSIGN }
  | "^="               { env, T_BIT_XOR_ASSIGN }
  | "<"                { env, T_LESS_THAN }
  | ">"                { env, T_GREATER_THAN }
  | "+"                { env, T_PLUS }
  | "-"                { env, T_MINUS }
  | "*"                { env, T_MULT }
  | "%"                { env, T_MOD }
  | "|"                { env, T_BIT_OR }
  | "&"                { env, T_BIT_AND }
  | "^"                { env, T_BIT_XOR }
  | "!"                { env, T_NOT }
  | "~"                { env, T_BIT_NOT }
  | "="                { env, T_ASSIGN }
  | "=>"               { env, T_ARROW }
  | "/="               { env, T_DIV_ASSIGN }
  | "/"                { env, T_DIV }
  | "@"                { env, T_AT }
  (* Others *)
  | eof                { let () = if env.lex_in_comment_syntax then
                           let loc = lb_to_loc env.lex_source lexbuf in
                           lex_error env loc Parse_error.UnexpectedEOS
                         in
                         env, T_EOF }
  | _                  { illegal env (lb_to_loc env.lex_source lexbuf);
                         env, T_ERROR }

(* There are some tokens that never show up in a type and which can cause
 * ambiguity. For example, Foo<Bar<number>> ends with two angle brackets, not
 * with a right shift.
 *)
and type_token env = parse
  (* Ignored *)
  | '\n'               {
                           Lexing.new_line lexbuf;
                           type_token env lexbuf
                       }
  | whitespace+        {
                         unicode_fix_cols lexbuf;
                         type_token env lexbuf }
  | "/*"               {
                            let start = lb_to_loc env.lex_source lexbuf in
                            let buf = Buffer.create 127 in
                            let _end = comment env buf lexbuf in
                            save_comment env start _end buf true;
                         type_token env lexbuf
                       }
  | "/*" whitespace* (":" | "::" | "flow-include" as escape_type) as pattern
                       {
                         let () = if env.lex_in_comment_syntax then
                           let loc = lb_to_loc env.lex_source lexbuf in
                           unexpected_error env loc pattern
                         in
                         let env = { env with lex_in_comment_syntax = true } in
                         match escape_type with
                         | ":" -> env, T_COLON
                         | _ -> type_token env lexbuf
                       }
  | "*/"               {
                         let () = if not env.lex_in_comment_syntax then
                           let loc = lb_to_loc env.lex_source lexbuf in
                           unexpected_error env loc "*/"
                         in
                         let env = { env with lex_in_comment_syntax = false } in
                         type_token env lexbuf
                       }
  | "//"               {
                         let start = lb_to_loc env.lex_source lexbuf in
                         let buf = Buffer.create 127 in
                         let _end = line_comment env buf lexbuf in
                         save_comment env start _end buf true;
                         type_token env lexbuf
                       }
  | ("'"|'"') as quote {
                         let start = lb_to_loc env.lex_source lexbuf in
                         let buf = Buffer.create 127 in
                         let raw = Buffer.create 127 in
                         Buffer.add_char raw quote;
                         let octal = false in
                         let _end, octal = string_quote env quote buf raw octal lexbuf in
                         env, T_STRING (Loc.btwn start _end, Buffer.contents buf, Buffer.contents raw, octal)
                       }

  (**
   * Number literals
   *)

  (* Numbers cannot be immediately followed by words *)
  | (neg? as neg) (binnumber as num) ((letter | ['2'-'9']) alphanumeric* as w)
      { illegal_number env lexbuf w (mk_num_singleton BINARY num neg) }
  | (neg? as neg) (binnumber as num)
      { env, mk_num_singleton BINARY num neg }
  | (neg? as neg) (octnumber as num) ((letter | ['8'-'9']) alphanumeric* as w)
      { illegal_number env lexbuf w (mk_num_singleton OCTAL num neg) }
  | (neg? as neg) (octnumber as num)
      { env, mk_num_singleton OCTAL num neg }
  | (neg? as neg) (legacyoctnumber as num) ((letter | ['8'-'9']) alphanumeric* as w)
      { illegal_number env lexbuf w (mk_num_singleton LEGACY_OCTAL num neg) }
  | (neg? as neg) (legacyoctnumber as num)
      { env, mk_num_singleton LEGACY_OCTAL num neg }
  | (neg? as neg) (hexnumber as num) (non_hex_letter alphanumeric* as w)
      { illegal_number env lexbuf w (mk_num_singleton NORMAL num neg) }
  | (neg? as neg) (hexnumber as num)
      { env, mk_num_singleton NORMAL num neg }
  | (neg? as neg) (scinumber as num) (word as w)
      { illegal_number env lexbuf w (mk_num_singleton NORMAL num neg) }
  | (neg? as neg) (scinumber as num)
      { env, mk_num_singleton NORMAL num neg }
  | (neg? as neg) ((wholenumber | floatnumber) as num) (word as w)
      { illegal_number env lexbuf w (mk_num_singleton NORMAL num neg) }
  | (neg? as neg) (wholenumber as num)
  | (neg? as neg) (floatnumber as num)
      { env, mk_num_singleton NORMAL num neg }

  (* Keyword or Identifier *)
  | word as word       {
                         unicode_fix_cols lexbuf;
                         try env, Hashtbl.find type_keywords word
                         with Not_found -> env, T_IDENTIFIER
                       }
  (* Syntax *)
  | "["                { env, T_LBRACKET }
  | "]"                { env, T_RBRACKET }
  | "{"                { env, T_LCURLY }
  | "}"                { env, T_RCURLY }
  | "("                { env, T_LPAREN }
  | ")"                { env, T_RPAREN }
  | "..."              { env, T_ELLIPSIS }
  | "."                { env, T_PERIOD }
  | ";"                { env, T_SEMICOLON }
  | ","                { env, T_COMMA }
  | ":"                { env, T_COLON }
  | "?"                { env, T_PLING }
  | "["                { env, T_LBRACKET }
  | "]"                { env, T_RBRACKET }
  (* Generics *)
  | "<"                { env, T_LESS_THAN }
  | ">"                { env, T_GREATER_THAN }
  (* Optional or nullable *)
  | "?"                { env, T_PLING }
  (* Existential *)
  | "*"                { env, T_MULT }
  (* Annotation or bound *)
  | ":"                { env, T_COLON }
  (* Union *)
  | '|'                { env, T_BIT_OR }
  (* Intersection *)
  | '&'                { env, T_BIT_AND }
  (* typeof *)
  | "typeof"           { env, T_TYPEOF }
  (* Function type *)
  | "=>"               { env, T_ARROW }
  (* Type alias *)
  | '='                { env, T_ASSIGN }
  (* Others *)
  | eof                { let () = if env.lex_in_comment_syntax then
                           let loc = lb_to_loc env.lex_source lexbuf in
                           lex_error env loc Parse_error.UnexpectedEOS
                         in
                         env, T_EOF }
  | _                  { env, T_ERROR }

(* Really simple version of string lexing. Just try to find beginning and end of
 * string. We can inspect the string later to find invalid escapes, etc *)
and string_quote env q buf raw octal = parse
    | ("'"|'"') as q'    {  Buffer.add_char raw q';
                          if q = q'
                          then lb_to_loc env.lex_source lexbuf, octal
                          else begin
                            Buffer.add_char buf q';
                            string_quote env q buf raw octal lexbuf
                          end
                       }
  | '\\' as e          { Buffer.add_char raw e;
                         let octal = string_escape env buf lexbuf || octal in
                         Buffer.add_string raw (Lexing.lexeme lexbuf);
                         string_quote env q buf raw octal lexbuf }
  | ('\n' | eof) as x  { Buffer.add_string raw x;
                         illegal env (lb_to_loc env.lex_source lexbuf);
                         Buffer.add_string buf x;
                         lb_to_loc env.lex_source lexbuf, octal
                       }
  | _ as x             { Buffer.add_char raw x;
                         Buffer.add_char buf x;
                         string_quote env q buf raw octal lexbuf }

and string_escape env buf = parse
  | eof               { false }
  | '\\'              { Buffer.add_string buf "\\";
                        false }
  | 'x' (hex as a) (hex as b)
                      { let code = hexa_to_int a * 16 + hexa_to_int b in
                        List.iter (Buffer.add_char buf) (utf16to8 code);
                        false }
  | (['0'-'7'] as a) (['0'-'7'] as b) (['0'-'7'] as c)
                      { let code =
                          (oct_to_int a lsl 6) +
                          (oct_to_int b lsl 3) +
                          (oct_to_int c) in
                        (* If the 3 character octal code is larger than 256
                         * then it is parsed as a 2 character octal code *)
                        if code < 256
                        then List.iter (Buffer.add_char buf) (utf16to8 code)
                        else begin
                          let code =
                            (oct_to_int a lsl 3) +
                            (oct_to_int b) in
                          List.iter (Buffer.add_char buf) (utf16to8 code);
                          Buffer.add_char buf c
                        end;
                        true
                      }
  | (['0'-'7'] as a) (['0'-'7'] as b)
                      { let code =
                          (oct_to_int a lsl 3) +
                          (oct_to_int b) in
                        List.iter (Buffer.add_char buf) (utf16to8 code);
                        true
                      }
  | '0'               { Buffer.add_char buf (Char.chr 0x0); false }
  | 'b'               { Buffer.add_char buf (Char.chr 0x8); false }
  | 'f'               { Buffer.add_char buf (Char.chr 0xC); false }
  | 'n'               { Buffer.add_char buf (Char.chr 0xA); false }
  | 'r'               { Buffer.add_char buf (Char.chr 0xD); false }
  | 't'               { Buffer.add_char buf (Char.chr 0x9); false }
  | 'v'               { Buffer.add_char buf (Char.chr 0xB); false }
  | (['0'-'7'] as a)
                      { let code = oct_to_int a in
                        List.iter (Buffer.add_char buf) (utf16to8 code);
                        true
                      }
  | 'u' (hex as a) (hex as b) (hex as c) (hex as d)
                      { let code =
                          (hexa_to_int a lsl 12) +
                          (hexa_to_int b lsl 8) +
                          (hexa_to_int c lsl 4) +
                          (hexa_to_int d) in
                        List.iter (Buffer.add_char buf) (utf16to8 code);
                        false
                      }
  | "u{" (hex+ as hex_code) '}'
                      {
                        let code = int_of_string ("0x"^hex_code) in
                        (* 11.8.4.1 *)
                        if code > 1114111
                        then illegal env (lb_to_loc env.lex_source lexbuf);
                        List.iter (Buffer.add_char buf) (utf16to8 code);
                        false
                      }
  | ['u''x''0'-'7'] as c
                      { illegal env (lb_to_loc env.lex_source lexbuf);
                        Buffer.add_char buf c;
                        false }
  | line_terminator_sequence
                      { Lexing.new_line lexbuf; false }
  | _ as c            { Buffer.add_char buf c; false }

and comment env buf = parse
  | eof                { illegal env (lb_to_loc env.lex_source lexbuf);
                         lb_to_loc env.lex_source lexbuf }
  | '\n'               { Lexing.new_line lexbuf;
                         Buffer.add_char buf '\n';
                         comment env buf lexbuf }
  | "*/"               {
                         let loc = lb_to_loc env.lex_source lexbuf in
                         if env.lex_in_comment_syntax
                         then unexpected_error_w_suggest env loc "*/" "*-/";
                         loc
                       }
  | "*-/"              {
                         if env.lex_in_comment_syntax
                         then lb_to_loc env.lex_source lexbuf
                         else (
                           Buffer.add_string buf "*-/";
                           comment env buf lexbuf
                         )
                       }
  | _  as c            { Buffer.add_char buf c;
                         comment env buf lexbuf }

and line_comment env buf = parse
  | eof                { lb_to_loc env.lex_source lexbuf }
  | '\n'               { Loc.(
                            let { source; start; _end = { line; column; offset } }
                             = lb_to_loc env.lex_source lexbuf in
                           Lexing.new_line lexbuf;
                           { source; start; _end
                             = { line; column = column - 1; offset = offset - 1; } }
                       ) }
  | _ as c             { Buffer.add_char buf c;
                         line_comment env buf lexbuf }

and regexp_body env buf = parse
  | eof               { let loc = lb_to_loc env.lex_source lexbuf in
                        lex_error env loc Parse_error.UnterminatedRegExp;
                        "" }
  | '\\' line_terminator_sequence
                      { let loc = lb_to_loc env.lex_source lexbuf in
                        lex_error env loc Parse_error.UnterminatedRegExp;
                        "" }
  | ( '\\' _ ) as s   { Buffer.add_string buf s;
                        regexp_body env buf lexbuf }
  | '/' (letter+ as flags)
                      { flags }
  | '/'               { "" }
  | '[' as c          { Buffer.add_char buf c;
                        regexp_class buf lexbuf;
                        regexp_body env buf lexbuf }
  | line_terminator_sequence
                      { let loc = lb_to_loc env.lex_source lexbuf in
                        lex_error env loc Parse_error.UnterminatedRegExp;
                        "" }
  | _ as c            { Buffer.add_char buf c;
                        regexp_body env buf lexbuf }

and regexp_class buf = parse
  | eof               { }
  | "\\\\" as s       { Buffer.add_string buf s;
                        regexp_class buf lexbuf }
  | ( '\\' ']' ) as s { Buffer.add_string buf s;
                        regexp_class buf lexbuf }
  | ']' as c          { Buffer.add_char buf c }
  | _ as c            { Buffer.add_char buf c;
                        regexp_class buf lexbuf }

and lex_jsx_tag env = parse
  | eof               { env, T_EOF }
  | line_terminator_sequence
                      { Lexing.new_line lexbuf;
                        lex_jsx_tag env lexbuf }
  | whitespace+       { unicode_fix_cols lexbuf;
                        lex_jsx_tag env lexbuf }
  | "//"              { let start = lb_to_loc env.lex_source lexbuf in
                        let buf = Buffer.create 127 in
                        let _end = line_comment env buf lexbuf in
                        save_comment env start _end buf true;
                        lex_jsx_tag env lexbuf }
  | "/*"              { let start = lb_to_loc env.lex_source lexbuf in
                        let buf = Buffer.create 127 in
                        let _end = comment env buf lexbuf in
                        save_comment env start _end buf true;
                        lex_jsx_tag env lexbuf }
  | '<'               { env, T_LESS_THAN }
  | '/'               { env, T_DIV }
  | '>'               { env, T_GREATER_THAN }
  | '{'               { env, T_LCURLY }
  | ':'               { env, T_COLON }
  | '.'               { env, T_PERIOD }
  | '='               { env, T_ASSIGN }
  | letter ('-' | alphanumeric)*
                      { unicode_fix_cols lexbuf;
                        env, T_JSX_IDENTIFIER }
  | ('\''|'"') as quote
                      {
                        let start = lb_to_loc env.lex_source lexbuf in
                        let buf = Buffer.create 127 in
                        let raw = Buffer.create 127 in
                        Buffer.add_char raw quote;
                        let mode = if quote = '\''
                          then JSX_SINGLE_QUOTED_TEXT
                          else JSX_DOUBLE_QUOTED_TEXT in
                        let _end = jsx_text env mode buf raw lexbuf in
                        Buffer.add_char raw quote;
                        let value = Buffer.contents buf in
                        let raw = Buffer.contents raw in
                        env, T_JSX_TEXT (Loc.btwn start _end, value, raw)
                      }
  | _                 { env, T_ERROR }

and lex_jsx_child env start buf raw = parse
(*
  | whitespace+ as ws { Buffer.add_string buf ws;
                        lex_jsx_child env start buf lexbuf
                      }
*)
  | line_terminator_sequence as lt
                      { Buffer.add_string raw lt;
                        Buffer.add_string buf lt;
                        Lexing.new_line lexbuf;
                        let _end = jsx_text env JSX_CHILD_TEXT buf raw lexbuf in
                        let value = Buffer.contents buf in
                        let raw = Buffer.contents raw in
                        env, T_JSX_TEXT (Loc.btwn start _end, value, raw)
                      }
  | eof               { env, T_EOF }
  | '<'               { env, T_LESS_THAN }
  | '{'               { env, T_LCURLY }
  | _ as c            { Buffer.add_char raw c;
                        Buffer.add_char buf c;
                        let _end = jsx_text env JSX_CHILD_TEXT buf raw lexbuf in
                        let value = Buffer.contents buf in
                        let raw = Buffer.contents raw in
                        env, T_JSX_TEXT (Loc.btwn start _end, value, raw)
                      }

and jsx_text env mode buf raw = parse
  | ("'"|'"'|'<'|'{') as c
                      { match mode, c with
                        | JSX_SINGLE_QUOTED_TEXT, '\''
                        | JSX_DOUBLE_QUOTED_TEXT, '"' ->
                            lb_to_loc env.lex_source lexbuf
                        | JSX_CHILD_TEXT, ('<' | '{') ->
                            (* Don't actually want to consume these guys
                             * yet...they're not part of the JSX text *)
                            back lexbuf;
                            lb_to_loc env.lex_source lexbuf
                        | _ ->
                            Buffer.add_char raw c;
                            Buffer.add_char buf c;
                            jsx_text env mode buf raw lexbuf
                      }
  | eof               { illegal env (lb_to_loc env.lex_source lexbuf);
                        lb_to_loc env.lex_source lexbuf
                      }
  | line_terminator_sequence as lt
                      { Buffer.add_string raw lt;
                        Buffer.add_string buf lt;
                        Lexing.new_line lexbuf;
                        jsx_text env mode buf raw lexbuf
                      }
  | "&#x" (hex+ as n) ';' as s
                      { Buffer.add_string raw s;
                        let code = int_of_string ("0x" ^ n) in
                        List.iter (Buffer.add_char buf) (utf16to8 code);
                        jsx_text env mode buf raw lexbuf
                      }
  | "&#" (digit+ as n) ';' as s
                      { Buffer.add_string raw s;
                        let code = int_of_string n in
                        List.iter (Buffer.add_char buf) (utf16to8 code);
                        jsx_text env mode buf raw lexbuf
                      }
  | "&" (htmlentity as entity) ';' as s
                      {
                        Buffer.add_string raw s;
                        let code = match entity with
                        | "quot" -> Some 0x0022
                        | "amp" -> Some 0x0026
                        | "apos" -> Some 0x0027
                        | "lt" -> Some 0x003C
                        | "gt" -> Some 0x003E
                        | "nbsp" -> Some 0x00A0
                        | "iexcl" -> Some 0x00A1
                        | "cent" -> Some 0x00A2
                        | "pound" -> Some 0x00A3
                        | "curren" -> Some 0x00A4
                        | "yen" -> Some 0x00A5
                        | "brvbar" -> Some 0x00A6
                        | "sect" -> Some 0x00A7
                        | "uml" -> Some 0x00A8
                        | "copy" -> Some 0x00A9
                        | "ordf" -> Some 0x00AA
                        | "laquo" -> Some 0x00AB
                        | "not" -> Some 0x00AC
                        | "shy" -> Some 0x00AD
                        | "reg" -> Some 0x00AE
                        | "macr" -> Some 0x00AF
                        | "deg" -> Some 0x00B0
                        | "plusmn" -> Some 0x00B1
                        | "sup2" -> Some 0x00B2
                        | "sup3" -> Some 0x00B3
                        | "acute" -> Some 0x00B4
                        | "micro" -> Some 0x00B5
                        | "para" -> Some 0x00B6
                        | "middot" -> Some 0x00B7
                        | "cedil" -> Some 0x00B8
                        | "sup1" -> Some 0x00B9
                        | "ordm" -> Some 0x00BA
                        | "raquo" -> Some 0x00BB
                        | "frac14" -> Some 0x00BC
                        | "frac12" -> Some 0x00BD
                        | "frac34" -> Some 0x00BE
                        | "iquest" -> Some 0x00BF
                        | "Agrave" -> Some 0x00C0
                        | "Aacute" -> Some 0x00C1
                        | "Acirc" -> Some 0x00C2
                        | "Atilde" -> Some 0x00C3
                        | "Auml" -> Some 0x00C4
                        | "Aring" -> Some 0x00C5
                        | "AElig" -> Some 0x00C6
                        | "Ccedil" -> Some 0x00C7
                        | "Egrave" -> Some 0x00C8
                        | "Eacute" -> Some 0x00C9
                        | "Ecirc" -> Some 0x00CA
                        | "Euml" -> Some 0x00CB
                        | "Igrave" -> Some 0x00CC
                        | "Iacute" -> Some 0x00CD
                        | "Icirc" -> Some 0x00CE
                        | "Iuml" -> Some 0x00CF
                        | "ETH" -> Some 0x00D0
                        | "Ntilde" -> Some 0x00D1
                        | "Ograve" -> Some 0x00D2
                        | "Oacute" -> Some 0x00D3
                        | "Ocirc" -> Some 0x00D4
                        | "Otilde" -> Some 0x00D5
                        | "Ouml" -> Some 0x00D6
                        | "times" -> Some 0x00D7
                        | "Oslash" -> Some 0x00D8
                        | "Ugrave" -> Some 0x00D9
                        | "Uacute" -> Some 0x00DA
                        | "Ucirc" -> Some 0x00DB
                        | "Uuml" -> Some 0x00DC
                        | "Yacute" -> Some 0x00DD
                        | "THORN" -> Some 0x00DE
                        | "szlig" -> Some 0x00DF
                        | "agrave" -> Some 0x00E0
                        | "aacute" -> Some 0x00E1
                        | "acirc" -> Some 0x00E2
                        | "atilde" -> Some 0x00E3
                        | "auml" -> Some 0x00E4
                        | "aring" -> Some 0x00E5
                        | "aelig" -> Some 0x00E6
                        | "ccedil" -> Some 0x00E7
                        | "egrave" -> Some 0x00E8
                        | "eacute" -> Some 0x00E9
                        | "ecirc" -> Some 0x00EA
                        | "euml" -> Some 0x00EB
                        | "igrave" -> Some 0x00EC
                        | "iacute" -> Some 0x00ED
                        | "icirc" -> Some 0x00EE
                        | "iuml" -> Some 0x00EF
                        | "eth" -> Some 0x00F0
                        | "ntilde" -> Some 0x00F1
                        | "ograve" -> Some 0x00F2
                        | "oacute" -> Some 0x00F3
                        | "ocirc" -> Some 0x00F4
                        | "otilde" -> Some 0x00F5
                        | "ouml" -> Some 0x00F6
                        | "divide" -> Some 0x00F7
                        | "oslash" -> Some 0x00F8
                        | "ugrave" -> Some 0x00F9
                        | "uacute" -> Some 0x00FA
                        | "ucirc" -> Some 0x00FB
                        | "uuml" -> Some 0x00FC
                        | "yacute" -> Some 0x00FD
                        | "thorn" -> Some 0x00FE
                        | "yuml" -> Some 0x00FF
                        | "OElig" -> Some 0x0152
                        | "oelig" -> Some 0x0153
                        | "Scaron" -> Some 0x0160
                        | "scaron" -> Some 0x0161
                        | "Yuml" -> Some 0x0178
                        | "fnof" -> Some 0x0192
                        | "circ" -> Some 0x02C6
                        | "tilde" -> Some 0x02DC
                        | "Alpha" -> Some 0x0391
                        | "Beta" -> Some 0x0392
                        | "Gamma" -> Some 0x0393
                        | "Delta" -> Some 0x0394
                        | "Epsilon" -> Some 0x0395
                        | "Zeta" -> Some 0x0396
                        | "Eta" -> Some 0x0397
                        | "Theta" -> Some 0x0398
                        | "Iota" -> Some 0x0399
                        | "Kappa" -> Some 0x039A
                        | "Lambda" -> Some 0x039B
                        | "Mu" -> Some 0x039C
                        | "Nu" -> Some 0x039D
                        | "Xi" -> Some 0x039E
                        | "Omicron" -> Some 0x039F
                        | "Pi" -> Some 0x03A0
                        | "Rho" -> Some 0x03A1
                        | "Sigma" -> Some 0x03A3
                        | "Tau" -> Some 0x03A4
                        | "Upsilon" -> Some 0x03A5
                        | "Phi" -> Some 0x03A6
                        | "Chi" -> Some 0x03A7
                        | "Psi" -> Some 0x03A8
                        | "Omega" -> Some 0x03A9
                        | "alpha" -> Some 0x03B1
                        | "beta" -> Some 0x03B2
                        | "gamma" -> Some 0x03B3
                        | "delta" -> Some 0x03B4
                        | "epsilon" -> Some 0x03B5
                        | "zeta" -> Some 0x03B6
                        | "eta" -> Some 0x03B7
                        | "theta" -> Some 0x03B8
                        | "iota" -> Some 0x03B9
                        | "kappa" -> Some 0x03BA
                        | "lambda" -> Some 0x03BB
                        | "mu" -> Some 0x03BC
                        | "nu" -> Some 0x03BD
                        | "xi" -> Some 0x03BE
                        | "omicron" -> Some 0x03BF
                        | "pi" -> Some 0x03C0
                        | "rho" -> Some 0x03C1
                        | "sigmaf" -> Some 0x03C2
                        | "sigma" -> Some 0x03C3
                        | "tau" -> Some 0x03C4
                        | "upsilon" -> Some 0x03C5
                        | "phi" -> Some 0x03C6
                        | "chi" -> Some 0x03C7
                        | "psi" -> Some 0x03C8
                        | "omega" -> Some 0x03C9
                        | "thetasym" -> Some 0x03D1
                        | "upsih" -> Some 0x03D2
                        | "piv" -> Some 0x03D6
                        | "ensp" -> Some 0x2002
                        | "emsp" -> Some 0x2003
                        | "thinsp" -> Some 0x2009
                        | "zwnj" -> Some 0x200C
                        | "zwj" -> Some 0x200D
                        | "lrm" -> Some 0x200E
                        | "rlm" -> Some 0x200F
                        | "ndash" -> Some 0x2013
                        | "mdash" -> Some 0x2014
                        | "lsquo" -> Some 0x2018
                        | "rsquo" -> Some 0x2019
                        | "sbquo" -> Some 0x201A
                        | "ldquo" -> Some 0x201C
                        | "rdquo" -> Some 0x201D
                        | "bdquo" -> Some 0x201E
                        | "dagger" -> Some 0x2020
                        | "Dagger" -> Some 0x2021
                        | "bull" -> Some 0x2022
                        | "hellip" -> Some 0x2026
                        | "permil" -> Some 0x2030
                        | "prime" -> Some 0x2032
                        | "Prime" -> Some 0x2033
                        | "lsaquo" -> Some 0x2039
                        | "rsaquo" -> Some 0x203A
                        | "oline" -> Some 0x203E
                        | "frasl" -> Some 0x2044
                        | "euro" -> Some 0x20AC
                        | "image" -> Some 0x2111
                        | "weierp" -> Some 0x2118
                        | "real" -> Some 0x211C
                        | "trade" -> Some 0x2122
                        | "alefsym" -> Some 0x2135
                        | "larr" -> Some 0x2190
                        | "uarr" -> Some 0x2191
                        | "rarr" -> Some 0x2192
                        | "darr" -> Some 0x2193
                        | "harr" -> Some 0x2194
                        | "crarr" -> Some 0x21B5
                        | "lArr" -> Some 0x21D0
                        | "uArr" -> Some 0x21D1
                        | "rArr" -> Some 0x21D2
                        | "dArr" -> Some 0x21D3
                        | "hArr" -> Some 0x21D4
                        | "forall" -> Some 0x2200
                        | "part" -> Some 0x2202
                        | "exist" -> Some 0x2203
                        | "empty" -> Some 0x2205
                        | "nabla" -> Some 0x2207
                        | "isin" -> Some 0x2208
                        | "notin" -> Some 0x2209
                        | "ni" -> Some 0x220B
                        | "prod" -> Some 0x220F
                        | "sum" -> Some 0x2211
                        | "minus" -> Some 0x2212
                        | "lowast" -> Some 0x2217
                        | "radic" -> Some 0x221A
                        | "prop" -> Some 0x221D
                        | "infin" -> Some 0x221E
                        | "ang" -> Some 0x2220
                        | "and" -> Some 0x2227
                        | "or" -> Some 0x2228
                        | "cap" -> Some 0x2229
                        | "cup" -> Some 0x222A
                        | "'int'" -> Some 0x222B
                        | "there4" -> Some 0x2234
                        | "sim" -> Some 0x223C
                        | "cong" -> Some 0x2245
                        | "asymp" -> Some 0x2248
                        | "ne" -> Some 0x2260
                        | "equiv" -> Some 0x2261
                        | "le" -> Some 0x2264
                        | "ge" -> Some 0x2265
                        | "sub" -> Some 0x2282
                        | "sup" -> Some 0x2283
                        | "nsub" -> Some 0x2284
                        | "sube" -> Some 0x2286
                        | "supe" -> Some 0x2287
                        | "oplus" -> Some 0x2295
                        | "otimes" -> Some 0x2297
                        | "perp" -> Some 0x22A5
                        | "sdot" -> Some 0x22C5
                        | "lceil" -> Some 0x2308
                        | "rceil" -> Some 0x2309
                        | "lfloor" -> Some 0x230A
                        | "rfloor" -> Some 0x230B
                        | "lang" -> Some 0x2329
                        | "rang" -> Some 0x232A
                        | "loz" -> Some 0x25CA
                        | "spades" -> Some 0x2660
                        | "clubs" -> Some 0x2663
                        | "hearts" -> Some 0x2665
                        | "diams" -> Some 0x2666
                        | _ -> None in
                        (match code with
                        | Some code -> List.iter (Buffer.add_char buf) (utf16to8 code)
                        | None -> Buffer.add_string buf ("&" ^ entity ^";"));
                        jsx_text env mode buf raw lexbuf
                      }
  | _ as c            { Buffer.add_char raw c;
                        Buffer.add_char buf c;
                        jsx_text env mode buf raw lexbuf }

and template_part env cooked uncooked raw = parse
  | eof               { illegal env (lb_to_loc env.lex_source lexbuf);
                        lb_to_loc env.lex_source lexbuf, true }
  | '`'               { Buffer.add_char raw '`';
                        lb_to_loc env.lex_source lexbuf, true }
  | "${"              { Buffer.add_string raw "${";
                        lb_to_loc env.lex_source lexbuf, false }
  | '\\'              { Buffer.add_char uncooked '\\';
                        Buffer.add_char raw '\\';
                        let _ = string_escape env cooked lexbuf in
                        let str = Lexing.lexeme lexbuf in
                        Buffer.add_string uncooked str;
                        Buffer.add_string raw str;
                        template_part env cooked uncooked raw lexbuf }
  (* ECMAScript 6th Syntax, 11.8.6.1 Static Semantics: TV’s and TRV’s
   * Long story short, <LF> is 0xA, <CR> is 0xA, and <CR><LF> is 0xA
   * *)
  | "\r\n" as lf
                      { Buffer.add_string uncooked lf;
                        Buffer.add_string raw lf;
                        Buffer.add_string cooked "\n";
                        Lexing.new_line lexbuf;
                        template_part env cooked uncooked raw lexbuf }
  | ("\n" | "\r") as lf
                      { Buffer.add_char uncooked lf;
                        Buffer.add_char raw lf;
                        Buffer.add_char cooked '\n';
                        Lexing.new_line lexbuf;
                        template_part env cooked uncooked raw lexbuf }
  | _ as c            { Buffer.add_char uncooked c;
                        Buffer.add_char raw c;
                        Buffer.add_char cooked c;
                        template_part env cooked uncooked raw lexbuf }

{
  let lex_regexp env prefix =
    (* At this point we already lex'd the /, so we can get the position of the
     * last lexed token as the start location *)
    let start = lb_to_loc env.lex_source env.lex_lb in
    let buf = Buffer.create 127 in
    (* For regexps like /=/, we parse the /= as T_DIV_ASSIGN, so we just parse
     * the regexp knowing that the pattern has the "=" prefix *)
    Buffer.add_string buf prefix;
    let flags = regexp_body env buf env.lex_lb in
    let _end = lb_to_loc env.lex_source env.lex_lb in
    let regexp =
      T_REGEXP (Loc.btwn start _end, Buffer.contents buf, flags) in
    get_result_and_clear_state (env, regexp)

  (* Lexing JSX children requires a string buffer to keep track of whitespace
   * *)
  let lex_jsx_child env =
    let start = Loc.from_curr_lb env.lex_source env.lex_lb in
    let buf = Buffer.create 127 in
    let raw = Buffer.create 127 in
    let env, child = lex_jsx_child env start buf raw env.lex_lb in
    get_result_and_clear_state (env, child)

  let lex_jsx_tag env =
    get_result_and_clear_state (lex_jsx_tag env env.lex_lb)

  let lex_template_part env =
    let env, part = lex_template_part env template_part in
    get_result_and_clear_state (env, part)

  let type_token env =
    get_result_and_clear_state (type_token env env.lex_lb)

  let token env =
    get_result_and_clear_state (token env env.lex_lb)
}
