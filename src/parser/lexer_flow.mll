(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

{
module Ast = Spider_monkey_ast

module Token = struct
  type t =
    | T_NUMBER of number_type
    | T_STRING of (Loc.t * string * string * bool) (* loc, value, raw, octal *)
    | T_TEMPLATE_PART of (Loc.t * template_part * bool) (* loc, value, is_tail *)
    | T_IDENTIFIER
    | T_REGEXP of (Loc.t * string * string) (* /pattern/flags *)
    (* Syntax *)
    | T_LCURLY
    | T_RCURLY
    | T_LCURLYBAR
    | T_RCURLYBAR
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
    | T_CHECKS
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
    | T_EXP_ASSIGN
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
    | T_EXP
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
    | T_MIXED_TYPE
    | T_EMPTY_TYPE
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

  and template_part = {
    cooked: string; (* string after processing special chars *)
    raw: string; (* string as specified in source *)
    literal: string; (* same as raw, plus characters like ` and ${ *)
  }

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
    | T_CHECKS -> "T_CHECKS"
    | T_LCURLY -> "T_LCURLY"
    | T_RCURLY -> "T_RCURLY"
    | T_LCURLYBAR -> "T_LCURLYBAR"
    | T_RCURLYBAR -> "T_RCURLYBAR"
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
    | T_EXP_ASSIGN -> "T_EXP_ASSIGN"
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
    | T_EXP -> "T_EXP"
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
    | T_MIXED_TYPE -> "T_MIXED_TYPE"
    | T_EMPTY_TYPE -> "T_EMPTY_TYPE"
    | T_BOOLEAN_TYPE -> "T_BOOLEAN_TYPE"
    | T_NUMBER_TYPE -> "T_NUMBER_TYPE"
    | T_NUMBER_SINGLETON_TYPE _ -> "T_NUMBER_SINGLETON_TYPE"
    | T_STRING_TYPE -> "T_STRING_TYPE"
    | T_VOID_TYPE -> "T_VOID_TYPE"
end
open Token

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
(* Environment *)
(*****************************************************************************)

let debug_string_of_lexing_position position =
  Printf.sprintf
    "{pos_fname=%S; pos_lnum=%d; pos_bol=%d; pos_cnum=%d}"
    position.Lexing.pos_fname
    position.Lexing.pos_lnum
    position.Lexing.pos_bol
    position.Lexing.pos_cnum

let debug_string_of_lexbuf (lb: Lexing.lexbuf) =
  Printf.sprintf
    "{ \
      lex_buffer = %S; \
      lex_buffer_len = %d; \
      lex_abs_pos = %d; \
      lex_start_pos = %d; \
      lex_curr_pos = %d; \
      lex_last_pos = %d; \
      lex_last_action = %d; \
      lex_eof_reached = %b; \
      lex_mem = TODO; \
      lex_start_p = %s; \
      lex_curr_p = %s; \
    }"
    lb.Lexing.lex_buffer
    lb.Lexing.lex_buffer_len
    lb.Lexing.lex_abs_pos
    lb.Lexing.lex_start_pos
    lb.Lexing.lex_curr_pos
    lb.Lexing.lex_last_pos
    lb.Lexing.lex_last_action
    lb.Lexing.lex_eof_reached
    (debug_string_of_lexing_position lb.Lexing.lex_start_p)
    (debug_string_of_lexing_position lb.Lexing.lex_curr_p)

module Lex_env = struct
  type t = {
    lex_source            : Loc.filename option;
    lex_lb                : Lexing.lexbuf;
    lex_in_comment_syntax : bool;
    lex_enable_comment_syntax: bool;
    lex_state             : lex_state;
  }

  and lex_state = {
    lex_errors_acc: (Loc.t * Parse_error.t) list;
    lex_comments_acc: Ast.Comment.t list;
  }

  let empty_lex_state = {
    lex_errors_acc = [];
    lex_comments_acc = [];
  }

  let new_lex_env lex_source lex_lb ~enable_types_in_comments = {
    lex_source;
    lex_lb;
    lex_in_comment_syntax = false;
    lex_enable_comment_syntax = enable_types_in_comments;
    lex_state = empty_lex_state;
  }

  let get_and_clear_state env =
    let state = env.lex_state in
    let env = if state != empty_lex_state
      then { env with lex_state = empty_lex_state }
      else env
    in
    env, state

  let lexbuf env = env.lex_lb
  let with_lexbuf ~lexbuf env = { env with lex_lb = lexbuf }
  let source env = env.lex_source
  let state env = env.lex_state
  let is_in_comment_syntax env = env.lex_in_comment_syntax
  let is_comment_syntax_enabled env = env.lex_enable_comment_syntax
  let in_comment_syntax is_in env =
    if is_in <> env.lex_in_comment_syntax
    then { env with lex_in_comment_syntax = is_in }
    else env

  let debug_string_of_lex_env (env: t) =
    let source = match (source env) with
      | None -> "None"
      | Some x -> Printf.sprintf "Some %S" (Loc.string_of_filename x)
    in
    Printf.sprintf
      "{\n  \
        lex_source = %s\n  \
        lex_lb = %s\n  \
        lex_in_comment_syntax = %b\n  \
        lex_enable_comment_syntax = %b\n  \
        lex_state = {errors = (count = %d); comments = (count = %d)}\n\
      }"
      source
      (debug_string_of_lexbuf env.lex_lb)
      (is_in_comment_syntax env)
      (is_comment_syntax_enabled env)
      (List.length (state env).lex_errors_acc)
      (List.length (state env).lex_comments_acc)
end
open Lex_env

module Lex_result = struct
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
end

  let loc_of_lexbuf env lexbuf = Loc.from_lb (source env) lexbuf

  let get_result_and_clear_state (env, lex_token) =
    let env, state = get_and_clear_state env in
    let (lex_loc, lex_value) = match lex_token with
    | T_STRING (loc, _, raw, _) ->
        loc, raw
    | T_JSX_TEXT (loc, _, raw) -> loc, raw
    | T_TEMPLATE_PART (loc, {literal; _}, _) ->
        loc, literal
    | T_REGEXP (loc, pattern, flags) -> loc, "/" ^ pattern ^ "/" ^ flags
    | _ -> loc_of_lexbuf env env.lex_lb, Lexing.lexeme env.lex_lb in
    env, {
      Lex_result.lex_token;
      lex_loc;
      lex_value;
      lex_errors = List.rev state.lex_errors_acc;
      lex_comments = List.rev state.lex_comments_acc;
    }

  let lex_error (env: Lex_env.t) loc err: Lex_env.t =
    let lex_errors_acc = (loc, err)::env.lex_state.lex_errors_acc in
    { env with lex_state = { env.lex_state with lex_errors_acc; } }

  let unexpected_error env loc value =
    lex_error env loc (Parse_error.UnexpectedToken value)

  let unexpected_error_w_suggest (env: Lex_env.t) (loc: Loc.t) value suggest =
    lex_error env loc (Parse_error.UnexpectedTokenWithSuggestion (value, suggest))

  let illegal (env: Lex_env.t) (loc: Loc.t) =
    lex_error env loc (Parse_error.UnexpectedToken "ILLEGAL")

  let illegal_number (env: Lex_env.t) lexbuf word token =
    let loc = loc_of_lexbuf env lexbuf in
    yyback (String.length word) lexbuf;
    let env = illegal env loc in
    env, token

module FloatOfString : sig
  val float_of_string: string -> float
end = struct
  type t = {
    negative: bool;
    mantissa: int;
    exponent: int;
    decimal_exponent: int option;
    todo: char list;
  }

  exception No_good

  let eat f =
    match f.todo with
    | _::todo -> { f with todo; }
    | _ -> raise No_good

  let start str =
    let todo = ref [] in
    String.iter (fun c -> todo := c::(!todo)) str;
    {
      negative = false;
      mantissa = 0;
      exponent = 0;
      decimal_exponent = None;
      todo = List.rev (!todo);
    }

  let parse_sign f =
    match f.todo with
    | '+'::_ -> eat f
    | '-'::_ -> { (eat f) with negative = true; }
    | _ -> f

  let parse_hex_symbol f =
    match f.todo with
    | '0'::('x' | 'X')::_ -> f |> eat |> eat
    | _ -> raise No_good

  let parse_exponent f =
    let todo_str = f.todo
      |> List.map Char.escaped
      |> String.concat "" in
    let exponent =
      try int_of_string todo_str
      with Failure _ -> raise No_good in
    { f with exponent; todo = [] }

  let rec parse_body f =
    match f.todo with
    | [] -> f
    (* _ is just ignored *)
    | '_'::_ -> parse_body (eat f)
    | '.'::_ ->
        if f.decimal_exponent = None
        then parse_body { (eat f) with decimal_exponent = Some 0 }
        else raise No_good
    | ('p' | 'P')::_ ->
        parse_exponent (eat f)
    | c::_ ->
        let ref_char_code =
          if c >= '0' && c <= '9'
          then Char.code '0'
          else if c >= 'A' && c <= 'F'
          then Char.code 'A' - 10
          else if c >= 'a' && c <= 'f'
          then Char.code 'a' - 10
          else raise No_good in
        let value = (Char.code c) - ref_char_code in
        let decimal_exponent = match f.decimal_exponent with
        | None -> None
        | Some e -> Some (e - 4) in
        let mantissa = (f.mantissa lsl 4) + value in
        parse_body { (eat f) with decimal_exponent; mantissa; }

  let float_of_t f =
    assert (f.todo = []);
    let ret = float_of_int f.mantissa in
    let exponent = match f.decimal_exponent with
    | None -> f.exponent
    | Some decimal_exponent -> f.exponent + decimal_exponent in
    let ret =
      if exponent = 0
      then ret
      else ret ** (float_of_int exponent) in
    if f.negative
    then -.ret
    else ret

  let float_of_string str =
    try Pervasives.float_of_string str
    with e when Sys.win32 ->
      try
        start str
          |> parse_sign
          |> parse_hex_symbol
          |> parse_body
          |> float_of_t
      with No_good -> raise e
end

  let save_comment
    (env: Lex_env.t)
    (start: Loc.t) (_end: Loc.t)
    (buf: Buffer.t)
    (multiline: bool)
  : Lex_env.t = Ast.Comment.(
    let loc = Loc.btwn start _end in
    let s = Buffer.contents buf in
    let c = if multiline then Block s else Line s in
    let lex_comments_acc = (loc, c) :: env.lex_state.lex_comments_acc in
    { env with lex_state = { env.lex_state with lex_comments_acc; } }
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
      FloatOfString.float_of_string num
    in
    let value = if neg = "" then value else ~-.value in
    T_NUMBER_SINGLETON_TYPE (number_type, value)

  type jsx_text_mode =
    | JSX_SINGLE_QUOTED_TEXT
    | JSX_DOUBLE_QUOTED_TEXT
    | JSX_CHILD_TEXT

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
      "mixed",   T_MIXED_TYPE;
      "empty",   T_EMPTY_TYPE;
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
  | '\\'               { let env = illegal env (loc_of_lexbuf env lexbuf) in
                         token env lexbuf }
  | whitespace+        {
                         unicode_fix_cols lexbuf;
                         token env lexbuf }
  | "/*"               {
                         let start = loc_of_lexbuf env lexbuf in
                         let buf = Buffer.create 127 in
                         let env, _end = comment env buf lexbuf in
                         let env = save_comment env start _end buf true in
                         token env lexbuf
                       }
  | "/*" (whitespace* as sp) (":" | "::" | "flow-include" as escape_type) as pattern
                       {
                         if not (is_comment_syntax_enabled env) then
                           let start = loc_of_lexbuf env lexbuf in
                           let buf = Buffer.create 127 in
                           Buffer.add_string buf sp;
                           Buffer.add_string buf escape_type;
                           let env, _end = comment env buf lexbuf in
                           let env = save_comment env start _end buf true in
                           token env lexbuf
                         else
                           let env =
                             if is_in_comment_syntax env then
                               let loc = loc_of_lexbuf env lexbuf in
                               unexpected_error env loc pattern
                             else env
                           in
                           let env = in_comment_syntax true env in
                           match escape_type with
                           | ":" -> env, T_COLON
                           | _ -> token env lexbuf
                       }
  | "*/"               {
                         if is_in_comment_syntax env then
                           let env = in_comment_syntax false env in
                           token env lexbuf
                         else
                           let () = yyback 1 lexbuf in
                           env, T_MULT
                       }
  | "//"               {
                         let start = loc_of_lexbuf env lexbuf in
                         let buf = Buffer.create 127 in
                         let env, _end = line_comment env buf lexbuf in
                         let env = save_comment env start _end buf false in
                         token env lexbuf
                       }
  (* Support for the shebang at the beginning of a file. It is treated like a
   * comment at the beginning or an error elsewhere *)
  | "#!"               { if lexbuf.Lexing.lex_start_pos = 0
                         then begin
                           let env, _ =
                             line_comment env (Buffer.create 127) lexbuf in
                           token env lexbuf
                          end else env, T_ERROR
                       }
  (* Values *)
  | ("'"|'"') as quote {
                         let start = loc_of_lexbuf env lexbuf in
                         let buf = Buffer.create 127 in
                         let raw = Buffer.create 127 in
                         Buffer.add_char raw quote;
                         let octal = false in
                         let env, _end, octal =
                           string_quote env quote buf raw octal lexbuf in
                         env, T_STRING (Loc.btwn start _end, Buffer.contents buf, Buffer.contents raw, octal)
                       }
  | '`'                { let cooked = Buffer.create 127 in
                         let raw = Buffer.create 127 in
                         let literal = Buffer.create 127 in
                         Buffer.add_string literal (Lexing.lexeme lexbuf);

                         let start = loc_of_lexbuf env lexbuf in
                         let env, loc, is_tail =
                           template_part env start cooked raw literal lexbuf in
                         env, T_TEMPLATE_PART (
                           loc,
                           {
                             cooked = Buffer.contents cooked;
                             raw = Buffer.contents raw;
                             literal = Buffer.contents literal;
                           },
                           is_tail
                         )
                       }
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
  | "**="              { env, T_EXP_ASSIGN }
  | "%="               { env, T_MOD_ASSIGN }
  | "&="               { env, T_BIT_AND_ASSIGN }
  | "|="               { env, T_BIT_OR_ASSIGN }
  | "^="               { env, T_BIT_XOR_ASSIGN }
  | "<"                { env, T_LESS_THAN }
  | ">"                { env, T_GREATER_THAN }
  | "+"                { env, T_PLUS }
  | "-"                { env, T_MINUS }
  | "*"                { env, T_MULT }
  | "**"               { env, T_EXP }
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
  | eof                { let env =
                           if is_in_comment_syntax env then
                             let loc = loc_of_lexbuf env lexbuf in
                             lex_error env loc Parse_error.UnexpectedEOS
                           else env
                         in
                         env, T_EOF }
  | _                  { let env = illegal env (loc_of_lexbuf env lexbuf) in
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
  | "/*"               { let start = loc_of_lexbuf env lexbuf in
                         let buf = Buffer.create 127 in
                         let env, _end = comment env buf lexbuf in
                         let env = save_comment env start _end buf true in
                         type_token env lexbuf
                       }
  | "/*" (whitespace* as sp) (":" | "::" | "flow-include" as escape_type) as pattern
                       {
                         if not (is_comment_syntax_enabled env) then
                           let start = loc_of_lexbuf env lexbuf in
                           let buf = Buffer.create 127 in
                           Buffer.add_string buf sp;
                           Buffer.add_string buf escape_type;
                           let env, _end = comment env buf lexbuf in
                           let env = save_comment env start _end buf true in
                           type_token env lexbuf
                         else
                           let env =
                             if is_in_comment_syntax env then
                               let loc = loc_of_lexbuf env lexbuf in
                               unexpected_error env loc pattern
                             else env
                           in
                           let env = in_comment_syntax true env in
                           match escape_type with
                           | ":" -> env, T_COLON
                           | _ -> type_token env lexbuf
                       }
  | "*/"               {
                         if is_in_comment_syntax env then
                           let env = in_comment_syntax false env in
                           type_token env lexbuf
                         else
                           let () = yyback 1 lexbuf in
                           env, T_MULT
                       }
  | "//"               {
                         let start = loc_of_lexbuf env lexbuf in
                         let buf = Buffer.create 127 in
                         let env, _end = line_comment env buf lexbuf in
                         let env = save_comment env start _end buf true in
                         type_token env lexbuf
                       }
  | ("'"|'"') as quote {
                         let start = loc_of_lexbuf env lexbuf in
                         let buf = Buffer.create 127 in
                         let raw = Buffer.create 127 in
                         Buffer.add_char raw quote;
                         let octal = false in
                         let env, _end, octal =
                           string_quote env quote buf raw octal lexbuf in
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
      {
        let env, singleton =
          try env, mk_num_singleton NORMAL num neg
          with _ when Sys.win32 ->
            let loc = loc_of_lexbuf env lexbuf in
            let env = lex_error env loc Parse_error.WindowsFloatOfString in
            env, T_NUMBER_SINGLETON_TYPE (NORMAL, 789.0) in
        illegal_number env lexbuf w singleton
      }
  | (neg? as neg) (hexnumber as num)
      {
        try env, mk_num_singleton NORMAL num neg
        with _ when Sys.win32 ->
          let loc = loc_of_lexbuf env lexbuf in
          let env = lex_error env loc Parse_error.WindowsFloatOfString in
          env, T_NUMBER_SINGLETON_TYPE (NORMAL, 789.0)
      }
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
  | "%checks"          { env, T_CHECKS }
  (* Syntax *)
  | "["                { env, T_LBRACKET }
  | "]"                { env, T_RBRACKET }
  | "{"                { env, T_LCURLY }
  | "}"                { env, T_RCURLY }
  | "{|"               { env, T_LCURLYBAR }
  | "|}"               { env, T_RCURLYBAR }
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
  (* Generic default *)
  | "="                { env, T_ASSIGN }
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
  (* Variance annotations *)
  | '+'                { env, T_PLUS }
  | '-'                { env, T_MINUS }

  (* Others *)
  | eof                { let env =
                           if is_in_comment_syntax env then
                             let loc = loc_of_lexbuf env lexbuf in
                             lex_error env loc Parse_error.UnexpectedEOS
                           else env
                         in
                         env, T_EOF }
  | _                  { env, T_ERROR }

(* Really simple version of string lexing. Just try to find beginning and end of
 * string. We can inspect the string later to find invalid escapes, etc *)
and string_quote env q buf raw octal = parse
    | ("'"|'"') as q'    {  Buffer.add_char raw q';
                          if q = q'
                          then env, loc_of_lexbuf env lexbuf, octal
                          else begin
                            Buffer.add_char buf q';
                            string_quote env q buf raw octal lexbuf
                          end
                       }
  | '\\' as e          { Buffer.add_char raw e;
                         let env, octal' = string_escape env buf lexbuf in
                         let octal = octal' || octal in
                         Buffer.add_string raw (Lexing.lexeme lexbuf);
                         string_quote env q buf raw octal lexbuf }
  | ('\n' | eof) as x  { Buffer.add_string raw x;
                         let env = illegal env (loc_of_lexbuf env lexbuf) in
                         Buffer.add_string buf x;
                         env, loc_of_lexbuf env lexbuf, octal
                       }
  | _ as x             { Buffer.add_char raw x;
                         Buffer.add_char buf x;
                         string_quote env q buf raw octal lexbuf }

and string_escape env buf = parse
  | eof               { env, false }
  | '\\'              { Buffer.add_string buf "\\";
                        env, false }
  | 'x' (hex as a) (hex as b)
                      { let code = hexa_to_int a * 16 + hexa_to_int b in
                        List.iter (Buffer.add_char buf) (utf16to8 code);
                        env, false }
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
                        env, true
                      }
  | (['0'-'7'] as a) (['0'-'7'] as b)
                      { let code =
                          (oct_to_int a lsl 3) +
                          (oct_to_int b) in
                        List.iter (Buffer.add_char buf) (utf16to8 code);
                        env, true
                      }
  | '0'               { Buffer.add_char buf (Char.chr 0x0); env, false }
  | 'b'               { Buffer.add_char buf (Char.chr 0x8); env, false }
  | 'f'               { Buffer.add_char buf (Char.chr 0xC); env, false }
  | 'n'               { Buffer.add_char buf (Char.chr 0xA); env, false }
  | 'r'               { Buffer.add_char buf (Char.chr 0xD); env, false }
  | 't'               { Buffer.add_char buf (Char.chr 0x9); env, false }
  | 'v'               { Buffer.add_char buf (Char.chr 0xB); env, false }
  | (['0'-'7'] as a)
                      { let code = oct_to_int a in
                        List.iter (Buffer.add_char buf) (utf16to8 code);
                        env, true
                      }
  | 'u' (hex as a) (hex as b) (hex as c) (hex as d)
                      { let code =
                          (hexa_to_int a lsl 12) +
                          (hexa_to_int b lsl 8) +
                          (hexa_to_int c lsl 4) +
                          (hexa_to_int d) in
                        List.iter (Buffer.add_char buf) (utf16to8 code);
                        env, false
                      }
  | "u{" (hex+ as hex_code) '}'
                      {
                        let code = int_of_string ("0x"^hex_code) in
                        (* 11.8.4.1 *)
                        let env = if code > 1114111
                          then illegal env (loc_of_lexbuf env lexbuf)
                          else env
                        in
                        List.iter (Buffer.add_char buf) (utf16to8 code);
                        env, false
                      }
  | ['u''x''0'-'7'] as c
                      { let env = illegal env (loc_of_lexbuf env lexbuf) in
                        Buffer.add_char buf c;
                        env, false }
  | line_terminator_sequence
                      { Lexing.new_line lexbuf; env, false }
  | _ as c            { Buffer.add_char buf c; env, false }

and comment env buf = parse
  | eof                { let env = illegal env (loc_of_lexbuf env lexbuf) in
                         env, loc_of_lexbuf env lexbuf }
  | '\n'               { Lexing.new_line lexbuf;
                         Buffer.add_char buf '\n';
                         comment env buf lexbuf }
  | "*/"               {
                         let loc = loc_of_lexbuf env lexbuf in
                         let env = if is_in_comment_syntax env
                           then unexpected_error_w_suggest env loc "*/" "*-/"
                           else env
                         in
                         env, loc
                       }
  | "*-/"              {
                         if is_in_comment_syntax env
                         then env, loc_of_lexbuf env lexbuf
                         else (
                           Buffer.add_string buf "*-/";
                           comment env buf lexbuf
                         )
                       }
  | _  as c            { Buffer.add_char buf c;
                         comment env buf lexbuf }

and line_comment env buf = parse
  | eof                { env, loc_of_lexbuf env lexbuf }
  | '\n'               { let open Loc in
                         let { source; start; _end = { line; column; offset } }
                           = loc_of_lexbuf env lexbuf in
                         Lexing.new_line lexbuf;
                         let _end = {
                           line;
                           column = column - 1;
                           offset = offset - 1;
                         } in
                         env, { source; start; _end; }
                       }
  | _ as c             { Buffer.add_char buf c;
                         line_comment env buf lexbuf }

and regexp env = parse
  | eof               { env, T_EOF }
  | line_terminator_sequence
                      { Lexing.new_line lexbuf;
                        regexp env lexbuf }
  | whitespace+       { unicode_fix_cols lexbuf;
                        regexp env lexbuf }
  | "//"              { let start = loc_of_lexbuf env lexbuf in
                        let buf = Buffer.create 127 in
                        let env, _end = line_comment env buf lexbuf in
                        let env = save_comment env start _end buf true in
                        regexp env lexbuf }
  | "/*"              { let start = loc_of_lexbuf env lexbuf in
                        let buf = Buffer.create 127 in
                        let env, _end = comment env buf lexbuf in
                        let env = save_comment env start _end buf true in
                        regexp env lexbuf }
  | '/'               { let start = loc_of_lexbuf env lexbuf in
                        let buf = Buffer.create 127 in
                        let env, flags = regexp_body env buf lexbuf in
                        let end_ = loc_of_lexbuf env lexbuf in
                        let loc = Loc.btwn start end_ in
                        env, T_REGEXP (loc, Buffer.contents buf, flags) }
  | _                 { let env = illegal env (loc_of_lexbuf env lexbuf) in
                        env, T_ERROR }

and regexp_body env buf = parse
  | eof               { let loc = loc_of_lexbuf env lexbuf in
                        let env = lex_error env loc Parse_error.UnterminatedRegExp in
                        env, "" }
  | '\\' line_terminator_sequence
                      { let loc = loc_of_lexbuf env lexbuf in
                        let env = lex_error env loc Parse_error.UnterminatedRegExp in
                        env, "" }
  | ( '\\' _ ) as s   { Buffer.add_string buf s;
                        regexp_body env buf lexbuf }
  | '/' (letter+ as flags)
                      { env, flags }
  | '/'               { env, "" }
  | '[' as c          { Buffer.add_char buf c;
                        let env = regexp_class env buf lexbuf in
                        regexp_body env buf lexbuf }
  | line_terminator_sequence
                      { let loc = loc_of_lexbuf env lexbuf in
                        let env = lex_error env loc Parse_error.UnterminatedRegExp in
                        env, "" }
  | _ as c            { Buffer.add_char buf c;
                        regexp_body env buf lexbuf }

and regexp_class env buf = parse
  | eof               { env }
  | "\\\\" as s       { Buffer.add_string buf s;
                        regexp_class env buf lexbuf }
  | ( '\\' ']' ) as s { Buffer.add_string buf s;
                        regexp_class env buf lexbuf }
  | ']' as c          { Buffer.add_char buf c; env }
  | _ as c            { Buffer.add_char buf c;
                        regexp_class env buf lexbuf }

and jsx_tag env = parse
  | eof               { env, T_EOF }
  | line_terminator_sequence
                      { Lexing.new_line lexbuf;
                        jsx_tag env lexbuf }
  | whitespace+       { unicode_fix_cols lexbuf;
                        jsx_tag env lexbuf }
  | "//"              { let start = loc_of_lexbuf env lexbuf in
                        let buf = Buffer.create 127 in
                        let env, _end = line_comment env buf lexbuf in
                        let env = save_comment env start _end buf true in
                        jsx_tag env lexbuf }
  | "/*"              { let start = loc_of_lexbuf env lexbuf in
                        let buf = Buffer.create 127 in
                        let env, _end = comment env buf lexbuf in
                        let env = save_comment env start _end buf true in
                        jsx_tag env lexbuf }
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
                        let start = loc_of_lexbuf env lexbuf in
                        let buf = Buffer.create 127 in
                        let raw = Buffer.create 127 in
                        Buffer.add_char raw quote;
                        let mode = if quote = '\''
                          then JSX_SINGLE_QUOTED_TEXT
                          else JSX_DOUBLE_QUOTED_TEXT in
                        let env, _end = jsx_text env mode buf raw lexbuf in
                        Buffer.add_char raw quote;
                        let value = Buffer.contents buf in
                        let raw = Buffer.contents raw in
                        env, T_JSX_TEXT (Loc.btwn start _end, value, raw)
                      }
  | _                 { env, T_ERROR }

and jsx_child env start buf raw = parse
(*
  | whitespace+ as ws { Buffer.add_string buf ws;
                        jsx_child env start buf lexbuf
                      }
*)
  | line_terminator_sequence as lt
                      { Buffer.add_string raw lt;
                        Buffer.add_string buf lt;
                        Lexing.new_line lexbuf;
                        let env, _end =
                          jsx_text env JSX_CHILD_TEXT buf raw lexbuf in
                        let value = Buffer.contents buf in
                        let raw = Buffer.contents raw in
                        env, T_JSX_TEXT (Loc.btwn start _end, value, raw)
                      }
  | eof               { env, T_EOF }
  | '<'               { env, T_LESS_THAN }
  | '{'               { env, T_LCURLY }
  | _ as c            { Buffer.add_char raw c;
                        Buffer.add_char buf c;
                        let env, _end =
                          jsx_text env JSX_CHILD_TEXT buf raw lexbuf in
                        let value = Buffer.contents buf in
                        let raw = Buffer.contents raw in
                        env, T_JSX_TEXT (Loc.btwn start _end, value, raw)
                      }

and jsx_text env mode buf raw = parse
  | ("'"|'"'|'<'|'{') as c
                      { match mode, c with
                        | JSX_SINGLE_QUOTED_TEXT, '\''
                        | JSX_DOUBLE_QUOTED_TEXT, '"' ->
                            env, loc_of_lexbuf env lexbuf
                        | JSX_CHILD_TEXT, ('<' | '{') ->
                            (* Don't actually want to consume these guys
                             * yet...they're not part of the JSX text *)
                            back lexbuf;
                            env, loc_of_lexbuf env lexbuf
                        | _ ->
                            Buffer.add_char raw c;
                            Buffer.add_char buf c;
                            jsx_text env mode buf raw lexbuf
                      }
  | eof               { let env = illegal env (loc_of_lexbuf env lexbuf) in
                        env, loc_of_lexbuf env lexbuf
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
                        | "lang" -> Some 0x27E8 (* 0x2329 in HTML4 *)
                        | "rang" -> Some 0x27E9 (* 0x232A in HTML4 *)
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

and template_tail env = parse
  | line_terminator_sequence
                      { Lexing.new_line lexbuf;
                        template_tail env lexbuf }
  | whitespace+       { unicode_fix_cols lexbuf;
                        template_tail env lexbuf }
  | "//"              { let start = loc_of_lexbuf env lexbuf in
                        let buf = Buffer.create 127 in
                        let env, _end = line_comment env buf lexbuf in
                        let env = save_comment env start _end buf true in
                        template_tail env lexbuf }
  | "/*"              { let start = loc_of_lexbuf env lexbuf in
                        let buf = Buffer.create 127 in
                        let env, _end = comment env buf lexbuf in
                        let env = save_comment env start _end buf true in
                        template_tail env lexbuf }
  | '}'               { let start = loc_of_lexbuf env lexbuf in
                        let cooked = Buffer.create 127 in
                        let raw = Buffer.create 127 in
                        let literal = Buffer.create 127 in
                        Buffer.add_string literal "}";
                        let env, loc, is_tail =
                          template_part env start cooked raw literal lexbuf in
                        env, (T_TEMPLATE_PART (loc, {
                          cooked = Buffer.contents cooked;
                          raw = Buffer.contents raw;
                          literal = Buffer.contents literal;
                        }, is_tail))
                      }
  | _                 { let env = illegal env (loc_of_lexbuf env lexbuf) in
                        env, (T_TEMPLATE_PART (
                          loc_of_lexbuf env lexbuf,
                          { cooked = ""; raw = ""; literal = ""; },
                          true
                        ))
                      }

and template_part env start cooked raw literal = parse
  | eof               { let env = illegal env (loc_of_lexbuf env lexbuf) in
                        env, Loc.btwn start (loc_of_lexbuf env lexbuf), true }
  | '`'               { Buffer.add_char literal '`';
                        env, Loc.btwn start (loc_of_lexbuf env lexbuf), true }
  | "${"              { Buffer.add_string literal "${";
                        env, Loc.btwn start (loc_of_lexbuf env lexbuf), false }
  | '\\'              { Buffer.add_char raw '\\';
                        Buffer.add_char literal '\\';
                        let env, _ = string_escape env cooked lexbuf in
                        let str = Lexing.lexeme lexbuf in
                        Buffer.add_string raw str;
                        Buffer.add_string literal str;
                        template_part env start cooked raw literal lexbuf }
  (* ECMAScript 6th Syntax, 11.8.6.1 Static Semantics: TV’s and TRV’s
   * Long story short, <LF> is 0xA, <CR> is 0xA, and <CR><LF> is 0xA
   * *)
  | "\r\n" as lf
                      { Buffer.add_string raw lf;
                        Buffer.add_string literal lf;
                        Buffer.add_string cooked "\n";
                        Lexing.new_line lexbuf;
                        template_part env start cooked raw literal lexbuf }
  | ("\n" | "\r") as lf
                      { Buffer.add_char raw lf;
                        Buffer.add_char literal lf;
                        Buffer.add_char cooked '\n';
                        Lexing.new_line lexbuf;
                        template_part env start cooked raw literal lexbuf }
  | _ as c            { Buffer.add_char raw c;
                        Buffer.add_char literal c;
                        Buffer.add_char cooked c;
                        template_part env start cooked raw literal lexbuf }

{
  let regexp env =
    get_result_and_clear_state (regexp env env.lex_lb)

  (* Lexing JSX children requires a string buffer to keep track of whitespace
   * *)
  let jsx_child env =
    let start = Loc.from_curr_lb (source env) env.lex_lb in
    let buf = Buffer.create 127 in
    let raw = Buffer.create 127 in
    let env, child = jsx_child env start buf raw env.lex_lb in
    get_result_and_clear_state (env, child)

  let jsx_tag env =
    get_result_and_clear_state (jsx_tag env env.lex_lb)

  let template_tail env =
    get_result_and_clear_state (template_tail env env.lex_lb)

  let type_token env =
    get_result_and_clear_state (type_token env env.lex_lb)

  let token env =
    get_result_and_clear_state (token env env.lex_lb)
}
