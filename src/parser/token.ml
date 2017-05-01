(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

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
