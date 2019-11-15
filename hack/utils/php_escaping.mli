(*
 * Copyright (c) 2019, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

(* This `.mli` file was generated automatically. It may include extra
definitions that should not actually be exposed to the caller. If you notice
that this interface file is a poor interface, please take a few minutes to
clean it up manually, and then delete this comment once the interface is in
shape. *)

exception Invalid_string of string

val is_printable : char -> bool

val is_lit_printable : char -> bool

val is_hex : char -> bool

val is_oct : char -> bool

val escape_char : char -> string

val escape : ?f:(char -> string) -> string -> string

val codepoint_to_utf8 : int -> Buffer.t -> unit

val parse_int : string -> int

val parse_numeric_escape : ?trim_to_byte:bool -> string -> char

type literal_kind =
  | Literal_heredoc
  | Literal_double_quote
  | Literal_backtick
  | Literal_long_string

val unescape_literal : literal_kind -> string -> string

val unescape_double : string -> string

val unescape_backtick : string -> string

val unescape_heredoc : string -> string

val unescape_single_or_nowdoc : is_nowdoc:bool -> string -> string

val unescape_single : string -> string

val unescape_nowdoc : string -> string

val unescape_long_string : string -> string

val extract_unquoted_string : start:int -> len:int -> string -> string
