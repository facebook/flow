(*
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This module provides a layer between the lexer and the parser which includes
 * some parser state and some lexer state *)

module SSet : Set.S with type t = Set.Make(String).t

module Lex_mode : sig
  type t =
    | NORMAL
    | TYPE
    | JSX_TAG
    | JSX_CHILD
    | TEMPLATE
    | REGEXP
  val debug_string_of_lex_mode: t -> string
end

type token_sink_result = {
  token_loc: Loc.t;
  token: Lexer_flow.Token.t;
  token_context: Lex_mode.t;
  token_value: string;
}

type parse_options = {
  esproposal_class_instance_fields: bool;
  esproposal_class_static_fields: bool;
  esproposal_decorators: bool;
  esproposal_export_star_as: bool;
  types: bool;
  use_strict: bool;
}
val default_parse_options : parse_options

type env

(* constructor: *)
val init_env :
  ?token_sink:(token_sink_result -> unit) option
  -> ?parse_options:parse_options option
  -> Loc.filename option
  -> string
  -> env

(* getters: *)
val in_strict_mode : env -> bool
val last_loc : env -> Loc.t option
val in_export : env -> bool
val labels : env -> SSet.t
val comments : env -> Spider_monkey_ast.Comment.t list
val in_loop : env -> bool
val in_switch : env -> bool
val in_function : env -> bool
val allow_yield : env -> bool
val allow_await: env -> bool
val no_in : env -> bool
val no_call : env -> bool
val no_let : env -> bool
val errors : env -> (Loc.t * Parse_error.t) list
val parse_options : env -> parse_options
val source : env -> Loc.filename option
val should_parse_types : env -> bool

(* mutators: *)
val error_at : env -> Loc.t * Parse_error.t -> unit
val error : env -> Parse_error.t -> unit
val error_unexpected : env -> unit
val error_on_decorators : env -> (Loc.t * 'a) list -> unit
val strict_error : env -> Parse_error.t -> unit
val strict_error_at : env -> Loc.t * Parse_error.t -> unit
val get_unexpected_error : Lexer_flow.Token.t * string -> Parse_error.t
val comment_list : env -> Spider_monkey_ast.Comment.t list -> unit
val error_list : env -> (Loc.t * Parse_error.t) list -> unit
val record_export: env -> Loc.t * string -> unit

(* functional operations -- these return shallow copies, so future mutations to
 * the returned env will also affect the original: *)
val with_strict : bool -> env -> env
val with_in_function : bool -> env -> env
val with_allow_yield : bool -> env -> env
val with_allow_await : bool -> env -> env
val with_no_let : bool -> env -> env
val with_in_loop : bool -> env -> env
val with_no_in : bool -> env -> env
val with_in_switch : bool -> env -> env
val with_in_export : bool -> env -> env
val with_no_call : bool -> env -> env
val with_error_callback : (env -> Parse_error.t -> unit) -> env -> env

val without_error_callback : env -> env

val add_label : env -> string -> env
val enter_function : env -> async:bool -> generator:bool -> env

val is_future_reserved : string -> bool
val is_strict_reserved : string -> bool
val is_restricted : string -> bool

module Peek : sig
  val token : ?i:int -> env -> Lexer_flow.Token.t
  val value : ?i:int -> env -> string
  val loc : ?i:int -> env -> Loc.t
  val errors : ?i:int -> env -> (Loc.t * Parse_error.t) list
  val comments : ?i:int -> env -> Spider_monkey_ast.Comment.t list
  val is_line_terminator : env -> bool
  val is_implicit_semicolon : env -> bool
  val semicolon_loc : ?i:int -> env -> Loc.t option
  val is_identifier : ?i:int -> env -> bool
  val is_function : ?i:int -> env -> bool
  val is_class : ?i:int -> env -> bool
end

module Eat : sig
  val token : env -> unit
  val push_lex_mode : env -> Lex_mode.t -> unit
  val pop_lex_mode : env -> unit
  val double_pop_lex_mode : env -> unit
  val semicolon : env -> unit
end

module Expect : sig
  val token : env -> Lexer_flow.Token.t -> unit
  val maybe : env -> Lexer_flow.Token.t -> bool
  val contextual : env -> string -> unit
end

module Try : sig
  type 'a parse_result =
    | ParsedSuccessfully of 'a
    | FailedToParse

  exception Rollback

  val to_parse: env -> (env -> 'a) -> 'a parse_result
end
