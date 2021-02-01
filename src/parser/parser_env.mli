(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

  val debug_string_of_lex_mode : t -> string
end

type token_sink_result = {
  token_loc: Loc.t;
  token: Token.t;
  token_context: Lex_mode.t;
}

type parse_options = {
  enums: bool;
  esproposal_class_instance_fields: bool;
  esproposal_class_static_fields: bool;
  esproposal_decorators: bool;
  esproposal_export_star_as: bool;
  esproposal_optional_chaining: bool;
  esproposal_nullish_coalescing: bool;
  types: bool;
  use_strict: bool;
}

val default_parse_options : parse_options

type env

type allowed_super =
  | No_super
  | Super_prop
  | Super_prop_or_call

(* constructor: *)
val init_env :
  ?token_sink:(token_sink_result -> unit) option ->
  ?parse_options:parse_options option ->
  File_key.t option ->
  string ->
  env

(* getters: *)
val in_strict_mode : env -> bool

val last_loc : env -> Loc.t option

val last_token : env -> Token.t option

val in_export : env -> bool

val labels : env -> SSet.t

val comments : env -> Loc.t Flow_ast.Comment.t list

val in_loop : env -> bool

val in_switch : env -> bool

val in_formal_parameters : env -> bool

val in_function : env -> bool

val allow_yield : env -> bool

val allow_await : env -> bool

val allow_directive : env -> bool

val allow_super : env -> allowed_super

val no_in : env -> bool

val no_call : env -> bool

val no_let : env -> bool

val no_anon_function_type : env -> bool

val no_new : env -> bool

val errors : env -> (Loc.t * Parse_error.t) list

val parse_options : env -> parse_options

val source : env -> File_key.t option

val should_parse_types : env -> bool

(* mutators: *)
val error_at : env -> Loc.t * Parse_error.t -> unit

val error : env -> Parse_error.t -> unit

val error_unexpected : ?expected:string -> env -> unit

val error_on_decorators : env -> (Loc.t * 'a) list -> unit

val strict_error : env -> Parse_error.t -> unit

val strict_error_at : env -> Loc.t * Parse_error.t -> unit

val function_as_statement_error_at : env -> Loc.t -> unit

val error_list : env -> (Loc.t * Parse_error.t) list -> unit

val record_export : env -> (Loc.t, Loc.t) Flow_ast.Identifier.t -> unit

val enter_class : env -> unit

val exit_class : env -> unit

val add_declared_private : env -> string -> unit

val add_used_private : env -> string -> Loc.t -> unit

val consume_comments_until : env -> Loc.position -> unit

(* functional operations -- these return shallow copies, so future mutations to
 * the returned env will also affect the original: *)
val with_strict : bool -> env -> env

val with_in_formal_parameters : bool -> env -> env

val with_in_function : bool -> env -> env

val with_allow_yield : bool -> env -> env

val with_allow_await : bool -> env -> env

val with_allow_directive : bool -> env -> env

val with_allow_super : allowed_super -> env -> env

val with_no_let : bool -> env -> env

val with_in_loop : bool -> env -> env

val with_no_in : bool -> env -> env

val with_no_anon_function_type : bool -> env -> env

val with_no_new : bool -> env -> env

val with_in_switch : bool -> env -> env

val with_in_export : bool -> env -> env

val with_no_call : bool -> env -> env

val with_error_callback : (env -> Parse_error.t -> unit) -> env -> env

val without_error_callback : env -> env

val add_label : env -> string -> env

val enter_function : env -> async:bool -> generator:bool -> env

val is_reserved : string -> bool

val token_is_reserved : Token.t -> bool

val is_future_reserved : string -> bool

val is_strict_reserved : string -> bool

val token_is_strict_reserved : Token.t -> bool

val is_restricted : string -> bool

val is_reserved_type : string -> bool

val token_is_restricted : Token.t -> bool

module Peek : sig
  val token : env -> Token.t

  val loc : env -> Loc.t

  val loc_skip_lookahead : env -> Loc.t

  val errors : env -> (Loc.t * Parse_error.t) list

  val comments : env -> Loc.t Flow_ast.Comment.t list

  val has_eaten_comments : env -> bool

  val is_line_terminator : env -> bool

  val is_implicit_semicolon : env -> bool

  val is_identifier : env -> bool

  val is_type_identifier : env -> bool

  val is_identifier_name : env -> bool

  val is_function : env -> bool

  val is_class : env -> bool

  val ith_token : i:int -> env -> Token.t

  val ith_loc : i:int -> env -> Loc.t

  val ith_errors : i:int -> env -> (Loc.t * Parse_error.t) list

  val ith_comments : i:int -> env -> Loc.t Flow_ast.Comment.t list

  val ith_is_line_terminator : i:int -> env -> bool

  val ith_is_implicit_semicolon : i:int -> env -> bool

  val ith_is_identifier : i:int -> env -> bool

  val ith_is_identifier_name : i:int -> env -> bool

  val ith_is_type_identifier : i:int -> env -> bool
end

module Eat : sig
  val token : env -> unit

  val maybe : env -> Token.t -> bool

  val push_lex_mode : env -> Lex_mode.t -> unit

  val pop_lex_mode : env -> unit

  val double_pop_lex_mode : env -> unit

  val trailing_comments : env -> Loc.t Flow_ast.Comment.t list

  val comments_until_next_line : env -> Loc.t Flow_ast.Comment.t list

  val program_comments : env -> Loc.t Flow_ast.Comment.t list
end

module Expect : sig
  val error : env -> Token.t -> unit

  val token : env -> Token.t -> unit

  val token_opt : env -> Token.t -> unit

  val identifier : env -> string -> unit
end

module Try : sig
  type 'a parse_result =
    | ParsedSuccessfully of 'a
    | FailedToParse

  exception Rollback

  val to_parse : env -> (env -> 'a) -> 'a parse_result

  val or_else : env -> fallback:'a -> (env -> 'a) -> 'a
end
