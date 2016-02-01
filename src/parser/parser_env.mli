(*
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This module provides a layer between the lexer and the parser which includes
 * some parser state and some lexer state *)

open Lexer_flow
open Spider_monkey_ast
module SSet : Set.S with type t = Set.Make(String).t

type lex_mode =
  | NORMAL_LEX
  | TYPE_LEX
  | JSX_TAG
  | JSX_CHILD

type token_sink_result = {
  token_loc: Loc.t;
  token: Lexer_flow.token;
  token_context: lex_mode;
  token_value: string;
}

val mode_to_string : lex_mode -> string

type parse_options = {
  esproposal_class_instance_fields: bool;
  esproposal_class_static_fields: bool;
  esproposal_decorators: bool;
  esproposal_export_star_as: bool;
  types: bool;
}
val default_parse_options : parse_options

type env

(* constructor: *)
val init_env :
  ?token_sink:(token_sink_result -> unit) option
  -> ?parse_options:parse_options option
  -> Loc.filename option
  -> Lexing.lexbuf
  -> env

(* getters: *)
val strict : env -> bool
val last : env -> (lex_env * lex_result) option
val last_token : env -> token option
val lb : env -> Lexing.lexbuf
val lookahead : ?i:int -> env -> lex_result
val lex_env : env -> lex_env
val lex_mode : env -> lex_mode
val in_export : env -> bool
val labels : env -> SSet.t
val comments : env -> Comment.t list
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

(* miscellaneous operations *)
val last_opt : env -> (lex_result -> 'a) -> 'a option
val last_value : env -> string option
val last_loc : env -> Loc.t option

(* mutators: *)
val error_at : env -> Loc.t * Parse_error.t -> unit
val comment_list : env -> Comment.t list -> unit
val error_list : env -> (Loc.t * Parse_error.t) list -> unit
val push_lex_mode : env -> lex_mode -> unit
val pop_lex_mode : env -> unit
val double_pop_lex_mode : env -> unit
val set_lex_env : env -> lex_env -> unit
val clear_lookahead_errors : env -> unit

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

module Try : sig
  type 'a parse_result =
    | ParsedSuccessfully of 'a
    | FailedToParse

  exception Rollback

  val to_parse: env -> (env -> 'a) -> 'a parse_result
end

(* TODO get rid of this abomination *)
val advance : env -> lex_env * lex_result -> lex_mode -> unit
