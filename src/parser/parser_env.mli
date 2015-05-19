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
module Ast = Spider_monkey_ast
open Ast
module Error = Parse_error
module SSet : Set.S with type t = Set.Make(String).t

type lex_mode =
  | NORMAL_LEX
  | TYPE_LEX
  | JSX_TAG
  | JSX_CHILD

val mode_to_string : lex_mode -> string

type env

val lex : env -> lex_mode -> lex_result

(* constructor: *)
val init_env : Lexing.lexbuf -> env

(* getters: *)
val strict : env -> bool
val last : env -> (lex_env * lex_result) option
val last_token : env -> token option
val lb : env -> Lexing.lexbuf
val lookahead : env -> lex_result
val lex_env : env -> lex_env
val lex_mode : env -> lex_mode
val in_export : env -> bool
val labels : env -> SSet.t
val comments : env -> Comment.t list
val in_loop : env -> bool
val in_switch : env -> bool
val in_function : env -> bool
val allow_yield : env -> bool
val no_in : env -> bool
val no_call : env -> bool
val no_let : env -> bool
val errors : env -> (Loc.t * Error.t) list

(* miscellaneous operations *)
val last_opt : env -> (lex_result -> 'a) -> 'a option
val last_value : env -> string option
val last_loc : env -> Loc.t option

(* mutators: *)
val error_at : env -> Loc.t * Error.t -> unit
val comment_list : env -> Comment.t list -> unit
val set_lookahead : env -> lex_result -> unit
val error_list : env -> (Loc.t * Error.t) list -> unit
val push_lex_mode : env -> lex_mode -> unit
val pop_lex_mode : env -> unit
val double_pop_lex_mode : env -> unit
val set_lex_env : env -> lex_env -> unit

(* functional operations -- these return shallow copies, so future mutations to
 * the returned env will also affect the original: *)
val with_strict : env -> bool -> env
val with_in_function : env -> bool -> env
val with_allow_yield : env -> bool -> env
val with_no_let : env -> bool -> env
val with_in_loop : env -> bool -> env
val with_no_in : env -> bool -> env
val with_in_switch : env -> bool -> env
val with_in_export : env -> bool -> env
val with_no_call : env -> bool -> env
val with_error_callback : env -> (env -> Error.t -> unit) -> env

val without_error_callback : env -> env

val add_label : env -> string -> env
val enter_function : env -> env

module Try : sig
  type 'a parse_result =
    | ParsedSuccessfully of 'a
    | FailedToParse

  exception Rollback

  val to_parse: env -> (env -> 'a) -> 'a parse_result
end

(* TODO get rid of these abominations *)
val advance : env -> lex_env * lex_result -> lex_result -> unit
val vomit : env -> unit
