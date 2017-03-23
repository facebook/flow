(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Ast = Spider_monkey_ast

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
