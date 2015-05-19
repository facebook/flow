(*
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Lexer_flow
module Ast = Spider_monkey_ast
open Ast
module Error = Parse_error
module SSet = Set.Make(String)
module SMap = Map.Make(String)

type lex_mode =
  | NORMAL_LEX
  | TYPE_LEX
  | JSX_TAG
  | JSX_CHILD

let mode_to_string = function
  | NORMAL_LEX -> "NORMAL"
  | TYPE_LEX -> "TYPE"
  | JSX_TAG -> "JSX TAG"
  | JSX_CHILD -> "JSX CHILD"

let lex lex_env = function
  | NORMAL_LEX -> token lex_env
  | TYPE_LEX -> type_token lex_env
  | JSX_TAG -> lex_jsx_tag lex_env
  | JSX_CHILD -> lex_jsx_child lex_env

type env = {
  errors          : (Loc.t * Error.t) list ref;
  comments        : Comment.t list ref;
  labels          : SSet.t;
  lb              : Lexing.lexbuf;
  lookahead       : lex_result ref;
  last            : (lex_env * lex_result) option ref;
  priority        : int;
  strict          : bool;
  in_export       : bool;
  in_loop         : bool;
  in_switch       : bool;
  in_function     : bool;
  no_in           : bool;
  no_call         : bool;
  no_let          : bool;
  allow_yield     : bool;
  (* Use this to indicate that the "()" as in "() => 123" is not allowed in
   * this expression *)
  error_callback  : (env -> Error.t -> unit) option;
  lex_mode_stack  : lex_mode list ref;
  lex_env         : lex_env ref;
}

(* constructor *)
let init_env lb =
  let lex_env = new_lex_env lb in
  let lex_env, lookahead = lex lex_env NORMAL_LEX in
  {
    errors          = ref [];
    comments        = ref [];
    labels          = SSet.empty;
    lb              = lb;
    lookahead       = ref lookahead;
    last            = ref None;
    priority        = 0;
    strict          = false;
    in_export       = false;
    in_loop         = false;
    in_switch       = false;
    in_function     = false;
    no_in           = false;
    no_call         = false;
    no_let          = false;
    allow_yield     = true;
    error_callback  = None;
    lex_mode_stack  = ref [NORMAL_LEX];
    lex_env         = ref lex_env;
  }

(* getters: *)
let strict env = env.strict
let lookahead env = !(env.lookahead)
let lb env = env.lb
let lex_mode env = List.hd !(env.lex_mode_stack)
let lex_env env = !(env.lex_env)
let last env = !(env.last)
let in_export env = env.in_export
let comments env = !(env.comments)
let labels env = env.labels
let in_loop env = env.in_loop
let in_switch env = env.in_switch
let in_function env = env.in_function
let allow_yield env = env.allow_yield
let no_in env = env.no_in
let no_call env = env.no_call
let no_let env = env.no_let
let errors env = !(env.errors)

(* mutators: *)
let error_at env (loc, e) =
  env.errors := (loc, e) :: !(env.errors);
  match env.error_callback with
  | None -> ()
  | Some callback -> callback env e
let comment_list env =
  List.iter (fun c -> env.comments := c :: !(env.comments))
let set_lookahead env l = env.lookahead := l
let set_lex_env env lex_env = env.lex_env := lex_env

(* functional operations: *)
let with_strict strict env = { env with strict }
let with_in_function in_function env = { env with in_function }
let with_allow_yield allow_yield env = { env with allow_yield }
let with_no_let no_let env = { env with no_let }
let with_in_loop in_loop env = { env with in_loop }
let with_no_in no_in env = { env with no_in }
let with_in_switch in_switch env = { env with in_switch }
let with_in_export in_export env = { env with in_export }
let with_no_call no_call env = { env with no_call }
let with_error_callback error_callback env =
  { env with error_callback = Some error_callback }

(* other helper functions: *)
let error_list env = List.iter (error_at env)
let last_opt env fn = match !(env.last) with
  | None -> None
  | Some (_, result) -> Some (fn result)
let last_token env = last_opt env (fun result -> result.lex_token)
let last_value env = last_opt env (fun result -> result.lex_value)
let last_loc env = last_opt env (fun result -> result.lex_loc)

let lex env mode =
  let lex_env, lex_result = lex !(env.lex_env) mode in
  env.lex_env := lex_env;
  lex_result

let advance env (lex_env, lex_result) next_lex_result =
  error_list env lex_result.lex_errors;
  comment_list env lex_result.lex_comments;
  env.last := Some (lex_env, lex_result);
  env.lookahead := next_lex_result

let vomit env =
  Lexing.(match last env with
  | None ->
      env.lb.lex_curr_pos <- 0;
      let none_curr_p = {
        pos_fname = env.lb.lex_curr_p.pos_fname;
        pos_bol = 0;
        pos_lnum = 1;
        pos_cnum = 0;
      } in
      env.lb.lex_curr_p <- none_curr_p;
      env.lookahead := {
        lex_token = T_ERROR;
        lex_loc = Loc.none;
        lex_value = "";
        lex_errors = [];
        lex_comments = [];
        lex_lb_curr_p = none_curr_p;
      }
  | Some (lex_env, result) ->
      env.lex_env := lex_env;
      let bytes = env.lb.lex_curr_p.pos_cnum - result.lex_lb_curr_p.pos_cnum in
      env.lb.lex_curr_pos <- env.lb.lex_curr_pos - bytes;
      env.lb.lex_curr_p <- result.lex_lb_curr_p;
      env.lookahead := result
  )

(* Switching modes requires rolling back one token so that we can re-lex the
 * lookahead token *)
let re_lex_lookahead env =
  vomit env;
  set_lookahead env (lex env (lex_mode env))

let push_lex_mode env mode =
  env.lex_mode_stack := mode :: !(env.lex_mode_stack);
  re_lex_lookahead env

let pop_lex_mode env =
  let new_stack = match !(env.lex_mode_stack) with
  | _mode::stack -> stack
  | _ -> failwith "Popping lex mode from empty stack" in
  env.lex_mode_stack := new_stack;
  re_lex_lookahead env

let double_pop_lex_mode env =
  let new_stack = match !(env.lex_mode_stack) with
  | _::_::stack -> stack
  | _ -> failwith "Popping lex mode from empty stack" in
  env.lex_mode_stack := new_stack;
  re_lex_lookahead env

let without_error_callback env = { env with error_callback = None }

let add_label env label = { env with labels = SSet.add label env.labels }
let enter_function env = { env with
    in_function = true;
    in_loop = false;
    in_switch = false;
    labels = SSet.empty;
  }

(* This module allows you to try parsing and rollback if you need. This is not
 * cheap and its usage is strongly discouraged *)
module Try = struct
  type 'a parse_result =
    | ParsedSuccessfully of 'a
    | FailedToParse

  exception Rollback

  type saved_state = {
    saved_errors         : (Loc.t * Error.t) list;
    saved_comments       : Ast.Comment.t list;
    saved_lb             : Lexing.lexbuf;
    saved_lookahead      : lex_result;
    saved_last           : (lex_env * lex_result) option;
    saved_lex_mode_stack : lex_mode list;
    saved_lex_env        : lex_env;
  }

  let save_state env = {
    saved_errors         = !(env.errors);
    saved_comments       = !(env.comments);
    saved_lb             =
      Lexing.({env.lb with lex_abs_pos=env.lb.lex_abs_pos});
    saved_lookahead      = !(env.lookahead);
    saved_last           = !(env.last);
    saved_lex_mode_stack = !(env.lex_mode_stack);
    saved_lex_env        = !(env.lex_env);
  }

  let rollback_state env saved_state =
    env.errors := saved_state.saved_errors;
    env.comments := saved_state.saved_comments;
    env.lookahead := saved_state.saved_lookahead;
    env.last := saved_state.saved_last;
    env.lex_mode_stack := saved_state.saved_lex_mode_stack;
    env.lex_env := saved_state.saved_lex_env;

    Lexing.(begin
      env.lb.lex_buffer <- saved_state.saved_lb.lex_buffer;
      env.lb.lex_buffer_len <- saved_state.saved_lb.lex_buffer_len;
      env.lb.lex_abs_pos <- saved_state.saved_lb.lex_abs_pos;
      env.lb.lex_start_pos <- saved_state.saved_lb.lex_start_pos;
      env.lb.lex_curr_pos <- saved_state.saved_lb.lex_curr_pos;
      env.lb.lex_last_pos <- saved_state.saved_lb.lex_last_pos;
      env.lb.lex_last_action <- saved_state.saved_lb.lex_last_action;
      env.lb.lex_eof_reached <- saved_state.saved_lb.lex_eof_reached;
      env.lb.lex_mem <- saved_state.saved_lb.lex_mem;
      env.lb.lex_start_p <- saved_state.saved_lb.lex_start_p;
      env.lb.lex_curr_p <- saved_state.saved_lb.lex_curr_p;
    end);

    FailedToParse

  let to_parse env parse =
    let saved_state = save_state env in
    try ParsedSuccessfully (parse env)
    with Rollback -> rollback_state env saved_state
end
