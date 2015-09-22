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

(* READ THIS BEFORE YOU MODIFY:
 *
 * The current implementation for lookahead beyond a single token is
 * inefficient. If you believe you need to increase this constant, do one of the
 * following:
 * - Find another way
 * - Benchmark your change and provide convincing evidence that it doesn't
 *   actually have a significant perf impact.
 * - Refactor this to memoize all requested lookahead, so we aren't lexing the
 *   same token multiple times.
 *)
let maximum_lookahead = 2

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

module Lookahead : sig
  type t
  val create : Lexing.lexbuf -> lex_env -> lex_mode -> t
  val peek : t -> int -> lex_result
end = struct
  type t = {
    mutable la_results    : lex_result option array;
    mutable la_num_lexed  : int;
    la_lex_mode           : lex_mode;
    mutable la_lex_env    : lex_env;
  }

  let create lb lex_env mode =
    (* copy all the mutable things so that we have a distinct lexing environment
     * that does not interfere with ordinary lexer operations *)
    (* lex_buffer has type bytes, which is itself mutable, but the lexer
     * promises not to change it so a shallow copy should be fine *)
    (* I don't know how to do a copy without an update *)
    let lb = Lexing.({ lb with lex_buffer = lb.lex_buffer }) in
    let lex_env = { lex_env with
      lex_lb = lb;
      lex_state = ref !(lex_env.lex_state)
    } in
    {
      la_results = [||];
      la_num_lexed = 0;
      la_lex_mode = mode;
      la_lex_env = lex_env;
    }

  let next_power_of_two n =
    let rec f i =
      if i >= n then
        i
      else
        f (i * 2) in
    f 1

  (* resize the tokens array to have at least n elements *)
  let grow t n =
    if Array.length t.la_results < n then begin
      let new_size = next_power_of_two n in
      let filler i =
        if i < Array.length t.la_results then
          t.la_results.(i)
        else
          None in
      let new_arr = Array.init new_size filler in
      t.la_results <- new_arr
    end

  (* precondition: there is enough room in t.la_results for the result *)
  let lex t =
    let lex_env, lex_result = lex t.la_lex_env t.la_lex_mode in
    t.la_lex_env <- lex_env;
    t.la_results.(t.la_num_lexed) <- Some lex_result;
    t.la_num_lexed <- t.la_num_lexed + 1

  let lex_until t i =
    grow t (i + 1);
    while t.la_num_lexed <= i do
      lex t
    done

  let peek t i =
    lex_until t i;
    match t.la_results.(i) with
      | Some result -> result
      (* only happens if there is a defect in the lookahead module *)
      | None -> failwith "Lookahead.peek failed"
end

type token_sink_result = {
  token_loc: Loc.t;
  token: Lexer_flow.token;
  token_context: lex_mode;
  token_value: string;
}

type parse_options = {
  experimental_decorators: bool;
}
let default_parse_options = {
  experimental_decorators = false;
}

type env = {
  errors            : (Loc.t * Error.t) list ref;
  comments          : Comment.t list ref;
  labels            : SSet.t;
  (* the lex buffer in the state after a single lookahead *)
  lb                : Lexing.lexbuf;
  single_lookahead  : lex_result ref;
  last              : (lex_env * lex_result) option ref;
  priority          : int;
  strict            : bool;
  in_export         : bool;
  in_loop           : bool;
  in_switch         : bool;
  in_function       : bool;
  no_in             : bool;
  no_call           : bool;
  no_let            : bool;
  allow_yield       : bool;
  allow_await       : bool;
  error_callback    : (env -> Error.t -> unit) option;
  lex_mode_stack    : lex_mode list ref;
  (* lex_env is the lex_env after the single lookahead has been lexed *)
  lex_env           : lex_env ref;
  (* infinite lookahead -- serves tokens two through infinity. For the first
   * token of lookahead, use single_lookahead it's convoluted because too many
   * places assume that the state here is consistent with having lexed exactly
   * one token of lookahead. This needs to be cleared whenever we advance. *)
  lookahead         : Lookahead.t option ref;
  token_sink        : (token_sink_result -> unit) option ref;
  parse_options     : parse_options;
}

(* constructor *)
let init_env ?(token_sink=None) ?(parse_options=None) lb =
  let parse_options =
    match parse_options with
    | Some opts -> opts
    | None -> default_parse_options
  in
  let lex_env = new_lex_env lb in
  let lex_env, lookahead = lex lex_env NORMAL_LEX in
  {
    errors            = ref [];
    comments          = ref [];
    labels            = SSet.empty;
    lb                = lb;
    single_lookahead  = ref lookahead;
    last              = ref None;
    priority          = 0;
    strict            = false;
    in_export         = false;
    in_loop           = false;
    in_switch         = false;
    in_function       = false;
    no_in             = false;
    no_call           = false;
    no_let            = false;
    allow_yield       = true;
    allow_await       = false;
    error_callback    = None;
    lex_mode_stack    = ref [NORMAL_LEX];
    lex_env           = ref lex_env;
    lookahead         = ref None;
    token_sink        = ref token_sink;
    parse_options;
  }

(* getters: *)
let strict env = env.strict
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
let allow_await env = env.allow_await
let no_in env = env.no_in
let no_call env = env.no_call
let no_let env = env.no_let
let errors env = !(env.errors)
let parse_options env = env.parse_options

(* mutators: *)
let error_at env (loc, e) =
  env.errors := (loc, e) :: !(env.errors);
  match env.error_callback with
  | None -> ()
  | Some callback -> callback env e
let comment_list env =
  List.iter (fun c -> env.comments := c :: !(env.comments))
let set_lex_env env lex_env = env.lex_env := lex_env

(* lookahead: *)
let lookahead ?(i=0) env =
  assert (i < maximum_lookahead);
  if i == 0 then
    !(env.single_lookahead)
  else
    let lookahead = match !(env.lookahead) with
      | Some l -> l
      | None -> begin
          let l = Lookahead.create (lb env) (lex_env env) (lex_mode env) in
          env.lookahead := Some l;
          l
        end in
    Lookahead.peek lookahead (i - 1)

let set_lookahead env l = env.single_lookahead := l
let clear_lookahead_errors env =
  let lookahead = { (lookahead env) with lex_errors = [] } in
  set_lookahead env lookahead

(* functional operations: *)
let with_strict strict env = { env with strict }
let with_in_function in_function env = { env with in_function }
let with_allow_yield allow_yield env = { env with allow_yield }
let with_allow_await allow_await env = { env with allow_await }
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

let advance env (lex_env, lex_result) next_lex_mode =
  (* If there's a token_sink, emit the lexed token before moving forward *)
  (match !(env.token_sink) with
    | None -> ()
    | Some token_sink ->
        let {lex_loc; lex_token; lex_value; _;} = lex_result in
        token_sink {
          token_loc=lex_loc;
          token=lex_token;
          (**
           * The lex mode is useful because it gives context to some
           * context-sensitive tokens.
           *
           * Some examples of such tokens include:
           *
           * `=>` - Part of an arrow function? or part of a type annotation?
           * `<`  - A less-than? Or an opening to a JSX element?
           * ...etc...
           *)
          token_context=(lex_mode env);
          token_value=lex_value
        }
  );
  let next_lex_result =
    if lex_result.lex_token = T_EOF then
      (* There's no next token, so we don't lex, we just pretend that the EOF is
       * next *)
      lex_result
    else
      lex env next_lex_mode
  in
  error_list env lex_result.lex_errors;
  comment_list env lex_result.lex_comments;
  env.last := Some (lex_env, lex_result);
  env.single_lookahead := next_lex_result;
  env.lookahead := None

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
      env.single_lookahead := {
        lex_token = T_ERROR;
        lex_loc = Loc.none;
        lex_value = "";
        lex_errors = [];
        lex_comments = [];
        lex_lb_curr_p = none_curr_p;
      };
      env.lookahead := None
  | Some (lex_env, result) ->
      env.lex_env := lex_env;
      let bytes = env.lb.lex_curr_p.pos_cnum - result.lex_lb_curr_p.pos_cnum in
      env.lb.lex_curr_pos <- env.lb.lex_curr_pos - bytes;
      env.lb.lex_curr_p <- result.lex_lb_curr_p;
      env.single_lookahead := result;
      env.lookahead := None
  )

(* Switching modes requires rolling back one token so that we can re-lex the
 * lookahead token *)
let re_lex_lookahead env =
  vomit env;
  set_lookahead env (lex env (lex_mode env));
  env.lookahead := None

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
let enter_function env ~async ~generator = { env with
    in_function = true;
    in_loop = false;
    in_switch = false;
    labels = SSet.empty;
    allow_await = async;
    allow_yield = generator;
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
    token_buffer         : ((token_sink_result -> unit) * token_sink_result Queue.t) option;
  }

  let save_state env =
    let token_buffer =
      match !(env.token_sink) with
      | None -> None
      | Some orig_token_sink ->
          let buffer = Queue.create () in
          env.token_sink := Some(fun token_data ->
            Queue.add token_data buffer
          );
          Some(orig_token_sink, buffer)
    in
    {
      saved_errors         = !(env.errors);
      saved_comments       = !(env.comments);
      saved_lb             =
        Lexing.({env.lb with lex_abs_pos=env.lb.lex_abs_pos});
      saved_lookahead      = !(env.single_lookahead);
      saved_last           = !(env.last);
      saved_lex_mode_stack = !(env.lex_mode_stack);
      saved_lex_env        = !(env.lex_env);
      token_buffer;
    }

  let reset_token_sink ~flush env token_buffer_info =
    match token_buffer_info with
    | None -> ()
    | Some(orig_token_sink, token_buffer) ->
        env.token_sink := Some orig_token_sink;
        if flush then Queue.iter orig_token_sink token_buffer

  let rollback_state env saved_state =
    reset_token_sink ~flush:false env saved_state.token_buffer;
    env.errors := saved_state.saved_errors;
    env.comments := saved_state.saved_comments;
    env.single_lookahead := saved_state.saved_lookahead;
    env.last := saved_state.saved_last;
    env.lex_mode_stack := saved_state.saved_lex_mode_stack;
    env.lex_env := saved_state.saved_lex_env;
    env.lookahead := None;

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

  let success env saved_state result =
    reset_token_sink ~flush:true env saved_state.token_buffer;
    ParsedSuccessfully result

  let to_parse env parse =
    let saved_state = save_state env in
    try success env saved_state (parse env)
    with Rollback -> rollback_state env saved_state
end
