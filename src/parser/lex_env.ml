(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  lex_source: File_key.t option;
  lex_lb: Sedlexing.lexbuf;
  lex_bol: bol;
  lex_in_comment_syntax: bool;
  lex_enable_comment_syntax: bool;
  lex_state: lex_state;
}

(* bol = Beginning Of Line *)
and bol = {
  line: int;
  offset: int;
}

and lex_state = { lex_errors_acc: (Loc.t * Parse_error.t) list }

let empty_lex_state = { lex_errors_acc = [] }

let new_lex_env lex_source lex_lb ~enable_types_in_comments =
  {
    lex_source;
    lex_lb;
    lex_bol = { line = 1; offset = 0 };
    lex_in_comment_syntax = false;
    lex_enable_comment_syntax = enable_types_in_comments;
    lex_state = empty_lex_state;
  }

(* copy all the mutable things so that we have a distinct lexing environment
   that does not interfere with ordinary lexer operations *)
let clone env =
  let lex_lb = env.lex_lb |> Obj.repr |> Obj.dup |> Obj.obj in
  { env with lex_lb }

let get_and_clear_state env =
  let state = env.lex_state in
  let env =
    if state != empty_lex_state then
      { env with lex_state = empty_lex_state }
    else
      env
  in
  (env, state)

let lexbuf env = env.lex_lb

let source env = env.lex_source

let state env = env.lex_state

let line env = env.lex_bol.line

let bol_offset env = env.lex_bol.offset

let is_in_comment_syntax env = env.lex_in_comment_syntax

let is_comment_syntax_enabled env = env.lex_enable_comment_syntax

let in_comment_syntax is_in env =
  if is_in <> env.lex_in_comment_syntax then
    { env with lex_in_comment_syntax = is_in }
  else
    env

(* TODO *)
let debug_string_of_lexbuf _lb = ""

let debug_string_of_lex_env (env : t) =
  let source =
    match source env with
    | None -> "None"
    | Some x -> Printf.sprintf "Some %S" (File_key.to_string x)
  in
  Printf.sprintf
    "{\n  lex_source = %s\n  lex_lb = %s\n  lex_in_comment_syntax = %b\n  lex_enable_comment_syntax = %b\n  lex_state = {errors = (count = %d)}\n}"
    source
    (debug_string_of_lexbuf env.lex_lb)
    (is_in_comment_syntax env)
    (is_comment_syntax_enabled env)
    (List.length (state env).lex_errors_acc)
