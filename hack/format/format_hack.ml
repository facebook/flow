(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
(*****************************************************************************)
(* Imported modules. *)
(*****************************************************************************)
open Core
open Lexer_hack

exception Format_error
exception PHP
exception One_line

(*****************************************************************************)
(* The precedence of the Tarrow operator (->). *)
(*****************************************************************************)

let tarrow_prec = snd (Parser_hack.get_priority Tarrow)

(*****************************************************************************)
(* The environment *)
(*****************************************************************************)

type char_kind =
  (* The last emitted token was a new line *)
  | Newline

  (* The last emitted token was XHP text *)
  | Text

  (* The last emitted token was a space *)
  | Space

  (* Everything else *)
  | Other

(* Absolute character position in the input file. *)
type char_pos = int

type source_tag =
  (* Line number in the input file *)
  | Line of int

  (* Beginning of an indivisible formatting block *)
  | Block

(* Meta-data to be able to reconcile the input file and the
 * formatted output (useful for Format_diff)
 *)
type source_pos = char_pos * source_tag

type env = {
    (* The number of spaces for the margin *)
    margin     : int ref              ;

    (* The last kind of token emitted *)
    last       : char_kind ref        ;

    (* The last token emitted *)
    last_token : Lexer_hack.token ref ;

    (* The string representing last token emitted *)
    last_str   : string ref           ;

    (* The string that must be outputted when printing
     * the last token (can be different from last_str, cf function token) *)
    last_out   : string ref           ;

    (* The output buffer *)
    buffer     : Buffer.t             ;

    (* The path of the current file *)
    file       : Path.t               ;

    (* The state of the lexer *)
    lexbuf     : Lexing.lexbuf        ;

    (* The line number of the input *)
    lb_line    : int ref              ;

    (* The precedence of the current binary operator (0 otherwise) *)
    priority   : int                  ;

    (* The output character position (from the beginning of the line) *)
    char_pos   : int ref              ;

    (* The output absolute character position *)
    abs_pos    : int ref              ;

    (* The char position after which we break (typically 80).
     * It can be different from char_size, because in some cases we need
     * room to leave the semi-colon on the same line.
     *)
    char_break : int                  ;
    char_size  : int                  ;

    (* The precedence of the operator we should try to break *)
    break_on   : int                  ;

    (* The output line number *)
    line       : int ref              ;

    (* > 0 if the current try failed
     * = 0 if the current not trying anything
     * < 0 if we should not bother trying but force to break
     * You should checkout the module Try if this doesn't make sense.
     *)
    failed     : int ref              ;

    (* The depth of nested try_outer we are currently in *)
    try_depth  : int                  ;

    (* True if we are trying to emit something that must fit on one line *)
    one_line   : bool                 ;

    (* True if we should set env.failed on failure *)
    report_fit : bool                 ;

    (* True if we are in an attribute (<< ... >>) *)
    in_attr    : bool                 ;

    (* The amount of spaces that must be emitted on the next token
     * (this is for the margin).
     *)
    spaces     : int ref              ;

    (* True if we should stop after a certain position. *)
    stop       : int                  ;

    (* True when we should not emit to the buffer (useful for region mode) *)
    silent     : bool ref             ;

    (* The beginning of the region we are trying to format. *)
    from       : int                  ;

    (* The end of the region we are trying to format. *)
    to_        : int                  ;

    (* When the "keep_source_pos" option is turned on,
     * the formatter outputs extra tags in the field source_pos.
     * There are 2 kinds of tags 'Line and 'Block'
     * (these tags are used by Format_diff).
     *
     * The tags of the form 'Line' should be read as: at this
     * point in the text, the input line number was LINE_NUMBER
     * (useful to reconcile input/output line numbers).
     *
     * The tags of the form 'Block' should be read as: we have reached the
     * beginning or the end of an indivisible block.
     *
     * This information is useful to know which pieces can be formatted
     * separately. For example, let's consider the following diff:
     *  $x = array(
     * -  1,
     * +  23,
     *    2,
     *  );
     * It doesn't make sense to only format the line that changed.
     * The formatter is probably going to regroup the entire array on
     * one line (because it fits).
     * Thanks to these extra tags, we know that we have to treat the entire
     * array as an indivisible entity.
     *)
    keep_source_pos : bool             ;
    source_pos_l : source_pos list ref ;

    (* When no_trailing_commas is false (default), multiline comma separated items
      include a trailing comma, when it is false, we omit trailing commas. The
      standard php parser does not support trailing commas, but hhvm does.
    *)
    no_trailing_commas : bool             ;
  }

(*****************************************************************************)
(* The "saved" environment (to allow backtracking). *)
(*****************************************************************************)

type saved_env = {
    sv_margin     : int                  ;
    sv_last       : char_kind            ;
    sv_last_token : Lexer_hack.token     ;
    sv_last_str   : string               ;
    sv_last_out   : string               ;
    sv_buffer     : Buffer.t             ;
    sv_lexbuf     : Parser_hack.saved_lb ;
    sv_lb_line    : int                  ;
    sv_char_pos   : int                  ;
    sv_abs_pos    : int                  ;
    sv_line       : int                  ;
    sv_failed     : int                  ;
    sv_spaces     : int                  ;
    sv_silent     : bool                 ;
    sv_source_pos_l  : source_pos list     ;
  }

let empty file lexbuf from to_ keep_source_pos no_trailing_commas = {
  margin     = ref 0                          ;
  last       = ref Newline                    ;
  last_token = ref Terror                     ;
  last_str   = ref ""                         ;
  last_out   = ref ""                         ;
  buffer     = Buffer.create 256              ;
  file       = file                           ;
  lexbuf     = lexbuf                         ;
  lb_line    = ref 1                          ;
  priority   = 0                              ;
  char_pos   = ref 0                          ;
  abs_pos    = ref 0                          ;
  char_size  = 80                             ;
  char_break = 80                             ;
  break_on   = max_int                        ;
  line       = ref 0                          ;
  report_fit = false                          ;
  failed     = ref 0                          ;
  try_depth  = 0                              ;
  one_line   = false                          ;
  in_attr    = false                          ;
  spaces     = ref 0                          ;
  stop       = max_int                        ;
  silent     = ref false                      ;
  from                                        ;
  to_                                         ;
  keep_source_pos                             ;
  source_pos_l  = ref []                      ;
  no_trailing_commas = no_trailing_commas     ;
}

(* Saves all the references of the environment *)
let save_env env =
  let { margin; last; last_token; buffer; file; lexbuf; lb_line;
        priority; char_pos; abs_pos; char_break;
        char_size; silent; one_line;
        last_str; last_out; keep_source_pos; source_pos_l;
        break_on; line; failed; try_depth; spaces;
        report_fit; in_attr; stop; from; to_; no_trailing_commas } = env in
  { sv_margin = !margin;
    sv_last = !last;
    sv_buffer = env.buffer;
    sv_last_token = !last_token;
    sv_last_str = !last_str;
    sv_last_out = !last_out;
    sv_lexbuf = Parser_hack.save_lexbuf_state lexbuf;
    sv_lb_line = !lb_line;
    sv_char_pos = !char_pos;
    sv_abs_pos = !abs_pos;
    sv_line = !line;
    sv_failed = !failed;
    sv_spaces = !spaces;
    sv_silent = !silent;
    sv_source_pos_l = !source_pos_l;
  }

let restore_env env saved_env =
  Parser_hack.restore_lexbuf_state env.lexbuf saved_env.sv_lexbuf;
  env.lb_line := saved_env.sv_lb_line;
  env.margin := saved_env.sv_margin;
  env.last := saved_env.sv_last;
  env.last_token := saved_env.sv_last_token;
  env.last_str := saved_env.sv_last_str;
  env.last_out := saved_env.sv_last_out;
  env.char_pos := saved_env.sv_char_pos;
  env.abs_pos := saved_env.sv_abs_pos;
  env.line := saved_env.sv_line;
  env.failed := saved_env.sv_failed;
  env.spaces := saved_env.sv_spaces;
  env.silent := saved_env.sv_silent;
  env.source_pos_l := saved_env.sv_source_pos_l;
  { env with buffer = saved_env.sv_buffer }

(*****************************************************************************)
(* Consumes the next token.
 * The logic is a bit complex because of the regions.
 * If hh_format is called with -from -to options, we want to start/stop
 * emitting text depending on the position.
 * The problem is that -from could point to the middle of a token.
 * When that happens we split the token in 2, the relevant part (the one
 * we want to emit) is kept in env.last_out, the full string is kept in
 * env.last_str.
 * Both env.last_out/env.last_str are useful:
 *   -) env.last_out is used to emit the token (that's why we use the function
 *      last_token instead of emitting a string directly)
 *   -) env.last_str is used for the logic of the parser (we can't use a
 *      truncated token for that)
 *)
(*****************************************************************************)

let make_tokenizer next_token env =
  let pos = env.lexbuf.Lexing.lex_curr_pos in
  if pos >= env.stop then Teof else
  let tok = next_token env.lexbuf in
  let new_pos = env.lexbuf.Lexing.lex_curr_pos in
  env.silent := (new_pos <= env.from || pos >= env.to_ - 1);
  env.last_token := tok;
  let str_value = Lexing.lexeme env.lexbuf in
  env.last_str := str_value;
  (* Splitting the token (-from) *)
  if pos < env.from && new_pos > env.from
  then begin
    let sub_size = new_pos - env.from + 1 in
    let str_size = String.length str_value in
    let start = str_size - sub_size in
    env.last_out := String.sub str_value start sub_size;
  end
  (* Splitting the token (-to) *)
  else if pos < env.to_ - 1 && new_pos >= env.to_ - 1
  then begin
    let sub_size = env.to_ - pos - 1 in
    env.last_out := String.sub str_value 0 sub_size;
  end
  else env.last_out := str_value;
  (match tok with
  | Tnewline ->
      env.lb_line := !(env.lb_line) + 1
  | _ -> ()
  );
  tok

(* Normal tokenizer *)
let token = make_tokenizer Lexer_hack.format_token

(* XHP tokenizer *)
let xhp_token = make_tokenizer Lexer_hack.format_xhptoken

(* Comment tokenizer *)
let comment_token = make_tokenizer Lexer_hack.format_comment

(*****************************************************************************)
(* Backtracking. *)
(*****************************************************************************)

let back env =
  if !(env.last_token) = Tnewline
  then env.lb_line := !(env.lb_line) - 1;
  env.last_token := Terror;
  Lexer_hack.back env.lexbuf

(*****************************************************************************)
(* Primitives used to look ahead. *)
(*****************************************************************************)

(* Attempt does not modify the state of the environment *)
let attempt env f =
  let buffer = Buffer.create 256 in
  let saved_env = save_env env in
  let f_result = f { env with buffer } in
  let _ = restore_env env saved_env in
  f_result

let attempt_keep_lines env f =
  attempt env begin fun env ->
    let buffer = Buffer.create 256 in
    let env = { env with buffer } in
    let line = !(env.line) in
    let _ = f { env with report_fit = true } in
    let nbr_lines = !(env.line) - line in
    buffer, nbr_lines, !(env.failed)
  end

(*****************************************************************************)
(* Primitives for branching.
 *
 * The branching logic always tries to break the outer-most expression.
 *
 * For example:
 * array(array(array(...)))
 * should be first rewritten by trying:
 * array(
 *   array(array(...))
 * )
 * Then:
 * array(
 *   array(
 *     array(...)
 *   )
 * )
 *
 * However, the logic is a bit complicated because the algorithm is
 * exponential and that becomes a problem on very large nested arrays.
 * The solution consist in breaking multiple layers at once when a
 * certain depth is reached.
 *
 * Let's consider: array(array(.. array N times ))
 * If the array breaks at a depth larger than 6 we directly try:
 * array(
 *   array(
 *     array( ... N/2 times
 *
 * We preemptively break the array N/2 times, to avoid the exponential.
 *)
(*****************************************************************************)

module Try: sig

  val one_line: env -> (env -> unit) -> (env -> unit) -> unit
  val outer: env -> (env -> unit) -> (env -> unit) -> unit
end = struct

  let try_raw env f1 f2 =
    let saved_env = save_env env in
    let buffer = Buffer.create 256 in
    let env = { env with buffer } in
    let f1_result = f1 { env with report_fit = true } in
    if !(env.failed) <= 0
    then begin
      Buffer.add_buffer saved_env.sv_buffer buffer;
      f1_result
    end
    else begin
      let env = restore_env env saved_env in
      f2 env
    end

  let one_line env f1 f2 =
    if env.one_line then f1 env else
    try_raw env
      begin fun env ->
        try ignore (f1 { env with one_line = true })
        with One_line ->
          env.failed := 1
      end
      f2

  let outer env f1 f2 =
    if env.try_depth > 0
    then f1 { env with try_depth = env.try_depth + 1 }
    else if env.try_depth < 0
    then f2 { env with try_depth = env.try_depth + 1 }
    else
      let depth_failed = ref 0 in
      let big_buffer = ref false in
      try_raw env
        (fun env ->
          f1 { env with try_depth = 1 };
          big_buffer := Buffer.length env.buffer > 10_000;
          depth_failed := !(env.failed);
        )
        (fun env ->
          if !depth_failed > 6
          then f2 { env with try_depth = - (!depth_failed / 2) }
          else if !big_buffer
          then f2 { env with try_depth = - 3 }
          else f2 env)
end

(*****************************************************************************)
(* Scoring functions.
 * There are cases where multiple choices could fit. When that's the case,
 * we pick the one that "looks" nicer.
 * The "looks" function is pretty subjective ;-)
 *)
(*****************************************************************************)

let rec aligned last_tok count lexbuf =
  match Lexer_hack.format_token lexbuf with
  | Teof -> count
  | Tspace -> aligned last_tok count lexbuf
  | _ ->
      let tok = Lexing.lexeme lexbuf in
      if last_tok = ")" && tok = "->" then -100 else
      let count = if last_tok = tok && last_tok = "->"
        then count + 1 else count in
      aligned_look_for_newline tok count lexbuf

and aligned_look_for_newline last_tok count lexbuf =
  match Lexer_hack.format_token lexbuf with
  | Teof -> count
  | Tnewline -> aligned last_tok count lexbuf
  | _ -> aligned_look_for_newline last_tok count lexbuf

let keep_best env f1 f2 =
  if env.one_line then f1 env else
  let env = { env with try_depth = 0 } in
  let buffer1, nbr_lines1, failed1 = attempt_keep_lines env f1 in
  let buffer2, nbr_lines2, failed2 = attempt_keep_lines env f2 in
  if failed1 > 0 then f2 env else
  if failed2 > 0 then f1 env else
  let buffer1 = Buffer.contents buffer1 in
  let buffer2 = Buffer.contents buffer2 in
  (* The logic to select the best solution *)
  let aligned_count1 = aligned "" 0 (Lexing.from_string buffer1) in
  let aligned_count2 = aligned "" 0 (Lexing.from_string buffer2) in
  if aligned_count2 > aligned_count1 then f2 env else
  if aligned_count1 < aligned_count2 then f1 env else
  if nbr_lines1 <= nbr_lines2 then f1 env else f2 env

(*****************************************************************************)
(* Returns the current position in the buffer. *)
(*****************************************************************************)

let get_pos env =
  env.lexbuf.Lexing.lex_curr_pos

(*****************************************************************************)
(* Pretty printing primitives.
 * We don't want to maintain the state of pretty-printer all the time.
 * This module keeps track of what the margin should be (adds spaces when
 * needed), removes spaces when they are followed by a new line etc ...
 *)
(*****************************************************************************)

module Pp: sig

  val out: string -> env -> unit
  val last_token: env -> unit
  val margin_set: int -> env -> (env -> 'a) -> 'a
  val right: env -> (env -> 'a) -> 'a
  val right_fun: (env -> 'a) -> env -> 'a
  val right_n: int -> env -> (env -> 'a) -> 'a
  val force_nl: env -> unit
  val newline: env -> unit
  val space: env -> unit
  val keep_space: env -> unit

end = struct

  let buf_add_char env c =
    if not !(env.silent) then begin
      Buffer.add_char env.buffer c
    end

  let buf_add_string env s =
    if not !(env.silent) then begin
      Buffer.add_string env.buffer s
    end

  let add_char_pos env n =
    env.char_pos := !(env.char_pos) + n;
    env.abs_pos := !(env.abs_pos) + n;
    if env.report_fit && !(env.char_pos) >= env.char_break then begin
      if env.one_line then raise One_line;
      env.failed := max 1 (max !(env.failed) env.try_depth)
    end;
    ()

  let add_char env c =
    buf_add_char env c;
    add_char_pos env 1

  let add_string env s =
    buf_add_string env s;
    add_char_pos env (String.length s)

  let force_nl env =
    env.char_pos := 0;
    env.last := Newline;
    env.line := !(env.line) + 1;
    env.spaces := 0;
    add_char env '\n';
    if env.keep_source_pos then begin
      let source_pos = !(env.abs_pos), Line !(env.lb_line) in
      env.source_pos_l := source_pos :: !(env.source_pos_l)
    end

  let newline env =
    if env.one_line then raise One_line;
    if !(env.last) <> Newline then force_nl env

  let space env =
    if !(env.last) <> Space then begin
      env.last := Space;
      env.spaces := !(env.spaces) + 1;
    end

  let keep_space env =
    assert (!(env.last_token) = Tspace);
    let str = !(env.last_out) in
    env.last := Space;
    String.iter (fun c -> assert (c = ' ')) str;
    env.spaces := !(env.spaces) + String.length str

  let right_n n env f =
    env.margin := !(env.margin) + n;
    let result = f env in
    env.margin := !(env.margin) - n;
    result

  let margin_set n env f =
    let margin_cpy = !(env.margin) in
    env.margin := n;
    let result = f env in
    env.margin := margin_cpy;
    result

  let right env f = right_n 2 env f
  let right_fun f = fun env -> right env f

  let out s env =
    if !(env.last) = Newline then env.spaces := !(env.margin);
    for i = 0 to !(env.spaces) - 1 do
      add_char env ' '
    done;
    env.spaces := 0;
    add_string env s;
    env.last := Other;
    ()

  let last_token env =
    out !(env.last_out) env

end

open Pp

(*****************************************************************************)
(* Some helpers to regroup sequences of pretty-printing functions. *)
(*****************************************************************************)

let rec seq env = function
  | [] -> ()
  | f :: rl -> f env; seq env rl

let seq_fun l = fun env -> seq env l

let line env l =
  seq env l;
  newline env

let out_next env =
  ignore (token env);
  last_token env

let ignore_ f env = ignore (f env)

(*****************************************************************************)
(* Precedence of binary operators.
 * We need to maintain that information to break expressions with the lowest
 * precedence first.
 * Example: 1 * 2 * 3 + 4
 * We must first try:
 *    1 * 2 * 3 +
 *    4
 * Before we try to to break (1 * 2 * 3).
 * These functions keep track the precedence of the current operator to later
 * on prioritize in what order we will break an expression (when necessary).
 *)
(*****************************************************************************)

let set_priority env priority =
  { env with priority }

let reset_priority env =
  { env with priority = 0 }

let with_priority env op f =
  let _, prio = Parser_hack.get_priority op in
  let env = set_priority env prio in
  f env

(*****************************************************************************)
(* Add block tag. Used for --diff mode.
 * We don't have to worry about Opening or Closing blocks, because the logic
 * is: whatever is in between 2 blocks is indivisible.
 * Why is that? Because the place where we add the block tag are the places
 * where we know it's acceptable to break the indentation.
 * Think of it this way: block tags tell us where we can break the formatting
 * given that, whatever is in between two block tags is indivisible.
 *
 * Note that we only insert Block tags where the existing code has a line
 * break. See Format_diff.TextBlocks for more details.
 *)
(*****************************************************************************)

let add_block_tag env =
  assert (!(env.last) = Newline);
  if env.keep_source_pos && attempt env token = Tnewline then begin
    let source_pos = !(env.abs_pos), Block in
    env.source_pos_l := source_pos :: !(env.source_pos_l)
  end

(*****************************************************************************)
(* Comments *)
(*****************************************************************************)

let rec skip_spaces env =
  match token env with
  | Teof -> ()
  | Tspace -> skip_spaces env
  | _ -> back env

let rec skip_spaces_and_nl env =
  match token env with
  | Teof -> ()
  | Tspace | Tnewline -> skip_spaces_and_nl env
  | _ -> back env

let rec comment env =
  right_n 1 env comment_loop

and comment_loop env =
  match comment_token env with
  | Teof -> ()
  | Tclose_comment ->
      last_token env;
  | Tnewline ->
      newline env;
      skip_spaces env;
      comment_loop env;
  | Tspace ->
      keep_space env;
      comment_loop env
  | _ ->
      last_token env;
      comment_loop env

let rec line_comment env =
  line_comment_loop env

and line_comment_loop env =
  match token env with
  | Teof -> ()
  | Tnewline -> back env
  | Tspace -> keep_space env; line_comment_loop env
  | _ -> last_token env; line_comment_loop env

(*****************************************************************************)
(* Generic handling of newlines + spaces + comments.
 * Default is:
 *   -) Newlines are removed
 *   -) Comments are preserved
 *   -) Spaces are removed
 *
 * There are some cases where we need to handle comments "by hand", but this
 * logic is the one we want most of the time.
 *)
(*****************************************************************************)

let rec keep_comment env =
  match token env with
  | Teof -> ()
  | Tspace -> keep_comment env
  | Topen_comment ->
      last_token env;
      comment env;
      space env
  | Tline_comment ->
      if !(env.last) <> Newline then space env;
      last_token env;
      line_comment_loop env;
      newline env;
      add_block_tag env
  | _ -> back env

let rec generic_nsc env =
  match !(env.last_token) with
  | Teof -> ()
  | Topen_comment ->
      if !(env.last) <> Newline && !(env.last) <> Space
      then space env;
      last_token env;
      comment env;
      if attempt env is_closing_list
      then ()
      else space env
  | Tline_comment ->
      if !(env.last) <> Newline
      then space env;
      last_token env;
      line_comment_loop env;
      newline env;
      add_block_tag env
  | Tspace
  | Tnewline ->
      ignore (token env);
      generic_nsc env
  | _ ->
      back env

and is_closing_list env =
  match token env with
  | Teof -> false
  | Tspace | Tnewline -> is_closing_list env
  | Trp | Trb | Tgt | Tcomma | Trcb | Tsc -> true
  | _ -> false

(*****************************************************************************)
(* Wrappers for newlines, spaces and comments.
 *
 * Most of the time (not always), we want to look at the next "real" token, in
 * other words: we want to skip white spaces and the comments to see what the
 * next token looks like (and presumably decide what to do based on that).
 *)
(*****************************************************************************)

let rec wrap_non_ws env f =
  match token env with
  | Tnewline | Tspace ->
      wrap_non_ws env f
  | x -> f x

let rec wrap_eof env f =
  match token env with
  | Tnewline | Tspace | Tline_comment | Topen_comment ->
      generic_nsc env;
      wrap_eof env f
  | x -> f x

let rec wrap_eof_xhp env f =
  match xhp_token env with
  | Tnewline | Tspace | Tline_comment | Topen_comment ->
      generic_nsc env;
      wrap_eof_xhp env f
  | x -> f x

let rec wrap env f =
  match token env with
  | Teof -> ()
  | Tnewline | Tspace | Tline_comment | Topen_comment ->
      generic_nsc env;
      wrap env f
  | x -> f x

let rec wrap_xhp env f =
  match xhp_token env with
  | Teof -> ()
  | Tnewline | Tspace | Tline_comment | Topen_comment ->
      generic_nsc env;
      wrap_xhp env f
  | x -> f x

let wrap_word env f = wrap env begin function
  | Tword -> f !(env.last_str)
  | _ -> back env
end

let next_real_token_info ~wrap env =
  attempt env begin fun env ->
    wrap env begin fun tok ->
      let tok_str = !(env.last_str) in
      tok, tok_str
    end
  end

let next_token ?(wrap=wrap_eof) env =
  let tok, _tok_str = next_real_token_info ~wrap env in
  tok

let next_token_str ?(wrap=wrap_eof) env =
  let _tok, tok_str = next_real_token_info ~wrap env in
  tok_str

let next_non_ws_token env =
  attempt env begin fun env ->
    wrap_non_ws env (fun tok -> tok)
  end

(*****************************************************************************)
(* Helpers to look ahead. *)
(*****************************************************************************)

let try_words env wordl f = wrap env begin function
  | Tword when List.mem wordl !(env.last_str) ->
      f env
  | _ -> back env
end

let try_word env word f = try_words env [word] f

let try_token env tok f = wrap env begin function
  | tok' when tok = tok' ->
      f env
  | _ ->
      back env
end

let opt_word word env = wrap env begin function
  | Tword when !(env.last_str) = word ->
      last_token env
  | _ -> back env
end

let opt_tok tok env = wrap env begin function
  | tok' when tok = tok' ->
      last_token env
  | _ -> back env
end

(*****************************************************************************)
(* There are cases where the formatter expects a token (e.g. a semi colon).
 * If the token is not found, the whole process stops, because one of the
 * assumption of the formatter is that we are dealing with correct Hack code
 * (at least for now ;-)).
 *
 * There is a debug mode (default turned to false) that gives a lot of context
 * on where the error was found. It's handy to leave it here in case someone
 * else wants to do some work with the formatter.
 *)
(*****************************************************************************)

let debug = false

let rec mycat n env =
  if n < 0 then () else
  match token env with
  | Teof -> ()
  | _ ->
      let n = n - (String.length !(env.last_str)) in
      Buffer.add_string env.buffer !(env.last_str);
      mycat n env

(* Used to give some context while debugging *)
let print_error tok_str env =
  Buffer.add_string env.buffer !(env.last_str);
  Buffer.add_string env.buffer "<----";
  mycat 200 env;
  let buffer = Buffer.contents env.buffer in
  let buffer =
    if String.length buffer > 400 then
      String.sub buffer (String.length buffer - 400 -1) 400
    else buffer
  in
  let error =
    (Pos.string (Pos.make (env.file :> string) env.lexbuf))^"\n"^
    (Printf.sprintf "Expected: %s, found: '%s'\n" tok_str !(env.last_str))^
    buffer^"\n"
  in
  output_string stderr error;
  flush stderr

let expect tok_str env = wrap env begin fun _ ->
  if !(env.last_str) = tok_str
  then last_token env
  else begin
    if debug then print_error tok_str env;
    raise Format_error
  end
end

let expect_token tok env = wrap env begin fun x ->
  if x = tok
  then last_token env
  else begin
    raise Format_error
  end
end

let expect_xhp tok_str env = wrap_xhp env begin fun _ ->
  if !(env.last_str) = tok_str
  then last_token env
  else begin
    if debug then begin
      print_error tok_str env;
      flush stderr
    end;
    raise Format_error
  end
end

(*****************************************************************************)
(* Helper functions to determine if a function has consumed tokens. *)
(*****************************************************************************)

let consume_value env f =
  let pos_before = get_pos env in
  let f_return = f env in
  let pos_after = get_pos env in
  let has_consumed = pos_before <> pos_after in
  has_consumed, f_return

let has_consumed env f =
  let result, _f_value = consume_value env f in
  result

let is_followed_by env f tok_str =
  attempt env begin fun env ->
    has_consumed env f &&
    next_token_str env = tok_str
  end

let wrap_would_consume env f =
  attempt env begin fun env ->
    wrap_eof env begin fun _ ->
      back env;
      has_consumed env f
    end
  end

(*****************************************************************************)
(* Logic preserving newlines. *)
(*****************************************************************************)

let empty_line env =
  let tok = ref Tspace in
  while !tok = Tspace do tok := token env done;
  match !tok with
  | Tnewline -> true
  | _ -> back env; false

let is_empty_line env =
  attempt env empty_line

let rec preserve_nl_space env =
  match token env with
  | Teof -> ()
  | Tspace ->
      preserve_nl_space env
  | Tnewline ->
      while is_empty_line env do
        ignore (empty_line env)
      done;
      back env;
      force_nl env;
  | _ ->
      back env

let rec preserve_nl env f =
  match token env with
  | Tline_comment ->
      generic_nsc env;
      assert (token env = Tnewline);
      preserve_nl_space env;
      preserve_nl env f
  | Topen_comment ->
      generic_nsc env;
      newline env;
      add_block_tag env;
      preserve_nl env f
  | Tspace when is_empty_line env ->
      preserve_nl_space env;
      preserve_nl env f
  | Tspace ->
      preserve_nl env f
  | Tnewline ->
      preserve_nl_space env;
      preserve_nl env f
  | Teof ->
      ()
  | _ ->
      back env;
      f env

(*****************************************************************************)
(* Dealing with lists. *)
(*****************************************************************************)

let rec list env element = preserve_nl env begin fun env ->
  if has_consumed env element
  then (newline env; add_block_tag env; list env element)
end

(*****************************************************************************)
(* List comma separated. *)
(*****************************************************************************)

let rec list_comma_loop :
'a. break:(env -> unit) -> ('a -> env -> 'a) -> 'a -> env -> 'a =
fun ~break element acc env ->
  while has_consumed env begin fun env ->
    wrap env begin function
      | Topen_comment | Tline_comment -> generic_nsc env
      | _ -> back env
    end
  end
  do () done;
  let has_consumed, acc = consume_value {
    env with char_break = env.char_break - 1
  } (element acc) in
  if has_consumed
  then list_comma_loop_remain ~break element acc env
  else acc

and list_comma_loop_remain :
'a. break:(env -> unit) -> ('a -> env -> 'a) -> 'a -> env -> 'a =
fun ~break element acc env ->
  wrap_eof env begin function
    | Teof -> acc
    | Tcomma ->
        let continue = wrap_would_consume env (element acc) in
        if continue
        then begin
          seq env [last_token; comment_after_comma ~break; break];
          list_comma_loop ~break element acc env
        end
        else acc
    | _ ->
        back env;
        acc
  end

and comment_after_comma ~break env =
  match token env with
  | Teof -> ()
  | Topen_comment ->
      if attempt env begin fun env ->
        comment env;
        empty_line env
      end
      then begin
        space env;
        last_token env;
        comment env;
        break env
      end
      else begin
        break env;
        back env
      end
  | Tline_comment ->
      space env;
      last_token env;
      line_comment_loop env;
      newline env
  | Tspace ->
      comment_after_comma ~break env
  | Tnewline ->
      break env
  | _ ->
      back env;
      break env

let rec list_comma_single_comment env =
  let k = list_comma_single_comment in
  match token env with
  | Teof -> ()
  | Tspace | Tnewline -> k env
  | Topen_comment ->
      last_token env;
      comment_loop env;
      space env
  | _ -> back env

let list_comma_single element env =
  list_comma_single_comment env;
  let f () env = element env in
  let () = list_comma_loop ~break:space f () env in
  ()

let list_comma_multi_maybe_trail ~trailing element env =
  let trailing = trailing && not env.no_trailing_commas in
  let break = newline in
  let trailing = list_comma_loop ~break element trailing env in
  if trailing && !(env.last) <> Newline
  then seq env [out ","; comment_after_comma ~break]

let list_comma_multi ~trailing element env =
  let f acc env = ignore (element env); acc in
  list_comma_multi_maybe_trail ~trailing f env

let list_comma_multi_nl ~trailing element env =
  newline env;
  list_comma_multi ~trailing element env;
  newline env

let list_comma ?(trailing=true) element env =
  Try.one_line env
    (list_comma_single element)
    (list_comma_multi ~trailing element)

let list_comma_nl ?(trailing=true) element env =
  Try.one_line env
    (list_comma_single element)
    (list_comma_multi_nl ~trailing element)

(*****************************************************************************)
(* Semi colons are special because we want to keep the comments on the same
 * line right after them.
 *)
(*****************************************************************************)

let semi_colon env =
  expect ";" env; space env; keep_comment env

(*****************************************************************************)
(* The entry point *)
(*****************************************************************************)

type 'a return =
  | Disabled_mode
  | Parsing_error of Errors.error list
  | Internal_error
  | Success of 'a

let rec entry ~keep_source_metadata ~no_trailing_commas ~modes
    (file : Path.t) from to_ content k =
  try
    let errorl, () = Errors.do_ begin fun () ->
      let rp = Relative_path.(create Dummy (file :> string)) in
      let {Parser_hack.file_mode; _} =
        Parser_hack.program rp content in
      if not (List.mem modes file_mode) then raise PHP;
    end in
    if errorl <> []
    then Parsing_error errorl
    else begin
      let lb = Lexing.from_string content in
      let env = empty file lb from to_ keep_source_metadata no_trailing_commas in
      header env;
      Success (k env)
    end
  with
    | PHP -> Disabled_mode
    | _ ->
        Printexc.print_backtrace stderr;
        Internal_error

(*****************************************************************************)
(* Hack header <?hh *)
(*****************************************************************************)

and header env = wrap env begin function
  | Thh | Tphp ->
      seq env [last_token; mode; newline];
      stmt_list ~is_toplevel:true env
  | _ -> assert false
end

and mode env =
  match token env with
  | Tspace -> mode env
  | Tline_comment ->
      space env; last_token env;
      line_comment env;
      newline env
  | _ -> back env

(*****************************************************************************)
(* Identifiers *)
(*****************************************************************************)

and name env =
  match token env with
  | Teof -> ()
  | Tnewline | Tspace | Tline_comment | Topen_comment ->
      generic_nsc env;
      name env
  | _ ->
      back env;
      name_loop env

and name_loop env =
  match token env with
  (* names can contain colons, but cannot end with them *)
  | Tcolon when attempt env begin fun env ->
      match token env with
        | Tword -> true
        | _ -> false
    end ->
      last_token env;
      name_loop env
  | Tpercent | Tminus | Tword | Tbslash ->
      last_token env;
      name_loop env
  | _ ->
      back env

(*****************************************************************************)
(* Shapes *)
(*****************************************************************************)

and shape_type_elt env =
  if has_consumed env expr
  then seq env [space; expect "=>"; space; hint]

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

and const env =
  last_token env;
  if attempt env begin fun env ->
    name env;
    next_token env = Teq
  end
  then ()
  else (space env; hint env);
  class_members env;
  newline env

and abs_const env =
  last_token env;
  if attempt env begin fun env ->
    name env;
    next_token env = Tsc
  end
  then ()
  else (space env; hint env);
  seq env [space ; name ; expect ";"]

(*****************************************************************************)
(* Type Constants *)
(*****************************************************************************)

and is_typeconst env =
  attempt env begin fun env ->
    wrap_non_ws env begin function
      | Tword when !(env.last_str) = "type" ->
         (match next_non_ws_token env with
         | Teq | Tsc -> false
         | _ -> true
         )
      | _ -> false
    end
  end

and type_const env =
  seq env [last_token; space; expect "type"; space; hint; as_constraint];
  if next_non_ws_token env = Teq
  then seq env [space; expect "="; space; hint; semi_colon]
  else semi_colon env

and abs_type_const env =
  seq env [last_token; space; expect "type"; space; hint];
  if attempt env begin fun env ->
    as_constraint env;
    next_token env = Tsc
  end
  then seq env [as_constraint; semi_colon]
  else semi_colon env

(*****************************************************************************)
(* Type hints. *)
(*****************************************************************************)

and hint_function_params env =
  expect "(" env;
  list_comma ~trailing:true hint_function_param env;
  expect ")" env

and hint_function_param env = wrap env begin function
  | Tellipsis -> last_token env
  | _ -> back env; hint env
end

and taccess_loop env = wrap env begin function
  | Tcolcol when next_token env = Tword ->
     last_token env;
     wrap env begin function
       | Tword ->
          last_token env;
          taccess_loop env
       | _ -> back env
     end
  | _ -> back env
end

and hint env = wrap env begin function
  | Tplus | Tminus | Tqm | Tat | Tbslash ->
      last_token env;
      hint env
  | Tpercent | Tcolon ->
      last_token env;
      name_loop env;
      taccess_loop env;
      hint_parameter env
  | Tword when !(env.last_str) = "shape" ->
      last_token env;
      expect "(" env;
      if next_token env = Trp || attempt env begin fun env ->
        (* does the shape have only one element? *)
        shape_type_elt env;
        wrap_eof env begin function
          | Tcomma -> next_token env = Trp
          | Trp -> true
          | _ -> false
        end
      end then
        list_comma_single shape_type_elt env
      else
        right env (list_comma_multi_nl ~trailing:true shape_type_elt);
      expect ")" env
  | Tword ->
      last_token env;
      name_loop env;
      taccess_loop env;
      typevar_constraint env;
      hint_parameter env
  | Tlp -> begin
      last_token env;
      (match token env with
      | Tword when !(env.last_str) = "function" ->
          last_token env;
          hint_function_params env;
          return_type env
      | _ ->
          back env;
          hint_list env);
      expect ")" env
    end
  | _ ->
      back env
end

and typevar_constraint env =
  try_words env ["as"; "super"] begin fun env ->
    space env;
    last_token env;
    space env;
    hint env
  end;

and as_constraint env =
  try_word env "as" begin fun env ->
    space env;
    last_token env;
    space env;
    hint env
  end;

and hint_parameter env = wrap env begin function
  | Tlt ->
      last_token env;
      hint_list ~trailing:false env;
      expect ">" env
  | _ -> back env
end

and hint_list ?(trailing=true) env =
  list_comma ~trailing:trailing hint env

(*****************************************************************************)
(* Enums *)
(*****************************************************************************)

and enum_ env =
  seq env [expect_token Tword; hint_parameter; space];
  try_token env Tcolon (seq_fun
    [last_token; space; hint; as_constraint; space]);
  (* stmt parses any list of statements, including things like $x = 1; which
   * are not valid in an enum body, but since we run the parser before
   * formatting the text, we can be sure tha we only encounter valid enum body
   * statements at this point. *)
  stmt ~is_toplevel:false env

(*****************************************************************************)
(* Functions *)
(*****************************************************************************)

and fun_ env =
  seq env [opt_tok Tamp; name; hint_parameter];
  Try.one_line env fun_signature_single fun_signature_multi;
  if next_token env = Tlcb
  then space env;
  stmt ~is_toplevel:false env

(*****************************************************************************)
(* function foo($arg1, $arg2 ...): return_type (all on one line) *)
(*****************************************************************************)

and fun_signature_single env =
  expect "(" env;
  right env (list_comma_single (ignore_ fun_param));
  seq env [expect ")"; return_type; use]

(*****************************************************************************)
(* Multi line function signature (adds a trailing comma if missing, unless
 * the last param is variadic).
 * function foo(
 *   $arg1,
 *   ...,
 *   )
 *
 * There is a special case with comments, when the only thing present is a
 * comment, we don't want to add a trailing comma.
 *)
(*****************************************************************************)

and fun_signature_multi env =
  seq env [expect "("; newline];
  if next_token env = Trp
  then right env (fun env -> wrap env (fun _ -> back env))
  else right env fun_params_multi;
  seq env [newline; expect ")"; return_type; use]

and fun_params_multi env = list_comma_multi_maybe_trail
  ~trailing:true
  begin fun trailing env ->
    let is_variadic = fun_param env in
    trailing && not is_variadic
  end
  env

and fun_param env =
  let curr_pos = !(env.abs_pos) in
  let space_opt env =
    if !(env.abs_pos) != curr_pos
    then space env
  in
  seq env [attribute; space_opt; modifier_list; space_opt; hint; space_opt];
  opt_tok Tamp env;
  let is_variadic = wrap_eof env begin function
    | Tellipsis -> last_token env; true
    | _ -> back env; false
  end in
  opt_tok Tlvar env;
  seq env [opt_tok Tamp; opt_tok Tellipsis; opt_tok Tlvar];
  try_token env Teq (seq_fun [space; last_token; space; expr]);
  is_variadic

and return_type env =
  try_token env Tcolon (seq_fun [last_token; space; hint])

(*****************************************************************************)
(* Classes *)
(*****************************************************************************)

and class_ env =
  seq env [name; hint_parameter; class_extends; space; class_body]

(*****************************************************************************)
(* Class extends/implements:
 * class ... extends A, B, C (on the same line)
 *)
(*****************************************************************************)

and class_extends_single env =
  seq env [last_token; space; list_comma_single hint]

(*****************************************************************************)
(* Class extends/implements:
 *
 * class ...
 *   extends A, B, C  (on a different line)
 *
 * OR:
 *
 * class ...
 *   extends
 *     A, B, C  (on a different line)
 *)
(*****************************************************************************)

and nl_class_extends_single ~break env =
  newline env;
  let line = !(env.line) in
  right env begin fun env ->
    last_token env;
    break env;
    right env begin fun env ->
      list_comma_single hint env
    end
  end;
  if line <> !(env.line) && env.report_fit
  then env.failed := 1

(*****************************************************************************)
(* Class extends/implements:
 *
 * class ...
 *   extends
 *     A,
 *     B
 *)
(*****************************************************************************)

and class_extends_multi env =
  right env begin fun env ->
    newline env;
    last_token env;
    newline env;
    right env begin fun env ->
      list_comma_multi ~trailing:false hint env
    end
  end

and class_extends env = wrap_word env begin function
  | "extends" | "implements" ->
      space env;
      Try.one_line env
        class_extends_single
        (fun env ->
          Try.outer env
            (nl_class_extends_single ~break:space)
            (fun env ->
              Try.outer env
                (nl_class_extends_single ~break:newline)
                class_extends_multi
            )
        );
      class_extends env
  | _ ->
      back env
end

and class_body env =
  expect "{" env;
  if next_non_ws_token env = Trcb  (* Empty class body *)
  then expect "}" env
  else begin
    newline env;
    add_block_tag env;
    right env begin fun env ->
      list env class_element;
    end;
    expect "}" env;
    newline env
  end

and class_element env = wrap env begin function
  | Trcb ->
      back env
  | Tword ->
      newline env;
      class_element_word env !(env.last_str)
  | Tltlt ->
      newline env;
      last_token env;
      expr_list ~trailing:false { env with in_attr = true };
      expect ">" env;
      expect ">" env;
      newline env;
      class_element env
  | _ ->
      back env
end

and class_element_word env = function
  | "function" ->
      seq env [space; last_token; space; fun_; newline]
  | "public" | "protected" | "private" | "abstract"
  | "final"| "static" | "async" ->
      back env;
      seq env [modifier_list; after_modifier; newline]
  | "const" ->
     if is_typeconst env
     then type_const env
     else const env
  | "require" ->
      seq env [last_token; space; class_extends; semi_colon]
  | "use" ->
      seq env
        [last_token; space; hint_list ~trailing:false; semi_colon; newline]
  | "category" ->
      seq env [last_token; xhp_category; semi_colon]
  | "attribute" ->
      last_token env;
      xhp_class_attribute_list env
  | "children" ->
      last_token env;
      space env;
      xhp_children env;
      semi_colon env
  | _ ->
      back env

and modifier_list env =
  let pos_before = get_pos env in
  modifier env;
  let pos_after = get_pos env in
  if pos_before = pos_after then () else begin
    space env;
    modifier_list env
  end

and modifier env = try_token env Tword begin fun env ->
  match !(env.last_str) with
  | "public" | "protected" | "private" | "abstract"
  | "final"| "static" | "async" ->
      last_token env
  | _ -> back env
end

and attribute env = try_token env Tltlt begin fun env ->
  last_token env;
  expr_list ~trailing:false { env with in_attr = true };
  expect ">" env;
  expect ">" env;
end

and use env = try_word env "use" begin fun env ->
  seq env [space; last_token; space; expect "("; expr_list; expect ")"]
end

and after_modifier env = wrap env begin function
  | Tword when !(env.last_str) = "const" ->
      if is_typeconst env
      then abs_type_const env
      else abs_const env
  | Tword when !(env.last_str) = "function" ->
      seq env [last_token; space; fun_]
  | _ ->
      back env;
      hint env;
      class_members env
end

and class_members env = class_members_list class_member env

and class_members_list member_handler env =
  Try.one_line env
    (class_member_list_single member_handler)
    (fun env -> right env (class_member_list_multi member_handler));
  semi_colon env

and class_member_list_single member_handler env =
  space env;
  list_comma_single member_handler env

and class_member_list_multi member_handler env =
  newline env;
  list_comma_multi ~trailing:false member_handler env

and class_member env = wrap env begin function
  | Tword (* In case we are dealing with a constant *)
  | Tlvar ->
      last_token env;
      try_token env Teq
        (seq_fun [space; last_token; space; expr])
  | _ ->
      back env
end

(*****************************************************************************)
(* XHP formatting *)
(*****************************************************************************)

and xhp_children env = wrap env begin function
  | Tlp ->
      last_token env;
      right env (list_comma_nl ~trailing:false xhp_children);
      expect ")" env;
      xhp_children_post env;
      xhp_children_remain env
  | _ ->
      back env;
      name env;
      xhp_children_post env;
      xhp_children_remain env
end

and xhp_children_post env = wrap env begin function
  | Tplus | Tqm | Tstar ->
      last_token env
  | _ -> back env
end

and xhp_children_remain env = wrap env begin function
  | Tbar ->
      seq env [space; last_token; space; xhp_children]
  | _ -> back env
end

and xhp_category env =
  space env; list_comma ~trailing:false name env

and xhp_class_attribute_list env =
  Try.one_line env
    (class_member_list_single xhp_class_attribute)
    (fun env -> newline env; right env xhp_class_attribute_list_multi);
  semi_colon env

and xhp_class_attribute_list_multi env = preserve_nl env begin fun env ->
  xhp_class_attribute env;
  match token env with
  | Tcomma ->
      seq env [last_token; keep_comment; newline; add_block_tag];
      xhp_class_attribute_list_multi env
  | _ -> back env
end

and xhp_class_attribute env =
  Try.one_line env
    (xhp_class_attribute_impl ~enum_list_elts:(list_comma_single expr))
    (xhp_class_attribute_impl ~enum_list_elts:
      (fun env -> right env (list_comma_multi_nl ~trailing:true expr)))

and xhp_class_attribute_impl ~enum_list_elts env =
  let curr_pos = !(env.abs_pos) in
  wrap env begin function
    | Tword when !(env.last_str) = "enum" ->
        last_token env;
        space env;
        expect "{" env;
        enum_list_elts env;
        expect "}" env
    | _ -> back env; hint env
  end;
  if !(env.abs_pos) != curr_pos then begin
    match next_token env with
      | Tsc | Tcomma -> ()
      | _ -> space env
  end;
  name env;
  wrap env begin function
    | Teq -> seq env [space; last_token; space; expr]
    | _ -> back env
  end;
  (match next_token env with
  | Tsc | Tcomma -> ()
  | _ -> space env);
  hint env

(*****************************************************************************)
(* XHP *)
(*****************************************************************************)

and is_xhp env =
  attempt env begin fun env ->
    match token env with
    | Tpercent | Tcolon | Tword ->
        name_loop env;
        wrap_eof_xhp env begin function
          | Tgt | Tword | Tslash -> true
          | _ -> false
        end
    | _ ->
        false
  end

and xhp_tag_kind env =
  attempt env begin fun env ->
    expect_xhp "<" env;
    name env;
    xhp_attribute_list ~break:space env;
    match next_token ~wrap:wrap_eof_xhp env with
    | Tslash -> `Xhp_self_closing
    | Tgt -> `Xhp_paired
    | _ -> raise Format_error
  end

(* First we try inserting the XHP on the current line, e.g.
 *
 *   $foo = <aaaaaaaaaa>1</aaaaaaaaaa>;
 *
 * If that fails to fit within char_break, then we try inserting the same XHP
 * on the next line:
 *
 *   $foo =
 *     <aaaaaaaaaa>1</aaaaaaaaaa>;
 *
 * If that *still* fails to fit, we split the XHP up into multiple lines:
 *
 *   $foo =
 *     <aaaaaaaaaa>
 *       1
 *     </aaaaaaaaaa>;
 *)
and xhp env =
  match xhp_tag_kind env with
  | `Xhp_self_closing ->
      Try.one_line env
        xhp_self_closing_single
        (fun env ->
          newline env;
          Try.one_line env
            xhp_self_closing_single
            xhp_self_closing_multi)
  | `Xhp_paired ->
      Try.one_line env
        xhp_paired_single
        (fun env ->
          newline env;
          Try.one_line env
            xhp_paired_single
            xhp_paired_multi)

and xhp_self_closing_single env =
  seq env [expect_xhp "<"; name; xhp_attribute_list ~break:space];
  seq env [space; expect_xhp "/"; expect_xhp ">"];

and xhp_self_closing_multi env =
  seq env [expect_xhp "<"; name];
  right env (xhp_attribute_list ~break:newline);
  seq env [newline; expect_xhp "/"; expect_xhp ">"];
  xhp_multi_post env

and xhp_paired_single env =
  seq env [expect_xhp "<"; name; xhp_attribute_list ~break:space];
  seq env [expect_xhp ">"; skip_spaces_and_nl; xhp_body];
  env.spaces := 0;
  xhp_close_tag env;

and xhp_paired_multi env =
  expect_xhp "<" env;
  let margin_pos = !(env.char_pos) in
  name env;
  Try.one_line env
    begin fun env ->
      xhp_attribute_list ~break:space env;
      expect_xhp ">" env
    end
    begin fun env ->
      margin_set margin_pos env
        (xhp_attribute_list ~break:newline);
      expect_xhp ">" env;
    end;
  newline env;
  skip_spaces_and_nl env;
  margin_set margin_pos env begin fun env ->
    xhp_body env;
  end;
  newline env;
  xhp_close_tag env;
  xhp_multi_post env

and xhp_multi_post env =
  match xhp_token env with
  | Tnewline | Tspace ->
      newline env
  | Tlt when is_xhp env ->
      back env;
      newline env
  | _ -> back env

and xhp_close_tag env =
  seq env [expect_xhp "<"; expect_xhp "/"; name; expect_xhp ">"]

and xhp_attribute_list ~break env = wrap_xhp env begin function
  | Tword ->
      break env;
      last_token env;
      Try.one_line env
        xhp_attribute_assign
        begin fun env ->
          expect "=" env;
          newline env;
          right env xhp_attribute_value
        end;
      xhp_attribute_list ~break env
  | _ ->
      back env
end

and xhp_attribute_assign env =
  expect_xhp "=" env;
  xhp_attribute_value env

and xhp_attribute_value env = wrap_xhp env begin function
  | Tquote | Tdquote as tok ->
      last_token env;
      string ~last:tok env
  | Tlcb ->
      last_token env;
      expr env;
      expect_xhp "}" env
  | _ ->
      back env
  end

(* It seems like whitespace is significant in XHP, but only insofar as it acts
 * as a separator of non-whitespace characters. That is, consecutive whitespace
 * will be rendered as a single space at runtime. Thus the handling of xhp_body
 * has to be slightly different from the rest of the syntax, which does not
 * treat whitespace as significant. In particular, we output consecutive
 * whitespace here as a single space, unless we have just wrapped a line, in
 * which case we output the necessary number of spaces required by the
 * indentation level. *)
and xhp_body env =
  let k = xhp_body in
  match xhp_token env with
  | Teof -> ()
  | Tnewline | Tspace ->
      if !(env.last) <> Newline then space env;
      k env
  | Topen_xhp_comment ->
      last_token env;
      xhp_comment env;
      k env
  | Tlt when is_xhp env ->
      back env;
      xhp env;
      xhp_keep_one_nl env;
      k env;
  | Tlt ->
      back env
  | Tlcb ->
      Try.one_line env
        begin fun env ->
          last_token env;
          expr env;
          expect_xhp "}" env;
        end
        begin fun env ->
          newline env;
          last_token env;
          expr env;
          expect_xhp "}" env;
        end;
      xhp_keep_one_nl env;
      k env
  | x ->
      let pos = !(env.char_pos) in
      let text = xhp_text env (Buffer.create 256) x in
      if pos + String.length text >= env.char_size
      then newline env;
      out text { env with report_fit = false };
      env.last := Text;
      k env

(* preserves up to one empty line between XHP blocks *)
and xhp_keep_one_nl env =
  match xhp_token env with
  | Teof -> ()
  | Tnewline ->
      newline env;
      (match xhp_token env with
      | Tnewline -> force_nl env
      | _ -> back env);
      while xhp_token env = Tnewline do () done;
      back env;
  | _ ->
      back env

and xhp_text env buf = function
  | Tnewline | Tspace | Tlt | Tlcb | Teof | Tclose_xhp_comment ->
      back env;
      Buffer.contents buf
  | _ ->
      Buffer.add_string buf !(env.last_out);
      xhp_text env buf (xhp_token env)

and xhp_comment env = Try.one_line env xhp_comment_single xhp_comment_multi

and xhp_comment_single env =
  seq env [xhp_comment_body; expect_xhp "-->"]

and xhp_comment_multi env =
  newline env;
  right env xhp_comment_body;
  newline env;
  expect_xhp "-->" env

and xhp_comment_body env =
  match xhp_token env with
  | Teof -> ()
  | Tnewline | Tspace ->
      if !(env.last) <> Newline then space env;
      xhp_comment_body env
  | Tclose_xhp_comment ->
      back env
  | Tlt | Tlcb ->
      last_token env;
      xhp_comment_body env
  | x ->
      let pos = !(env.char_pos) in
      let text = xhp_text env (Buffer.create 256) x in
      if pos + String.length text >= env.char_size
      then newline env;
      out text env;
      env.last := Text;
      xhp_comment_body env

(*****************************************************************************)
(* Statements *)
(*****************************************************************************)

and stmt ~is_toplevel env = wrap env begin function
  | Tltlt ->
      line { env with in_attr = true }
        [last_token; expr_list ~trailing:false; expect ">"; expect ">"];
      stmt ~is_toplevel env
  | Tword ->
      let word = !(env.last_str) in
      stmt_word ~is_toplevel env word
  | Tlcb ->
      last_token env;
      if next_non_ws_token env <> Trcb then begin
        seq env [space; keep_comment; newline];
        add_block_tag env;
        right env (stmt_list ~is_toplevel);
      end;
      expect "}" env;
  | Tsc ->
      seq env [last_token; space; keep_comment; newline]
  | _ ->
      back env;
      if has_consumed env expr
      then semi_colon env
end

and stmt_word ~is_toplevel env word =
  match word with
  | "type" | "newtype" | "namespace" | "use"
  | "abstract" | "final" | "interface" | "const"
  | "class" | "trait" | "function" | "async" | "enum" as word ->
      if is_toplevel
      then stmt_toplevel_word env word
      else back env
  | "public" | "protected" | "private" | "case" | "default" ->
      back env
  | "print" | "echo"
  | "require" | "require_once" | "include" | "include_once" ->
      seq env [last_token; space];
      right env (list_comma_nl ~trailing:false expr);
      semi_colon env
  | "throw" ->
      seq env [last_token; space; expr; semi_colon]
  | "break" | "continue" | "return" ->
      last_token env;
      if wrap_would_consume env expr
      then rhs_assign env;
      semi_colon env
  | "static" when next_token env <> Tcolcol ->
      seq env [last_token; space];
      Try.one_line env
        (seq_fun [space; list_comma_single expr])
        (seq_fun [newline; right_fun (list_comma_multi ~trailing:false expr)]);
      semi_colon env
  | "if" ->
      last_token env;
      if_ ~is_toplevel env
  | "do" ->
      seq env [last_token; block];
      seq env [space; expect "while"; space; expr_paren; opt_tok Tsc]
  | "while" ->
      seq env [last_token; space; expr_paren; block; newline]
  | "for" ->
      last_token env;
      for_loop env
  | "switch" ->
      last_token env;
      switch env
  | "foreach" ->
      last_token env;
      foreach env;
  | "try" ->
      seq env [last_token; space; block; space];
      catch_list env
  | _ ->
      back env;
      seq env [expr; semi_colon]

and stmt_toplevel_word env = function
  | "abstract"  | "final" | "async" ->
      seq env [last_token; space; stmt ~is_toplevel:true]
  | "interface" | "class" | "trait" ->
      seq env [last_token; space; class_]
  | "enum" ->
      seq env [last_token; space; enum_]
  | "function" ->
      seq env [last_token; space; fun_]
  | "const" ->
      const env
  | "type" | "newtype" ->
      seq env [last_token; space; hint; as_constraint; space;
               expect "="; space];
      hint env;
      semi_colon env;
  | "namespace" ->
      last_token env;
      namespace env
  | "use" ->
      last_token env;
      namespace_use env;
  | _ ->
      back env

and stmt_list ~is_toplevel env =
  (* -1 for the trailing semicolon *)
  let env = {env with char_break = min env.char_break (env.char_size - 1)} in
  list env (stmt ~is_toplevel)

and block ?(is_toplevel=false) env = wrap env begin function
  | Tlcb ->
      seq env [space; last_token; space; keep_comment; newline];
      add_block_tag env;
      right env (stmt_list ~is_toplevel);
      expect "}" env
  | _ ->
      back env;
      newline env;
      right env (stmt ~is_toplevel)
end

(*****************************************************************************)
(* If statement *)
(*****************************************************************************)

and if_ ~is_toplevel env =
  seq env [space; expr_paren; block ~is_toplevel; else_ ~is_toplevel]

and else_ ~is_toplevel env =
  match next_token_str env with
  | "else" | "elseif" ->
      space env;
      else_word ~is_toplevel env;
      else_ ~is_toplevel env
  | _ -> newline env

and else_word ~is_toplevel env = wrap_word env begin function
  | "else" ->
      seq env [last_token; space];
      wrap_word env (function
        | "if" -> seq env [last_token; space; expr_paren; space]
        | _ -> back env);
      block ~is_toplevel env;
  | "elseif" ->
      seq env [out "else"; space; out "if"; space; expr_paren; space];
      block ~is_toplevel env;
  | _ -> assert false
end

(*****************************************************************************)
(* Namespaces *)
(*****************************************************************************)

and namespace env =
  seq env [space; name];
  wrap env begin function
    | Tsc -> back env; semi_colon env;
    | Tlcb ->
        space env; last_token env; newline env;
        right env (stmt_list ~is_toplevel:true);
        expect "}" env
    | _ ->
        expect ";" env
  end

and namespace_use env =
  seq env [space; name;];
  let rem = match (next_token_str env) with
    | "as" -> [space; expect "as"; space; name; semi_colon;]
    | _ -> [semi_colon] in
  seq env rem

(*****************************************************************************)
(* Foreach loop *)
(*****************************************************************************)

and foreach env =
  seq env [space; expect "("];
  margin_set (!(env.char_pos) - 1) env foreach_as;
  expect ")" env;
  block env;
  newline env

and foreach_as env =
  seq env [expr; space; opt_word "await"; space; expect "as"];
  Try.outer env
    (fun env -> seq env [space; expr; arrow_opt])
    (fun env -> seq env [newline; expr; arrow_opt])

(*****************************************************************************)
(* For loop *)
(*****************************************************************************)

and for_loop env =
  seq env [space; expect "("];
  (* the expr_list at toplevel adds newlines before and after the list, which
   * we don't want *)
  let expr_list = list_comma ~trailing:false expr in
  let for_exprs ~break = begin fun env ->
    seq env [expr_list; semi_colon];
    seq env [break; expr_list; semi_colon];
    seq env [break; expr_list]
  end in
  Try.one_line env
    (for_exprs ~break:space)
    begin fun env ->
      newline env;
      right env (for_exprs ~break:newline);
      newline env;
    end;
  seq env [expect ")"; block; newline]

(*****************************************************************************)
(* Switch statement *)
(*****************************************************************************)

and switch env =
  seq env  [space; expr_paren; space];
  line env [expect "{"];
  add_block_tag env;
  case_list env;
  line env [expect "}"]

and case_list env =
  right env begin fun env ->
    list env case
  end

and case env =
  wrap env begin function
    | Trcb ->
        back env
    | Tword ->
        case_word env !(env.last_str)
    | _ -> back env
  end

and case_word env = function
  | "case" ->
      seq env [last_token; space; expr; expect ":"; space; keep_comment;
        newline];
      right env (stmt_list ~is_toplevel:false)
  | "default" ->
      seq env [last_token; expect ":"; keep_comment; newline];
      right env (stmt_list ~is_toplevel:false)
  | _ ->
      back env

and catch_list env = wrap_word env begin function
  | "catch" ->
      last_token env;
      catch_remain env;
      (match next_token_str env with
      | "catch" | "finally" ->
          space env; catch_list env
      | _ -> newline env)
  | "finally" ->
      last_token env;
      block env;
      newline env
  | _ -> back env
end

and catch_remain env =
  seq env [space; expect "("; (ignore_ fun_param); expect ")"; block]

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

and rhs_assign env =
  wrap env begin function
    | Theredoc ->
        last_token env;
        heredoc env
    (* XHP *)
    | Tlt ->
        back env;
        Try.one_line env
          (fun env -> space env; expr env)
          (fun env -> newline env; right env expr)
    | Tword when
          !(env.last_str) = "array" || !(env.last_str) = "shape" ||
          !(env.last_str) = "tuple" ->
        back env;
        space env; expr env
    | Tword when next_token env = Tlcb ->
        back env;
        space env; expr env
    | _ ->
        back env;
        try_word env "await" begin fun env ->
          space env;
          last_token env
        end;
        keep_best env
          begin fun env ->
            let line = !(env.line) in
            space env;
            let lowest_pri = expr_lowest env in
            if lowest_pri > 0 &&
               lowest_pri != tarrow_prec &&
               line <> !(env.line)
            then env.failed := 1;
          end
          begin fun env ->
            newline env;
            right env expr;
          end
  end

and expr_paren env =
  expect "(" env;
  (* an expr_paren is usually followed by `) {`, so take that into account *)
  let env = {env with char_break = min env.char_break (env.char_size - 3)} in
  margin_set (!(env.char_pos) - 1) env expr;
  expect ")" env

and expr_break_tarrow env =
  let env = { env with break_on = tarrow_prec } in
  expr_atomic env;
  right env (fun env -> ignore (expr_remain_loop 0 env))

and expr env =
  let break_on = ref 0 in
  Try.outer env
    begin fun env ->
      let line = !(env.line) in
      let lowest = expr_lowest env in
      break_on := lowest;
      if !(env.failed) <= 0 && line <> !(env.line) && lowest > 0
      then env.failed := max 1 (max !(env.failed) env.try_depth);
    end
    begin fun env ->
      let break_on = !break_on in
      if break_on = tarrow_prec (* Operator -> is special *)
      then keep_best env ignore_expr_lowest expr_break_tarrow
      else ignore_expr_lowest { env with break_on };
      ()
    end

and ignore_expr_lowest env =
  ignore (expr_lowest env)

and expr_lowest env =
  let env = reset_priority env in
  expr_atomic env;
  expr_remain_loop 0 env

and expr_remain_loop lowest env =
  let pos_before = get_pos env in
  let lowest = expr_remain lowest env in
  let pos_after = get_pos env in
  if pos_before = pos_after
  then lowest
  else expr_remain_loop lowest env

and expr_list ?(trailing=true) env =
  list_comma_nl ~trailing expr { env with break_on = 0; priority = 0 }

and expr_binop lowest str_op op env =
  with_priority env op begin fun env ->
    space env;
    out str_op env;
    if env.priority = env.break_on
    then newline env
    else space env;
    expr_atomic env;
    let lowest =
      if lowest = 0 then env.priority else
      if env.priority = 0 then lowest
      else min env.priority lowest in
    expr_remain_loop lowest env
  end

and expr_binop_arrow lowest str_op tok env =
  with_priority env tok begin fun env ->
    if env.priority = env.break_on
    then begin
      seq env [newline; out str_op];
    end
    else out str_op env;
    wrap env begin function
      | Tword ->
          last_token env
      | Tlcb -> (* $xx->{...} *)
          last_token env;
          expr env;
          expect "}" env
      | _ ->
          back env;
          expr_atomic env
    end;
    let lowest =
      if lowest = 0 then env.priority else
      min env.priority lowest in
    expr_remain_loop lowest env
  end

and expr_binop_dot lowest str_op env =
  with_priority env Tdot begin fun env ->
    out str_op env;
    if env.priority = env.break_on
    then newline env;
    (match next_token env with
    | Tminus | Tplus | Tint | Tfloat -> space env
    | _ -> ());
    expr_atomic env;
    let lowest =
      if lowest = 0 then env.priority else
      min env.priority lowest in
    expr_remain_loop lowest env
  end

and expr_remain lowest env =
  let tok = token env in
  let tok_str = !(env.last_out) in
  match tok with
  | Topen_comment ->
      seq env [space; last_token; comment];
      expr_remain lowest env
  | Tline_comment ->
      seq env [space; last_token; line_comment; newline];
      expr_remain lowest env
  | Tnewline | Tspace  ->
      expr_remain lowest env
  | Tplus | Tminus | Tstar | Tslash | Tstarstar
  | Teqeqeq | Tpercent
  | Teqeq | Tampamp | Tbarbar
  | Tdiff | Tlt | Tdiff2 | Tgte
  | Tlte | Tamp | Tbar | Tltlt
  | Tgtgt | Txor as op ->
      expr_binop lowest tok_str op env
  | Tdot ->
      expr_binop_dot lowest tok_str env
  | Tarrow | Tnsarrow ->
      expr_binop_arrow lowest tok_str tok env
  | Tgt when env.in_attr ->
      back env;
      lowest
  | Tlambda ->
      space env;
      last_token env;
      space env;
      if next_token env = Tlcb
      then block env
      else expr env;
      lowest
  | Tgt ->
      (match token env with
      | Tgt ->
          expr_binop lowest ">>" Tgtgt env
      | _ ->
          back env;
          expr_binop lowest ">" Tgt env
      )
  | Teq | Tbareq | Tpluseq | Tstareq | Tslasheq
  | Tdoteq | Tminuseq | Tpercenteq | Txoreq
  | Tampeq | Tlshifteq | Trshifteq ->
      space env;
      last_token env;
      space env;
      rhs_assign env;
      lowest
  | Tincr | Tdecr ->
      out tok_str env;
      lowest
  | Tcolcol ->
      out tok_str env;
      expr_atomic env;
      lowest
  | Tlp ->
      let env = { env with break_on = 0 } in
      out tok_str env;
      keep_comment env;
      if next_token env <> Trp
      then right env expr_call_list;
      expect ")" env;
      lowest
  | Tlb ->
      last_token env;
      (match token env with
      | Trb -> last_token env
      | _ -> back env; expr env; expect "]" env
      );
      lowest
  | Tqm when attempt env begin fun env ->
        wrap_eof env begin function
          | Tcolon ->
              token env <> Tword
          | _ -> false
        end
    end ->
      seq env [space; out "?"; expect ":"; space; expr];
      lowest
  | Tqm ->
      Try.one_line env
        ternary_one_line
        ternary_multi_line;
      (* Horrible Hack. We pretend the ternary operator is a binary operator
       * to make sure we get a new line after an assignment.
       * Without this hack, we could have results looking like this:
       * $x = (my_cond)?
       *   ...
       *)
      1
  | Tword when !(env.last_str) = "xor" ->
      expr_binop lowest "xor" Txor env
  | Tword when !(env.last_str) = "instanceof" ->
      space env;
      last_token env;
      space env;
      expr_atomic env;
      lowest
  | _ ->
      back env;
      lowest

and expr_atomic env =
  let last = !(env.last_token) in
  match token env with
  | Tline_comment ->
      seq env [last_token; line_comment; newline; expr_atomic]
  | Topen_comment ->
      seq env [last_token; comment; space; expr_atomic]
  | Tnewline | Tspace ->
      expr_atomic env
  | Tlvar ->
      last_token env;
      (match next_token env with
      | Tarrow | Tnsarrow as tok ->
          (match tok with
          | Tarrow -> expect "->" env
          | Tnsarrow -> expect "?->" env
          | _ -> assert false);
          wrap env begin function
            | Tword ->
                last_token env
            | Tlcb ->
                last_token env;
                expr env;
                expect "}" env
            | _ ->
                back env;
                expr_atomic env
          end
      | _ -> ()
      )
  | Tint | Tfloat ->
      last_token env;
      if next_token env = Tdot
      then space env
  | Tquote | Tdquote as tok ->
      last_token env;
      string ~last:tok env
 | Tcolon ->
     last_token env;
     name_loop env
 | Tamp | Tat | Tbslash
 | Tem | Tincr | Tdecr | Ttild | Tplus | Tminus ->
     last_token env;
     expr_atomic env
 | Tword ->
      let word = !(env.last_str) in
      expr_atomic_word env last word
 | Tlb ->
     last_token env;
     right env array_body;
     expect "]" env
 | Tlp ->
     let env = { env with break_on = 0 } in
     (* CAST *)
     if is_followed_by env name ")"
     then begin
       seq env [last_token; out_next; expect ")"; space; expr]
     end
     else if next_token_str env = "new"
     then begin
       seq env [last_token; expr; expect ")"]
     end
     (* Short lambda parameters *)
     else if attempt env begin fun env ->
       try
         list_comma (ignore_ fun_param) env;
         seq env [expect ")"; return_type];
         wrap_eof env (fun tok -> tok = Tlambda)
       with Format_error -> false
     end then begin
       back env;
       Try.one_line env fun_signature_single fun_signature_multi;
     end
     (* Expression *)
     else begin
       last_token env;
       margin_set (!(env.char_pos) -1) env begin fun env ->
         expr env
       end;
       expect ")" env;
     end
  | Tlt when is_xhp env ->
      back env;
      xhp env;
  | Theredoc ->
      last_token env;
      heredoc env
  | _ ->
      back env

and expr_atomic_word env last_tok = function
  | "true" | "false" | "null" ->
      last_token env
  | "array" | "shape" | "tuple" as v ->
      out v env;
      expect "(" env;
      right env array_body;
      expect ")" env
  | "new" ->
      last_token env;
      space env;
      expr env;
  | "async" ->
      last_token env;
      space env;
      begin match next_token env with
      | Tlcb -> stmt ~is_toplevel:false env
      | _ -> expr_atomic env
      end
  | "function" when last_tok <> Tarrow && last_tok <> Tnsarrow ->
      last_token env;
      if next_non_ws_token env <> Tlp then space env;
      fun_ env
  | "await" ->
      last_token env;
      space env;
      with_priority env Tawait expr
  | "yield" ->
      last_token env;
      space env;
      with_priority env Tyield array_element_single
  | "clone" ->
      last_token env;
      space env;
      with_priority env Tclone expr
  | _ ->
      last_token env;
      wrap env begin function
        (* Collection *)
        | Tlcb ->
            space env;
            last_token env;
            if next_token env <> Trcb
            then right env array_body;
            expect "}" env
        | Tlp ->
            back env;
            let _ = expr_remain 0 env in
            ()
        | Tbslash ->
            last_token env;
            name_loop env
        | _ ->
            back env
      end

and expr_call_list env =
  let env = { env with break_on = 0; priority = 0 } in
  list_comma_nl ~trailing:true expr_call_elt env

and expr_call_elt env = wrap env begin function
  | Tellipsis -> seq env [last_token; expr]
  | _ -> back env; expr env
end

(*****************************************************************************)
(* Ternary operator ... ? ... : ... *)
(*****************************************************************************)

and ternary_one_line env =
  seq env [space; last_token; space; expr; space; expect ":"; space; expr]

and ternary_multi_line env =
  right env begin fun env ->
    seq env
      [newline; last_token; space; expr; newline; expect ":"; space; expr]
  end


(*****************************************************************************)
(* Strings *)
(*****************************************************************************)

and string ~last env =
  match token env with
  | Teof -> ()
  | tok when tok = last -> last_token env
  | tok -> string_char env tok; string ~last env

and string_char env = function
  | Teof     -> ()
  | Tbslash  -> last_token env; out_next env
  | Tnewline -> force_nl env
  | Tspace   -> keep_space env
  | _        -> last_token env

(*****************************************************************************)
(* Heredocs *)
(*****************************************************************************)

and heredoc env =
  let env = { env with margin = ref 0 } in
  (match token env with
  | Tspace -> heredoc env
  (* <<<'MYSTRING' *)
  | Tquote ->
      last_token env;
      let abs_start = env.lexbuf.Lexing.lex_curr_pos in
      string ~last:Tquote env;
      let len = env.lexbuf.Lexing.lex_curr_pos - abs_start - 1 in
      let str_value = String.sub env.lexbuf.Lexing.lex_buffer abs_start len in
      heredoc_loop str_value env
  (* <<<MYWORD *)
  | Tword ->
      last_token env;
      heredoc_loop !(env.last_str) env
  (* <<< *)
  | _ ->
      last_token env;
      heredoc_loop "EOT" env
  );
  (match token env with
  | Tsc -> back env
  | _ -> back env; newline env)

and heredoc_loop close env =
  match token env with
  | Teof -> ()
  | Tnewline ->
      force_nl env;
      if attempt env begin fun env ->
        token env = Tword &&
        !(env.last_str) = close &&
        match token env with
        | Tsc | Tnewline -> true
        | _ -> false
        end
      then (ignore (token env); last_token env)
      else heredoc_loop close env
  | Tspace -> keep_space env; heredoc_loop close env
  | _ -> last_token env; heredoc_loop close env

(*****************************************************************************)
(* Arrays *)
(*****************************************************************************)

and array_body env =
  Try.one_line env
    array_one_line
    array_multi_line

and array_one_line env =
  list_comma_single array_element_single env

and array_multi_line env =
  list_comma_multi_nl ~trailing:true array_element_multi env

and array_element_single env =
  expr env;
  arrow_opt env

and array_element_multi env = wrap env begin fun _ ->
  back env;
  newline env;
  expr env;
  arrow_opt env
end

and arrow_opt env =
  match token env with
  | Tsarrow ->
      space env;
      last_token env;
      Try.outer env
        (fun env -> space env; expr env)
        (fun env -> newline env; right env expr)
  | _ ->
      back env

(*****************************************************************************)
(* The outside API *)
(*****************************************************************************)

let region modes file ~start ~end_ content =
  entry ~keep_source_metadata:false file start end_ content
    ~no_trailing_commas:false ~modes
    (fun env -> Buffer.contents env.buffer)

let program ?no_trailing_commas:(no_trailing_commas = false) modes file
    content =
  entry ~keep_source_metadata:false file 0 max_int content
    ~no_trailing_commas ~modes
    (fun env -> Buffer.contents env.buffer)

let program_with_source_metadata modes file content =
  entry ~keep_source_metadata:true file 0 max_int content
    ~no_trailing_commas:false ~modes begin
    fun env ->
      Buffer.contents env.buffer, List.rev !(env.source_pos_l)
  end
