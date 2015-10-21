(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Ast = Spider_monkey_ast

type flow_mode = OptIn | OptInWeak

type t = {
  flow: flow_mode option;
  preventMunge: bool option;
  providesModule: string option;
}

let default_info = {
  flow = None;
  preventMunge = None;
  providesModule = None;
}

let extract =
  let words_rx = Str.regexp "[ \t\n\\*/]+" in

  let rec parse_attributes acc = function
    | "@flow" :: "weak" :: xs ->
        parse_attributes { acc with flow = Some OptInWeak } xs
    | "@flow" :: xs ->
        parse_attributes { acc with flow = Some OptIn } xs
    | "@providesModule" :: m :: xs ->
        parse_attributes { acc with providesModule = Some m } xs
    | _ :: xs ->
        parse_attributes acc xs
    | [] -> acc
  in

  fun content ->
    (* Consume the first token in the file. This also consumes any comments. *)
    let lb = Lexing.from_string content in
    let env = Lexer_flow.new_lex_env None lb in
    let env, lexer_result = Lexer_flow.token env in
    match lexer_result.Lexer_flow.lex_comments with
    | [] -> default_info
    | (_, Ast.Comment.Block s) :: _
    | (_, Ast.Comment.Line s) :: _ ->
        parse_attributes default_info (Str.split words_rx s)

(* accessors *)
let flow info = info.flow
let preventMunge info = info.preventMunge
let providesModule info = info.providesModule

let is_flow info = match info.flow with
  | Some OptIn
  | Some OptInWeak -> true
  | None -> false
