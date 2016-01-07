(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Ast = Spider_monkey_ast

type flow_mode = OptIn | OptInWeak | OptOut

type t = {
  flow: flow_mode option;
  preventMunge: bool option;
  providesModule: string option;
  isDeclarationFile: bool;
}

let default_info = {
  flow = None;
  preventMunge = None;
  providesModule = None;
  isDeclarationFile = false;
}

(* Avoid lexing unbounded in perverse cases *)
let max_tokens = 10

let extract =
  let words_rx = Str.regexp "[ \t\n\\*/]+" in

  let rec parse_attributes acc = function
    | "@flow" :: "weak" :: xs ->
        parse_attributes { acc with flow = Some OptInWeak } xs
    | "@flow" :: xs ->
        parse_attributes { acc with flow = Some OptIn } xs
    | "@noflow" :: xs ->
        parse_attributes { acc with flow = Some OptOut } xs
    | "@providesModule" :: m :: xs ->
        parse_attributes { acc with providesModule = Some m } xs
    | _ :: xs ->
        parse_attributes acc xs
    | [] -> acc
  in

  fun filename content ->
    (* Consume tokens in the file until we get a comment. This is a hack to
     * support Nuclide, which needs 'use babel' as the first token due to
     * contstraints with Atom (see https://github.com/atom/atom/issues/8416 for
     * more context). At some point this should change back to consuming only
     * the first token. *)
    let lb = Lexing.from_string content in
    let env = Lexer_flow.new_lex_env None lb in
    let rec get_first_comment_contents ?(i=0) env =
      if i < max_tokens then
        let env, lexer_result = Lexer_flow.token env in
        match lexer_result.Lexer_flow.lex_comments with
        | [] -> Lexer_flow.(
            (**
             * Stop looking for docblocks if we see any tokens other than a
             * string or a semicolon (`"use babel";` or `"use strict";`).
             *)
            match lexer_result.lex_token with
            | T_STRING _
            | T_SEMICOLON
              -> get_first_comment_contents ~i:(i + 1) env
            | _ -> None
          )
        | (_, Ast.Comment.Block s) :: _
        | (_, Ast.Comment.Line s) :: _
          -> Some s
      else None in
    let info =
      if Filename.check_suffix filename FlowConfig.flow_ext
      then { default_info with isDeclarationFile = true; }
      else default_info in
    match get_first_comment_contents env with
      | Some s -> parse_attributes info (Str.split words_rx s)
      | None -> info

(* accessors *)
let flow info = info.flow
let preventMunge info = info.preventMunge
let providesModule info = info.providesModule
let isDeclarationFile info = info.isDeclarationFile

let is_flow info = match info.flow with
  | Some OptIn
  | Some OptInWeak -> true
  | Some OptOut
  | None -> false

(* debugging *)
let json_of_docblock info =
  let open Hh_json in
  let flow = match flow info with
  | Some OptIn -> JSON_String "OptIn"
  | Some OptInWeak -> JSON_String "OptInWeak"
  | Some OptOut -> JSON_String "OptOut"
  | None -> JSON_Null in

  let preventsMunge = match preventMunge info with
  | Some b -> JSON_Bool b
  | None -> JSON_Null in

  let providesModule = match providesModule info with
  | Some str -> JSON_String str
  | None -> JSON_Null in

  let isDeclarationFile = JSON_Bool (isDeclarationFile info) in

  JSON_Object [
    "flow", flow;
    "preventMunge", preventsMunge;
    "providesModule", providesModule;
    "isDeclarationFile", isDeclarationFile;
  ]
