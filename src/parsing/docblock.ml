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
module Lex_result = Lexer_flow.Lex_result

type flow_mode = OptIn | OptInWeak | OptOut

type t = {
  flow: flow_mode option;
  preventMunge: bool option;
  providesModule: string option;
  isDeclarationFile: bool;
  jsx: (string * Spider_monkey_ast.Expression.t) option;
}

type error = Loc.t * error_kind
and error_kind =
  | MultipleFlowAttributes
  | MultipleProvidesModuleAttributes
  | MultipleJSXAttributes
  | InvalidJSXAttribute of string option

let default_info = {
  flow = None;
  preventMunge = None;
  providesModule = None;
  isDeclarationFile = false;
  jsx = None;
}

(* Avoid lexing unbounded in perverse cases *)
let max_tokens = 10

let extract : max_tokens:int -> Loc.filename -> string -> error list * t =
  let words_rx = Str.regexp "[ \t\\*/]+" in
  let lines_rx = Str.regexp "[\r\n]" in

  (* walks a list of words, returns a list of errors and the extracted info.
     if @flow or @providesModule is found more than once, the first one is used
     and an error is returned. *)
  let rec parse_attributes (errors, info) loc line = function
    | "@flow" :: "weak" :: xs ->
        let acc =
          if info.flow <> None then (loc, MultipleFlowAttributes)::errors, info
          else errors, { info with flow = Some OptInWeak } in
        parse_attributes acc loc line xs
    | "@flow" :: xs ->
        let acc =
          if info.flow <> None then (loc, MultipleFlowAttributes)::errors, info
          else errors, { info with flow = Some OptIn } in
        parse_attributes acc loc line xs
    | "@noflow" :: xs ->
        let acc =
          if info.flow <> None then (loc, MultipleFlowAttributes)::errors, info
          else errors, { info with flow = Some OptOut } in
        parse_attributes acc loc line xs
    | "@providesModule" :: m :: xs ->
        let acc =
          if info.providesModule <> None then
            (loc, MultipleProvidesModuleAttributes)::errors, info
          else
            errors, { info with providesModule = Some m }
        in
        parse_attributes acc loc line xs
    | "@preventMunge" :: xs ->
        (* dupes are ok since they can only be truthy *)
        let preventMunge = Some true in
        parse_attributes (errors, { info with preventMunge }) loc line xs
    | "@jsx" :: xs ->
        let padding, line = line in
        let acc =
          if info.jsx <> None
          then (loc, MultipleJSXAttributes)::errors, info
          else if Str.string_match (Str.regexp "\\(.*@jsx \\)\\([^ \t]*\\)") line 0
          then begin
            let padding = padding ^
              String.make (String.length (Str.matched_group 1 line)) ' ' in
            let raw_jsx = Str.matched_group 2 line in
            try
              (* The point of the padding is to make the parsed code line up
               * with the comment in the original source *)
              let (jsx_expr, _) = Parser_flow.jsx_pragma_expression
                (padding ^ raw_jsx)
                loc.Loc.source in
              errors, { info with jsx = Some (raw_jsx, jsx_expr) }
            with
            | Parse_error.Error [] ->
                (loc, InvalidJSXAttribute None)::errors, info
            | Parse_error.Error ((_, e)::_) ->
                let first_error = Some (Parse_error.PP.error e) in
                (loc, InvalidJSXAttribute first_error)::errors, info
          end else (loc, InvalidJSXAttribute None)::errors, info in
        parse_attributes acc loc (padding, line) xs

    | _ :: xs ->
        parse_attributes (errors, info) loc line xs
    | [] -> (errors, info)
  in

  let parse_lines (errors, info) loc lines =
    let padding = String.make Loc.(loc.start.line - 1) '\n' ^
      String.make Loc.(loc.start.column + 2) ' ' in
    let acc, _ = List.fold_left (fun (acc, padding) line ->
      let acc =
        parse_attributes acc loc (padding, line) (Str.split words_rx line) in
      acc, padding ^ (String.make (String.length line) ' ') ^ "\n"
    ) ((errors, info), padding) lines in
    acc
  in

  let string_of_comment = function
  | (loc, Ast.Comment.Block s)
  | (loc, Ast.Comment.Line s)
    -> loc, s
  in

  let map_n =
    let rec helper f remaining acc = function
      | [] -> List.rev acc
      | hd::rest ->
        if remaining <= 0 then List.rev acc
        else helper f (remaining - 1) ((f hd)::acc) rest
    in
    fun f n lst -> helper f n [] lst
  in

  fun ~max_tokens filename content ->
    (* Consume tokens in the file until we get a comment. This is a hack to
     * support Nuclide, which needs 'use babel' as the first token due to
     * contstraints with Atom (see https://github.com/atom/atom/issues/8416 for
     * more context). At some point this should change back to consuming only
     * the first token. *)
    let lb = Lexing.from_string content in
    let env =
      Lexer_flow.Lex_env.new_lex_env (Some filename) lb ~enable_types_in_comments:false in
    let rec get_first_comment_contents ?(i=0) env =
      if i < max_tokens then
        let env, lexer_result = Lexer_flow.token env in
        match Lex_result.comments lexer_result with
        | [] -> Lexer_flow.Token.(
            (**
             * Stop looking for docblocks if we see any tokens other than a
             * string or a semicolon (`"use babel";` or `"use strict";`).
             *)
            match Lex_result.token lexer_result with
            | T_STRING _
            | T_SEMICOLON
              -> get_first_comment_contents ~i:(i + 1) env
            | _ -> None
          )
        | comments ->
          Some (map_n string_of_comment (max_tokens - i) comments)
      else None in
    let info =
      let filename_str = Loc.string_of_filename filename in
      if Filename.check_suffix filename_str Files.flow_ext
      then { default_info with isDeclarationFile = true; }
      else default_info in
    match get_first_comment_contents env with
      | Some comments ->
          List.fold_left (fun acc (loc, s) ->
            parse_lines acc loc (Str.split_delim lines_rx s)
          ) ([], info) comments
      | None -> [], info

(* accessors *)
let flow info = info.flow
let preventMunge info = info.preventMunge
let providesModule info = info.providesModule
let isDeclarationFile info = info.isDeclarationFile
let jsx info = info.jsx

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
