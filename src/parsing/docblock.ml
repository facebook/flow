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
  (* walks a list of words, returns a list of errors and the extracted info.
     if @flow or @providesModule is found more than once, the first one is used
     and an error is returned. *)
  let rec parse_attributes (errors, info) = function
    | (loc, "@flow") :: (_, "weak") :: xs ->
        let acc =
          if info.flow <> None then (loc, MultipleFlowAttributes)::errors, info
          else errors, { info with flow = Some OptInWeak } in
        parse_attributes acc xs
    | (loc, "@flow") :: xs ->
        let acc =
          if info.flow <> None then (loc, MultipleFlowAttributes)::errors, info
          else errors, { info with flow = Some OptIn } in
        parse_attributes acc xs
    | (loc, "@noflow") :: xs ->
        let acc =
          if info.flow <> None then (loc, MultipleFlowAttributes)::errors, info
          else errors, { info with flow = Some OptOut } in
        parse_attributes acc xs
    | (loc, "@providesModule") :: (_, m) :: xs ->
        let acc =
          if info.providesModule <> None then
            (loc, MultipleProvidesModuleAttributes)::errors, info
          else
            errors, { info with providesModule = Some m }
        in
        parse_attributes acc xs
    | (_, "@preventMunge") :: xs ->
        (* dupes are ok since they can only be truthy *)
        let preventMunge = Some true in
        parse_attributes (errors, { info with preventMunge }) xs
    | [jsx_loc, "@jsx"] -> (jsx_loc, InvalidJSXAttribute None)::errors, info
    | (jsx_loc, "@jsx") :: (expr_loc, expr) :: xs ->
        let acc =
          if info.jsx <> None
          then (jsx_loc, MultipleJSXAttributes)::errors, info
          else begin
            (* The point of the padding is to make the parsed code line up
             * with the comment in the original source *)
            let padding = (String.make Loc.(expr_loc.start.line - 1) '\n') ^
              (String.make Loc.(expr_loc.start.column) ' ') in
            try
              let (jsx_expr, _) = Parser_flow.jsx_pragma_expression
                (padding ^ expr)
                expr_loc.Loc.source in
              errors, { info with jsx = Some (expr, jsx_expr) }
            with
            | Parse_error.Error [] ->
                (expr_loc, InvalidJSXAttribute None)::errors, info
            | Parse_error.Error ((_, e)::_) ->
                let first_error = Some (Parse_error.PP.error e) in
                (expr_loc, InvalidJSXAttribute first_error)::errors, info
          end in
        parse_attributes acc xs

    | _ :: xs ->
        parse_attributes (errors, info) xs
    | [] -> (errors, info)
  in

  let attributes_rx = Str.regexp "[ \t\r\n\\*/]+" in
  let lines_rx = Str.regexp "\\(\r\n\\|\n\\|\r\\)" in
  let calc_end start s =
    Str.full_split lines_rx s
    |> List.fold_left Loc.(fun _end elem ->
      match elem with
      | Str.Delim delim ->
          let line_incr = if "delim" = "\r" then 0 else 1 in
          let column = 0 in
          let line = _end.line + line_incr in
          let offset = _end.offset + (String.length delim) in
          { column; line; offset; }
      | Str.Text text ->
          let length = String.length text in
          let column = _end.column + length in
          let offset = _end.offset + length in
          { _end with column; offset; }
    ) start in
  let split loc s =
    (* Need to add 2 characters for the start of the comment *)
    let start = Loc.({ loc.start with
      column = loc.start.column + 2;
      offset = loc.start.offset + 2;
    }) in
    Str.full_split attributes_rx s
    |> List.fold_left (fun (start, attributes) elem ->
      match elem with
      | Str.Delim s ->
          (calc_end start s, attributes)
      | Str.Text s ->
          let _end = calc_end start s in
          (_end, Loc.({loc with start; _end; }, s)::attributes)
    ) (start, [])
    |> snd
    |> List.rev

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
            parse_attributes acc (split loc s)
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
