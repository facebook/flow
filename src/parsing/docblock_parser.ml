(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Sedlexing = Flow_sedlexing
module Ast = Flow_ast

(* Avoid lexing unbounded in perverse cases *)
let docblock_max_tokens = 10

type docblock_error_kind =
  | MultipleFlowAttributes
  | InvalidFlowMode of string
  | MultipleJSXAttributes
  | InvalidJSXAttribute of string option
  | MultipleJSXRuntimeAttributes
  | InvalidJSXRuntimeAttribute
  | InvalidSupportsPlatform of string
  | DisallowedSupportsPlatform

type docblock_error = Loc.t * docblock_error_kind

let attributes_rx = Str.regexp "[ \t\r\n\\*/]+"

let lines_rx = Str.regexp "\\(\r\n\\|\n\\|\r\\)"

let pragma_rx = Str.regexp "^@"

let extract_docblock =
  Docblock.(
    (* walks a list of words, returns a list of errors and the extracted info.
       if @flow or @providesModule is found more than once, the first one is used
       and an error is returned. *)
    let rec parse_attributes ~file_options ~filename (errors, info) = function
      | (loc, "@flow") :: (_, "strict") :: xs ->
        let acc =
          if info.flow <> None then
            ((loc, MultipleFlowAttributes) :: errors, info)
          else
            (errors, { info with flow = Some OptInStrict })
        in
        parse_attributes ~file_options ~filename acc xs
      | (loc, "@flow") :: (_, "strict-local") :: xs ->
        let acc =
          if info.flow <> None then
            ((loc, MultipleFlowAttributes) :: errors, info)
          else
            (errors, { info with flow = Some OptInStrictLocal })
        in
        parse_attributes ~file_options ~filename acc xs
      | (flow_loc, "@flow") :: (next_loc, next) :: xs
        when Loc.lines_intersect flow_loc next_loc && (not @@ Str.string_match pragma_rx next 0) ->
        let (errors, info) =
          if info.flow <> None then
            ((flow_loc, MultipleFlowAttributes) :: errors, info)
          else
            (errors, { info with flow = Some OptIn })
        in
        let errors = (next_loc, InvalidFlowMode next) :: errors in
        parse_attributes ~file_options ~filename (errors, info) xs
      | (loc, "@flow") :: xs ->
        let acc =
          if info.flow <> None then
            ((loc, MultipleFlowAttributes) :: errors, info)
          else
            (errors, { info with flow = Some OptIn })
        in
        parse_attributes ~file_options ~filename acc xs
      | (loc, "@noflow") :: xs ->
        let acc =
          if info.flow <> None then
            ((loc, MultipleFlowAttributes) :: errors, info)
          else
            (errors, { info with flow = Some OptOut })
        in
        parse_attributes ~file_options ~filename acc xs
      | (_, "@preventMunge") :: xs ->
        (* dupes are ok since they can only be truthy *)
        parse_attributes ~file_options ~filename (errors, { info with preventMunge = true }) xs
      | [(jsx_loc, "@jsx")] -> ((jsx_loc, InvalidJSXAttribute None) :: errors, info)
      | (jsx_loc, "@jsx") :: (expr_loc, expr) :: xs ->
        let acc =
          if info.jsx <> None then
            ((jsx_loc, MultipleJSXAttributes) :: errors, info)
          else
            (* The point of the padding is to make the parsed code line up
             * with the comment in the original source *)
            let padding =
              String.make Loc.(expr_loc.start.line - 1) '\n'
              ^ String.make Loc.(expr_loc.start.column) ' '
            in
            try
              let (jsx_expr, _) =
                Parser_flow.jsx_pragma_expression (padding ^ expr) expr_loc.Loc.source
              in
              (errors, { info with jsx = Some (expr, jsx_expr) })
            with
            | Parse_error.Error ((_, e), _) ->
              let e = Some (Parse_error.PP.error e) in
              ((expr_loc, InvalidJSXAttribute e) :: errors, info)
        in
        parse_attributes ~file_options ~filename acc xs
      | (loc, "@jsxRuntime") :: (_, "classic") :: xs ->
        let acc =
          if info.jsxRuntime <> None then
            ((loc, MultipleJSXRuntimeAttributes) :: errors, info)
          else
            (errors, { info with jsxRuntime = Some JsxRuntimePragmaClassic })
        in
        parse_attributes ~file_options ~filename acc xs
      | (loc, "@jsxRuntime") :: (_, "automatic") :: xs ->
        let acc =
          if info.jsxRuntime <> None then
            ((loc, MultipleJSXRuntimeAttributes) :: errors, info)
          else
            (errors, { info with jsxRuntime = Some JsxRuntimePragmaAutomatic })
        in
        parse_attributes ~file_options ~filename acc xs
      | (loc, "@jsxRuntime") :: _ :: xs ->
        let acc = ((loc, InvalidJSXRuntimeAttribute) :: errors, info) in
        parse_attributes ~file_options ~filename acc xs
      | (loc, "@supportsPlatform") :: (_, platform) :: xs when Files.multi_platform file_options ->
        let acc =
          if
            filename
            |> File_key.to_string
            |> Files.platform_specific_extensions_and_indices_opt ~options:file_options
            |> Option.is_some
          then
            ((loc, DisallowedSupportsPlatform) :: errors, info)
          else if
            Base.List.mem
              ~equal:String.equal
              (Files.multi_platform_extensions file_options)
              ("." ^ platform)
          then
            let existing_platforms = Base.Option.value info.supportsPlatform ~default:[] in
            let platforms =
              if Base.List.mem ~equal:String.equal existing_platforms platform then
                existing_platforms
              else
                platform :: existing_platforms
            in
            (errors, { info with supportsPlatform = Some platforms })
          else
            ((loc, InvalidSupportsPlatform platform) :: errors, info)
        in
        parse_attributes ~file_options ~filename acc xs
      | _ :: xs -> parse_attributes ~file_options ~filename (errors, info) xs
      | [] -> (errors, info)
    in
    let calc_end start s =
      Str.full_split lines_rx s
      |> List.fold_left
           Loc.(
             fun _end elem ->
               match elem with
               | Str.Delim delim ->
                 let line_incr =
                   if delim = "\r" then
                     0
                   else
                     1
                 in
                 let column = 0 in
                 let line = _end.line + line_incr in
                 { column; line }
               | Str.Text text ->
                 let length = String.length text in
                 let column = _end.column + length in
                 { _end with column }
           )
           start
    in
    let split loc s =
      (* Need to add 2 characters for the start of the comment *)
      let start = Loc.{ loc.start with column = loc.start.column + 2 } in
      Str.full_split attributes_rx s
      |> List.fold_left
           (fun (start, attributes) elem ->
             match elem with
             | Str.Delim s -> (calc_end start s, attributes)
             | Str.Text s ->
               let _end = calc_end start s in
               (_end, Loc.({ loc with start; _end }, s) :: attributes))
           (start, [])
      |> snd
      |> List.rev
    in
    let string_of_comment = function
      | (loc, { Ast.Comment.text; _ }) -> (loc, text)
    in
    let map_n =
      let rec helper f remaining acc = function
        | [] -> List.rev acc
        | hd :: rest ->
          if remaining <= 0 then
            List.rev acc
          else
            helper f (remaining - 1) (f hd :: acc) rest
      in
      (fun f n lst -> helper f n [] lst)
    in
    fun ~max_tokens ~file_options filename content ->
      (* Consume tokens in the file until we get a comment. This is a hack to
       * support Nuclide, which needs 'use babel' as the first token due to
       * constraints with Atom (see https://github.com/atom/atom/issues/8416 for
       * more context). At some point this should change back to consuming only
       * the first token. *)
      let lb =
        try Sedlexing.Utf8.from_string content with
        | Sedlexing.MalFormed ->
          Hh_logger.warn "File %s is malformed" (File_key.to_string filename);
          Sedlexing.Utf8.from_string ""
      in
      let env = Lex_env.new_lex_env (Some filename) lb ~enable_types_in_comments:false in
      let rec get_first_comment_contents ?(i = 0) env =
        if i < max_tokens then
          let (env, lexer_result) = Flow_lexer.token env in
          match Lex_result.comments lexer_result with
          | [] ->
            Token.(
              (*
               * Stop looking for docblocks if we see any tokens other than a
               * string or a semicolon (`"use babel";` or `"use strict";`) or
               * an interpreter directive (`#!/bin/env node`)
               *)
              (match Lex_result.token lexer_result with
              | T_STRING _
              | T_SEMICOLON
              | T_INTERPRETER _ ->
                get_first_comment_contents ~i:(i + 1) env
              | _ -> None)
            )
          | comments -> Some (map_n string_of_comment (max_tokens - i) comments)
        else
          None
      in
      match get_first_comment_contents env with
      | Some comments ->
        List.fold_left
          (fun acc (loc, s) -> parse_attributes ~file_options ~filename acc (split loc s))
          ([], default_info)
          comments
      | None -> ([], default_info)
  )

let parse_docblock ~max_tokens ~file_options file content : docblock_error list * Docblock.t =
  match file with
  | File_key.ResourceFile _
  | File_key.JsonFile _ ->
    ([], Docblock.default_info)
  | _ -> extract_docblock ~max_tokens ~file_options file content
