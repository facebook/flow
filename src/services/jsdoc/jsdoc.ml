(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = { description: string option }

(*************)
(* accessors *)
(*************)

let description { description; _ } = description

(***********)
(* parsing *)
(***********)

module Parser = struct
  (* regexps copied from Flow_lexer since sedlex doesn't let us import them *)

  let whitespace =
    [%sedlex.regexp?
      ( 0x0009 | 0x000B | 0x000C | 0x0020 | 0x00A0 | 0xfeff | 0x1680
      | 0x2000 .. 0x200a
      | 0x202f | 0x205f | 0x3000 )]

  let line_terminator_sequence = [%sedlex.regexp? '\n' | '\r' | "\r\n" | 0x2028 | 0x2029]

  (* Helpers *)

  let trimmed_string_of_buffer buffer = buffer |> Buffer.contents |> String.trim

  let description_of_desc_buf desc_buf =
    match trimmed_string_of_buffer desc_buf with
    | "" -> None
    | s -> Some s

  (* Parsing functions *)

  (* TODO: parse certain tags like @param *)
  let tag description _ = Some { description }

  let rec description desc_buf lexbuf =
    match%sedlex lexbuf with
    | line_terminator_sequence ->
      Buffer.add_string desc_buf (Sedlexing.Utf8.lexeme lexbuf);
      description_startline desc_buf lexbuf
    | any ->
      Buffer.add_string desc_buf (Sedlexing.Utf8.lexeme lexbuf);
      description desc_buf lexbuf
    | _ (* eof *) -> Some { description = description_of_desc_buf desc_buf }

  and description_or_tag desc_buf lexbuf =
    match%sedlex lexbuf with
    | '@' -> tag (description_of_desc_buf desc_buf) lexbuf
    | _ -> description desc_buf lexbuf

  and description_startline desc_buf lexbuf =
    match%sedlex lexbuf with
    | '*'
    | whitespace ->
      description_startline desc_buf lexbuf
    | _ -> description_or_tag desc_buf lexbuf

  let initial lexbuf =
    match%sedlex lexbuf with
    | ('*', Compl '*') ->
      Sedlexing.rollback lexbuf;
      let desc_buf = Buffer.create 127 in
      description_startline desc_buf lexbuf
    | _ -> None
end

let parse str =
  let lexbuf = Sedlexing.Utf8.from_string str in
  Parser.initial lexbuf

(* find and parse the last jsdoc-containing comment in the list if exists *)
let of_comments =
  let open Flow_ast in
  let of_comment = function
    | (_, Comment.{ kind = Block; text; _ }) -> parse text
    | (_, Comment.{ kind = Line; _ }) -> None
  in
  let rec of_comment_list = function
    | [] -> None
    | c :: cs ->
      (match of_comment_list cs with
      | Some _ as j -> j
      | None -> of_comment c)
  in
  let of_syntax Syntax.{ leading; _ } = of_comment_list leading in
  Base.Option.bind ~f:of_syntax
