(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  description: string option;
  params: string SMap.t;
}

(*************)
(* accessors *)
(*************)

let description { description; _ } = description

let param { params; _ } name = SMap.find_opt name params

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

  let identifier = [%sedlex.regexp? Plus (Compl (white_space | '[' | '.' | ']' | '=' | '{'))]

  (* Helpers *)

  let empty = { description = None; params = SMap.empty }

  let trimmed_string_of_buffer buffer = buffer |> Buffer.contents |> String.trim

  let description_of_desc_buf desc_buf =
    match trimmed_string_of_buffer desc_buf with
    | "" -> None
    | s -> Some s

  let add_param jsdoc name description =
    { jsdoc with params = SMap.add name description jsdoc.params }

  (* Parsing functions *)

  (*
    `description`, `description_or_tag`, and `description_startline` are
    helpers for parsing descriptions: a description is a possibly-multiline
    string terminated by EOF or a new tag. The beginning of each line could
    contain whitespace and asterisks, which are stripped out when parsing.
   *)
  let rec description desc_buf lexbuf =
    match%sedlex lexbuf with
    | line_terminator_sequence ->
      Buffer.add_string desc_buf (Sedlexing.Utf8.lexeme lexbuf);
      description_startline desc_buf lexbuf
    | any ->
      Buffer.add_string desc_buf (Sedlexing.Utf8.lexeme lexbuf);
      description desc_buf lexbuf
    | _ (* eof *) -> description_of_desc_buf desc_buf

  and description_or_tag desc_buf lexbuf =
    match%sedlex lexbuf with
    | '@' -> description_of_desc_buf desc_buf
    | _ -> description desc_buf lexbuf

  and description_startline desc_buf lexbuf =
    match%sedlex lexbuf with
    | '*'
    | whitespace ->
      description_startline desc_buf lexbuf
    | _ -> description_or_tag desc_buf lexbuf

  let rec skip_tag jsdoc lexbuf =
    match%sedlex lexbuf with
    | Plus (Compl '@') -> skip_tag jsdoc lexbuf
    | '@' -> tag jsdoc lexbuf
    | _ (* eof *) -> jsdoc

  and param_tag_description jsdoc name lexbuf =
    let desc_buf = Buffer.create 127 in
    let desc_opt = description desc_buf lexbuf in
    let jsdoc =
      Base.Option.fold desc_opt ~init:jsdoc ~f:(fun jsdoc desc -> add_param jsdoc name desc)
    in
    tag jsdoc lexbuf

  and param_tag_pre_description jsdoc name lexbuf =
    match%sedlex lexbuf with
    | ' ' -> param_tag_pre_description jsdoc name lexbuf
    | '-' -> param_tag_description jsdoc name lexbuf
    | _ -> param_tag_description jsdoc name lexbuf

  (* ignore jsdoc type annotation *)
  and param_tag_type jsdoc lexbuf =
    match%sedlex lexbuf with
    | '}' -> param_tag jsdoc lexbuf
    | Plus (Compl '}') -> param_tag_type jsdoc lexbuf
    | _ -> jsdoc

  and param_tag jsdoc lexbuf =
    match%sedlex lexbuf with
    | whitespace -> param_tag jsdoc lexbuf
    | '{' -> param_tag_type jsdoc lexbuf
    | identifier ->
      let name = Sedlexing.Utf8.lexeme lexbuf in
      param_tag_pre_description jsdoc name lexbuf
    | _ -> skip_tag jsdoc lexbuf

  and tag jsdoc lexbuf =
    match%sedlex lexbuf with
    | "param"
    | "arg"
    | "argument" ->
      param_tag jsdoc lexbuf
    | _ -> skip_tag jsdoc lexbuf

  let initial lexbuf =
    match%sedlex lexbuf with
    | ('*', Compl '*') ->
      Sedlexing.rollback lexbuf;
      let desc_buf = Buffer.create 127 in
      let description = description_startline desc_buf lexbuf in
      let jsdoc = { empty with description } in
      Some (tag jsdoc lexbuf)
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
