(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Param = struct
  type optionality =
    | NotOptional
    | Optional
    | OptionalWithDefault of string
  [@@deriving ord, show, eq]

  type info = {
    description: string option;
    optional: optionality;
  }
  [@@deriving ord, show, eq]

  type path =
    | Name
    | Element of path
    | Member of path * string
  [@@deriving ord, show, eq]
end

module PMap = WrappedMap.Make (struct
  type t = Param.path

  let compare = Param.compare_path
end)

type t = {
  description: string option;
  params: Param.info PMap.t SMap.t;
}

(*************)
(* accessors *)
(*************)

let description { description; _ } = description

let param { params; _ } name path =
  Base.Option.bind (SMap.find_opt name params) ~f:(PMap.find_opt path)

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

  let add_param jsdoc name path description optional =
    let old_param_infos =
      match SMap.find_opt name jsdoc.params with
      | None -> PMap.empty
      | Some pmap -> pmap
    in
    let new_param_infos = PMap.add path { Param.description; optional } old_param_infos in
    { jsdoc with params = SMap.add name new_param_infos jsdoc.params }

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

  let rec param_path ?(path = Param.Name) lexbuf =
    match%sedlex lexbuf with
    | "[]" -> param_path ~path:(Param.Element path) lexbuf
    | ('.', identifier) ->
      let member = Sedlexing.Utf8.sub_lexeme lexbuf 1 (Sedlexing.lexeme_length lexbuf - 1) in
      param_path ~path:(Param.Member (path, member)) lexbuf
    | _ -> path

  let rec skip_tag jsdoc lexbuf =
    match%sedlex lexbuf with
    | Plus (Compl '@') -> skip_tag jsdoc lexbuf
    | '@' -> tag jsdoc lexbuf
    | _ (* eof *) -> jsdoc

  and param_tag_description jsdoc name path optional lexbuf =
    let desc_buf = Buffer.create 127 in
    let description = description desc_buf lexbuf in
    let jsdoc = add_param jsdoc name path description optional in
    tag jsdoc lexbuf

  and param_tag_pre_description jsdoc name path optional lexbuf =
    match%sedlex lexbuf with
    | ' ' -> param_tag_pre_description jsdoc name path optional lexbuf
    | '-' -> param_tag_description jsdoc name path optional lexbuf
    | _ -> param_tag_description jsdoc name path optional lexbuf

  and param_tag_optional_default jsdoc name path def_buf lexbuf =
    match%sedlex lexbuf with
    | ']' ->
      let default = Buffer.contents def_buf in
      param_tag_pre_description jsdoc name path (Param.OptionalWithDefault default) lexbuf
    | Plus (Compl ']') ->
      Buffer.add_string def_buf (Sedlexing.Utf8.lexeme lexbuf);
      param_tag_optional_default jsdoc name path def_buf lexbuf
    | _ ->
      let default = Buffer.contents def_buf in
      param_tag_pre_description jsdoc name path (Param.OptionalWithDefault default) lexbuf

  and param_tag_optional jsdoc lexbuf =
    match%sedlex lexbuf with
    | identifier ->
      let name = Sedlexing.Utf8.lexeme lexbuf in
      let path = param_path lexbuf in
      (match%sedlex lexbuf with
      | ']' -> param_tag_pre_description jsdoc name path Param.Optional lexbuf
      | '=' ->
        let def_buf = Buffer.create 127 in
        param_tag_optional_default jsdoc name path def_buf lexbuf
      | _ -> param_tag_pre_description jsdoc name path Param.Optional lexbuf)
    | _ -> skip_tag jsdoc lexbuf

  (* ignore jsdoc type annotation *)
  and param_tag_type jsdoc lexbuf =
    match%sedlex lexbuf with
    | '}' -> param_tag jsdoc lexbuf
    | Plus (Compl '}') -> param_tag_type jsdoc lexbuf
    | _ (* eof *) -> jsdoc

  and param_tag jsdoc lexbuf =
    match%sedlex lexbuf with
    | ' ' -> param_tag jsdoc lexbuf
    | '{' -> param_tag_type jsdoc lexbuf
    | '[' -> param_tag_optional jsdoc lexbuf
    | identifier ->
      let name = Sedlexing.Utf8.lexeme lexbuf in
      let path = param_path lexbuf in
      param_tag_pre_description jsdoc name path Param.NotOptional lexbuf
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
