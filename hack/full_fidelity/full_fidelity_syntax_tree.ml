(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(**
 * A syntax tree is just a thin wrapper around all the output of the parser:
 * the source text that was parsed, the root of the parse tree, and a
 * collection of parser and lexer errors.
 *
 * "Making" a syntax tree from text parses the text.
 *
 *)

module SourceText = Full_fidelity_source_text
module Parser = Full_fidelity_parser
module SyntaxError = Full_fidelity_syntax_error
module MinimalSyntax = Full_fidelity_minimal_syntax

type t = {
  text : SourceText.t;
  root : MinimalSyntax.t;
  errors : SyntaxError.t list;
  language : string;
  mode : string
}

let strip_comment_start s =
  let len = String.length s in
  if len >= 2 && (String.get s 0) = '/' && (String.get s 1) = '/' then
    String.sub s 2 (len - 2)
  else
    s

let analyze_header text header =
  match MinimalSyntax.syntax header with
  | MinimalSyntax.ScriptHeader h ->
    let lt = MinimalSyntax.header_less_than h in
    let qm = MinimalSyntax.header_question h in
    let lang = MinimalSyntax.header_language h in
    let lt_full_width = MinimalSyntax.full_width lt in
    let qm_full_width = MinimalSyntax.full_width qm in
    let lang_leading = MinimalSyntax.leading_width lang in
    let lang_width = MinimalSyntax.width lang in
    let lang_trailing = MinimalSyntax.trailing_width lang in
    let language = SourceText.sub text (lt_full_width +
      qm_full_width) lang_width in
    let mode = SourceText.sub text (lt_full_width + qm_full_width +
      lang_leading + lang_width) lang_trailing in
    let mode = String.trim mode in
    let mode = strip_comment_start mode in
    let mode = String.trim mode in
    (language, mode)
  | _ -> failwith "unexpected missing header"
  (* The parser never produces a missing header; it fills one in with zero
     width tokens if it needs to. *)

let get_language_and_mode text root =
  match MinimalSyntax.syntax root with
  | MinimalSyntax.Script s ->
    let header = MinimalSyntax.script_header s in
    analyze_header text header
  | _ -> failwith "unexpected missing script node"
    (* The parser never produces a missing script, even if the file is empty *)

let make text =
  let parser = Parser.make text in
  let (parser, root) = Parser.parse_script parser in
  let errors = Parser.errors parser in
  let (language, mode) = get_language_and_mode text root in
  { text; root; errors; language; mode }

let root tree =
  tree.root

let text tree =
  tree.text

let errors tree =
  tree.errors

let language tree =
  tree.language

let mode tree =
  tree.mode

let is_hack tree =
  tree.language = "hh"

let is_php tree =
  tree.language = "php"

let is_strict tree =
  (is_hack tree) && tree.mode = "strict"
