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
  errors : SyntaxError.t list
}

let make text =
  let parser = Parser.make text in
  let (parser, root) = Parser.parse_script parser in
  let errors = Parser.errors parser in
  { text; root; errors }

let root tree =
  tree.root

let text tree =
  tree.text

let errors tree =
  tree.errors
