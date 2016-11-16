(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module SyntaxError = Full_fidelity_syntax_error
module SyntaxTree = Full_fidelity_syntax_tree
module SourceText = Full_fidelity_source_text

let usage = Printf.sprintf "Usage: %s filename\n" Sys.argv.(0)

let parse_and_print filename =
  let file = Relative_path.create Relative_path.Dummy filename in
  let source_text = SourceText.from_file file in
  let syntax_tree = SyntaxTree.make source_text in
  let str = Debug.dump_full_fidelity syntax_tree in

  Printf.printf "%s\n" (SourceText.get_text source_text);
  Printf.printf "%s\n" str;
  let editable = Full_fidelity_editable_syntax.from_tree syntax_tree in
  let chunk_groups = Hack_format.run ~debug:true editable in
  let formatted_string = Line_splitter.solve chunk_groups in
  Printf.printf ("Formatting result:\n%s") formatted_string;
  ()

let () =
  Arg.parse [] parse_and_print usage;
