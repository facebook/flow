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

open Core

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
  let solve_states = List.map chunk_groups ~f:(fun chunk_group ->
    let chunks = chunk_group.Chunk_group.chunks in
    let ra = chunk_group.Chunk_group.ra in
    let rvm = Rule_allocator.get_initial_rvm ra in
    let bi = chunk_group.Chunk_group.bi in
    let init_state = Solve_state.make chunks rvm ra bi in
    let state_queue = State_queue.make [init_state] in
    Line_splitter.solve state_queue
  ) in
  (*
  Printf.printf "%s\n" (Rule.dependency_map_to_string ());
  Printf.printf "%b\n" (Rule.is_rule_value_map_valid result.Solve_state.rvm);
  Printf.printf "%s\n" (Solve_state.__debug result);
  *)
  Printf.printf ("Formatting result:\n");
  List.iter solve_states ~f:(fun ss ->
    Printf.printf "%s" (State_printer.print_state ss)
  );
  Printf.printf ("\n");
  ()

let () =
  Arg.parse [] parse_and_print usage;
