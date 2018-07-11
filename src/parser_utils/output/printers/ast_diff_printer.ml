(**
 * Copyright (c) 2014, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast_differ

let layout_of_node = function
  | Statement stmt -> Js_layout_generator.statement stmt
  | Program ast -> Js_layout_generator.program ~preserve_docblock:true ~checksum:None ast
  | Expression expr ->
    (* Wrap the expression in parentheses because we don't know what context we are in. *)
    (* TODO keep track of the expression context for printing, which will only insert parens when
     * actually needed. *)
    Layout.fuse [
      Layout.Atom "(";
      Js_layout_generator.expression expr;
      Layout.Atom ")";
    ]

let text_of_node node =
  let layout = layout_of_node node in
  (* TODO if we are reprinting the entire program we probably want this to be
   * false. Add some tests and make sure we get it right. *)
  let skip_endline = true in
  let source = Pretty_printer.print ~source_maps:None ~skip_endline layout in
  Source.contents source

let edit_of_change = function
  | loc, Replace (_, new_node) -> (loc, text_of_node new_node)

let edits_of_changes changes = List.map edit_of_change changes
