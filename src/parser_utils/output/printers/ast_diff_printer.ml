(**
 * Copyright (c) 2014, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast_differ
open Utils_js

let layout_of_node comments node =
  let old = !Js_layout_generator.with_attached_comments in
  Js_layout_generator.with_attached_comments := comments;
  let layout = match node with
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
  | Identifier id -> Js_layout_generator.identifier id
  | Pattern pat -> Js_layout_generator.pattern pat
  | TypeAnnotation annot -> Js_layout_generator.type_annotation annot
  | ClassProperty prop -> Js_layout_generator.class_property prop in
  Js_layout_generator.with_attached_comments := old;
  layout

let text_of_node comments =
  layout_of_node comments
  (* TODO if we are reprinting the entire program we probably want this to be
   * false. Add some tests and make sure we get it right. *)
  %> Pretty_printer.print ~source_maps:None ~skip_endline:true
  %> Source.contents

let text_of_nodes = text_of_node %> ListUtils.to_string "\n"

let edit_of_change comments = function
  | loc, Replace (_, new_node) -> (loc, text_of_node comments new_node)
  | loc, Insert new_nodes -> (loc, text_of_nodes comments new_nodes)
  | loc, Delete _ -> (loc, "")

let edits_of_changes comments changes =
  List.map (edit_of_change comments) changes
