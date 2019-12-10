(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast_differ
open Utils_js

let layout_of_node comments node =
  let old = !Js_layout_generator.with_attached_comments in
  Js_layout_generator.with_attached_comments := comments;
  let layout =
    match node with
    | Raw str -> Layout.Atom str
    | Comment c -> Js_layout_generator.comment c
    | NumberLiteralNode t -> Js_layout_generator.number_literal_type t
    | Literal lit -> Js_layout_generator.literal lit
    | StringLiteral lit -> Js_layout_generator.string_literal_type lit
    | Statement stmt -> Js_layout_generator.statement stmt
    | Program ast -> Js_layout_generator.program ~preserve_docblock:true ~checksum:None ast
    | Expression expr ->
      (* Wrap the expression in parentheses because we don't know what context we are in. *)
      (* TODO keep track of the expression context for printing, which will only insert parens when
       * actually needed. *)
      Layout.fuse [Layout.Atom "("; Js_layout_generator.expression expr; Layout.Atom ")"]
    | Pattern pat -> Js_layout_generator.pattern pat
    | Params params -> Js_layout_generator.arrow_function_params params
    | Variance var -> Js_layout_generator.variance var
    | Type typ -> Js_layout_generator.type_ typ
    | TypeParam t_param -> Js_layout_generator.type_param t_param
    | TypeAnnotation annot -> Js_layout_generator.type_annotation ~parens:false annot
    | FunctionTypeAnnotation annot -> Js_layout_generator.type_annotation ~parens:true annot
    | ClassProperty prop -> Js_layout_generator.class_property prop
    | ObjectProperty prop -> Js_layout_generator.object_property prop
    | TemplateLiteral t_lit -> Js_layout_generator.template_literal t_lit
    | JSXChild child ->
      begin
        match Js_layout_generator.jsx_child child with
        | Some (_, layout_node) -> layout_node
        (* This case shouldn't happen, so return Empty *)
        | None -> Layout.Empty
      end
    | JSXIdentifier id -> Js_layout_generator.jsx_identifier id
  in
  Js_layout_generator.with_attached_comments := old;
  layout

let text_of_node comments =
  layout_of_node comments
  (* TODO if we are reprinting the entire program we probably want this to be
   * false. Add some tests and make sure we get it right. *)
  %> Pretty_printer.print ~source_maps:None ~skip_endline:true
  %> Source.contents

let text_of_nodes break =
  let sep =
    match break with
    | Some str -> str
    | None -> "\n"
  in
  text_of_node %> ListUtils.to_string sep

let edit_of_change comments = function
  | (loc, Replace (_, new_node)) -> (loc, text_of_node comments new_node)
  | (loc, Insert (break, new_nodes)) -> (loc, text_of_nodes break comments new_nodes)
  | (loc, Delete _) -> (loc, "")

let edits_of_changes comments changes = List.map (edit_of_change comments) changes
