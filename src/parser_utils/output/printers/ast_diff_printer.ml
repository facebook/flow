(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast_differ
open Utils_js

let layout_of_node ~opts = function
  | Raw str -> Layout.Atom str
  | Comment c -> Js_layout_generator.comment c
  | Literal (loc, lit) -> Js_layout_generator.literal ~opts loc lit
  | StringLiteral (loc, lit) -> Js_layout_generator.string_literal_type loc lit
  | NumberLiteral (loc, lit) -> Js_layout_generator.number_literal_type loc lit
  | BigIntLiteral (loc, lit) -> Js_layout_generator.bigint_literal_type loc lit
  | BooleanLiteral (loc, lit) -> Js_layout_generator.boolean_literal_type loc lit
  | Statement stmt -> Js_layout_generator.statement ~opts stmt
  | Program ast -> Js_layout_generator.program ~preserve_docblock:true ~checksum:None ast
  | Expression expr ->
    (* Wrap the expression in parentheses because we don't know what context we are in. *)
    (* TODO keep track of the expression context for printing, which will only insert parens when
     * actually needed. *)
    Layout.fuse [Layout.Atom "("; Js_layout_generator.expression ~opts expr; Layout.Atom ")"]
  | Pattern pat -> Js_layout_generator.pattern ~opts pat
  | Params params -> Js_layout_generator.function_params ~opts params
  | Variance var -> Js_layout_generator.variance var
  | Type typ -> Js_layout_generator.type_ ~opts typ
  | TypeParam t_param -> Js_layout_generator.type_param ~opts t_param
  | TypeAnnotation annot -> Js_layout_generator.type_annotation ~opts ~parens:false annot
  | FunctionTypeAnnotation annot -> Js_layout_generator.type_annotation ~opts ~parens:true annot
  | ClassProperty prop -> Js_layout_generator.class_property ~opts prop
  | ObjectProperty prop -> Js_layout_generator.object_property ~opts prop
  | TemplateLiteral (_, t_lit) -> Js_layout_generator.template_literal ~opts t_lit
  | JSXChild child ->
    begin
      match Js_layout_generator.jsx_child ~opts child with
      | Some (_, layout_node) -> layout_node
      (* This case shouldn't happen, so return Empty *)
      | None -> Layout.Empty
    end
  | JSXIdentifier id -> Js_layout_generator.jsx_identifier id

let text_of_node ~opts =
  layout_of_node ~opts
  (* TODO if we are reprinting the entire program we probably want this to be
   * false. Add some tests and make sure we get it right. *)
  %> Pretty_printer.print ~source_maps:None ~skip_endline:true
  %> Source.contents

let text_of_nodes ~opts break nodes =
  let sep =
    match break with
    | Some str -> str
    | None -> "\n"
  in
  ListUtils.to_string sep (text_of_node ~opts) nodes

let edit_of_change ~opts = function
  | (loc, Replace (_, new_node)) -> (loc, text_of_node ~opts new_node)
  | (loc, Insert (break, new_nodes)) -> (loc, text_of_nodes ~opts break new_nodes)
  | (loc, Delete _) -> (loc, "")

let edits_of_changes ?(opts = Js_layout_generator.default_opts) changes =
  List.map (edit_of_change ~opts) changes
