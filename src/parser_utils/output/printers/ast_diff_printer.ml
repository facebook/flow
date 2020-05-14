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

let expand_loc_with_comments loc node =
  let open Comment_attachment in
  let bounds (loc, node) f =
    let collector = new comment_bounds_collector ~loc in
    ignore (f collector (loc, node));
    collector#comment_bounds
  in
  let comment_bounds =
    match node with
    | Literal (loc, lit) -> bounds (loc, lit) (fun collect (loc, lit) -> collect#literal loc lit)
    | StringLiteral (loc, lit) ->
      bounds (loc, lit) (fun collect (loc, lit) -> collect#string_literal_type loc lit)
    | NumberLiteral (loc, lit) ->
      bounds (loc, lit) (fun collect (loc, lit) -> collect#number_literal_type loc lit)
    | BigIntLiteral (loc, lit) ->
      bounds (loc, lit) (fun collect (loc, lit) -> collect#bigint_literal_type loc lit)
    | BooleanLiteral (loc, lit) ->
      bounds (loc, lit) (fun collect (loc, lit) -> collect#boolean_literal_type loc lit)
    | Statement stmt -> bounds stmt (fun collect stmt -> collect#statement stmt)
    | Expression expr -> bounds expr (fun collect expr -> collect#expression expr)
    | Pattern pat -> bounds pat (fun collect pat -> collect#pattern pat)
    | Params params -> bounds params (fun collect params -> collect#function_params params)
    | Variance var -> bounds var (fun collect var -> collect#variance (Some var))
    | Type ty -> bounds ty (fun collect ty -> collect#type_ ty)
    | TypeParam tparam -> bounds tparam (fun collect tparam -> collect#type_param tparam)
    | TypeAnnotation annot
    | FunctionTypeAnnotation annot ->
      bounds annot (fun collect annot -> collect#type_annotation annot)
    | ClassProperty prop -> bounds prop (fun collect (loc, prop) -> collect#class_property loc prop)
    | ObjectProperty (Ast.Expression.Object.Property prop) ->
      bounds prop (fun collect prop -> collect#object_property prop)
    | ObjectProperty (Ast.Expression.Object.SpreadProperty prop) ->
      bounds prop (fun collect prop -> collect#spread_property prop)
    | TemplateLiteral (loc, lit) ->
      bounds (loc, lit) (fun collect (loc, lit) -> collect#template_literal loc lit)
    | JSXIdentifier id -> bounds id (fun collect id -> collect#jsx_identifier id)
    (* Nodes that do have attached comments *)
    | Raw _
    | Comment _
    | Program _
    | JSXChild _ ->
      (None, None)
  in
  expand_loc_with_comment_bounds loc comment_bounds

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
  | (loc, Replace (old_node, new_node)) ->
    (expand_loc_with_comments loc old_node, text_of_node ~opts new_node)
  | (loc, Insert (break, new_nodes)) -> (loc, text_of_nodes ~opts break new_nodes)
  | (loc, Delete node) -> (expand_loc_with_comments loc node, "")

let edits_of_changes ?(opts = Js_layout_generator.default_opts) changes =
  List.map (edit_of_change ~opts) changes
