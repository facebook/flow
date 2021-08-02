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
  | Statement (stmt, _) -> Js_layout_generator.statement ~opts stmt
  | Program ast -> Js_layout_generator.program ~preserve_docblock:true ~checksum:None ast
  (* Do not wrap expression in parentheses for cases where we know parentheses are not needed. *)
  | Expression (expr, (StatementParentOfExpression _ | SlotParentOfExpression)) ->
    Js_layout_generator.expression ~opts expr
  | Expression (expr, _) ->
    (* TODO use expression context for printing to insert parens when actually needed. *)
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

let text_of_layout =
  (* TODO if we are reprinting the entire program we probably want this to be
   * false. Add some tests and make sure we get it right. *)
  Pretty_printer.print ~source_maps:None ~skip_endline:true %> Source.contents

let text_of_node ~opts = layout_of_node ~opts %> text_of_layout

let is_statement_list =
  Base.List.for_all ~f:(function
      | Statement _ -> true
      | _ -> false)

let text_of_statement_list ~opts nodes =
  let stmts =
    Base.List.filter_map
      ~f:(function
        | Statement (stmt, _) -> Some stmt
        | _ -> None)
      nodes
  in
  text_of_layout (Layout.fuse (Js_layout_generator.statement_list ~opts stmts))

let text_of_nodes ~opts ~separator ~leading_separator nodes =
  let sep =
    match separator with
    | Some str -> str
    | None -> "\n"
  in
  let text = ListUtils.to_string sep (text_of_node ~opts) nodes in
  if leading_separator then
    sep ^ text
  else
    text

let edit_of_change ~opts = function
  | (loc, Replace (_, new_node)) -> (loc, text_of_node ~opts new_node)
  | (loc, Insert { items; separator; leading_separator }) when is_statement_list items ->
    let text = text_of_statement_list ~opts items in
    if leading_separator then
      let sep =
        match separator with
        | Some str -> str
        | None -> "\n"
      in
      (loc, sep ^ text)
    else
      (loc, text)
  | (loc, Insert { items; separator; leading_separator }) ->
    (loc, text_of_nodes ~opts ~separator ~leading_separator items)
  | (loc, Delete _) -> (loc, "")

let rec edits_of_changes ?(opts = Js_layout_generator.default_opts) changes =
  match changes with
  | [] -> []
  (* Detect the case when a statement list was broken up into a Replace with a statement
     followed by an Insert containing a statement list. Reconstruct the original statement
     list to print so that whitespace can be preserved between the first and second statements. *)
  | (loc1, Replace (_, Statement item)) :: (loc2, Insert { items; _ }) :: tl
    when Loc.equal (Loc.end_loc loc1) loc2 && is_statement_list items ->
    (loc1, text_of_statement_list ~opts (Statement item :: items)) :: edits_of_changes ~opts tl
  (* Detect the case when we want to replace a list of statements with a single statement.
     The AST diffing algorithm will translate this into a replace followed by a delete.
     We should coalesce this into a single replace spanning both the replace and delete with
     the replacement, so that we can avoid an empty line being printed in the place of deleted
     statements. *)
  | (loc1, Replace (old_node, (Statement ((new_loc, _), _) as new_node))) :: (loc2, Delete _) :: tl
    when Loc.contains new_loc (Loc.btwn loc1 loc2) ->
    edits_of_changes ~opts ((new_loc, Replace (old_node, new_node)) :: tl)
  (* Similar to the case above, but sometimes we have diff patterns like:
     expr1; // expr replaced by expr2, not the entire statement
     s2;    // deleted
     s3;    // deleted
     // ...

     This case changes the diff on (expr1, expr2) to be (expr1;, expr2;), so it can be coalesced
     with later deleted statements like above.
  *)
  | ( loc1,
      Replace
        ( old_node,
          Expression
            ( (new_loc, _),
              StatementParentOfExpression
                (_, (Flow_ast.Statement.Expression _ as expression_statement)) ) ) )
    :: (loc2, Delete _) :: tl
    when Loc.contains new_loc (Loc.btwn loc1 loc2) ->
    edits_of_changes
      ~opts
      (( new_loc,
         Replace (old_node, Statement ((new_loc, expression_statement), TopLevelParentOfStatement))
       )
       :: tl)
  | hd :: tl -> edit_of_change ~opts hd :: edits_of_changes ~opts tl
