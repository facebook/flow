(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

class useless_mapper = object
  inherit Flow_ast_mapper.mapper as super

  method! literal (expr: Ast.Literal.t) =
    let open Ast.Literal in
    match expr.value with
    | Number 4.0 ->
      {value=Number 5.0; raw="5"}
    | _ -> expr

  method! binary (expr: Loc.t Ast.Expression.Binary.t) =
    let open Ast.Expression.Binary in
    let expr = super#binary expr in
    let { operator; _ } = expr in
    match operator with
    | Plus ->
      { expr with operator=Minus }
    | _ -> expr
end

let edits_of_source source =
  let ast, _ = Parser_flow.program source in
  let new_ast = (new useless_mapper)#program ast in
  let edits =
    Flow_ast_differ.program ast new_ast
    |> Ast_diff_printer.edits_of_changes
  in
  (* Extract columns from the locs *)
  List.map (fun (loc, text) -> Loc.((loc.start.column, loc._end.column), text)) edits

let debug_string_of_edit ((start, end_), text) =
  Printf.sprintf "((%d, %d), %s)" start end_ text

let debug_string_of_edits edits =
  edits
  |> List.map debug_string_of_edit
  |> String.concat ", "

let tests = "ast_differ" >::: [
  "simple" >:: begin fun ctxt ->
    let source = "function foo() { (5 * 3); 4; (6 + 4); }" in
    let edits = edits_of_source source in
    assert_equal ~ctxt [((26, 28), "5;"); ((29, 37), "6 - 5;")] edits
  end;
  "class" >:: begin fun ctxt ->
    let source = "class Foo { bar() { 4; } }" in
    let edits = edits_of_source source in
    assert_equal ~ctxt [((20, 22), "5;")] edits
  end;
]
