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

  method! binary (expr: (Loc.t, Loc.t) Ast.Expression.Binary.t) =
    let open Ast.Expression.Binary in
    let expr = super#binary expr in
    let { operator; _ } = expr in
    match operator with
    | Plus ->
      { expr with operator=Minus }
    | Mult ->
      { expr with operator=Plus }
    | _ -> expr

  method! identifier id =
    let (loc, name) = id in
    if name = "rename" then
      (loc, "gotRenamed")
    else
      id

  method! variable_declaration (decl: (Loc.t, Loc.t) Ast.Statement.VariableDeclaration.t) =
    let open Ast.Statement.VariableDeclaration in
    let decl = super#variable_declaration decl in
    let { declarations; kind } = decl in
    if kind = Var then { declarations; kind = Const }
    else decl
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
    let source = "function foo() { (5 - 3); 4; (6 + 4); }" in
    let edits = edits_of_source source in
    assert_equal ~ctxt [((26, 27), "(5)"); ((30, 35), "(6 - 5)")] edits
  end;
  "class" >:: begin fun ctxt ->
    let source = "class Foo { bar() { 4; } }" in
    let edits = edits_of_source source in
    assert_equal ~ctxt [((20, 21), "(5)")] edits
  end;
  "precedence" >:: begin fun ctxt ->
    let source = "5 - 3 * 3" in
    let edits = edits_of_source source in
    (* It is mandatory to insert the parens here *)
    assert_equal ~ctxt [((4, 9), "(3 + 3)")] edits
  end;
  "identifier" >:: begin fun ctxt ->
    let source = "5 - rename" in
    let edits = edits_of_source source in
    assert_equal ~ctxt [((4, 10), "gotRenamed")] edits
  end;
  "new" >:: begin fun ctxt ->
    let source = "new rename()" in
    let edits = edits_of_source source in
    assert_equal ~ctxt [((4, 10), "gotRenamed")] edits
  end;
  "block" >:: begin fun ctxt ->
    let source = "{ 2; 4; 10; rename; }" in
    let edits = edits_of_source source in
    assert_equal ~ctxt [((5, 6), "(5)"); ((12, 18), "gotRenamed")] edits
  end;
  "if_nochange" >:: begin fun ctxt ->
    let source = "if (true) { false; } else { true; }" in
    let edits = edits_of_source source in
    assert_equal ~ctxt [] edits
  end;
  "if_noblock" >:: begin fun ctxt ->
    let source = "if (4) rename;" in
    let edits = edits_of_source source in
    assert_equal ~ctxt [((4, 5), "(5)"); ((7, 13), "gotRenamed");] edits
  end;
  "if_partial" >:: begin fun ctxt ->
    let source = "if (4) { rename; }" in
    let edits = edits_of_source source in
    assert_equal ~ctxt [((4, 5), "(5)"); ((9, 15), "gotRenamed");] edits
  end;
  "if_full" >:: begin fun ctxt ->
    let source = "if (4) { 4; } else { rename }" in
    let edits = edits_of_source source in
    assert_equal ~ctxt [((4, 5), "(5)"); ((9, 10), "(5)"); ((21, 27), "gotRenamed")] edits
  end;
  "function_expression" >:: begin fun ctxt ->
    let source = "(function() { 4; })" in
    let edits = edits_of_source source in
    assert_equal ~ctxt [((14, 15), "(5)")] edits
  end;
  "call" >:: begin fun ctxt ->
    let source = "rename()" in
    let edits = edits_of_source source in
    assert_equal ~ctxt [((0, 6), "gotRenamed")] edits
  end;
  "variable_declaration_kind" >:: begin fun ctxt ->
    let source = "var x = 5;" in
    let edits = edits_of_source source in
    assert_equal ~ctxt [((0, 10), "const x = 5;")] edits
  end;
  "variable_declaration_expression" >:: begin fun ctxt ->
    let source = "let x = 4;" in
    let edits = edits_of_source source in
    assert_equal ~ctxt [((8, 9), "(5)")] edits
  end;
  "variable_declaration_kind_expression" >:: begin fun ctxt ->
    let source = "var x = 4;" in
    let edits = edits_of_source source in
    assert_equal ~ctxt [((0, 10), "const x = 5;")] edits
  end;
  "for" >:: begin fun ctxt ->
    let source = "for (i = 7; i < rename; i++) {}" in
    let edits = edits_of_source source in
    assert_equal ~ctxt [(16, 22) , "gotRenamed"] edits
  end;
  "for_init" >:: begin fun ctxt ->
    let source = "for (let i = 4; i < 10; i++) {}" in
    let edits = edits_of_source source in
    assert_equal ~msg:(debug_string_of_edits edits) ~ctxt [(13, 14), "(5)"] edits
  end;
  "for_body" >:: begin fun ctxt ->
    let source = "for (i = 7; i < top; i++) { rename; }" in
    let edits = edits_of_source source in
    assert_equal ~ctxt [(28, 34), "gotRenamed"] edits
  end;
]
