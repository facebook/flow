(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Flow_ast_differ
open Utils_js

open OUnit2

class useless_mapper = object
  inherit Flow_ast_mapper.mapper as super

  method! literal _loc (expr: Ast.Literal.t) =
    let open Ast.Literal in
    match expr.value with
    | Number 4.0 ->
      {value=Number 5.0; raw="5"}
    | _ -> expr

  method! binary loc (expr: (Loc.t, Loc.t) Ast.Expression.Binary.t) =
    let open Ast.Expression.Binary in
    let expr = super#binary loc expr in
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

  method! variable_declaration loc (decl: (Loc.t, Loc.t) Ast.Statement.VariableDeclaration.t) =
    let open Ast.Statement.VariableDeclaration in
    let decl = super#variable_declaration loc decl in
    let { declarations; kind } = decl in
    if kind = Var then { declarations; kind = Const }
    else decl
end

class insert_end_mapper = object
  inherit Flow_ast_mapper.mapper
  method! statement_list stmts =
    let stmt = List.nth stmts (List.length stmts - 1) in
    stmts @ [stmt]
end

class insert_begin_mapper = object
  inherit Flow_ast_mapper.mapper
  method! statement_list stmts =
    let stmt = List.nth stmts (List.length stmts - 1) in
    stmt :: stmts
end

class insert_dup_mapper = object
  inherit Flow_ast_mapper.mapper
  method! statement_list stmts =
    let rec dup = function
    | [] -> []
    | h :: t -> h :: h :: (dup t) in
    dup stmts
end

class delete_mapper = object
  inherit Flow_ast_mapper.mapper
  method! statement_list = List.tl
end

let edits_of_source algo source mapper =
  let ast, _ = Parser_flow.program source in
  let new_ast = mapper#program ast in
  let edits =
    program algo ast new_ast
    |> Ast_diff_printer.edits_of_changes
  in
  (* Extract columns from the locs *)
  List.map (fun (loc, text) -> Loc.((loc.start.column, loc._end.column), text)) edits

let debug_string_of_edit ((start, end_), text) =
  Printf.sprintf "((%d, %d), %s)" start end_ text

let debug_string_of_edits =
  List.map debug_string_of_edit
  %> String.concat ", "

let debug_print_string_script script =
  let print_string_result (i, chg) =
    match chg with
    | Replace (ol, ne) -> print_endline (Utils_js.spf "Replace %s with %s at %d" ol ne i)
    | Insert ins -> print_endline (Utils_js.spf "Insert %s at %d" (String.concat ", " ins) i)
    | Delete d -> print_endline (Utils_js.spf "Delete %s at %d" d i) in
  match script with
  | None -> print_endline "no script"
  | Some sc -> List.iter print_string_result sc


let apply_edits source edits =
  let apply_edit acc ((_begin, _end), str) =
    let before = Str.string_before acc (_begin) in
    let after = Str.string_after acc (_end) in
    before ^ str ^ after in
  List.fold_left apply_edit source (List.rev edits)

let assert_edits_equal ctxt ~edits ~source ~expected ~mapper =
  let edits_trivial = edits_of_source Trivial source mapper in
  let edits_standard = edits_of_source Standard source mapper in
  assert_equal ~ctxt edits edits_trivial;
  assert_equal ~ctxt edits edits_standard;
  assert_equal ~ctxt expected (apply_edits source edits_trivial);
  assert_equal ~ctxt expected (apply_edits source edits_standard)

let assert_edits_differ ctxt ~edits_trivial ~edits_standard ~source
  ~trivial_expected ~standard_expected ~mapper =
  let edits_trivial' = edits_of_source Trivial source mapper in
  let edits_standard' = edits_of_source Standard source mapper in
  assert_equal ~ctxt edits_trivial edits_trivial';
  assert_equal ~ctxt edits_standard edits_standard';
  assert_equal ~ctxt trivial_expected (apply_edits source edits_trivial');
  assert_equal ~ctxt standard_expected (apply_edits source edits_standard')

let tests = "ast_differ" >::: [
  "simple" >:: begin fun ctxt ->
    let source = "function foo() { (5 - 3); 4; (6 + 4); }" in
    assert_edits_equal ctxt ~edits:[((26, 27), "(5)"); ((30, 35), "(6 - 5)")]
      ~source ~expected:"function foo() { (5 - 3); (5); ((6 - 5)); }" ~mapper:(new useless_mapper)
  end;
  "class" >:: begin fun ctxt ->
    let source = "class Foo { bar() { 4; } }" in
    assert_edits_equal ctxt ~edits:[((20, 21), "(5)")] ~source
      ~expected:"class Foo { bar() { (5); } }" ~mapper:(new useless_mapper)
  end;
  "precedence" >:: begin fun ctxt ->
    let source = "5 - 3 * 3" in
    (* It is mandatory to insert the parens here *)
    assert_edits_equal ctxt ~edits:[((4, 9), "(3 + 3)")] ~source
      ~expected:"5 - (3 + 3)" ~mapper:(new useless_mapper)
  end;
  "identifier" >:: begin fun ctxt ->
    let source = "5 - rename" in
    assert_edits_equal ctxt ~edits:[((4, 10), "gotRenamed")] ~source
      ~expected:"5 - gotRenamed" ~mapper:(new useless_mapper)
  end;
  "new" >:: begin fun ctxt ->
    let source = "new rename()" in
    assert_edits_equal ctxt ~edits:[((4, 10), "gotRenamed")] ~source ~expected:"new gotRenamed()"
      ~mapper:(new useless_mapper)
  end;
  "block" >:: begin fun ctxt ->
    let source = "{ 2; 4; 10; rename; }" in
    assert_edits_equal ctxt ~edits:[((5, 6), "(5)"); ((12, 18), "gotRenamed")] ~source
      ~expected:"{ 2; (5); 10; gotRenamed; }" ~mapper:(new useless_mapper)
  end;
  "if_nochange" >:: begin fun ctxt ->
    let source = "if (true) { false; } else { true; }" in
    assert_edits_equal ctxt ~edits:[] ~source
      ~expected:"if (true) { false; } else { true; }" ~mapper:(new useless_mapper)
  end;
  "if_noblock" >:: begin fun ctxt ->
    let source = "if (4) rename;" in
    assert_edits_equal ctxt ~edits:[((4, 5), "(5)"); ((7, 13), "gotRenamed");] ~source
      ~expected:"if ((5)) gotRenamed;" ~mapper:(new useless_mapper)
  end;
  "if_partial" >:: begin fun ctxt ->
    let source = "if (4) { rename; }" in
    assert_edits_equal ctxt ~edits:[((4, 5), "(5)"); ((9, 15), "gotRenamed");] ~source
      ~expected:"if ((5)) { gotRenamed; }" ~mapper:(new useless_mapper)
  end;
  "if_full" >:: begin fun ctxt ->
    let source = "if (4) { 4; } else { rename }" in
    assert_edits_equal ctxt ~edits:[((4, 5), "(5)"); ((9, 10), "(5)"); ((21, 27), "gotRenamed")]
      ~source ~expected:"if ((5)) { (5); } else { gotRenamed }" ~mapper:(new useless_mapper)
  end;
  "with_nochange" >:: begin fun ctxt ->
    let source = "with (object) { foo = true; }" in
    assert_edits_equal ctxt ~edits:[] ~source
      ~expected:"with (object) { foo = true; }" ~mapper:(new useless_mapper)
  end;
  "with_object" >:: begin fun ctxt ->
    let source = "with (rename) { foo = true; };" in
    assert_edits_equal ctxt ~edits:[(6, 12), "gotRenamed"] ~source
      ~expected:"with (gotRenamed) { foo = true; };" ~mapper:(new useless_mapper)
  end;
  "with_body" >:: begin fun ctxt ->
    let source = "with (objct) { rename; };" in
    assert_edits_equal ctxt ~edits:[(15, 21), "gotRenamed"] ~source
      ~expected:"with (objct) { gotRenamed; };" ~mapper:(new useless_mapper)
  end;
  "function_expression" >:: begin fun ctxt ->
    let source = "(function() { 4; })" in
    assert_edits_equal ctxt ~edits:[((14, 15), "(5)")] ~source ~expected:"(function() { (5); })"
    ~mapper:(new useless_mapper)
  end;
  "call" >:: begin fun ctxt ->
    let source = "rename()" in
    assert_edits_equal ctxt ~edits:[((0, 6), "gotRenamed")] ~source ~expected:"gotRenamed()"
    ~mapper:(new useless_mapper)
  end;
  "variable_declaration_kind" >:: begin fun ctxt ->
    let source = "var x = 5;" in
    assert_edits_equal ctxt ~edits:[((0, 10), "const x = 5;")] ~source ~expected:"const x = 5;"
    ~mapper:(new useless_mapper)
  end;
  "variable_declaration_expression" >:: begin fun ctxt ->
    let source = "let x = 4;" in
    assert_edits_equal ctxt ~edits:[((8, 9), "(5)")] ~source ~expected:"let x = (5);"
    ~mapper:(new useless_mapper)
  end;
  "variable_declaration_kind_expression" >:: begin fun ctxt ->
    let source = "var x = 4;" in
    assert_edits_equal ctxt ~edits:[((0, 10), "const x = 5;")] ~source ~expected:"const x = 5;"
    ~mapper:(new useless_mapper)
  end;
  "for" >:: begin fun ctxt ->
    let source = "for (i = 7; i < rename; i++) {}" in
    assert_edits_equal ctxt  ~edits:[(16, 22) , "gotRenamed"] ~source
      ~expected:"for (i = 7; i < gotRenamed; i++) {}" ~mapper:(new useless_mapper)
  end;
  "for_init" >:: begin fun ctxt ->
    let source = "for (let i = 4; i < 10; i++) {}" in
    assert_edits_equal ctxt  ~edits:[(13, 14), "(5)"] ~source
      ~expected:"for (let i = (5); i < 10; i++) {}" ~mapper:(new useless_mapper)
  end;
  "for_body" >:: begin fun ctxt ->
    let source = "for (i = 7; i < top; i++) { rename; }" in
    assert_edits_equal ctxt  ~edits:[(28, 34), "gotRenamed"] ~source
      ~expected:"for (i = 7; i < top; i++) { gotRenamed; }" ~mapper:(new useless_mapper)
  end;
  "for_in_left" >:: begin fun ctxt ->
    let source = "for (var x in xs) { continue; }" in
    assert_edits_equal ctxt ~edits:[(0, 31), "for (const x in xs) {\n  continue;\n}"]
      ~source ~expected:"for (const x in xs) {\n  continue;\n}"
      ~mapper:(new useless_mapper)
  end;
  "for_in_right" >:: begin fun ctxt ->
    let source = "for (let x in rename) { continue; }" in
    assert_edits_equal ctxt ~edits:[(14, 20), "gotRenamed"] ~source
      ~expected:"for (let x in gotRenamed) { continue; }" ~mapper:(new useless_mapper)
  end;
  "for_in_body" >:: begin fun ctxt ->
    let source = "for (let x in xs) { rename; }" in
    assert_edits_equal ctxt ~edits:[(20, 26), "gotRenamed"] ~source
      ~expected:"for (let x in xs) { gotRenamed; }" ~mapper:(new useless_mapper)
  end;
  "while_test" >:: begin fun ctxt ->
    let source = "while (rename) { break; };" in
    assert_edits_equal ctxt ~edits:[(7, 13), "gotRenamed"] ~source
      ~expected:"while (gotRenamed) { break; };" ~mapper:(new useless_mapper)
  end;
  "while_body" >:: begin fun ctxt ->
    let source = "while (true) { rename; };" in
    assert_edits_equal ctxt ~edits:[(15, 21), "gotRenamed"] ~source
      ~expected:"while (true) { gotRenamed; };" ~mapper:(new useless_mapper)
  end;
  "for_of_left" >:: begin fun ctxt ->
    let source = "for (var x of xs) { continue; }" in
    assert_edits_equal ctxt ~edits:[(0, 31), "for (const x of xs) {\n  continue;\n}"]
      ~source ~expected:"for (const x of xs) {\n  continue;\n}" ~mapper:(new useless_mapper)
  end;
  "for_of_right" >:: begin fun ctxt ->
    let source = "for (let x of rename) { continue; }" in
    assert_edits_equal ctxt ~edits:[(14, 20), "gotRenamed"] ~source
      ~expected:"for (let x of gotRenamed) { continue; }" ~mapper:(new useless_mapper)
  end;
  "for_of_body" >:: begin fun ctxt ->
    let source = "for (let x of xs) { rename; }" in
    assert_edits_equal ctxt ~edits:[(20, 26), "gotRenamed"] ~source
      ~expected:"for (let x of xs) { gotRenamed; }" ~mapper:(new useless_mapper)
  end;
  "do_while_body" >:: begin fun ctxt ->
    let source = "do { rename; } while (true);" in
    assert_edits_equal ctxt ~edits:[(5, 11), "gotRenamed"] ~source
      ~expected:"do { gotRenamed; } while (true);" ~mapper:(new useless_mapper)
  end;
  "do_while_condition" >:: begin fun ctxt ->
    let source = "do { continue; } while (rename);" in
    assert_edits_equal ctxt ~edits:[(24, 30), "gotRenamed"] ~source
      ~expected:"do { continue; } while (gotRenamed);" ~mapper:(new useless_mapper)
  end;
  "switch_discriminant" >:: begin fun ctxt ->
    let source = "switch (rename) { case true: break; }" in
    assert_edits_equal ctxt ~edits:[(8, 14), "gotRenamed"] ~source
      ~expected:"switch (gotRenamed) { case true: break; }" ~mapper:(new useless_mapper)
  end;
  "switch_case_test" >:: begin fun ctxt ->
    let source = "switch (true) { case rename: break; }" in
    assert_edits_equal ctxt ~edits:[(21, 27), "gotRenamed"] ~source
      ~expected:"switch (true) { case gotRenamed: break; }" ~mapper:(new useless_mapper)
  end;
  "switch_case_consequent" >:: begin fun ctxt ->
    let source = "switch (true) { case true: rename; }" in
    assert_edits_equal ctxt ~edits:[(27, 33), "gotRenamed"] ~source
      ~expected:"switch (true) { case true: gotRenamed; }" ~mapper:(new useless_mapper)
  end;
  "algo_diff_end_insert" >:: begin fun ctxt ->
    let source = "var x = 5; var y = 6;" in
    assert_edits_differ ctxt ~edits_trivial:[(0, 21), "var x = 5;\nvar y = 6;\nvar y = 6;"]
      ~edits_standard:[(21, 21), "var y = 6;"] ~source
      ~trivial_expected:"var x = 5;\nvar y = 6;\nvar y = 6;"
      ~standard_expected:"var x = 5; var y = 6;var y = 6;" ~mapper:(new insert_end_mapper)
  end;
  "algo_diff_delete" >:: begin fun ctxt ->
    let source = "var x = 5; var y = 6; var z = 7;" in
    assert_edits_differ ctxt ~edits_trivial:[(0, 32), "var y = 6;\nvar z = 7;"]
      ~edits_standard:[(0, 10), ""] ~source
      ~trivial_expected:"var y = 6;\nvar z = 7;"
      ~standard_expected:" var y = 6; var z = 7;" ~mapper:(new delete_mapper)
  end;
  "algo_diff_begin_insert" >:: begin fun ctxt ->
    let source = "var x = 5; var y = 6;" in
    assert_edits_differ ctxt ~edits_trivial:[(0, 21), "var y = 6;\nvar x = 5;\nvar y = 6;"]
      ~edits_standard:[(0, 0), "var y = 6;"] ~source
      ~trivial_expected:"var y = 6;\nvar x = 5;\nvar y = 6;"
      ~standard_expected:"var y = 6;var x = 5; var y = 6;" ~mapper:(new insert_begin_mapper)
  end;
  "algo_diff_middle_insert" >:: begin fun ctxt ->
    let source = "var x = 5; var y = 6;" in
    assert_edits_differ ctxt
      ~edits_trivial:[(0, 21), "var x = 5;\nvar x = 5;\nvar y = 6;\nvar y = 6;"]
      ~edits_standard:[((10, 10), "var x = 5;"); ((21, 21), "var y = 6;");] ~source
      ~trivial_expected:"var x = 5;\nvar x = 5;\nvar y = 6;\nvar y = 6;"
      ~standard_expected:"var x = 5;var x = 5; var y = 6;var y = 6;" ~mapper:(new insert_dup_mapper)
  end;
  "algo_diff_empty" >:: begin fun ctxt ->
    let source = "" in
    let ast_empty, _ = Parser_flow.program source in
    let ast_var, _ = Parser_flow.program "var x = 6;" in
    let edits_trivial = program Trivial ast_empty ast_var |> Ast_diff_printer.edits_of_changes |>
    List.map (fun (loc, text) -> Loc.((loc.start.column, loc._end.column), text)) in
    let edits_standard = program Trivial ast_empty ast_var |> Ast_diff_printer.edits_of_changes |>
    List.map (fun (loc, text) -> Loc.((loc.start.column, loc._end.column), text)) in
    assert_equal ~ctxt edits_trivial [(0, 0), "var x = 6;"];
    assert_equal ~ctxt edits_standard [((0, 0), "var x = 6;");];
    assert_equal ~ctxt (apply_edits source edits_trivial) "var x = 6;";
    assert_equal ~ctxt (apply_edits source edits_standard) "var x = 6;"
  end;
  "unnamed_class_expression" >:: begin fun ctxt ->
    let source = "(class { method() { rename; } })" in
    assert_edits_equal ctxt ~edits:[(20, 26), "gotRenamed"] ~source
       ~expected:"(class { method() { gotRenamed; } })" ~mapper:(new useless_mapper)
  end;
  "named_class_expression" >:: begin fun ctxt ->
    let source = "(class Foo { method() { rename; } })" in
    assert_edits_equal ctxt ~edits:[(24, 30), "gotRenamed"] ~source
      ~expected:"(class Foo { method() { gotRenamed; } })" ~mapper:(new useless_mapper)
  end;
  "return_statement_with_expression" >:: begin fun ctxt ->
    let source = "function foo() { return rename; }" in
    assert_edits_equal ctxt ~edits:[(24, 30), "gotRenamed"] ~source
      ~expected:"function foo() { return gotRenamed; }" ~mapper:(new useless_mapper)
  end;
  "list_diff_simple" >:: begin fun ctxt ->
    let a = "a" in
    let b = "b" in
    let old_list = [a] in
    let new_list = [b] in
    let edits = [(0, Replace (a, b))] in
    let script = list_diff Standard old_list new_list in
    assert_equal ~ctxt (Some edits) script
  end;
  "list_diff_simple2" >:: begin fun ctxt ->
    let a = "a" in
    let b = "b" in
    let old_list = [a;a] in
    let new_list = [b;b] in
    let edits = [(0, Replace (a, b));(1, Replace (a, b))] in
    let script = list_diff Standard old_list new_list in
    assert_equal ~ctxt (Some edits) script
  end;
  "list_diff_simple3" >:: begin fun ctxt ->
    let a = "a" in
    let b = "b" in
    let old_list = [a;a] in
    let new_list = [b;b;b;b] in
    let edits = [(0, Replace (a, b));(1, Replace (a, b));
                 (1, Insert [b;b])] in
    let script = list_diff Standard old_list new_list in
    assert_equal ~ctxt (Some edits) script
  end;
  "list_diff_simple4" >:: begin fun ctxt ->
    let a = "a" in
    let b = "b" in
    let old_list = [a;a;a;a] in
    let new_list = [b;b] in
    let edits = [(0, Replace (a, b));(1, Replace (a,b));
                 (2, Delete a);(3, Delete a)] in
    let script = list_diff Standard old_list new_list in
    assert_equal ~ctxt (Some edits) script
  end;
  "list_diff_paper" >:: begin fun ctxt ->
    let a = "a" in
    let b = "b" in
    let c = "c" in
    let old_list = [a;b;c;a;b;b;a] in
    let new_list = [c;b;a;b;a;c] in
    let edits = [(0, Delete a); (1, Delete b); (3, Delete a); (4, Insert [a]); (6, Insert [c])] in
    let script = list_diff Standard old_list new_list in
    assert_equal ~ctxt (Some edits) script
  end;
  "list_diff_flip" >:: begin fun ctxt ->
    let x = "x" in
    let y = "y" in
    let old_list = [x;x;x;y;y;y] in
    let new_list = [y;y;y;x;x;x] in
    let edits = [(0, Delete x); (1, Delete x); (2, Delete x); (5, Insert [x;x;x])] in
    let script = list_diff Standard old_list new_list in
    assert_equal ~ctxt (Some edits) script
  end;
  "list_diff_sentence" >:: begin fun ctxt ->
    let (t', h, i, s, space, e, n, t, c, o, pd, d) =
      "T", "h", "i", "s", " ", "e", "n", "t", "c", "o", ".", "d" in
    (*"This is sentence one."*)
    let old_list = [t';h;i;s;space;i;s;space;s;e;n;t;e;n;c;e;space;o;n;e;pd] in
    (*"This is the second sentence"*)
    let new_list = [t';h;i;s;space;i;s;space;t;h;e;space;s;e;c;o;n;d;space;s;e;n;t;e;n;c;e;pd] in
    let edits = [(7, Insert [t;h;e;space]); (9, Insert [c;o]); (11, Replace (t,d));
                 (11, Insert [space;s]); (14, Replace (c,t)); (16, Delete space);
                 (17, Delete o); (18, Insert [c])] in
    let script = list_diff Standard old_list new_list in
    debug_print_string_script script;
    assert_equal ~ctxt (Some edits) script
  end;
  "list_diff_simple5" >:: begin fun ctxt ->
    let a = "a" in
    let b = "b" in
    let old_list = [a;b] in
    let new_list = [b] in
    let edits = [(0, Delete a)] in
    let script = list_diff Standard old_list new_list in
    assert_equal ~ctxt (Some edits) script
  end;
  "pattern_identifier" >:: begin fun ctxt ->
    let source = "let rename = 0" in
    assert_edits_equal ctxt ~edits:[(4,10), "gotRenamed"] ~source
      ~expected:"let gotRenamed = 0" ~mapper:(new useless_mapper)
  end;
  "pattern_array" >:: begin fun ctxt ->
    let source = "let [rename,rename] = [0]" in
    assert_edits_equal ctxt ~edits:[(5,11), "gotRenamed"; (12,18), "gotRenamed"] ~source
      ~expected:"let [gotRenamed,gotRenamed] = [0]" ~mapper:(new useless_mapper)
  end;
  "pattern_array_nested" >:: begin fun ctxt ->
    let source = "let [[[rename]]] = 0" in
    assert_edits_equal ctxt ~edits:[(7,13), "gotRenamed"] ~source
      ~expected:"let [[[gotRenamed]]] = 0" ~mapper:(new useless_mapper)
  end;
  "pattern_array_rest" >:: begin fun ctxt ->
    let source = "let [a,b,...rename] = 0" in
    assert_edits_equal ctxt ~edits:[(12,18), "gotRenamed"] ~source
        ~expected:"let [a,b,...gotRenamed] = 0" ~mapper:(new useless_mapper)
  end;
  "pattern_object_longhand" >:: begin fun ctxt ->
    let source = "let {rename: rename} = 0" in
    assert_edits_equal ctxt ~edits:[(5,11), "gotRenamed"; (13,19), "gotRenamed"] ~source
      ~expected:"let {gotRenamed: gotRenamed} = 0" ~mapper:(new useless_mapper)
  end;
  "pattern_object_rest" >:: begin fun ctxt ->
    let source = "let {a,b,...rename} = 0" in
    assert_edits_equal ctxt ~edits:[(12,18), "gotRenamed"] ~source
      ~expected:"let {a,b,...gotRenamed} = 0" ~mapper:(new useless_mapper)
  end;
  "pattern_assignment" >:: begin fun ctxt ->
    let source = "let [a=rename] = 0" in
    assert_edits_equal ctxt ~edits:[(7,13), "gotRenamed"] ~source
      ~expected:"let [a=gotRenamed] = 0" ~mapper:(new useless_mapper)
  end;
]
