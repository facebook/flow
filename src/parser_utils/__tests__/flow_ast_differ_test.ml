(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module Type = Ast.Type
open Flow_ast_differ
open Utils_js

open OUnit2

let parse_options = Some Parser_env.({
  esproposal_class_instance_fields = true;
  esproposal_class_static_fields = true;
  esproposal_decorators = true;
  esproposal_export_star_as = true;
  esproposal_optional_chaining = true;
  esproposal_nullish_coalescing = true;
  types = true;
  use_strict = false;
})

class useless_mapper = object(this)
  inherit [Loc.t] Flow_ast_mapper.mapper as super

  method! literal _loc (expr: Ast.Literal.t) =
    let open Ast.Literal in
    match expr.value with
    | Number 4.0 ->
      {value=Number 5.0; raw="5"}
    | _ -> expr

  method! logical loc (expr: (Loc.t, Loc.t) Ast.Expression.Logical.t) =
    let open Ast.Expression.Logical in
    let expr = super#logical loc expr in
    let { operator; _ } = expr in
    match operator with
    | NullishCoalesce ->
      { expr with operator=Or }
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

  method! unary_expression loc (expr: (Loc.t, Loc.t) Ast.Expression.Unary.t) =
    let open Ast.Expression.Unary in
    let expr = super#unary_expression loc expr in
    let { operator; _ } = expr in
    match operator with
    | Minus -> expr
    | _ ->
      { expr with operator=Minus }

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

  method! type_ (annot: (Loc.t, Loc.t) Type.t) =
    let annot = super#type_ annot in
    let (loc, typ) = annot in
    match typ with
    | Type.Number -> (loc, Type.String)
    | _ -> annot

  method! jsx_element _loc (elem: (Loc.t, Loc.t) Ast.JSX.element) =
    let open Ast.JSX in
    let { openingElement = (_, open_elem) as openingElement;
        closingElement; children } = elem in
    let openingElement' = this#jsx_opening_element openingElement in
    let closingElement' =
      let (loc, open_elem') = openingElement' in
      if open_elem'.Opening.selfClosing then None
      (* if selfClosing changed from true to false, construct a closing element *)
      else if open_elem.Opening.selfClosing then
        Some (loc, {Closing.name = open_elem'.Opening.name})
      else Flow_ast_mapper.map_opt super#jsx_closing_element closingElement in
    let children' = ListUtils.ident_map super#jsx_child children in
    if openingElement == openingElement' && closingElement == closingElement' &&
        children == children' then elem
    else
      { openingElement = openingElement'; closingElement = closingElement'; children = children' }

  method! jsx_opening_element (elem: (Loc.t, Loc.t) Ast.JSX.Opening.t) =
    let open Ast.JSX.Opening in
    let loc, { name; selfClosing; attributes } = elem in
    let name' = this#jsx_name name in
    let selfClosing' =
      match name' with
      | Ast.JSX.Identifier (_, {Ast.JSX.Identifier.name = id_name }) ->
        if id_name = "selfClosing" then true
        else if id_name = "notSelfClosing" then false
        else selfClosing
      | _ -> selfClosing in
    let attributes' = ListUtils.ident_map super#jsx_opening_attribute attributes in
    if name == name' && selfClosing == selfClosing' && attributes == attributes' then elem
    else (loc, { name = name'; selfClosing = selfClosing'; attributes = attributes'})

  method! jsx_identifier (id: Loc.t Ast.JSX.Identifier.t) =
    let open Ast.JSX.Identifier in
    let (loc, {name}) = id in
    match name with
      | "rename" -> (loc, {name = "gotRenamed"})
      | "Rename" -> (loc, {name = "GotRenamed"})
      | "RENAME" -> (loc, {name = "GOT_RENAMED"})
      | _ -> id

  method! jsx_attribute (attr: (Loc.t, Loc.t) Ast.JSX.Attribute.t) =
    let open Ast.JSX.Attribute in
    let loc, { name; value } = attr in
    let name' =
      match name with
      | Identifier id -> Identifier (this#jsx_identifier id)
      | _ -> name in
    let value' = Flow_ast_mapper.map_opt super#jsx_attribute_value value in
    if name == name' && value == value' then attr
    else (loc, { name = name'; value = value' })

end

class insert_end_mapper = object
  inherit [Loc.t] Flow_ast_mapper.mapper
  method! statement_list stmts =
    let stmt = List.nth stmts (List.length stmts - 1) in
    stmts @ [stmt]
end

class insert_begin_mapper = object
  inherit [Loc.t] Flow_ast_mapper.mapper
  method! statement_list stmts =
    let stmt = List.nth stmts (List.length stmts - 1) in
    stmt :: stmts
end

class insert_dup_mapper = object
  inherit [Loc.t] Flow_ast_mapper.mapper
  method! statement_list stmts =
    let rec dup = function
    | [] -> []
    | h :: t -> h :: h :: (dup t) in
    dup stmts
end

class first_last_dup_mapper = object
  inherit [Loc.t] Flow_ast_mapper.mapper
  method! statement_list stmts =
    (List.hd stmts)::stmts@[List.hd (List.rev stmts)]
end

class insert_import_mapper = object
  inherit useless_mapper as super
  method! statement_list stmts =
    if List.length stmts > 0 then begin
      let open Ast.Statement.ImportDeclaration in
      let open Ast.StringLiteral in
      let stmts = super#statement_list stmts in
      let loc, _ = List.hd stmts in
      let imp = loc, Ast.Statement.ImportDeclaration
        { importKind = Ast.Statement.ImportDeclaration.ImportValue;
          source = (loc, { value = "baz"; raw = "\"baz\"" });
          default = None;
          specifiers = Some
            (Ast.Statement.ImportDeclaration.ImportNamedSpecifiers
              [{ kind = None;
                 local = None; remote = (loc, "baz") }])}
        in
      imp::stmts
    end else super#statement_list stmts

end


class insert_second_import_mapper = object
  inherit useless_mapper as super
  method! statement_list stmts =
    if List.length stmts > 0 then begin
      let open Ast.Statement.ImportDeclaration in
      let open Ast.StringLiteral in
      let stmts = super#statement_list stmts in
      let loc, _ = List.hd stmts in
      let imp = loc, Ast.Statement.ImportDeclaration
        { importKind = Ast.Statement.ImportDeclaration.ImportValue;
          source = (loc, { value = "baz"; raw = "\"baz\"" });
          default = None;
          specifiers = Some
            (Ast.Statement.ImportDeclaration.ImportNamedSpecifiers
              [{ kind = None;
                 local = None; remote = (loc, "baz") }])}
        in
      (List.hd stmts)::imp::(List.tl stmts)
    end else super#statement_list stmts

end

class insert_second_cjsimport_mapper = object
  inherit useless_mapper as super

  method! statement_list stmts =
    if List.length stmts > 0 then begin
      let open Ast.Statement.Expression in
      let open Ast.Expression.Call in
      let open Ast.Literal in
      let stmts = super#statement_list stmts in
      let loc, _ = List.hd stmts in
      let imp = (loc, Ast.Statement.Expression (
        { expression = (loc, Ast.Expression.Call {
            callee = (loc, Ast.Expression.Identifier (loc, "require"));
            targs = None; arguments = [
              Ast.Expression.Expression (loc, Ast.Expression.Literal
                { value = Ast.Literal.String "baz"; raw = "\"baz\""})
            ]
          }); directive = None })) in
      (List.hd stmts)::imp::(List.tl stmts)
    end else super#statement_list stmts
end

class add_body_mapper = object
  inherit useless_mapper as super

  method! statement_list stmts =
    if List.length stmts > 0 then begin
      let open Ast.Statement.Expression in
      let open Ast.Expression.Call in
      let open Ast.Literal in
      let stmts = super#statement_list stmts in
      let loc, _ = List.rev stmts |> List.hd in
      let imp = (loc, Ast.Statement.Expression (
        { expression = (loc, Ast.Expression.Call {
            callee = (loc, Ast.Expression.Identifier (loc, "foo"));
            targs = None; arguments = [
              Ast.Expression.Expression (loc, Ast.Expression.Literal
                { value = Ast.Literal.String "baz"; raw = "\"baz\""})
            ]
          }); directive = None })) in
      stmts@[imp]
    end else super#statement_list stmts
end

class delete_mapper = object
  inherit [Loc.t] Flow_ast_mapper.mapper
  method! statement_list = List.tl
end

class delete_end_mapper = object
  inherit [Loc.t] Flow_ast_mapper.mapper
  method! statement_list stmt =
    List.rev stmt |> List.tl |> List.rev
end

class delete_annot_mapper = object
  inherit [Loc.t] Flow_ast_mapper.mapper as super

  method! pattern ?kind expr =
  let open Ast.Pattern in
  let open Ast.Pattern.Identifier in
  let expr = super#pattern ?kind expr in
  let (loc, patt) = expr in
  match patt with
    | Identifier id ->
      loc, Identifier { id with annot = Type.Missing Loc.none }
    | _ -> expr

  method! type_annotation_hint return =
    match super#type_annotation_hint return with
    | Type.Available (loc, _) -> Type.Missing loc
    | Type.Missing _ -> return
end

class insert_annot_mapper = object
  inherit [Loc.t] Flow_ast_mapper.mapper as super

  method! pattern ?kind expr =
  let open Ast.Pattern in
  let open Ast.Pattern.Identifier in
  let expr = super#pattern ?kind expr in
  let (loc, patt) = expr in
  match patt with
    | Identifier id ->
      loc, Identifier { id with annot = Type.Available (loc, (loc, Type.Number)) }
    | _ -> expr

  method! type_annotation_hint return =
    match super#type_annotation_hint return with
    | Type.Available _ -> return
    | Type.Missing _loc -> Type.Available (_loc, (_loc, Type.Number))
end

class prop_annot_mapper = object
  inherit [Loc.t] Flow_ast_mapper.mapper as super

  method! class_property _loc (prop: (Loc.t, Loc.t) Ast.Class.Property.t') =
    let open Ast.Class.Property in
    let prop = super#class_property _loc prop in
    let { annot; _ } = prop in
    let annot' = match annot with
      | Type.Available _ -> annot
      | Type.Missing _ -> Type.Available (Loc.none, (Loc.none, Type.Number)) in
    { prop with annot = annot' }
end

class insert_typecast_mapper = object
  inherit [Loc.t] Flow_ast_mapper.mapper
  method! expression expression =
    let loc, _ = expression in
    loc, Ast.Expression.TypeCast
      { Ast.Expression.TypeCast.annot=(loc, (loc, Type.Any)); expression }
end

let edits_of_source algo source mapper =
  let ast, _ = Parser_flow.program source ~parse_options in
  let new_ast = mapper#program ast in
  let edits =
    program algo ast new_ast
    |> Ast_diff_printer.edits_of_changes None
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
    | Insert (_, ins) -> print_endline (Utils_js.spf "Insert %s at %d" (String.concat ", " ins) i)
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

let print_debug_info source edits_trivial edits_standard =
  print_endline (spf "Trivial edits: %s" (debug_string_of_edits edits_trivial));
  print_endline (spf "Standard edits: %s" (debug_string_of_edits edits_standard));
  print_endline (spf "Trivial applied: %s" (apply_edits source edits_trivial));
  print_endline (spf "Standard applied: %s" (apply_edits source edits_standard))

let assert_edits_equal ctxt ~edits ~source ~expected ~mapper =
  let edits_trivial = edits_of_source Trivial source mapper in
  let edits_standard = edits_of_source Standard source mapper in
  print_debug_info source edits_trivial edits_standard;
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

let assert_edits_equal_standard_only ctxt ~edits ~source ~expected ~mapper =
  let edits_standard = edits_of_source Standard source mapper in
  assert_equal ~ctxt edits edits_standard;
  assert_equal ~ctxt expected (apply_edits source edits_standard)

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
  "class2" >:: begin fun ctxt ->
    let source = "class Foo { bar = 4; }" in
    assert_edits_equal ctxt ~edits:[((18, 19), "(5)")] ~source
      ~expected:"class Foo { bar = (5); }" ~mapper:(new useless_mapper)
  end;
  "class_prop_annot" >:: begin fun ctxt ->
    let source = "class A { f = (x: string) => x; }" in
    assert_edits_equal ctxt ~edits:[(11, 11), ": number"] ~source
      ~expected:"class A { f: number = (x: string) => x; }"
      ~mapper:(new prop_annot_mapper)
  end;
  "obj_prop" >:: begin fun ctxt ->
    let source = "let x = { rename : 4 }" in
    assert_edits_equal ctxt ~edits:[((10, 16), "gotRenamed"); ((19, 20), "(5)")] ~source
      ~expected:"let x = { gotRenamed : (5) }"
      ~mapper:(new useless_mapper)
  end;
  "obj_prop2" >:: begin fun ctxt ->
    let source = "let x = { bar() { rename; } }" in
    assert_edits_equal ctxt ~edits:[(18, 24), "gotRenamed"] ~source
      ~expected:"let x = { bar() { gotRenamed; } }"
      ~mapper:(new useless_mapper)
  end;
  "obj_prop3" >:: begin fun ctxt ->
    let source = "let x = { 4 : 3 }" in
    assert_edits_equal ctxt ~edits:[(10, 15), "5: 3"] ~source
      ~expected:"let x = { 5: 3 }"
      ~mapper:(new useless_mapper)
  end;
  "obj_spread_prop" >:: begin fun ctxt ->
    let source = "let x = { ...rename, x : 4}" in
    assert_edits_equal ctxt ~edits:[((13, 19), "gotRenamed"); ((25, 26), "(5)")] ~source
      ~expected:"let x = { ...gotRenamed, x : (5)}"
      ~mapper:(new useless_mapper)
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
  "member" >:: begin fun ctxt ->
    let source = "rename.a" in
    assert_edits_equal ctxt ~edits:[((0, 6), "gotRenamed")] ~source ~expected:"gotRenamed.a"
      ~mapper:(new useless_mapper)
  end;
  "member_identifier" >:: begin fun ctxt ->
    let source = "rename.rename" in
    assert_edits_equal ctxt ~edits:[((0, 6), "gotRenamed"); ((7, 13), "gotRenamed")] ~source
      ~expected:"gotRenamed.gotRenamed"
      ~mapper:(new useless_mapper)
  end;
  "member_expression" >:: begin fun ctxt ->
    let source = "obj[4]" in
    assert_edits_equal ctxt ~edits:[((4, 5), "(5)")] ~source ~expected:"obj[(5)]"
      ~mapper:(new useless_mapper)
  end;
  "unary_same_op" >:: begin fun ctxt ->
    let source = "-rename" in
    assert_edits_equal ctxt ~edits:[((1, 7), "gotRenamed")] ~source ~expected:"-gotRenamed"
      ~mapper:(new useless_mapper)
  end;
  "unary_diff_op" >:: begin fun ctxt ->
    let source = "+rename" in
    assert_edits_equal ctxt ~edits:[((0, 7), "(-gotRenamed)")] ~source ~expected:"(-gotRenamed)"
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
  "arrow_function" >:: begin fun ctxt ->
    let source = "let bar = (x) => 4;" in
    assert_edits_equal ctxt ~edits:[(17, 18), "(5)"] ~source
    ~expected:"let bar = (x) => (5);"
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
  "try_stmt_body" >:: begin fun ctxt ->
    let source = "try { rename; } catch(e) { other; };" in
    assert_edits_equal ctxt ~edits:[(6,12), "gotRenamed"] ~source
      ~expected:"try { gotRenamed; } catch(e) { other; };" ~mapper:(new useless_mapper)
  end;
  "try_stmt_catch" >:: begin fun ctxt ->
    let source = "try { thing; } catch(rename) { other; };" in
    assert_edits_equal ctxt ~edits:[(21,27), "gotRenamed"] ~source
      ~expected:"try { thing; } catch(gotRenamed) { other; };" ~mapper:(new useless_mapper)
  end;
  "try_stmt_handler" >:: begin fun ctxt ->
    let source = "try { thing; } catch(e) { rename; };" in
    assert_edits_equal ctxt ~edits:[(26,32), "gotRenamed"] ~source
      ~expected:"try { thing; } catch(e) { gotRenamed; };" ~mapper:(new useless_mapper)
  end;
  "try_stmt_finalizer" >:: begin fun ctxt ->
    let source = "try { thing; } finally { rename; };" in
    assert_edits_equal ctxt ~edits:[(25,31), "gotRenamed"] ~source
      ~expected:"try { thing; } finally { gotRenamed; };" ~mapper:(new useless_mapper)
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
    let edits_trivial =
      program Trivial ast_empty ast_var
      |> Ast_diff_printer.edits_of_changes None
      |> List.map (fun (loc, text) -> Loc.((loc.start.column, loc._end.column), text)) in
    let edits_standard =
      program Standard ast_empty ast_var
      |> Ast_diff_printer.edits_of_changes None
      |> List.map (fun (loc, text) -> Loc.((loc.start.column, loc._end.column), text)) in
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
  "type_annotation_delete" >:: begin fun ctxt ->
    let source = "let x : number = 3;" in
    assert_edits_equal ctxt ~edits:[(6, 14),""] ~source
      ~expected:"let x  = 3;" ~mapper:(new delete_annot_mapper)
  end;
  "type_annotation_insert" >:: begin fun ctxt ->
    let source = "let x = 3;" in
    assert_edits_equal ctxt ~edits:[(5, 5), ": number"] ~source
      ~expected:"let x: number = 3;" ~mapper:(new insert_annot_mapper)
  end;
  "type_annotation_replace" >:: begin fun ctxt ->
    let source = "let x : number = 3;" in
    assert_edits_equal ctxt ~edits:[(6, 14),": string"] ~source
      ~expected:"let x : string = 3;" ~mapper:(new useless_mapper)
  end;
  "return_type_replace" >:: begin fun ctxt ->
    let source = "function foo() : number { return 1; }" in
    assert_edits_equal ctxt ~edits:[(15, 23),": string"] ~source
      ~expected:"function foo() : string { return 1; }" ~mapper:(new useless_mapper)
  end;
  "return_type_delete" >:: begin fun ctxt ->
    let source = "function foo() : number { return 1; }" in
    assert_edits_equal ctxt ~edits:[(15, 23),""] ~source
      ~expected:"function foo()  { return 1; }" ~mapper:(new delete_annot_mapper)
  end;
  "return_type_insert" >:: begin fun ctxt ->
    let source = "function foo() { return 1; }" in
    assert_edits_equal ctxt ~edits:[(14, 14),": number"] ~source
      ~expected:"function foo(): number { return 1; }" ~mapper:(new insert_annot_mapper)
  end;
  "comments" >:: begin fun ctxt ->
    let source = "function foo() { /* comment */ (5 - 3); 4; (6 + 4); /* comment */}" in
    assert_edits_equal ctxt ~edits:[((40, 41), "(5)"); ((44, 49), "(6 - 5)")] ~source
      ~expected:"function foo() { /* comment */ (5 - 3); (5); ((6 - 5)); /* comment */}"
      ~mapper:(new useless_mapper)
  end;
  "fn_default_export" >:: begin fun ctxt ->
    let source = "export default function foo() { let x = rename; }" in
    assert_edits_equal ctxt ~edits:[(40, 46), "gotRenamed"] ~source
      ~expected:"export default function foo() { let x = gotRenamed; }"
      ~mapper:(new useless_mapper)
  end;
  "fn_export_named" >:: begin fun ctxt ->
    let source = "export function foo() { let x = rename; }" in
    assert_edits_equal ctxt ~edits:[(32, 38), "gotRenamed"] ~source
      ~expected:"export function foo() { let x = gotRenamed; }"
      ~mapper:(new useless_mapper)
  end;
  "assignment_left" >:: begin fun ctxt ->
    let source = "rename = 6;" in
    assert_edits_equal ctxt ~edits:[(0, 6), "gotRenamed"] ~source
      ~expected:"gotRenamed = 6;"
      ~mapper:(new useless_mapper)
  end;
  "assignment_right" >:: begin fun ctxt ->
    let source = "x = rename;" in
    assert_edits_equal ctxt ~edits:[(4, 10), "gotRenamed"] ~source
      ~expected:"x = gotRenamed;"
      ~mapper:(new useless_mapper)
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
                 (1, Insert (None, [b;b]))] in
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
    let edits = [(0, Delete a); (1, Delete b); (3, Delete a)
      ; (4, Insert (None, [a])); (6, Insert (None, [c]))] in
    let script = list_diff Standard old_list new_list in
    assert_equal ~ctxt (Some edits) script
  end;
  "list_diff_flip" >:: begin fun ctxt ->
    let x = "x" in
    let y = "y" in
    let old_list = [x;x;x;y;y;y] in
    let new_list = [y;y;y;x;x;x] in
    let edits = [(0, Delete x); (1, Delete x); (2, Delete x); (5, Insert (None, [x;x;x]))] in
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
    let edits = [(7, Insert (None, [t;h;e;space])); (9, Insert (None, [c;o])); (11, Replace (t,d));
                 (11, Insert (None, [space;s])); (14, Replace (c,t)); (16, Delete space);
                 (17, Delete o); (18, Insert (None, [c]))] in
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
  "type_cast_expr" >:: begin fun ctxt ->
    let source = "(rename: string)" in
    assert_edits_equal ctxt ~edits:[(1,7), "gotRenamed"] ~source
      ~expected:"(gotRenamed: string)" ~mapper:(new useless_mapper)
  end;
  "type_cast_type" >:: begin fun ctxt ->
    let source = "(dontrename: number)" in
    assert_edits_equal ctxt ~edits:[(11,19), ": string"] ~source
      ~expected:"(dontrename: string)" ~mapper:(new useless_mapper)
  end;
  "type_cast_add" >:: begin fun ctxt ->
    let source = "const dontrename = call( /* preserve spaces */  )" in
    assert_edits_equal ctxt ~edits:[(19, 19), "("; (49, 49), ": any)"]
      ~source ~mapper:(new insert_typecast_mapper)
      ~expected:"const dontrename = (call( /* preserve spaces */  ): any)"
  end;
  "class_type_param_instantiation" >:: begin fun ctxt ->
    let source = "class A extends B<{}> { m(): rename {} }" in
    assert_edits_equal ctxt ~edits:[((27, 35), ": gotRenamed")] ~source
    ~expected:"class A extends B<{}> { m(): gotRenamed {} }"
    ~mapper:(new useless_mapper)
  end;
  "logical_operator_left" >:: begin fun ctxt ->
    let source = "rename && b" in
    assert_edits_equal ctxt ~edits:[((0, 6), "gotRenamed")] ~source
    ~expected:"gotRenamed && b"
    ~mapper:(new useless_mapper)
  end;
  "logical_operator_right" >:: begin fun ctxt ->
    let source = "a || rename" in
    assert_edits_equal ctxt ~edits:[((5, 11), "gotRenamed")] ~source
    ~expected:"a || gotRenamed"
    ~mapper:(new useless_mapper)
  end;
  "logical_operator_changed" >:: begin fun ctxt ->
    let source = "a ?? b" in
    assert_edits_equal ctxt ~edits:[((0, 6), "(a || b)")] ~source
    ~expected:"(a || b)"
    ~mapper:(new useless_mapper)
  end;
  "insert_import_split" >:: begin fun ctxt ->
    let source = "5 - (2 + 2)" in
    assert_edits_equal_standard_only ctxt
    ~edits:[((0, 0), "import {baz} from \"baz\";"); ((5,10), "(2 - 2)")] ~source
    ~expected:"import {baz} from \"baz\";5 - ((2 - 2))"
    ~mapper:(new insert_import_mapper)
  end;
  "insert_import_existing_split" >:: begin fun ctxt ->
    let source = "foo; 5 - (2 + 2)" in
    assert_edits_equal_standard_only ctxt
    ~edits:[((0, 0), "import {baz} from \"baz\";"); ((10, 15), "(2 - 2)")] ~source
    ~expected:"import {baz} from \"baz\";foo; 5 - ((2 - 2))"
    ~mapper:(new insert_import_mapper)
  end;
  "insert_import_second_split" >:: begin fun ctxt ->
    let source = "import bing from 'bing'; 5 - (2 + 2)" in
    assert_edits_equal_standard_only ctxt
    ~edits:[((24, 24), "import {baz} from \"baz\";"); ((30, 35), "(2 - 2)")] ~source
    ~expected:"import bing from 'bing';import {baz} from \"baz\"; 5 - ((2 - 2))"
    ~mapper:(new insert_second_import_mapper)
  end;
  "existing_cjs_import_split" >:: begin fun ctxt ->
    let source = "const x = require('bing'); 5 - (2 + 2)" in
    assert_edits_equal_standard_only ctxt
    ~edits:[((26, 26), "import {baz} from \"baz\";"); ((32, 37), "(2 - 2)")] ~source
    ~expected:"const x = require('bing');import {baz} from \"baz\"; 5 - ((2 - 2))"
    ~mapper:(new insert_second_import_mapper)
  end;
  "insert_cjs_import_split" >:: begin fun ctxt ->
    let source = "import 'bing'; 5 - (2 + 2)" in
    assert_edits_equal_standard_only ctxt
    ~edits:[((14, 14), "require(\"baz\");"); ((20, 25), "(2 - 2)")] ~source
    ~expected:"import 'bing';require(\"baz\"); 5 - ((2 - 2))"
    ~mapper:(new insert_second_cjsimport_mapper)
  end;
  "pathological_import_split" >:: begin fun ctxt ->
    let source = "import 'baz'; import 'bing'; 5 - (2 + 2);" in
    assert_edits_equal_standard_only ctxt
    ~edits:[((0,0), "5 - (2 + 2);")] ~source
    ~expected:"5 - (2 + 2);import 'baz'; import 'bing'; 5 - (2 + 2);"
    ~mapper:(new insert_begin_mapper)
  end;
  "remove_import_split" >:: begin fun ctxt ->
    let source = "import 'baz';5 - (2 + 2);" in
    assert_edits_equal_standard_only ctxt
    ~edits:[((0,13), "")] ~source
    ~expected:"5 - (2 + 2);"
    ~mapper:(new delete_mapper)
  end;
  "add_body_split" >:: begin fun ctxt ->
    let source = "import 'baz';" in
    assert_edits_equal_standard_only ctxt
    ~edits:[((13,13), "foo(\"baz\");")] ~source
    ~expected:"import 'baz';foo(\"baz\");"
    ~mapper:(new add_body_mapper)
  end;
  "add_to_body_split" >:: begin fun ctxt ->
    let source = "import 'baz'; bar(qux);" in
    assert_edits_equal_standard_only ctxt
    ~edits:[((23,23), "foo(\"baz\");")] ~source
    ~expected:"import 'baz'; bar(qux);foo(\"baz\");"
    ~mapper:(new add_body_mapper)
  end;
  "remove_body_split" >:: begin fun ctxt ->
    let source = "import 'baz';5 - (2 + 2);" in
    assert_edits_equal_standard_only ctxt
    ~edits:[((13,25), "")] ~source
    ~expected:"import 'baz';"
    ~mapper:(new delete_end_mapper)
  end;
  "jsx_element_self_closing_simple" >:: begin fun ctxt ->
    let source = "<rename />" in
    assert_edits_equal ctxt ~edits:[(1, 7), "gotRenamed"] ~source
      ~expected:"<gotRenamed />"
      ~mapper:(new useless_mapper)
  end;
  "jsx_element_self_closing_namespaced_namespace" >:: begin fun ctxt ->
    let source = "<RENAME:dontRename />" in
    assert_edits_equal ctxt ~edits:[(1, 7), "GOT_RENAMED"] ~source
    ~expected:"<GOT_RENAMED:dontRename />"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_self_closing_namespaced_name" >:: begin fun ctxt ->
    let source = "<DONT_RENAME:rename />" in
    assert_edits_equal ctxt ~edits:[(13, 19), "gotRenamed"] ~source
    ~expected:"<DONT_RENAME:gotRenamed />"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_self_closing_member_expr_object" >:: begin fun ctxt ->
    let source = "<Rename.dontRename />" in
    assert_edits_equal ctxt ~edits:[(1, 7), "GotRenamed"] ~source
    ~expected:"<GotRenamed.dontRename />"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_self_closing_member_expr_name" >:: begin fun ctxt ->
    let source = "<DontRename.rename />" in
    assert_edits_equal ctxt ~edits:[(12, 18), "gotRenamed"] ~source
    ~expected:"<DontRename.gotRenamed />"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_self_closing_member_expr_nested_object" >:: begin fun ctxt ->
    let source = "<Rename.DontRename.Rename.dontRename />" in
    assert_edits_equal ctxt ~edits:[(1, 7), "GotRenamed"; (19, 25), "GotRenamed"]
    ~source ~expected:"<GotRenamed.DontRename.GotRenamed.dontRename />"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_simple" >:: begin fun ctxt ->
    let source = "<rename></rename>" in
    assert_edits_equal ctxt ~edits:[(1, 7), "gotRenamed"; (10, 16), "gotRenamed"]
    ~source ~expected:"<gotRenamed></gotRenamed>"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_member_expr_nested" >:: begin fun ctxt ->
    let source = "<Rename.DontRename.rename></Rename.DontRename.rename>" in
    assert_edits_equal ctxt
    ~edits:[(1, 7), "GotRenamed"; (19, 25), "gotRenamed"; (28, 34), "GotRenamed";
        (46, 52), "gotRenamed"] ~source
    ~expected:"<GotRenamed.DontRename.gotRenamed></GotRenamed.DontRename.gotRenamed>"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_to_self_closing" >:: begin fun ctxt ->
    let source = "<selfClosing></selfClosing>" in
    assert_edits_equal ctxt ~edits:[(0, 27), "(<selfClosing />)"]
    ~source ~expected:"(<selfClosing />)"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_from_self_closing" >:: begin fun ctxt ->
    let source = "<notSelfClosing />" in
    assert_edits_equal ctxt ~edits:[(0, 18), "(<notSelfClosing></notSelfClosing>)"]
    ~source ~expected:"(<notSelfClosing></notSelfClosing>)"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_attribute_name" >:: begin fun ctxt ->
    let source = "<Component rename={1} />" in
    assert_edits_equal ctxt ~edits:[(11, 17), "gotRenamed"]
    ~source ~expected:"<Component gotRenamed={1} />"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_attribute_value_expression_literal" >:: begin fun ctxt ->
    let source = "<Component someProp={4} />" in
    assert_edits_equal ctxt ~edits:[(21, 22), "(5)"]
    ~source ~expected:"<Component someProp={(5)} />"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_attribute_value_expression_binop" >:: begin fun ctxt ->
    let source = "<Component someProp={4 + 4} />" in
    assert_edits_equal ctxt ~edits:[((21, 26), "(5 - 5)")]
    ~source ~expected:"<Component someProp={(5 - 5)} />"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_attribute_name_and_value" >:: begin fun ctxt ->
    let source = "<Component rename={4} />" in
    assert_edits_equal ctxt ~edits:[((11, 17), "gotRenamed"); ((19, 20), "(5)")]
    ~source ~expected:"<Component gotRenamed={(5)} />"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_attribute_list_name" >:: begin fun ctxt ->
    let source = "<Component dontRename={1} rename={2} />" in
    assert_edits_equal ctxt ~edits:[(26, 32), "gotRenamed"]
    ~source ~expected:"<Component dontRename={1} gotRenamed={2} />"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_attribute_list_expression_literal" >:: begin fun ctxt ->
    let source = "<Component someProp={4} anotherProp={4} />" in
    assert_edits_equal ctxt ~edits:[((21, 22), "(5)"); (37,38), "(5)"]
    ~source ~expected:"<Component someProp={(5)} anotherProp={(5)} />"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_spread_attribute" >:: begin fun ctxt ->
    let source = "<Component {...rename} />" in
    assert_edits_equal ctxt ~edits:[(15, 21), "gotRenamed"]
    ~source ~expected:"<Component {...gotRenamed} />"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_spread_attribute_list_mixed" >:: begin fun ctxt ->
    let source = "<Component {...rename} rename={4}/>" in
    assert_edits_equal ctxt
    ~edits:[((15, 21), "gotRenamed"); ((23, 29), "gotRenamed"); ((31, 32), "(5)")]
    ~source ~expected:"<Component {...gotRenamed} gotRenamed={(5)}/>"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_attribute_list_name_and_value" >:: begin fun ctxt ->
    let source = "<Component rename={1} dontRename={4} />" in
    assert_edits_equal ctxt ~edits:[((11, 17), "gotRenamed"); ((34, 35), "(5)")]
    ~source ~expected:"<Component gotRenamed={1} dontRename={(5)} />"
    ~mapper:(new useless_mapper)
  end;
  "call_insert" >:: begin fun ctxt ->
    let source = "callFunction(class A { f = (x: string) => x; });" in
    assert_edits_equal ctxt ~edits:[(24, 24), ": number"] ~source
      ~expected:"callFunction(class A { f: number = (x: string) => x; });"
      ~mapper:(new prop_annot_mapper)
  end;
  "new_insert" >:: begin fun ctxt ->
    let source = "new MyClass(class A { f = (x: string) => x; });" in
    assert_edits_equal ctxt ~edits:[(23, 23), ": number"] ~source
      ~expected:"new MyClass(class A { f: number = (x: string) => x; });"
      ~mapper:(new prop_annot_mapper)
  end;
  "insert_inside_array" >:: begin fun ctxt ->
    let source = "[{ render() { class A { f = (x: string) => x; } return new A() } }]" in
    assert_edits_equal ctxt ~edits:[(25, 25), ": number"] ~source
      ~expected:"[{ render() { class A { f: number = (x: string) => x; } return new A() } }]"
      ~mapper:(new prop_annot_mapper)
  end;
]
