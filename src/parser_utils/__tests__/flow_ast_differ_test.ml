(**
 * Copyright (c) Facebook, Inc. and its affiliates.
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
  enums = true;
  esproposal_class_instance_fields = true;
  esproposal_class_static_fields = true;
  esproposal_decorators = true;
  esproposal_export_star_as = true;
  esproposal_optional_chaining = true;
  esproposal_nullish_coalescing = true;
  esproposal_fsharp_pipeline_operator = true;
  types = true;
  use_strict = false;
})

class useless_mapper = object(this)
  inherit [Loc.t] Flow_ast_mapper.mapper as super

  method! literal _loc (expr: Loc.t Ast.Literal.t) =
    let open Ast.Literal in
    match expr.value with
    | Number 4.0 ->
      { value = Number 5.0; raw = "5"; comments = Flow_ast_utils.mk_comments_opt () }
    | _ -> expr

  method! string_literal_type _loc (lit: Ast.StringLiteral.t) =
    let open Ast.StringLiteral in
    let { value; _ } = lit in
    if String.equal "RenameSL" value
    then { value = "\"GotRenamedSL\""; raw = "\"GotRenamedSL\"" }
    else lit

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
    let (loc, { Ast.Identifier.name; comments= _ }) = id in
    match name with
      | "rename" -> Flow_ast_utils.ident_of_source (loc, "gotRenamed")
      | "Rename" -> Flow_ast_utils.ident_of_source (loc, "GotRenamed")
      | "RENAME" -> Flow_ast_utils.ident_of_source (loc, "GOT_RENAMED")
      | _ -> id

  method! variable_declaration loc (decl: (Loc.t, Loc.t) Ast.Statement.VariableDeclaration.t) =
    let open Ast.Statement.VariableDeclaration in
    let decl = super#variable_declaration loc decl in
    let { declarations; kind } = decl in
    if kind = Var then { declarations; kind = Const }
    else decl

  method! template_literal_element (elem: 'loc Ast.Expression.TemplateLiteral.Element.t) =
    let open Ast.Expression.TemplateLiteral.Element in
    let loc, { value; tail } = elem in
    if value.raw = "rename" then
      loc, { value = { raw = "gotRenamed"; cooked = "gotRenamed" }; tail}
    else elem

  method! type_ (annot: (Loc.t, Loc.t) Type.t) =
    let open Ast.NumberLiteral in
    let annot = super#type_ annot in
    let (loc, typ) = annot in
    match typ with
    | Type.Number -> (loc, Type.String)
    | Type.NumberLiteral _ -> (loc, Type.NumberLiteral { value = 4.0; raw = "4.0"})
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
    let children' = this#jsx_children children in
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

  method! jsx_child (child: (Loc.t, Loc.t) Ast.JSX.child) =
    let open Ast.JSX in
    match child with
    | loc, Text txt ->
      let { Text.value; _ } = txt in
      if value = "rename" then
        (loc, Text { Text.value = "gotRenamed"; Text.raw = "gotRenamed"})
      else child
    | _ -> super#jsx_child child

  method! variance
      (variance: (Loc.t) Ast.Variance.t option) =
    let open Ast.Variance in
    match variance with
    | Some (loc, Minus) -> Some (loc, Plus)
    | _ -> variance

  method! type_parameter_instantiation_with_implicit (loc, targs) =
    let open Ast.Expression.TypeParameterInstantiation in
    let f targ =
      match targ with
      | Explicit targ'-> Explicit (this#type_ targ')
      | Implicit loc -> Explicit (loc, Ast.Type.Any) in
    (loc, Core_list.map ~f:f targs)

  method! function_param_type (fpt: (Loc.t, Loc.t) Ast.Type.Function.Param.t) =
    let open Ast.Type.Function.Param in
    let loc, fpt' as fpt = super#function_param_type fpt in
    let { name; _ } = fpt' in
    let name' = Flow_ast_mapper.map_opt this#identifier name in
    if name' == name then fpt
    else loc, { fpt' with name = name'}

  method! update_expression loc (expr: (Loc.t, Loc.t) Ast.Expression.Update.t) =
    let open Ast.Expression.Update in
    let expr = super#update_expression loc expr in
    let { operator; _ } = expr in
    match operator with
    | Increment -> expr
    | _ -> { expr with operator=Increment }

  method! object_property_type (opt: (Loc.t, Loc.t) Ast.Type.Object.Property.t) =
    let open Ast.Type.Object.Property in
    let loc, opt' as opt = super#object_property_type opt in
    let { key; variance; _ } = opt' in
    let key' = this#object_key key in
    let variance' = this#variance variance in
    if key' == key && variance' == variance then opt
    else loc, { opt' with key = key'; variance = variance' }

end

class literal_mapper = object
  inherit [Loc.t] Flow_ast_mapper.mapper
  method! literal _loc (expr: Loc.t Ast.Literal.t) =
    let open Ast.Literal in
    match expr.value with
    | String "rename" ->
      { value = String "gotRenamed"; raw = "gotRenamed"; comments = Flow_ast_utils.mk_comments_opt () }
    | Boolean false ->
      { value = Boolean true; raw = "true"; comments = Flow_ast_utils.mk_comments_opt () }
    | Null ->
      { value = String "wasNull"; raw = "wasNull"; comments = Flow_ast_utils.mk_comments_opt () }
    | Number 4.0 ->
      { value = Number 5.0; raw = "5"; comments = Flow_ast_utils.mk_comments_opt () }
    (* TODO: add test for RegExp case? *)
    | _ -> expr
end

class insert_variance_mapper = object(this)
  inherit useless_mapper as super

  method! type_parameter_declaration_type_param
      (type_param: (Loc.t, Loc.t) Ast.Type.ParameterDeclaration.TypeParam.t) =
    let open Ast.Type.ParameterDeclaration.TypeParam in
    let (loc, type_param') as orig = super#type_parameter_declaration_type_param type_param in
    let { variance; _ } = type_param' in
    let variance' = this#variance_ loc variance in
    if variance == variance' then orig
    else loc, { type_param' with variance = variance' }

  (* New variance method with a different type signature that allows us to insert a loc *)
  method variance_
      (loc: Loc.t) (variance: (Loc.t) Ast.Variance.t option) =
    let open Ast.Variance in
    match variance with
    | None -> Some (loc, Plus)
    | _ -> variance
end

class delete_variance_mapper = object
  inherit [Loc.t] Flow_ast_mapper.mapper
  method! variance (variance: (Loc.t) Ast.Variance.t option) =
    let open Ast.Variance in
    match variance with
    | Some (_loc, Minus) -> None
    | _ -> variance
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
                 local = None; remote = Flow_ast_utils.ident_of_source (loc, "baz") }])}
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
                 local = None; remote = Flow_ast_utils.ident_of_source (loc, "baz") }])}
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
            callee = (loc, Ast.Expression.Identifier (Flow_ast_utils.ident_of_source (loc, "require")));
            targs = None; arguments = [
              Ast.Expression.Expression (loc, Ast.Expression.Literal
                { value = Ast.Literal.String "baz"; raw = "\"baz\""; comments = Flow_ast_utils.mk_comments_opt ()})
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
            callee = (loc, Ast.Expression.Identifier (Flow_ast_utils.ident_of_source (loc, "foo")));
            targs = None; arguments = [
              Ast.Expression.Expression (loc, Ast.Expression.Literal
                { value = Ast.Literal.String "baz"; raw = "\"baz\""; comments = Flow_ast_utils.mk_comments_opt ()})
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

class insert_function_annot_mapper = object
  inherit [Loc.t] Flow_ast_mapper.mapper as super

  method! type_annotation_hint return =
    match super#type_annotation_hint return with
    | Type.Available _ -> return
    | Type.Missing loc ->
      Type.Available (loc,
        (loc, Type.Function {
          Type.Function.tparams = None;
          params =
            (loc, {
              Type.Function.Params.params= [];
              rest= None
            });
          return= loc, Type.Number
        } ))
end

class insert_import_and_annot_mapper = object
  inherit [Loc.t] Flow_ast_mapper.mapper as super

  method! type_annotation_hint return =
    match super#type_annotation_hint return with
    | Type.Available _ -> return
    | Type.Missing loc ->
      Type.Available (loc,
        (loc, Type.Function {
          Type.Function.tparams = None;
          params =
            (loc, {
              Type.Function.Params.params= [];
              rest= None
            });
          return= loc, Type.Number
        } ))

    method! program prog =
      let (loc, stmts, comments) = super#program prog in
      let import num =
        let imp = Printf.sprintf "new_import%d" num in
        Ast.Statement.(Loc.none, ImportDeclaration {
          ImportDeclaration.
          importKind = ImportDeclaration.ImportType;
          source = (Loc.none, {
            Ast.StringLiteral.value = imp;
            raw = imp;
          });
          default = None;
          specifiers = Some ImportDeclaration.(ImportNamedSpecifiers [{
            kind = None;
            local = Some (Flow_ast_utils.ident_of_source (Loc.none, "here"));
            remote = (Flow_ast_utils.ident_of_source (Loc.none, "there"));
           }])
        })
      in
      loc, (List.hd stmts) :: (import 1) :: (import 2) :: (List.tl stmts), comments
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

class insert_type_param_instantiation = object
  inherit [Loc.t] Flow_ast_mapper.mapper
  method! type_parameter_instantiation_with_implicit (loc, targs) =
    let open Ast.Expression.TypeParameterInstantiation in
    (loc, (Explicit (loc, Ast.Type.Any))::targs)
end

class add_comment_mapper = object
  inherit [Loc.t] Flow_ast_mapper.mapper
  method! identifier (loc, i) =
    let open Flow_ast.Syntax in
    loc, { i with Flow_ast.Identifier.comments =
      Some {leading= [(Loc.none, Flow_ast.Comment.Block "hello" )]; trailing= [(Loc.none, Flow_ast.Comment.Block "bye" )]; internal= () }
    }
end

class true_to_false_mapper = object
  inherit [Loc.t] Flow_ast_mapper.mapper

  method! literal _loc (expr: Loc.t Ast.Literal.t) =
    let open Ast.Literal in
    match expr.value with
    | Boolean true ->
      { value = Boolean false; raw = "false"; comments = Flow_ast_utils.mk_comments_opt () }
    | _ -> expr

  method! type_annotation (annot: (Loc.t, Loc.t) Ast.Type.annotation) =
    let open Ast.Type in
    let t1, a = annot in
    let t2, right_var = a in
    match right_var with
    | BooleanLiteral true ->
      (t1, (t2, BooleanLiteral false))
    | _ -> annot
  end

class remove_annotation_rest_mapper = object
  inherit [Loc.t] Flow_ast_mapper.mapper as super

  method! type_ (annot: (Loc.t, Loc.t) Type.t) =
    let annot = super#type_ annot in
    let (loc, typ) = annot in
    match typ with
    | Type.Intersection (t, t', _) -> (loc, Type.Intersection (t, t', []))
    | Type.Union (t, t', _) -> (loc, Type.Union (t, t', []))
    | _ -> annot
end

let edits_of_source algo source mapper =
  let ast, _ = Parser_flow.program source ~parse_options in
  let new_ast = mapper#program ast in
  let edits =
    program algo ast new_ast
    |> Ast_diff_printer.edits_of_changes None
  in
  (* Extract columns from the locs *)
  Core_list.map ~f:(fun (loc, text) -> Loc.((loc.start.column, loc._end.column), text)) edits

let debug_string_of_edit ((start, end_), text) =
  Printf.sprintf "((%d, %d), %s)" start end_ text

let debug_string_of_edits =
  Core_list.map ~f:debug_string_of_edit
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
  "literal_number" >:: begin fun ctxt ->
    let source = "4" in
    assert_edits_equal ctxt ~edits:[((0, 1), "5")]
      ~source ~expected:"5" ~mapper:(new literal_mapper)
  end;
  "literal_string" >:: begin fun ctxt ->
    let source = "\"rename\"" in
    assert_edits_equal ctxt ~edits:[((0, 8), "\"gotRenamed\"")]
      ~source ~expected:"\"gotRenamed\"" ~mapper:(new literal_mapper)
  end;
  "literal_bool" >:: begin fun ctxt ->
    let source = "false" in
    assert_edits_equal ctxt ~edits:[((0, 5), "true")]
      ~source ~expected:"true" ~mapper:(new literal_mapper)
  end;
  "literal_null" >:: begin fun ctxt ->
    let source = "null" in
    assert_edits_equal ctxt ~edits:[((0, 4), "\"wasNull\"")]
      ~source ~expected:"\"wasNull\"" ~mapper:(new literal_mapper)
  end;
  "string_literal_type" >:: begin fun ctxt ->
    let source = "(foo: \"RenameSL\")" in
    assert_edits_equal ctxt ~edits:[((6, 16), "\"GotRenamedSL\"")]
      ~source ~expected:"(foo: \"GotRenamedSL\")" ~mapper:(new useless_mapper)
  end;
  "simple" >:: begin fun ctxt ->
    let source = "function foo() { (5 - 3); 4; (6 + 4); }" in
    assert_edits_equal ctxt ~edits:[((26, 27), "5"); ((30, 35), "(6 - 5)")]
      ~source ~expected:"function foo() { (5 - 3); 5; ((6 - 5)); }" ~mapper:(new useless_mapper)
  end;
  "class" >:: begin fun ctxt ->
    let source = "class Foo { bar() { 4; } }" in
    assert_edits_equal ctxt ~edits:[((20, 21), "5")] ~source
      ~expected:"class Foo { bar() { 5; } }" ~mapper:(new useless_mapper)
  end;
  "class2" >:: begin fun ctxt ->
    let source = "class Foo { bar = 4; }" in
    assert_edits_equal ctxt ~edits:[((18, 19), "5")] ~source
      ~expected:"class Foo { bar = 5; }" ~mapper:(new useless_mapper)
  end;
  "class_prop_annot" >:: begin fun ctxt ->
    let source = "class A { f = (x: string) => x; }" in
    assert_edits_equal ctxt ~edits:[(11, 11), ": number"] ~source
      ~expected:"class A { f: number = (x: string) => x; }"
      ~mapper:(new prop_annot_mapper)
  end;
  "class_extends" >:: begin fun ctxt ->
    let source = "class A extends rename<rename, dontrename> { }" in
    assert_edits_equal ctxt ~edits:[((16, 22), "gotRenamed"); ((23, 29), "gotRenamed")] ~source
      ~expected:"class A extends gotRenamed<gotRenamed, dontrename> { }"
      ~mapper:(new useless_mapper)
  end;
  "class_extends_integration" >:: begin fun ctxt ->
    let source = "class A extends rename { bar = 4 }" in
    assert_edits_equal ctxt ~edits:[((16, 22), "gotRenamed"); ((31, 32), "5")] ~source
      ~expected:"class A extends gotRenamed { bar = 5 }"
      ~mapper:(new useless_mapper)
  end;
  "interface_id" >:: begin fun ctxt ->
    let source = "interface Rename { }" in
    assert_edits_equal ctxt ~edits:[((10, 16), "GotRenamed")] ~source
      ~expected:"interface GotRenamed { }" ~mapper:(new useless_mapper)
  end;
  "interface_tparams" >:: begin fun ctxt ->
    let source = "interface Foo<RENAME> { }" in
    assert_edits_equal ctxt ~edits:[((14, 20), "GOT_RENAMED")] ~source
      ~expected:"interface Foo<GOT_RENAMED> { }" ~mapper:(new useless_mapper)
  end;
  "interface_extends_id" >:: begin fun ctxt ->
    let source = "interface Foo extends Rename { }" in
    assert_edits_equal ctxt ~edits:[((22, 28), "GotRenamed")] ~source
      ~expected:"interface Foo extends GotRenamed { }" ~mapper:(new useless_mapper)
  end;
  "interface_extends_targ_simple" >:: begin fun ctxt ->
    let source = "interface Foo extends Bar<RENAME> { }" in
    assert_edits_equal ctxt ~edits:[((26, 32), "GOT_RENAMED")] ~source
      ~expected:"interface Foo extends Bar<GOT_RENAMED> { }"
      ~mapper:(new useless_mapper)
  end;
  "interface_extends_targs" >:: begin fun ctxt ->
    let source = "interface Foo extends Bar<RENAME, RENAME> { }" in
    assert_edits_equal ctxt
      ~edits:[((26, 32), "GOT_RENAMED"); ((34, 40), "GOT_RENAMED")] ~source
      ~expected:"interface Foo extends Bar<GOT_RENAMED, GOT_RENAMED> { }"
      ~mapper:(new useless_mapper)
  end;
  "interface_combo" >:: begin fun ctxt ->
    let source = "interface Rename extends Rename<RENAME> { }" in
    assert_edits_equal ctxt
      ~edits:[((10, 16), "GotRenamed"); ((25, 31), "GotRenamed"); ((32, 38), "GOT_RENAMED")]
      ~source
      ~expected:"interface GotRenamed extends GotRenamed<GOT_RENAMED> { }"
      ~mapper:(new useless_mapper)
  end;
  "interface_body_object_property_key" >:: begin fun ctxt ->
    let source = "interface Foo { rename: string }" in
    assert_edits_equal ctxt ~edits:[((16, 22), "gotRenamed")] ~source
      ~expected:"interface Foo { gotRenamed: string }" ~mapper:(new useless_mapper)
  end;
  "interface_body_object_property_value_init" >:: begin fun ctxt ->
    let source = "interface Foo { bar: number }" in
    assert_edits_equal ctxt ~edits:[((21, 27), "string")] ~source
      ~expected:"interface Foo { bar: string }" ~mapper:(new useless_mapper)
  end;
  "obj_prop" >:: begin fun ctxt ->
    let source = "let x = { rename : 4 }" in
    assert_edits_equal ctxt ~edits:[((10, 16), "gotRenamed"); ((19, 20), "5")] ~source
      ~expected:"let x = { gotRenamed : 5 }"
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
    assert_edits_equal ctxt ~edits:[(10, 11), "5"] ~source
      ~expected:"let x = { 5 : 3 }"
      ~mapper:(new useless_mapper)
  end;
  "obj_spread_prop" >:: begin fun ctxt ->
    let source = "let x = { ...rename, x : 4}" in
    assert_edits_equal ctxt ~edits:[((13, 19), "gotRenamed"); ((25, 26), "5")] ~source
      ~expected:"let x = { ...gotRenamed, x : 5}"
      ~mapper:(new useless_mapper)
  end;
  "precedence" >:: begin fun ctxt ->
    let source = "5 - 3 * 3" in
    (* It is mandatory to insert the parens here *)
    assert_edits_equal ctxt ~edits:[((4, 9), "(3 + 3)")] ~source
      ~expected:"5 - (3 + 3)" ~mapper:(new useless_mapper)
  end;
  "tuple" >:: begin fun ctxt ->
    let source = "type Foo = [number, number];" in
    assert_edits_equal ctxt ~edits:[((12, 18), "string"); ((20, 26), "string")] ~source
      ~expected:"type Foo = [string, string];" ~mapper:(new useless_mapper)
  end;
  "identifier" >:: begin fun ctxt ->
    let source = "5 - rename" in
    assert_edits_equal ctxt ~edits:[((4, 10), "gotRenamed")] ~source
      ~expected:"5 - gotRenamed" ~mapper:(new useless_mapper)
  end;
  "interface_type" >:: begin fun ctxt ->
      let source = "type Foo = interface { rename() : string }" in
      assert_edits_equal ctxt ~edits:[((23, 29), "gotRenamed")]
        ~source ~expected:"type Foo = interface { gotRenamed() : string }"
      ~mapper:(new useless_mapper)
  end;
  "new" >:: begin fun ctxt ->
    let source = "new rename()" in
    assert_edits_equal ctxt ~edits:[((4, 10), "gotRenamed")] ~source ~expected:"new gotRenamed()"
      ~mapper:(new useless_mapper)
  end;
  "typeof_type" >:: begin fun ctxt ->
    let source = "type Foo = typeof number" in
    assert_edits_equal ctxt ~edits:[((18, 24), "string")] ~source
      ~expected:"type Foo = typeof string" ~mapper:(new useless_mapper)
  end;
  "new_type_param" >:: begin fun ctxt ->
    let source = "new foo<RENAME>()" in
    assert_edits_equal ctxt ~edits:[((8, 14), "GOT_RENAMED")] ~source
      ~expected:"new foo<GOT_RENAMED>()"
      ~mapper:(new useless_mapper)
  end;
  "new_type_param_multiple" >:: begin fun ctxt ->
    let source = "new foo<RENAME, RENAME>()" in
    assert_edits_equal ctxt
      ~edits:[((8, 14), "GOT_RENAMED"); ((16, 22), "GOT_RENAMED")] ~source
      ~expected:"new foo<GOT_RENAMED, GOT_RENAMED>()"
      ~mapper:(new useless_mapper)
  end;
  "new_type_param_insert" >:: begin fun ctxt ->
    let source = "new foo<>()" in
    assert_edits_equal ctxt
      ~edits:[((0, 11), "(new foo<any>())")] ~source
      ~expected:"(new foo<any>())"
      ~mapper:(new insert_type_param_instantiation)
  end;
  "new_type_param_implicit" >:: begin fun ctxt ->
    let source = "new foo<_>()" in
    assert_edits_equal ctxt
      ~edits:[((0, 12), "(new foo<any>())")] ~source
      ~expected:"(new foo<any>())"
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
    assert_edits_equal ctxt ~edits:[((4, 5), "5")] ~source ~expected:"obj[5]"
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
    assert_edits_equal ctxt ~edits:[((5, 6), "5"); ((12, 18), "gotRenamed")] ~source
      ~expected:"{ 2; 5; 10; gotRenamed; }" ~mapper:(new useless_mapper)
  end;
  "if_nochange" >:: begin fun ctxt ->
    let source = "if (true) { false; } else { true; }" in
    assert_edits_equal ctxt ~edits:[] ~source
      ~expected:"if (true) { false; } else { true; }" ~mapper:(new useless_mapper)
  end;
  "if_noblock" >:: begin fun ctxt ->
    let source = "if (4) rename;" in
    assert_edits_equal ctxt ~edits:[((4, 5), "5"); ((7, 13), "gotRenamed");] ~source
      ~expected:"if (5) gotRenamed;" ~mapper:(new useless_mapper)
  end;
  "if_partial" >:: begin fun ctxt ->
    let source = "if (4) { rename; }" in
    assert_edits_equal ctxt ~edits:[((4, 5), "5"); ((9, 15), "gotRenamed");] ~source
      ~expected:"if (5) { gotRenamed; }" ~mapper:(new useless_mapper)
  end;
  "if_full" >:: begin fun ctxt ->
    let source = "if (4) { 4; } else { rename }" in
    assert_edits_equal ctxt ~edits:[((4, 5), "5"); ((9, 10), "5"); ((21, 27), "gotRenamed")]
      ~source ~expected:"if (5) { 5; } else { gotRenamed }" ~mapper:(new useless_mapper)
  end;
  "conditional_nochange" >:: begin fun ctxt ->
    let source = "1 > 0 ? false : true" in
    assert_edits_equal ctxt ~edits:[] ~source
      ~expected:"1 > 0 ? false : true" ~mapper:(new useless_mapper)
  end;
  "conditional_test" >:: begin fun ctxt ->
    let source = "rename ? false : true" in
    assert_edits_equal ctxt ~edits:[((0, 6), "gotRenamed")] ~source
      ~expected:"gotRenamed ? false : true" ~mapper:(new useless_mapper)
  end;
  "conditional_consequent" >:: begin fun ctxt ->
    let source = "1 > 0 ? rename : true" in
    assert_edits_equal ctxt ~edits:[((8, 14), "gotRenamed")] ~source
      ~expected:"1 > 0 ? gotRenamed : true" ~mapper:(new useless_mapper)
  end;
  "conditional_alternate" >:: begin fun ctxt ->
    let source = "1 > 0 ? false : rename" in
    assert_edits_equal ctxt ~edits:[((16, 22), "gotRenamed")] ~source
      ~expected:"1 > 0 ? false : gotRenamed" ~mapper:(new useless_mapper)
  end;
  "conditional_cons_and_alt" >:: begin fun ctxt ->
    let source = "1 > 0 ? 4 : rename" in
    assert_edits_equal ctxt ~edits:[((8, 9), "5"); ((12, 18), "gotRenamed")] ~source
      ~expected:"1 > 0 ? 5 : gotRenamed" ~mapper:(new useless_mapper)
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
    assert_edits_equal ctxt ~edits:[((14, 15), "5")] ~source ~expected:"(function() { 5; })"
    ~mapper:(new useless_mapper)
  end;
  "function_id" >:: begin fun ctxt ->
    let source = "(function rename() { return; })" in
    assert_edits_equal ctxt ~edits:[((10, 16), "gotRenamed")] ~source
    ~expected:"(function gotRenamed() { return; })"
    ~mapper:(new useless_mapper)
  end;
  "function_rest" >:: begin fun ctxt ->
    let source = "(function(...rename) { return; })" in
    assert_edits_equal ctxt ~edits:[(13, 19), "gotRenamed"] ~source
    ~expected:"(function(...gotRenamed) { return; })"
    ~mapper:(new useless_mapper)
  end;
  "function_param" >:: begin fun ctxt ->
    let source = "(function(rename, ...dontRename) { return; })" in
    assert_edits_equal ctxt ~edits:[(10, 16), "gotRenamed"] ~source
    ~expected:"(function(gotRenamed, ...dontRename) { return; })"
    ~mapper:(new useless_mapper)
  end;
  "function_params" >:: begin fun ctxt ->
    let source = "(function(rename, dontRename, rename) { return; })" in
    assert_edits_equal ctxt ~source
    ~edits:[((10, 16), "gotRenamed"); ((30, 36), "gotRenamed")]
    ~expected:"(function(gotRenamed, dontRename, gotRenamed) { return; })"
    ~mapper:(new useless_mapper)
  end;
  "function_type_params" >:: begin fun ctxt ->
    let source = "(function<RENAME>() { return; })" in
    assert_edits_equal ctxt ~edits:[(10, 16), "GOT_RENAMED"] ~source
    ~expected:"(function<GOT_RENAMED>() { return; })"
    ~mapper:(new useless_mapper)
  end;
  "function_combo" >:: begin fun ctxt ->
    let source = "(function rename<RENAME>(rename): Rename { return 4; })" in
    assert_edits_equal ctxt ~source
    ~edits:[((10, 16), "gotRenamed"); ((17, 23), "GOT_RENAMED"); ((25, 31), "gotRenamed");
      ((34, 40), "GotRenamed"); ((50, 51), "5")]
    ~expected:"(function gotRenamed<GOT_RENAMED>(gotRenamed): GotRenamed { return 5; })"
    ~mapper:(new useless_mapper)
  end;
  "arrow_function" >:: begin fun ctxt ->
    let source = "let bar = (x) => 4;" in
    assert_edits_equal ctxt ~edits:[(17, 18), "5"] ~source
    ~expected:"let bar = (x) => 5;"
    ~mapper:(new useless_mapper)
  end;
  "call" >:: begin fun ctxt ->
    let source = "rename()" in
    assert_edits_equal ctxt ~edits:[((0, 6), "gotRenamed")] ~source ~expected:"gotRenamed()"
    ~mapper:(new useless_mapper)
  end;
  "call_type_param" >:: begin fun ctxt ->
    let source = "rename<RENAME>()" in
    assert_edits_equal ctxt ~edits:[((7, 13), "GOT_RENAMED")] ~source
      ~expected:"rename<GOT_RENAMED>()" ~mapper:(new useless_mapper)
  end;
  "variable_declaration_kind" >:: begin fun ctxt ->
    let source = "var x = 5;" in
    assert_edits_equal ctxt ~edits:[((0, 10), "const x = 5;")] ~source ~expected:"const x = 5;"
    ~mapper:(new useless_mapper)
  end;
  "variable_declaration_expression" >:: begin fun ctxt ->
    let source = "let x = 4;" in
    assert_edits_equal ctxt ~edits:[((8, 9), "5")] ~source ~expected:"let x = 5;"
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
    assert_edits_equal ctxt  ~edits:[(13, 14), "5"] ~source
      ~expected:"for (let i = 5; i < 10; i++) {}" ~mapper:(new useless_mapper)
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
  "labeled_label" >:: begin fun ctxt ->
    let source = "rename: while (true) { }" in
    assert_edits_equal ctxt ~edits:[(0, 6), "gotRenamed"] ~source
      ~expected:"gotRenamed: while (true) { }" ~mapper:(new useless_mapper)
  end;
  "labeled_body" >:: begin fun ctxt ->
    let source = "foo: while (rename) { }" in
    assert_edits_equal ctxt ~edits:[(12, 18), "gotRenamed"] ~source
      ~expected:"foo: while (gotRenamed) { }" ~mapper:(new useless_mapper)
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
      |> Core_list.map ~f:(fun (loc, text) -> Loc.((loc.start.column, loc._end.column), text)) in
    let edits_standard =
      program Standard ast_empty ast_var
      |> Ast_diff_printer.edits_of_changes None
      |> Core_list.map ~f:(fun (loc, text) -> Loc.((loc.start.column, loc._end.column), text)) in
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
    assert_edits_equal ctxt ~edits:[(8, 14),"string"] ~source
      ~expected:"let x : string = 3;" ~mapper:(new useless_mapper)
  end;
  "type_annotation_rename_type_arg" >:: begin fun ctxt ->
    let source = "(foo: bar<rename>);" in
    assert_edits_equal ctxt ~edits:[(10, 16), "gotRenamed"] ~source
      ~expected:"(foo: bar<gotRenamed>);" ~mapper:(new useless_mapper)
  end;
  "type_annotation_rename_type" >:: begin fun ctxt ->
    let source = "(foo: rename<bar>);" in
    assert_edits_equal ctxt ~edits:[(6, 12), "gotRenamed"] ~source
      ~expected:"(foo: gotRenamed<bar>);" ~mapper:(new useless_mapper)
  end;
  "type_annotation_rename_type_and_typearg" >:: begin fun ctxt ->
    let source = "(foo: rename<rename>);" in
    assert_edits_equal ctxt ~edits:[((6, 12), "gotRenamed"); ((13, 19), "gotRenamed")] ~source
      ~expected:"(foo: gotRenamed<gotRenamed>);" ~mapper:(new useless_mapper)
  end;
  "type_annotation_rename_qualified_type" >:: begin fun ctxt ->
    let source = "(foo: Foo.rename<Bar>);" in
    assert_edits_equal ctxt ~edits:[((10, 16), "gotRenamed")] ~source
      ~expected:"(foo: Foo.gotRenamed<Bar>);" ~mapper:(new useless_mapper)
  end;
  "type_annotation_rename_qualified_typearg" >:: begin fun ctxt ->
    let source = "(foo: Foo.Bar<rename>);" in
    assert_edits_equal ctxt ~edits:[((14, 20), "gotRenamed")] ~source
      ~expected:"(foo: Foo.Bar<gotRenamed>);" ~mapper:(new useless_mapper)
  end;
  "type_annotation_rename_qualified_type_and_typearg" >:: begin fun ctxt ->
    let source = "(foo: Foo.rename<rename>);" in
    assert_edits_equal ctxt ~edits:[((10, 16), "gotRenamed"); ((17, 23), "gotRenamed")] ~source
      ~expected:"(foo: Foo.gotRenamed<gotRenamed>);" ~mapper:(new useless_mapper)
  end;
  "return_type_replace" >:: begin fun ctxt ->
    let source = "function foo() : number { return 1; }" in
    assert_edits_equal ctxt ~edits:[(17, 23),"string"] ~source
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
    assert_edits_equal ctxt ~edits:[((40, 41), "5"); ((44, 49), "(6 - 5)")] ~source
      ~expected:"function foo() { /* comment */ (5 - 3); 5; ((6 - 5)); /* comment */}"
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
  "pattern_array_annot" >:: begin fun ctxt ->
    let source = "let [foo,bar]: rename = [0]" in
    assert_edits_equal ctxt ~edits:[(15, 21), "gotRenamed"] ~source
      ~expected:"let [foo,bar]: gotRenamed = [0]" ~mapper:(new useless_mapper)
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
  "pattern_object_annot" >:: begin fun ctxt ->
    let source = "let {foo: bar}: rename = 0" in
    assert_edits_equal ctxt ~edits:[(16, 22), "gotRenamed"] ~source
      ~expected:"let {foo: bar}: gotRenamed = 0" ~mapper:(new useless_mapper)
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
    assert_edits_equal ctxt ~edits:[(13,19), "string"] ~source
      ~expected:"(dontrename: string)" ~mapper:(new useless_mapper)
  end;
  "type_cast_assign" >:: begin fun ctxt ->
    let source = "const x : number = (dontrename: number)" in
    assert_edits_equal ctxt ~edits:[(10, 16), "string"; (32, 38), "string"] ~source
      ~expected:"const x : string = (dontrename: string)" ~mapper:(new useless_mapper)
  end;
  "type_cast_add" >:: begin fun ctxt ->
    let source = "const dontrename = call( /* preserve spaces */  )" in
    assert_edits_equal ctxt ~edits:[(19, 19), "("; (49, 49), ": any)"]
      ~source ~mapper:(new insert_typecast_mapper)
      ~expected:"const dontrename = (call( /* preserve spaces */  ): any)"
  end;
  "class_type_param_instantiation" >:: begin fun ctxt ->
    let source = "class A extends B<{}> { m(): rename {} }" in
    assert_edits_equal ctxt ~edits:[((29, 35), "gotRenamed")] ~source
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
  "spread_simple" >:: begin fun ctxt ->
    let source = "[...rename]" in
    assert_edits_equal ctxt ~edits:[(4, 10), "gotRenamed"] ~source
      ~expected:"[...gotRenamed]"
      ~mapper:(new useless_mapper)
  end;
  "tagged_template_tag" >:: begin fun ctxt ->
    let source = "rename`dontRename`" in
    assert_edits_equal ctxt ~edits:[(0, 6), "gotRenamed"] ~source
      ~expected:"gotRenamed`dontRename`"
      ~mapper:(new useless_mapper)
  end;
  "tagged_template_literal" >:: begin fun ctxt ->
    let source = "dontRename`rename`" in
    assert_edits_equal ctxt ~edits:[(10, 18), "`gotRenamed`"] ~source
      ~expected:"dontRename`gotRenamed`"
      ~mapper:(new useless_mapper)
  end;
  "template_literal_simple" >:: begin fun ctxt ->
    let source = "`rename`" in
    assert_edits_equal ctxt ~edits:[(0, 8), "`gotRenamed`"] ~source
      ~expected:"`gotRenamed`"
      ~mapper:(new useless_mapper)
  end;
  "template_literal_expr" >:: begin fun ctxt ->
    let source = "`foo ${rename} bar`" in
    assert_edits_equal ctxt ~edits:[(7, 13), "gotRenamed"] ~source
      ~expected:"`foo ${gotRenamed} bar`"
      ~mapper:(new useless_mapper)
  end;
  "template_literal_expr_multiple" >:: begin fun ctxt ->
    let source = "let test = `${rename} ${foo} bar ${rename}`" in
    assert_edits_equal ctxt
      ~edits:[((14, 20), "gotRenamed"); ((35, 41), "gotRenamed")]
      ~source ~expected:"let test = `${gotRenamed} ${foo} bar ${gotRenamed}`"
      ~mapper:(new useless_mapper)
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
    assert_edits_equal ctxt ~edits:[(21, 22), "5"]
    ~source ~expected:"<Component someProp={5} />"
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
    assert_edits_equal ctxt ~edits:[((11, 17), "gotRenamed"); ((19, 20), "5")]
    ~source ~expected:"<Component gotRenamed={5} />"
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
    assert_edits_equal ctxt ~edits:[((21, 22), "5"); (37,38), "5"]
    ~source ~expected:"<Component someProp={5} anotherProp={5} />"
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
    ~edits:[((15, 21), "gotRenamed"); ((23, 29), "gotRenamed"); ((31, 32), "5")]
    ~source ~expected:"<Component {...gotRenamed} gotRenamed={5}/>"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_attribute_list_name_and_value" >:: begin fun ctxt ->
    let source = "<Component rename={1} dontRename={4} />" in
    assert_edits_equal ctxt ~edits:[((11, 17), "gotRenamed"); ((34, 35), "5")]
    ~source ~expected:"<Component gotRenamed={1} dontRename={5} />"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_child_element" >:: begin fun ctxt ->
    let source = "<div><rename /></div>" in
    assert_edits_equal ctxt ~edits:[(6, 12), "gotRenamed"]
    ~source ~expected:"<div><gotRenamed /></div>"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_child_fragment" >:: begin fun ctxt ->
    let source = "<div><>rename</></div>" in
    assert_edits_equal ctxt ~edits:[(7, 13), "gotRenamed"]
    ~source ~expected:"<div><>gotRenamed</></div>"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_child_expr" >:: begin fun ctxt ->
    let source = "<div>{rename}</div>" in
    assert_edits_equal ctxt ~edits:[(6, 12), "gotRenamed"]
    ~source ~expected:"<div>{gotRenamed}</div>"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_child_spread" >:: begin fun ctxt ->
    let source = "<div>{...rename}</div>" in
    assert_edits_equal ctxt ~edits:[(9, 15), "gotRenamed"]
    ~source ~expected:"<div>{...gotRenamed}</div>"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_child_text" >:: begin fun ctxt ->
    let source = "<div>rename</div>" in
    assert_edits_equal ctxt ~edits:[(5, 11), "gotRenamed"]
    ~source ~expected:"<div>gotRenamed</div>"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_children" >:: begin fun ctxt ->
    let source = "<div>{rename} <rename /></div>" in
    assert_edits_equal ctxt
    ~edits:[((6, 12), "gotRenamed"); ((15, 21), "gotRenamed")]
    ~source ~expected:"<div>{gotRenamed} <gotRenamed /></div>"
    ~mapper:(new useless_mapper)
  end;
  "jsx_element_children_nested" >:: begin fun ctxt ->
    let source = "<div><rename><><rename /></></rename></div>" in
    assert_edits_equal ctxt
    ~edits:[((6, 12), "gotRenamed"); ((16, 22), "gotRenamed"); ((30, 36), "gotRenamed")]
    ~source ~expected:"<div><gotRenamed><><gotRenamed /></></gotRenamed></div>"
    ~mapper:(new useless_mapper)
  end;
  "jsx_fragment_expr" >:: begin fun ctxt ->
    let source = "<>{rename}</>" in
    assert_edits_equal ctxt ~edits:[(3, 9), "gotRenamed"]
    ~source ~expected:"<>{gotRenamed}</>"
    ~mapper:(new useless_mapper)
  end;
  "declare_type_alias_id" >:: begin fun ctxt ->
    let source = "declare type Rename = string" in
    assert_edits_equal ctxt ~edits:[(13, 19), "GotRenamed"]
    ~source ~expected:"declare type GotRenamed = string"
    ~mapper:(new useless_mapper)
  end;
  "type_alias_id" >:: begin fun ctxt ->
    let source = "type Rename = string" in
    assert_edits_equal ctxt ~edits:[(5, 11), "GotRenamed"]
    ~source ~expected:"type GotRenamed = string"
    ~mapper:(new useless_mapper)
  end;
  "type_alias_intersection_left" >:: begin fun ctxt ->
    let source = "type foo = number & bar" in
    assert_edits_equal ctxt ~edits:[((11, 17), "string")]
      ~source ~expected:"type foo = string & bar"
      ~mapper:(new useless_mapper)
  end;
  "type_alias_intersection_right" >:: begin fun ctxt ->
    let source = "type foo = bar & number" in
    assert_edits_equal ctxt ~edits:[((17, 23), "string")]
      ~source ~expected:"type foo = bar & string"
      ~mapper:(new useless_mapper)
  end;
  "type_alias_intersection_rest" >:: begin fun ctxt ->
    let source = "type foo = bar & baz & number & number" in
    assert_edits_equal ctxt ~edits:[((23, 29), "string"); ((32, 38), "string")]
      ~source ~expected:"type foo = bar & baz & string & string"
      ~mapper:(new useless_mapper)
  end;
  "type_alias_intersection_argument_mismatch" >:: begin fun ctxt ->
    let source = "type foo = bar & true & boolean" in
    assert_edits_equal ctxt ~edits:[((11, 31), "bar & true")]
      ~source ~expected:"type foo = bar & true"
      ~mapper:(new remove_annotation_rest_mapper)
  end;
  "type_alias_nullable" >:: begin fun ctxt ->
    let source = "type foo = ?number" in
    assert_edits_equal ctxt ~edits:[((12, 18), "string")] ~source
      ~expected:"type foo = ?string"
      ~mapper:(new useless_mapper)
  end;
  "type_alias_number_literal" >:: begin fun ctxt ->
    let source = "type foo = 5.0" in
    assert_edits_equal ctxt ~edits:[((11, 14), "4.0")] ~source
      ~expected:"type foo = 4.0"
      ~mapper:(new useless_mapper)
  end;
  "type_alias_param_name" >:: begin fun ctxt ->
    let source = "type alias<RENAME> = string" in
    assert_edits_equal ctxt ~edits:[(11, 17), "GOT_RENAMED"]
    ~source ~expected:"type alias<GOT_RENAMED> = string"
    ~mapper:(new useless_mapper)
  end;
  "type_alias_param_bound" >:: begin fun ctxt ->
    let source = "type alias<A: number> = string" in
    assert_edits_equal ctxt ~edits:[(14, 20), "string"]
    ~source ~expected:"type alias<A: string> = string"
    ~mapper:(new useless_mapper)
  end;
  "type_alias_param_variance" >:: begin fun ctxt ->
    let source = "type alias<-A> = string" in
    assert_edits_equal ctxt ~edits:[(11, 12), "+"]
    ~source ~expected:"type alias<+A> = string"
    ~mapper:(new useless_mapper)
  end;
  "type_alias_param_insert_variance" >:: begin fun ctxt ->
    let source = "type alias<A> = string" in
    assert_edits_equal ctxt ~edits:[(11, 12), "+A"]
    ~source ~expected:"type alias<+A> = string"
    ~mapper:(new insert_variance_mapper)
  end;
  "type_alias_param_bound_insert_variance" >:: begin fun ctxt ->
    let source = "type alias<A: number> = string" in
    assert_edits_equal ctxt ~edits:[((11, 20), "+A: string")]
    ~source ~expected:"type alias<+A: string> = string"
    ~mapper:(new insert_variance_mapper)
  end;
  "type_alias_param_delete_variance" >:: begin fun ctxt ->
    let source = "type alias<-A> = string" in
    assert_edits_equal ctxt ~edits:[(11, 12), ""]
    ~source ~expected:"type alias<A> = string"
    ~mapper:(new delete_variance_mapper)
  end;
  "type_alias_param_default" >:: begin fun ctxt ->
    let source = "type alias<A = number> = string" in
    assert_edits_equal ctxt ~edits:[(15, 21), "string"]
    ~source ~expected:"type alias<A = string> = string"
    ~mapper:(new useless_mapper)
  end;
  "type_alias_param_combo" >:: begin fun ctxt ->
    let source = "type alias<-RENAME: number = number> = string" in
    assert_edits_equal ctxt
    ~edits:[((11, 12), "+"); ((12, 18), "GOT_RENAMED");
        ((20, 26), "string"); ((29, 35), "string")]
    ~source ~expected:"type alias<+GOT_RENAMED: string = string> = string"
    ~mapper:(new useless_mapper)
  end;
  "type_alias_param_list" >:: begin fun ctxt ->
    let source = "type alias<-RENAME, RENAME: number> = string" in
    assert_edits_equal ctxt
    ~edits:[((11, 12), "+"); ((12, 18), "GOT_RENAMED");
        ((20, 26), "GOT_RENAMED"); ((28, 34), "string")]
    ~source ~expected:"type alias<+GOT_RENAMED, GOT_RENAMED: string> = string"
    ~mapper:(new useless_mapper)
  end;
  "declare_type_alias_right" >:: begin fun ctxt ->
    let source = "declare type alias = number" in
    assert_edits_equal ctxt ~edits:[(21, 27), "string"]
    ~source ~expected:"declare type alias = string"
    ~mapper:(new useless_mapper)
  end;
  "type_alias_right" >:: begin fun ctxt ->
    let source = "type alias = number" in
    assert_edits_equal ctxt ~edits:[(13, 19), "string"]
    ~source ~expected:"type alias = string"
    ~mapper:(new useless_mapper)
  end;
  "type_alias_right_function_type_params" >:: begin fun ctxt ->
    let source = "type alias = (rename: string, bar: number, ...rename: string) => string" in
    assert_edits_equal ctxt
    ~edits:[((14, 20), "gotRenamed"); ((35, 41), "string"); ((46, 52), "gotRenamed")]
    ~source
    ~expected:"type alias = (gotRenamed: string, bar: string, ...gotRenamed: string) => string"
    ~mapper:(new useless_mapper)
  end;
  "type_alias_right_function_type_tparams" >:: begin fun ctxt ->
    let source = "type alias = <RENAME>(param: string) => string" in
    assert_edits_equal ctxt ~edits:[(14, 20), "GOT_RENAMED"]
    ~source ~expected:"type alias = <GOT_RENAMED>(param: string) => string"
    ~mapper:(new useless_mapper)
  end;
  "type_alias_right_function_type_return" >:: begin fun ctxt ->
    let source = "type alias = string => number" in
    assert_edits_equal ctxt ~edits:[(23, 29), "string"]
    ~source ~expected:"type alias = string => string"
    ~mapper:(new useless_mapper)
  end;
  "type_alias_right_object_type" >:: begin fun ctxt ->
    let source = "type alias = { rename: string }" in
    assert_edits_equal ctxt ~edits:[(15, 21), "gotRenamed"]
    ~source ~expected:"type alias = { gotRenamed: string }"
    ~mapper:(new useless_mapper)
  end;
  "type_alias_right_object_property_value_get" >:: begin fun ctxt ->
    let source = "type alias = { get rename(): void; }" in
    assert_edits_equal ctxt ~edits:[((19, 25), "gotRenamed")] ~source
      ~expected:"type alias = { get gotRenamed(): void; }"
      ~mapper:(new useless_mapper)
  end;
  "type_alias_right_object_property_value_set" >:: begin fun ctxt ->
    let source = "type alias = { set foo(value: number): void; }" in
    assert_edits_equal ctxt ~edits:[((30, 36), "string")] ~source
      ~expected:"type alias = { set foo(value: string): void; }"
      ~mapper:(new useless_mapper)
  end;
  "type_alias_right_object_variance" >:: begin fun ctxt ->
    let source = "type alias = { -foo: string }" in
    assert_edits_equal ctxt ~edits:[(15, 16), "+"]
    ~source ~expected:"type alias = { +foo: string }"
    ~mapper:(new useless_mapper)
  end;
  "opaque_type_id" >:: begin fun ctxt ->
    let source = "opaque type Rename = string" in
    assert_edits_equal ctxt ~edits:[(12, 18), "GotRenamed"]
    ~source ~expected:"opaque type GotRenamed = string"
    ~mapper:(new useless_mapper)
  end;
  "opaque_type_param" >:: begin fun ctxt ->
    let source = "opaque type foo<RENAME> = string" in
    assert_edits_equal ctxt ~edits:[(16, 22), "GOT_RENAMED"]
    ~source ~expected:"opaque type foo<GOT_RENAMED> = string"
    ~mapper:(new useless_mapper)
  end;
  "opaque_type_impl" >:: begin fun ctxt ->
    let source = "opaque type foo<A> = number" in
    assert_edits_equal ctxt ~edits:[(21, 27), "string"]
    ~source ~expected:"opaque type foo<A> = string"
    ~mapper:(new useless_mapper)
  end;
  "opaque_type_super" >:: begin fun ctxt ->
    let source = "opaque type foo: number = string" in
    assert_edits_equal ctxt ~edits:[(17, 23), "string"]
    ~source ~expected:"opaque type foo: string = string"
    ~mapper:(new useless_mapper)
  end;
  "opaque_type_combo" >:: begin fun ctxt ->
    let source = "opaque type Rename<RENAME>: number = number" in
    assert_edits_equal ctxt ~edits:[((12, 18), "GotRenamed"); ((19, 25), "GOT_RENAMED");
      ((28, 34), "string"); ((37, 43), "string")]
    ~source ~expected:"opaque type GotRenamed<GOT_RENAMED>: string = string"
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
  "update_same_op" >:: begin fun ctxt ->
    let source = "++rename" in
    assert_edits_equal ctxt ~edits:[((2, 8), "gotRenamed")] ~source ~expected:"++gotRenamed"
      ~mapper:(new useless_mapper)
  end;
  "update_diff_op" >:: begin fun ctxt ->
    let source = "--rename" in
    assert_edits_equal ctxt ~edits:[((0, 8), "(++gotRenamed)")] ~source ~expected:"(++gotRenamed)"
      ~mapper:(new useless_mapper)
  end;
  "update_arrow_function_single_param" >:: begin fun ctxt ->
    let source = "const x = bla => { return 0; };" in
    assert_edits_equal ctxt
      ~edits:[((7, 7), ": number"); ((10, 13), "(bla: number)"); ((13, 13), ": number")]
      ~source
      ~expected:"const x: number = (bla: number): number => { return 0; };"
      ~mapper:(new insert_annot_mapper)
  end;
  "update_arrow_function_function_return" >:: begin fun ctxt ->
    let source = "const x = bla => { return 0; };" in
    assert_edits_equal ctxt
      ~edits:[
        ((7, 7), ": (() => number)");
        ((10, 13), "(bla: () => number)");
        ((13, 13), ": (() => number)")
      ]
      ~source
      ~expected:"const x: (() => number) = (bla: () => number): (() => number) => { return 0; };"
      ~mapper:(new insert_function_annot_mapper)
  end;
  "new_imports_after_directive_dont_reprint_the_file" >:: begin fun ctxt ->
    let source = "'use strict';const x = bla => { return 0; };" in
    assert_edits_equal_standard_only ctxt
      ~edits:[
        ((13, 13), "import type {there as here} from \"new_import1\";
import type {there as here} from \"new_import2\";");
        ((20, 20), ": (() => number)");
        ((23, 26), "(bla: () => number)");
        ((26, 26), ": (() => number)")
      ]
      ~source
      ~expected:"'use strict';import type {there as here} from \"new_import1\";
import type {there as here} from \"new_import2\";const x: (() => number) = (bla: () => number): (() => number) => { return 0; };"
      ~mapper:(new insert_import_and_annot_mapper)
  end;
  "import_renamed_simple" >:: begin fun ctxt ->
    let source = "import rename from \"foo\";" in
    assert_edits_equal ctxt
      ~edits:[((7, 13), "gotRenamed")]
      ~source ~expected:"import gotRenamed from \"foo\";"
      ~mapper:(new useless_mapper)
  end;
  "import_renamed_simple_multiple1" >:: begin fun ctxt ->
    let source = "import rename, {bar} from \"foo\";" in
    assert_edits_equal ctxt
      ~edits:[((7, 13), "gotRenamed")]
      ~source ~expected:"import gotRenamed, {bar} from \"foo\";"
      ~mapper:(new useless_mapper)
  end;
  "import_renamed_simple_multiple2" >:: begin fun ctxt ->
    let source = "import bar, {rename} from \"foo\";" in
    assert_edits_equal ctxt
      ~edits:[((13, 19), "gotRenamed")]
      ~source ~expected:"import bar, {gotRenamed} from \"foo\";"
      ~mapper:(new useless_mapper)
  end;
  "import_renamed_simple1" >:: begin fun ctxt ->
    let source = "import {rename} from \"foo\";" in
    assert_edits_equal ctxt
      ~edits:[((8, 14), "gotRenamed")]
      ~source ~expected:"import {gotRenamed} from \"foo\";"
      ~mapper:(new useless_mapper)
  end;
  "import_renamed_multiple" >:: begin fun ctxt ->
    let source = "import {rename, bar} from \"foo\";" in
    assert_edits_equal ctxt
      ~edits:[((8,14), "gotRenamed")]
      ~source ~expected:"import {gotRenamed, bar} from \"foo\";"
      ~mapper:(new useless_mapper)
  end;
  "import_renamed_whole_module" >:: begin fun ctxt ->
    let source = "import * as rename from \"foo\";" in
    assert_edits_equal ctxt
      ~edits:[((12, 18), "gotRenamed")]
      ~source ~expected:"import * as gotRenamed from \"foo\";"
      ~mapper:(new useless_mapper)
  end;
  "import_type1" >:: begin fun ctxt ->
    let source = "import type rename from \"bar\";" in
    assert_edits_equal ctxt
      ~edits:[((12, 18), "gotRenamed")]
      ~source ~expected:"import type gotRenamed from \"bar\";"
      ~mapper:(new useless_mapper)
  end;
  "import_type_and_fn2" >:: begin fun ctxt ->
    let source = "import rename, {type foo} from \"bar\";" in
    assert_edits_equal ctxt
      ~edits:[((7, 13), "gotRenamed")]
      ~source ~expected:"import gotRenamed, {type foo} from \"bar\";"
      ~mapper:(new useless_mapper)
  end;
  "import_multiple_names1" >:: begin fun ctxt ->
    let source = "import rename, {myBar, myBaz} from \"bar\";" in
    assert_edits_equal ctxt
      ~edits:[((7, 13), "gotRenamed")]
      ~source ~expected:"import gotRenamed, {myBar, myBaz} from \"bar\";"
      ~mapper:(new useless_mapper)
  end;
  "import_multiple_names2" >:: begin fun ctxt ->
    let source = "import myBar, {rename, myBaz} from \"bar\";" in
    assert_edits_equal ctxt
      ~edits:[((15, 21), "gotRenamed")]
      ~source ~expected:"import myBar, {gotRenamed, myBaz} from \"bar\";"
      ~mapper:(new useless_mapper)
  end;
  "import_type2" >:: begin fun ctxt ->
    let source = "import type {rename} from \"bar\";" in
    assert_edits_equal ctxt
      ~edits:[((13, 19), "gotRenamed")]
      ~source ~expected:"import type {gotRenamed} from \"bar\";"
      ~mapper:(new useless_mapper)
  end;
  "import_fn_and_rename_module1" >:: begin fun ctxt ->
    let source = "import rename, * as myModule from \"bar\";" in
    assert_edits_equal ctxt
      ~edits:[((7, 13), "gotRenamed")]
      ~source ~expected:"import gotRenamed, * as myModule from \"bar\";"
      ~mapper:(new useless_mapper)
  end;
  "import_fn_and_rename_module2" >:: begin fun ctxt ->
    let source = "import foo, * as rename from \"bar\";" in
    assert_edits_equal ctxt
      ~edits:[((17, 23), "gotRenamed")]
      ~source ~expected:"import foo, * as gotRenamed from \"bar\";"
      ~mapper:(new useless_mapper)
  end;
  "import_rename" >:: begin fun ctxt ->
    let source = "import {rename as bar} from \"foo\";" in
    assert_edits_equal ctxt
      ~edits:[((8, 14), "gotRenamed")]
      ~source ~expected:"import {gotRenamed as bar} from \"foo\";"
      ~mapper:(new useless_mapper)
  end;
  "import_type_and_fn1" >:: begin fun ctxt ->
    let source = "import foo, {type rename} from \"bar\";" in
    assert_edits_equal ctxt
      ~edits:[((18,24), "gotRenamed")]
      ~source ~expected:"import foo, {type gotRenamed} from \"bar\";"
      ~mapper:(new useless_mapper)
  end;
  "import_type3" >:: begin fun ctxt ->
    let source = "import {type rename} from \"bar\";" in
    assert_edits_equal ctxt
      ~edits:[((13, 19), "gotRenamed")]
      ~source ~expected:"import {type gotRenamed} from \"bar\";"
      ~mapper:(new useless_mapper)
  end;
  "import_typeof1" >:: begin fun ctxt ->
    let source = "import typeof rename from \"bar\";" in
    assert_edits_equal ctxt
      ~edits:[((14, 20), "gotRenamed")]
      ~source ~expected:"import typeof gotRenamed from \"bar\";"
      ~mapper:(new useless_mapper)
  end;
  "import_typeof2" >:: begin fun ctxt ->
    let source = "import typeof {rename} from \"bar\";" in
    assert_edits_equal ctxt
      ~edits:[((15, 21), "gotRenamed")]
      ~source ~expected:"import typeof {gotRenamed} from \"bar\";"
      ~mapper:(new useless_mapper)
  end;
  "throw" >:: begin fun ctxt ->
    let source = "throw \"rename\";" in
    assert_edits_equal ctxt ~edits:[((6, 14), "\"gotRenamed\"")]
    ~source ~expected:"throw \"gotRenamed\";" ~mapper:(new literal_mapper)
  end;
  "bool1" >:: begin fun ctxt ->
    let source = "rename = true;" in
    assert_edits_equal ctxt ~edits:[((0, 6), "gotRenamed")]
    ~source ~expected:"gotRenamed = true;" ~mapper:(new useless_mapper)
  end;
  "bool2" >:: begin fun ctxt ->
    let source = "const rename = 0; Boolean(rename);" in
    assert_edits_equal ctxt ~edits:[((6, 12), "gotRenamed"); ((26, 32), "gotRenamed")]
    ~source ~expected:"const gotRenamed = 0; Boolean(gotRenamed);" ~mapper:(new useless_mapper)
  end;
  "bool3" >:: begin fun ctxt ->
    let source = "const rename = true; Boolean((false || rename));" in
    assert_edits_equal ctxt ~edits:[((6, 12), "gotRenamed"); ((39, 45), "gotRenamed")]
    ~source ~expected:"const gotRenamed = true; Boolean((false || gotRenamed));"
    ~mapper:(new useless_mapper)
  end;
  "bool_change" >:: begin fun ctxt ->
    let source = "const x = true; Boolean(true);" in
    assert_edits_equal ctxt ~edits:[((10, 14), "false"); ((24, 28), "false")]
    ~source ~expected:"const x = false; Boolean(false);" ~mapper:(new true_to_false_mapper)
  end;
  "bool_type_change" >:: begin fun ctxt ->
    let source = "const x: true = 'garbage';" in
    assert_edits_equal ctxt ~edits:[((9, 13), "false")]
    ~source ~expected:"const x: false = 'garbage';" ~mapper:(new true_to_false_mapper)
  end;
  "comment_add" >:: begin fun ctxt ->
    let source = "bla" in
    assert_edits_equal ctxt ~edits:[((0, 0), "/*hello*/"); ((3, 3), "/*bye*/")]
    ~source ~expected:"/*hello*/bla/*bye*/" ~mapper:(new add_comment_mapper)
  end;
  "comment_modify" >:: begin fun ctxt ->
    let source = "/*MAL*/bla/*WRONG*/" in
    assert_edits_equal ctxt ~edits:[((0, 7), "/*hello*/"); ((10, 19), "/*bye*/")]
    ~source ~expected:"/*hello*/bla/*bye*/" ~mapper:(new add_comment_mapper)
  end;
  "comment_annot_generic_deep" >:: begin fun ctxt ->
    let source = "const a: Box<Bla> = {}" in
    assert_edits_equal ctxt ~source ~mapper:(new add_comment_mapper)
    ~edits:[((6, 6), "/*hello*/"); ((7, 7), "/*bye*/"); ((9, 9), "/*hello*/"); ((12, 12), "/*bye*/");
      ((13, 13), "/*hello*/"); ((16, 16), "/*bye*/")]
    ~expected:"const /*hello*/a/*bye*/: /*hello*/Box/*bye*/</*hello*/Bla/*bye*/> = {}"
  end;
  "let_union_first" >:: begin fun ctxt ->
    let source = "let x : number | void = 42;" in
    assert_edits_equal ctxt ~edits:[((8, 14), "string")]
      ~source ~expected:"let x : string | void = 42;"
      ~mapper:(new useless_mapper)
  end;
  "let_union_second" >:: begin fun ctxt ->
    let source = "let x : boolean | number = 42;" in
    assert_edits_equal ctxt ~edits:[((18, 24), "string")]
      ~source ~expected:"let x : boolean | string = 42;"
      ~mapper:(new useless_mapper)
  end;
  "let_union_rest" >:: begin fun ctxt ->
    let source = "let x : boolean | void | number = 42;" in
    assert_edits_equal ctxt ~edits:[((25, 31), "string")]
      ~source ~expected:"let x : boolean | void | string = 42;"
      ~mapper:(new useless_mapper)
  end;
  "type_alias_union_argument_mismatch" >:: begin fun ctxt ->
    let source = "type foo = bar | true | boolean" in
    assert_edits_equal ctxt ~edits:[((11, 31), "bar | true")]
      ~source ~expected:"type foo = bar | true"
      ~mapper:(new remove_annotation_rest_mapper)
  end
]
