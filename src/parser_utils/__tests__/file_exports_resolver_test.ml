(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open File_exports_resolver

let substring_loc s loc =
  let open Loc in
  let {start={offset=a; _}; _end={offset=b; _}; _} = loc in
  String.sub s a (b - a)

let assert_singleton_smap ~ctxt key map =
  assert_equal ~ctxt 1 (SMap.cardinal map);
  SMap.find_unsafe key map

let assert_substring_equal ~ctxt expected source loc =
  assert_equal ~ctxt ~printer:(fun s -> s) expected (substring_loc source loc)

let assert_exports ?assert_default ?assert_named source =
  let parse_options = Some Parser_env.({
    default_parse_options with
    Parser_env.esproposal_class_static_fields = true;
  }) in
  let ast, _ = Parser_flow.program ~parse_options source in
  let exports = File_exports_resolver.program ast in
  begin match assert_default, exports.default with
  | Some _, None -> assert_failure "Default export not resolved"
  | None, Some _ -> assert_failure "Unexpected default export resolved"
  | Some assert_default, Some default -> assert_default default
  | None, None -> ()
  end;
  match assert_named, exports.named with
  | Some _, None -> assert_failure "Named exports not resolved"
  | None, Some _ -> assert_failure "Unexpected named exports resolved"
  | Some assert_named, Some named -> assert_named named
  | None, None -> ()

let tests = "require" >::: [
  "empty" >:: begin fun _ctxt ->
    let source = "" in
    (* Expect nothing *)
    assert_exports source;
  end;

  "cjs_rebound_module" >:: begin fun _ctxt ->
    let source = "var module = {}; module.exports = 0;" in
    (* Expect nothing *)
    assert_exports source;
  end;

  "cjs_rebound_exports" >:: begin fun _ctxt ->
    let source = "var exports = {}; exports.foo = 0;" in
    (* Expect nothing *)
    assert_exports source;
  end;

  "cjs_clobber_module" >:: begin fun _ctxt ->
    let source = "module = {}; module.exports = 0;" in
    (* Expect nothing *)
    assert_exports source ~assert_default:(fun _ ->
      (* TODO: Stop tracking after clobber *)
      ()
    );
  end;

  "cjs_clobber_exports" >:: begin fun _ctxt ->
    let source = "exports = {}; exports.baz = 0;" in
    (* Expect nothing *)
    assert_exports source ~assert_named:(fun _ ->
      (* TODO: Stop tracking after clobber *)
      ()
    );
  end;

  "cjs_default_expression" >:: begin fun ctxt ->
    let source = "module.exports = 0;" in
    assert_exports source ~assert_default:(function
      | ExportExpression { line_loc; expr = (loc, _) } ->
        assert_substring_equal ~ctxt source source line_loc;
        assert_substring_equal ~ctxt "0" source loc;
      | _ -> assert_failure "Unexpected export"
    );
  end;

  "cjs_default_expression_var" >:: begin fun ctxt ->
    let source_foo = "const foo = 0;" in
    let source_exp = "module.exports = foo;" in
    let source = source_foo ^ source_exp in
    assert_exports source ~assert_default:(function
      | ExportExpression { line_loc; expr = (loc, _) } ->
        assert_substring_equal ~ctxt source_foo source line_loc;
        assert_substring_equal ~ctxt "0" source loc;
      | _ -> assert_failure "Unexpected export"
    );
  end;

  "cjs_default_function" >:: begin fun ctxt ->
    let source = "module.exports = () => {};" in
    assert_exports source ~assert_default:(function
      | ExportFunction { line_loc; func = (loc, _) } ->
        assert_substring_equal ~ctxt source source line_loc;
        assert_substring_equal ~ctxt "() => {}" source loc;
      | _ -> assert_failure "Unexpected export"
    );
  end;

  "cjs_default_function_statement" >:: begin fun ctxt ->
    let source_foo = "function foo() {}" in
    let source_exp = "module.exports = foo;" in
    let source = source_foo ^ source_exp in
    assert_exports source ~assert_default:(function
      | ExportFunction { line_loc; func = (loc, _) } ->
        assert_substring_equal ~ctxt source_foo source line_loc;
        assert_substring_equal ~ctxt source_foo source loc;
      | _ -> assert_failure "Unexpected export"
    );
  end;

  "cjs_default_function_statement_statement" >:: begin fun ctxt ->
    let source_foo = "function foo() {}" in
    let source_bar = "const bar = foo;" in
    let source_exp = "module.exports = bar;" in
    let source = source_foo ^ source_bar ^ source_exp in
    assert_exports source ~assert_default:(function
      | ExportFunction { line_loc; func = (loc, _) } ->
        assert_substring_equal ~ctxt source_foo source line_loc;
        assert_substring_equal ~ctxt source_foo source loc;
      | _ -> assert_failure "Unexpected export"
    );
  end;

  "cjs_default_class" >:: begin fun ctxt ->
    let source = "module.exports = class {};" in
    assert_exports source ~assert_default:(function
      | ExportClass { line_loc; class_ = (loc, _) } ->
        assert_substring_equal ~ctxt source source line_loc;
        assert_substring_equal ~ctxt "class {}" source loc;
      | _ -> assert_failure "Unexpected export"
    );
  end;


  "cjs_default_class_statement" >:: begin fun ctxt ->
    let source_foo = "class foo {}" in
    let source_exp = "module.exports = foo;" in
    let source = source_foo ^ source_exp in
    assert_exports source ~assert_default:(function
      | ExportClass { line_loc; class_ = (loc, _) } ->
        assert_substring_equal ~ctxt source_foo source line_loc;
        assert_substring_equal ~ctxt source_foo source loc;
      | _ -> assert_failure "Unexpected export"
    );
  end;

  "cjs_default_clobber_named_exports" >:: begin fun ctxt ->
    let source = "exports.foo = 0; module.exports = 0;" in
    (* Expect nothing *)
    assert_exports source ~assert_default:(function
      | ExportExpression { line_loc; expr = (loc, _) } ->
        assert_substring_equal ~ctxt "module.exports = 0;" source line_loc;
        assert_substring_equal ~ctxt "0" source loc;
      | _ -> assert_failure "Unexpected export"
    );
  end;

  "cjs_named_assignment_function" >:: begin fun ctxt ->
    let source = "module.exports.foo = () => {};" in
    assert_exports source ~assert_named:(fun named ->
      match assert_singleton_smap ~ctxt "foo" named with
      | ExportFunction { line_loc; func = (loc, _) } ->
        assert_substring_equal ~ctxt source source line_loc;
        assert_substring_equal ~ctxt "() => {}" source loc;
      | _ -> assert_failure "Unexpected export"
    );
  end;

  "cjs_named_assignment_class" >:: begin fun ctxt ->
    let source = "module.exports.foo = class {};" in
    assert_exports source ~assert_named:(fun named ->
      match assert_singleton_smap ~ctxt "foo" named with
      | ExportClass { line_loc; class_ = (loc, _) } ->
        assert_substring_equal ~ctxt source source line_loc;
        assert_substring_equal ~ctxt "class {}" source loc;
      | _ -> assert_failure "Unexpected export"
    );
  end;

  "cjs_named_assignment_expression" >:: begin fun ctxt ->
    let source = "module.exports.foo = 0;" in
    assert_exports source ~assert_named:(fun named ->
      match assert_singleton_smap ~ctxt "foo" named with
      | ExportExpression { line_loc; expr = (loc, _) } ->
        assert_substring_equal ~ctxt source source line_loc;
        assert_substring_equal ~ctxt "0" source loc;
      | _ -> assert_failure "Unexpected export"
    );
  end;

  "cjs_named_assignment_expression_statement" >:: begin fun ctxt ->
    let source_foo = "const foo = 0;" in
    let source_bar = "module.exports.bar = foo;" in
    let source = source_foo ^ source_bar in
    assert_exports source ~assert_named:(fun named ->
      match assert_singleton_smap ~ctxt "bar" named with
      | ExportExpression { line_loc; expr = (loc, _) } ->
        assert_substring_equal ~ctxt source_foo source line_loc;
        assert_substring_equal ~ctxt "0" source loc;
      | _ -> assert_failure "Unexpected export"
    );
  end;

  "cjs_named_mutli_assignment" >:: begin fun ctxt ->
    let source_foo = "module.exports.foo = 0;" in
    let source_bar = "module.exports.bar = () => {};" in
    let source = source_foo ^ source_bar in
    assert_exports source ~assert_named:(fun named ->
      SMap.iter (fun k e -> match k, e with
      | "foo", ExportExpression { line_loc; expr = (loc, _) } ->
        assert_substring_equal ~ctxt source_foo source line_loc;
        assert_substring_equal ~ctxt "0" source loc;
      | "bar", ExportFunction { line_loc; func = (loc, _) } ->
        assert_substring_equal ~ctxt source_bar source line_loc;
        assert_substring_equal ~ctxt "() => {}" source loc;
      | _ -> assert_failure "Unexpected export"
      ) named;
    );
  end;

  "cjs_fancy_named_assignment" >:: begin fun ctxt ->
    let source_foo = "module.exports['foo'] = 0;" in
    let source_bar = "module.exports[bar] = 0;" in (* Not supported *)
    let source = source_foo ^ source_bar in
    assert_exports source ~assert_named:(fun named ->
      SMap.iter (fun k e -> match k, e with
      | "foo", ExportExpression { line_loc; expr = (loc, _) } ->
        assert_substring_equal ~ctxt source_foo source line_loc;
        assert_substring_equal ~ctxt "0" source loc;
      | _ -> assert_failure "Unexpected export"
      ) named;
    );
  end;

  "cjs_mixed_assignment" >:: begin fun ctxt ->
    let source_def = "module.exports = class {};" in
    let source_foo = "module.exports.foo = 0;" in
    let source = source_def ^ source_foo in
    assert_exports source
      ~assert_default:(function
        | ExportClass { line_loc; class_ = (loc, _) } ->
          assert_substring_equal ~ctxt source_def source line_loc;
          assert_substring_equal ~ctxt "class {}" source loc;
        | _ -> assert_failure "Unexpected export"
      )
      ~assert_named:(fun named ->
        match assert_singleton_smap ~ctxt "foo" named with
        | ExportExpression { line_loc; expr = (loc, _) } ->
          assert_substring_equal ~ctxt source_foo source line_loc;
          assert_substring_equal ~ctxt "0" source loc;
        | _ -> assert_failure "Unexpected export"
      );
  end;

  "cjs_named_object_function" >:: begin fun ctxt ->
    let source = "module.exports = { foo() {} };" in
    assert_exports source ~assert_named:(fun named ->
      match assert_singleton_smap ~ctxt "foo" named with
      | ExportFunction { line_loc; func = (loc, _) } ->
        assert_substring_equal ~ctxt "foo() {}" source line_loc;
        assert_substring_equal ~ctxt "() {}" source loc;
      | _ -> assert_failure "Unexpected export"
    );
  end;

  "cjs_named_object_expression" >:: begin fun ctxt ->
    let source = "module.exports = { foo: 0 };" in
    assert_exports source ~assert_named:(fun named ->
      match assert_singleton_smap ~ctxt "foo" named with
      | ExportExpression { line_loc; expr = (loc, _) } ->
        assert_substring_equal ~ctxt "foo: 0" source line_loc;
        assert_substring_equal ~ctxt "0" source loc;
      | _ -> assert_failure "Unexpected export"
    );
  end;

  "cjs_fancy_named_object_expression" >:: begin fun ctxt ->
    let source = "module.exports = { 'foo': 0, ['bar']: 0 };" in
    assert_exports source ~assert_named:(fun named ->
      SMap.iter (fun k e -> match k, e with
      | "foo", ExportExpression { line_loc; expr = (loc, _) } ->
        assert_substring_equal ~ctxt "'foo': 0" source line_loc;
        assert_substring_equal ~ctxt "0" source loc;
      | "bar", ExportExpression { line_loc; expr = (loc, _) } ->
        assert_substring_equal ~ctxt "['bar']: 0" source line_loc;
        assert_substring_equal ~ctxt "0" source loc;
      | _ -> assert_failure "Unexpected export"
      ) named;
    );
  end;

  "cjs_named_object_expression_statement" >:: begin fun ctxt ->
    let source_foo = "function foo() {}" in
    let source_bar = "const bar = { foo, baz: 0 };" in
    let source_exp = "module.exports = bar;" in
    let source = source_foo ^ source_bar ^ source_exp in
    assert_exports source ~assert_named:(fun named ->
      SMap.iter (fun k e -> match k, e with
      | "foo", ExportFunction { line_loc; func = (loc, _) } ->
        assert_substring_equal ~ctxt source_foo source line_loc;
        assert_substring_equal ~ctxt source_foo source loc;
      | "baz", ExportExpression { line_loc; expr = (loc, _) } ->
        assert_substring_equal ~ctxt "baz: 0" source line_loc;
        assert_substring_equal ~ctxt "0" source loc;
      | _ -> assert_failure "Unexpected export"
      ) named;
    );
  end;

  "cjs_class_statement_static_props" >:: begin fun ctxt ->
    let source_foo = "class foo { static bar() {} static baz = 0; }" in
    let source_exp = "module.exports = foo;" in
    let source = source_foo ^ source_exp in
    assert_exports source
      ~assert_default:(function
        | ExportClass { line_loc; class_ = (loc, _) } ->
          assert_substring_equal ~ctxt source_foo source line_loc;
          assert_substring_equal ~ctxt source_foo source loc;
        | _ -> assert_failure "Unexpected export"
      )
      ~assert_named:(fun named ->
        SMap.iter (fun k e -> match k, e with
        | "bar", ExportFunction { line_loc; func = (loc, _) } ->
          assert_substring_equal ~ctxt "static bar() {}" source line_loc;
          assert_substring_equal ~ctxt "() {}" source loc;
        | "baz", ExportExpression { line_loc; expr = (loc, _) } ->
          assert_substring_equal ~ctxt "static baz = 0;" source line_loc;
          assert_substring_equal ~ctxt "0" source loc;
        | _ -> assert_failure "Unexpected export"
        ) named;
      );
  end;
]
