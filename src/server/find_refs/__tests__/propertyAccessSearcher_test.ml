(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let run ctxt expected name text =
  let (ast, _, _) =
    FindRefsUtils.compute_ast_result (File_key.SourceFile "/dummy.js") text
    |> Core_result.ok_or_failwith
  in
  let result = PropertyAccessSearcher.search name ast in
  assert_equal ~ctxt expected result

let tests = "SymbolKind" >::: [
  "property_access_positive" >:: begin fun ctxt ->
    run ctxt true "bar" "foo.bar"
  end;
  "property_access_negative" >:: begin fun ctxt ->
    run ctxt false "bar" "foo.baz"
  end;
  "destructuring_shorthand_positive" >:: begin fun ctxt ->
    run ctxt true "bar" "const {bar} = baz"
  end;
  "destructuring_shorthand_negative" >:: begin fun ctxt ->
    run ctxt false "baz" "const {bar} = baz"
  end;
  "destructuring_positive" >:: begin fun ctxt ->
    run ctxt true "foo" "const {foo: bar} = baz"
  end;
  "destructuring_negative" >:: begin fun ctxt ->
    run ctxt false "bar" "const {foo: bar} = baz"
  end;
  "destructuring_negative" >:: begin fun ctxt ->
    run ctxt false "bar" "const {foo: bar} = baz"
  end;
  "export_default_positive" >:: begin fun ctxt ->
    run ctxt true "default" "export default 5"
  end;
  "export_default_negative" >:: begin fun ctxt ->
    run ctxt false "bar" "export default bar"
  end;
  "import_default_positive" >:: begin fun ctxt ->
    run ctxt true "default" "import bar from 'baz'"
  end;
  "import_default_negative" >:: begin fun ctxt ->
    run ctxt false "bar" "import bar from 'baz'"
  end;
  "class_method" >:: begin fun ctxt ->
    run ctxt true "bar" "class Foo { bar(): void {} }"
  end;
  "class_property" >:: begin fun ctxt ->
    run ctxt true "bar" "class Foo { bar: number }"
  end;
];
