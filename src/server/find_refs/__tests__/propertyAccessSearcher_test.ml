(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let run ctxt expected name content =
  let file = File_key.SourceFile "/dummy.js" in
  let info = FindRefsUtils.compute_docblock file content in
  let parse_options = Parsing_service_js.make_parse_options
    ~fail:false
    ~types_mode:Parsing_service_js.TypesAllowed
    ~use_strict:true
    ~module_ref_prefix:None
    ~facebook_fbt:None
    ~prevent_munge:false
    ()
  in
  let result = Parsing_service_js.do_parse ~parse_options ~info content file in
  let ast = match result with
    | Parsing_service_js.Parse_ok parse_ok ->
      let ast, _ = Parsing_service_js.basic parse_ok in ast
    | Parsing_service_js.Parse_fail _ -> failwith "Parse unexpectedly failed"
    | Parsing_service_js.Parse_skip _ -> failwith "Parse unexpectedly skipped"
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
  "optional_chain_new" >:: begin fun ctxt ->
    run ctxt true "bar" "foo?.bar"
  end;
  "optional_chain_continued" >:: begin fun ctxt ->
    run ctxt true "baz" "foo?.bar.baz"
  end;
];
