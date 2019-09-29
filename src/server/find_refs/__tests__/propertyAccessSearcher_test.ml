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
  Parsing_service_js.(
    let parse_options =
      {
        parse_fail = false;
        parse_types_mode = TypesAllowed;
        parse_use_strict = true;
        parse_prevent_munge = false;
        parse_module_ref_prefix = None;
        parse_facebook_fbt = None;
        parse_arch = Options.Classic;
        parse_abstract_locations = false;
      }
    in
    let result = Parsing_service_js.do_parse ~parse_options ~info content file in
    let ast =
      match result with
      | Parsing_service_js.Parse_ok parse_ok ->
        let (ast, _) = Parsing_service_js.basic parse_ok in
        ast
      | Parsing_service_js.Parse_fail _ -> failwith "Parse unexpectedly failed"
      | Parsing_service_js.Parse_skip _ -> failwith "Parse unexpectedly skipped"
    in
    let result = PropertyAccessSearcher.search name ast in
    assert_equal ~ctxt expected result)

let tests =
  "SymbolKind"
  >::: [
         ("property_access_positive" >:: (fun ctxt -> run ctxt true "bar" "foo.bar"));
         ("property_access_negative" >:: (fun ctxt -> run ctxt false "bar" "foo.baz"));
         ( "destructuring_shorthand_positive"
         >:: (fun ctxt -> run ctxt true "bar" "const {bar} = baz") );
         ( "destructuring_shorthand_negative"
         >:: (fun ctxt -> run ctxt false "baz" "const {bar} = baz") );
         ("destructuring_positive" >:: (fun ctxt -> run ctxt true "foo" "const {foo: bar} = baz"));
         ("destructuring_negative" >:: (fun ctxt -> run ctxt false "bar" "const {foo: bar} = baz"));
         ("destructuring_negative" >:: (fun ctxt -> run ctxt false "bar" "const {foo: bar} = baz"));
         ("export_default_positive" >:: (fun ctxt -> run ctxt true "default" "export default 5"));
         ("export_default_negative" >:: (fun ctxt -> run ctxt false "bar" "export default bar"));
         ( "import_default_positive"
         >:: (fun ctxt -> run ctxt true "default" "import bar from 'baz'") );
         ("import_default_negative" >:: (fun ctxt -> run ctxt false "bar" "import bar from 'baz'"));
         ("class_method" >:: (fun ctxt -> run ctxt true "bar" "class Foo { bar(): void {} }"));
         ("class_property" >:: (fun ctxt -> run ctxt true "bar" "class Foo { bar: number }"));
         ("optional_chain_new" >:: (fun ctxt -> run ctxt true "bar" "foo?.bar"));
         ("optional_chain_continued" >:: (fun ctxt -> run ctxt true "baz" "foo?.bar.baz"));
       ]
