(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let run ctxt expected name content =
  let open Parsing_service_js in
  let file = File_key.SourceFile "/dummy.js" in
  let (_docblock_errors, docblock) =
    Docblock_parser.(parse_docblock ~max_tokens:docblock_max_tokens file content)
  in
  let options =
    let options_flags = Test_utils.make_options_flags ~all:true () in
    Test_utils.make_options ~options_flags ()
  in
  let result = do_parse ~options ~docblock content file in
  let ast =
    match result with
    | Parse_ok { ast; _ } -> ast
    | Parse_recovered { ast; _ } -> ast
    | Parse_exn _ -> failwith "Parse unexpectedly failed"
    | Parse_skip _ -> failwith "Parse unexpectedly skipped"
  in
  let result = PropertyAccessSearcher.search name ast in
  assert_equal ~ctxt expected result

let tests =
  "SymbolKind"
  >::: [
         ("property_access_positive" >:: fun ctxt -> run ctxt true "bar" "foo.bar");
         ("property_access_negative" >:: fun ctxt -> run ctxt false "bar" "foo.baz");
         ("destructuring_shorthand_positive" >:: fun ctxt -> run ctxt true "bar" "const {bar} = baz");
         ( "destructuring_shorthand_negative" >:: fun ctxt ->
           run ctxt false "baz" "const {bar} = baz"
         );
         ("destructuring_positive" >:: fun ctxt -> run ctxt true "foo" "const {foo: bar} = baz");
         ("destructuring_negative" >:: fun ctxt -> run ctxt false "bar" "const {foo: bar} = baz");
         ("destructuring_negative" >:: fun ctxt -> run ctxt false "bar" "const {foo: bar} = baz");
         ("export_default_positive" >:: fun ctxt -> run ctxt true "default" "export default 5");
         ("export_default_negative" >:: fun ctxt -> run ctxt false "bar" "export default bar");
         ("import_default_positive" >:: fun ctxt -> run ctxt true "default" "import bar from 'baz'");
         ("import_default_negative" >:: fun ctxt -> run ctxt false "bar" "import bar from 'baz'");
         ("class_method" >:: fun ctxt -> run ctxt true "bar" "class Foo { bar(): void {} }");
         ("class_property" >:: fun ctxt -> run ctxt true "bar" "class Foo { bar: number }");
         ("optional_chain_new" >:: fun ctxt -> run ctxt true "bar" "foo?.bar");
         ("optional_chain_continued" >:: fun ctxt -> run ctxt true "baz" "foo?.bar.baz");
       ]
