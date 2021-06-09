(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let parse contents =
  let (ast, _) =
    Parser_flow.program ~parse_options:(Some Parser_env.default_parse_options) contents
  in
  ast

let pretty_print layout =
  let source = Pretty_printer.print ~source_maps:None ~skip_endline:true layout in
  Source.contents source

let assert_extracted ~ctxt expected source extract_range =
  let ast = parse source in
  let extracted_statements = Refactor_extract_function.extract_statements ast extract_range in
  let extracted_statements_str =
    extracted_statements
    |> Js_layout_generator.statement_list ~opts:Js_layout_generator.default_opts
    |> List.map pretty_print
    |> String.concat ""
    |> String.trim
  in
  assert_equal ~ctxt ~printer:(fun x -> x) (String.trim expected) extracted_statements_str

let extract_statements_tests =
  [
    (* Exactly select the first two statements. *)
    ( "extract_statements_linear_exact" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        let b = 4;
        console.log("I should not be selected");
      |}
      in
      let expected = {|
const a = 3;
let b = 4;
      |} in
      assert_extracted
        ~ctxt
        expected
        source
        {
          Loc.source = None;
          start = { Loc.line = 2; column = 8 };
          _end = { Loc.line = 3; column = 18 };
        } );
    (* Same as above, but move the start forwardso that `const a = 3;` is not selected in full. *)
    ( "extract_statements_linear_partial" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        let b = 4;
        console.log("I should not be selected");
      |}
      in
      let expected = "let b = 4;" in
      assert_extracted
        ~ctxt
        expected
        source
        {
          Loc.source = None;
          start = { Loc.line = 2; column = 10 };
          _end = { Loc.line = 3; column = 18 };
        } );
    (* Exactly select the first 3 statements, where the second one is a nested block. *)
    ( "extract_statements_with_nested" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        {
          let b = 4;
        }
        foo(a + b);
        console.log("I should not be selected");
      |}
      in
      let expected = {|
const a = 3;
{
  let b = 4;
}
foo(a + b);
      |} in
      assert_extracted
        ~ctxt
        expected
        source
        {
          Loc.source = None;
          start = { Loc.line = 2; column = 8 };
          _end = { Loc.line = 6; column = 19 };
        } );
    (* Select statements from nested block *)
    ( "extract_statements_from_nested" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        {
          let b = 4;
        }
        foo(a + b);
        console.log("I should not be selected");
      |}
      in
      let expected = "let b = 4;" in
      assert_extracted
        ~ctxt
        expected
        source
        {
          Loc.source = None;
          start = { Loc.line = 4; column = 0 };
          _end = { Loc.line = 4; column = 20 };
        } );
    (* Select part of the statements from a nested block.
       Partially selected block should be dropped. *)
    ( "extract_statements_from_nested_partial" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        {
          let b = 4;
          foo(a + b);
        }
        console.log("I should not be selected");
      |}
      in
      let expected = "const a = 3;" in
      assert_extracted
        ~ctxt
        expected
        source
        {
          Loc.source = None;
          start = { Loc.line = 2; column = 8 };
          _end = { Loc.line = 4; column = 20 };
        } );
  ]

let assert_allow_refactor_extraction ~ctxt expected source extract_range =
  let ast = parse source in
  let extracted_statements = Refactor_extract_function.extract_statements ast extract_range in
  let actual =
    Refactor_extract_function.allow_refactor_extraction ast extract_range extracted_statements
  in
  assert_equal ~ctxt expected actual

let allow_refactor_extraction_tests =
  [
    ( "allow_refactor_extraction_linear" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        let b = 4;
        console.log("I should not be selected");
      |}
      in
      (* Exact match should be allowed *)
      assert_allow_refactor_extraction
        ~ctxt
        true
        source
        {
          Loc.source = None;
          start = { Loc.line = 2; column = 8 };
          _end = { Loc.line = 3; column = 18 };
        };
      (* Include a few whitespaces before and after, and it should still be allowed. *)
      assert_allow_refactor_extraction
        ~ctxt
        true
        source
        {
          Loc.source = None;
          start = { Loc.line = 2; column = 8 };
          _end = { Loc.line = 4; column = 2 };
        };
      (* Partially selected statements are not allowed. *)
      assert_allow_refactor_extraction
        ~ctxt
        false
        source
        {
          Loc.source = None;
          start = { Loc.line = 2; column = 8 };
          _end = { Loc.line = 4; column = 20 };
        } );
    ( "allow_refactor_extraction_nested" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        {
          let b = 4;
          foo(a + b);
        }
        console.log("I should not be selected");
      |}
      in
      (* Selecting full block is allowed. *)
      assert_allow_refactor_extraction
        ~ctxt
        false
        source
        {
          Loc.source = None;
          start = { Loc.line = 2; column = 8 };
          _end = { Loc.line = 5; column = 9 };
        };
      (* Partially selecting a block is not allowed *)
      assert_allow_refactor_extraction
        ~ctxt
        false
        source
        {
          Loc.source = None;
          start = { Loc.line = 2; column = 8 };
          _end = { Loc.line = 4; column = 20 };
        } );
  ]

let create_extracted_function_tests =
  [
    ( "create_extracted_function_basic" >:: fun ctxt ->
      let statements =
        [
          (Loc.none, Flow_ast.Statement.(Break { Break.label = None; comments = None }));
          (Loc.none, Flow_ast.Statement.(Continue { Continue.label = None; comments = None }));
        ]
      in
      let generated_function_string =
        ( Loc.none,
          Flow_ast.Statement.FunctionDeclaration
            (Refactor_extract_function.create_extracted_function statements) )
        |> Js_layout_generator.statement ~opts:Js_layout_generator.default_opts
        |> pretty_print
      in
      let expected_function_string =
        String.trim {|
function newFunction() {
  break;
  continue;
}
      |}
      in
      assert_equal ~ctxt ~printer:(fun x -> x) expected_function_string generated_function_string );
  ]

let assert_replaced ~ctxt expected source extracted_statements_locations =
  let ast = parse source in
  let actual =
    match
      Refactor_extract_function.replace_statements_with_new_function_call
        ast
        extracted_statements_locations
    with
    | None -> ""
    | Some ast' ->
      ast'
      |> Js_layout_generator.program
           ~opts:Js_layout_generator.default_opts
           ~preserve_docblock:true
           ~checksum:None
      |> pretty_print
      |> String.trim
  in
  let expected =
    match expected with
    | None -> ""
    | Some s -> String.trim s
  in
  assert_equal ~ctxt ~printer:(fun x -> x) expected actual

let replace_statements_with_new_function_call_tests =
  [
    ( "replace_statements_with_new_function_call_one_statement" >:: fun ctxt ->
      let source =
        {|
        function test() {
          console.log("I should not be selected");
          const a = 3;
          let b = 4;
          console.log("I should not be selected");
        }
      |}
      in
      let expected =
        {|
function test() {
  console.log("I should not be selected");
  newFunction();
  let b = 4;
  console.log("I should not be selected");
}
      |}
      in
      assert_replaced
        ~ctxt
        (Some expected)
        source
        [
          {
            Loc.source = None;
            start = { Loc.line = 4; column = 10 };
            _end = { Loc.line = 4; column = 22 };
          };
        ] );
    ( "replace_statements_with_new_function_call_multiple_statements" >:: fun ctxt ->
      let source =
        {|
        function test() {
          console.log("I should not be selected");
          const a = 3;
          let b = 4;
          let c = 5;
          let d = 6;
          console.log("I should not be selected");
        }
      |}
      in
      let expected =
        {|
function test() {
  console.log("I should not be selected");
  newFunction();
  console.log("I should not be selected");
}
      |}
      in
      assert_replaced
        ~ctxt
        (Some expected)
        source
        [
          {
            Loc.source = None;
            start = { Loc.line = 4; column = 10 };
            _end = { Loc.line = 4; column = 22 };
          };
          {
            Loc.source = None;
            start = { Loc.line = 5; column = 10 };
            _end = { Loc.line = 5; column = 20 };
          };
          {
            Loc.source = None;
            start = { Loc.line = 6; column = 10 };
            _end = { Loc.line = 6; column = 20 };
          };
          {
            Loc.source = None;
            start = { Loc.line = 7; column = 10 };
            _end = { Loc.line = 7; column = 20 };
          };
        ] );
  ]

let tests =
  "refactor_extract_function"
  >::: [
         "extract_statements" >::: extract_statements_tests;
         "allow_refactor_extraction" >::: allow_refactor_extraction_tests;
         "create_extracted_function" >::: create_extracted_function_tests;
         "replace_statements_with_new_function_call"
         >::: replace_statements_with_new_function_call_tests;
       ]
