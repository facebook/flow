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
  let extracted_statements_str =
    match Refactor_extract_function.extract_statements ast extract_range with
    | None -> None
    | Some extracted_statements ->
      Some
        ( extracted_statements
        |> Js_layout_generator.statement_list ~opts:Js_layout_generator.default_opts
        |> List.map pretty_print
        |> String.concat ""
        |> String.trim )
  in
  let expected =
    match expected with
    | None -> None
    | Some s -> Some (String.trim s)
  in
  let printer = function
    | None -> "not_allowed"
    | Some s -> s
  in
  assert_equal ~ctxt ~printer expected extracted_statements_str

let extract_statements_tests =
  [
    (* Exactly select the first statement. *)
    ( "extract_statements_linear_exact_one_statement" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        let b = 4;
        console.log("I should not be selected");
      |}
      in
      let expected = Some "const a = 3;" in
      assert_extracted
        ~ctxt
        expected
        source
        {
          Loc.source = None;
          start = { Loc.line = 2; column = 8 };
          _end = { Loc.line = 2; column = 20 };
        } );
    (* Exactly select the first two statements. *)
    ( "extract_statements_linear_exact_multiple_statements" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        let b = 4;
        console.log("I should not be selected");
      |}
      in
      let expected = Some {|
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
    (* Exactly select the first two statements, but with some whitespaces. *)
    ( "extract_statements_linear_multiple_statements_with_whitespaces" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        let b = 4;
        console.log("I should not be selected");
      |}
      in
      let expected = Some {|
const a = 3;
let b = 4;
      |} in
      assert_extracted
        ~ctxt
        expected
        source
        {
          Loc.source = None;
          start = { Loc.line = 2; column = 0 };
          _end = { Loc.line = 4; column = 0 };
        } );
    (* Partially select a statement is not alllowed. *)
    ( "extract_statements_linear_partial" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        let b = 4;
        console.log("I should not be selected");
      |}
      in
      assert_extracted
        ~ctxt
        None
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
      let expected = Some {|
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
      let expected = Some "let b = 4;" in
      assert_extracted
        ~ctxt
        expected
        source
        {
          Loc.source = None;
          start = { Loc.line = 4; column = 0 };
          _end = { Loc.line = 4; column = 20 };
        } );
    (* Selecting part of the statements from a nested block is not allowed *)
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
      assert_extracted
        ~ctxt
        None
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

let assert_refactored ~ctxt expected source extract_range =
  let ast = parse source in
  let actual =
    Refactor_extract_function.provide_available_refactors ast extract_range
    |> List.map (fun (title, ast') ->
           ( title,
             ast'
             |> Js_layout_generator.program
                  ~opts:Js_layout_generator.default_opts
                  ~preserve_docblock:true
                  ~checksum:None
             |> pretty_print
             |> String.trim ))
  in
  let expected : (string * string) list =
    List.map (fun (title, s) -> (title, String.trim s)) expected
  in
  let printer refactors =
    refactors
    |> List.map (fun (title, program_string) -> Printf.sprintf "// %s\n\n%s\n" title program_string)
    |> String.concat "\n"
  in
  assert_equal ~ctxt ~printer expected actual

let provide_available_refactor_tests =
  [
    ( "single_line_extract" >:: fun ctxt ->
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
        [
          ( "Extract to function in module scope",
            {|
function test() {
  console.log("I should not be selected");
  newFunction();
  let b = 4;
  console.log("I should not be selected");
}

function newFunction() {
  const a = 3;
}
      |}
          );
          ( "Extract to inner function in function 'test'",
            {|
function test() {
  console.log("I should not be selected");
  newFunction();
  let b = 4;
  console.log("I should not be selected");
  function newFunction() {
    const a = 3;
  }
}
|}
          );
        ]
      in
      assert_refactored
        ~ctxt
        expected
        source
        {
          Loc.source = None;
          start = { Loc.line = 4; column = 10 };
          _end = { Loc.line = 4; column = 22 };
        } );
    ( "multi_line_extract" >:: fun ctxt ->
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
        [
          ( "Extract to function in module scope",
            {|
function test() {
  console.log("I should not be selected");
  newFunction();
  console.log("I should not be selected");
}

function newFunction() {
  const a = 3;
  let b = 4;
  let c = 5;
  let d = 6;
}
      |}
          );
          ( "Extract to inner function in function 'test'",
            {|
function test() {
  console.log("I should not be selected");
  newFunction();
  console.log("I should not be selected");
  function newFunction() {
    const a = 3;
    let b = 4;
    let c = 5;
    let d = 6;
  }
}
|}
          );
        ]
      in
      assert_refactored
        ~ctxt
        expected
        source
        {
          Loc.source = None;
          start = { Loc.line = 4; column = 10 };
          _end = { Loc.line = 7; column = 20 };
        } );
    ( "very_nested_extract" >:: fun ctxt ->
      let source =
        {|
        function level1() {
          function level2() {
            function level3() {
              function level4() {
                console.log("I should not be selected");
                const a = 3;
                let b = 4;
                let c = 5;
                let d = 6;
                console.log("I should not be selected");
              }
            }
          }
        }
      |}
      in
      let expected =
        [
          ( "Extract to function in module scope",
            {|
function level1() {
  function level2() {
    function level3() {
      function level4() {
        console.log("I should not be selected");
        newFunction();
        console.log("I should not be selected");
      }
    }
  }
}

function newFunction() {
  const a = 3;
  let b = 4;
  let c = 5;
  let d = 6;
}
|}
          );
          ( "Extract to inner function in function 'level4'",
            {|
function level1() {
  function level2() {
    function level3() {
      function level4() {
        console.log("I should not be selected");
        newFunction();
        console.log("I should not be selected");
        function newFunction() {
          const a = 3;
          let b = 4;
          let c = 5;
          let d = 6;
        }
      }
    }
  }
}
|}
          );
          ( "Extract to inner function in function 'level3'",
            {|
function level1() {
  function level2() {
    function level3() {
      function level4() {
        console.log("I should not be selected");
        newFunction();
        console.log("I should not be selected");
      }
      function newFunction() {
        const a = 3;
        let b = 4;
        let c = 5;
        let d = 6;
      }
    }
  }
}
|}
          );
          ( "Extract to inner function in function 'level2'",
            {|
function level1() {
  function level2() {
    function level3() {
      function level4() {
        console.log("I should not be selected");
        newFunction();
        console.log("I should not be selected");
      }
    }
    function newFunction() {
      const a = 3;
      let b = 4;
      let c = 5;
      let d = 6;
    }
  }
}
|}
          );
          ( "Extract to inner function in function 'level1'",
            {|
function level1() {
  function level2() {
    function level3() {
      function level4() {
        console.log("I should not be selected");
        newFunction();
        console.log("I should not be selected");
      }
    }
  }
  function newFunction() {
    const a = 3;
    let b = 4;
    let c = 5;
    let d = 6;
  }
}
|}
          );
        ]
      in
      assert_refactored
        ~ctxt
        expected
        source
        {
          Loc.source = None;
          start = { Loc.line = 7; column = 16 };
          _end = { Loc.line = 10; column = 26 };
        } );
  ]

let tests =
  "refactor_extract_function"
  >::: [
         "extract_statements" >::: extract_statements_tests;
         "create_extracted_function" >::: create_extracted_function_tests;
         "provide_available_refactor" >::: provide_available_refactor_tests;
       ]
