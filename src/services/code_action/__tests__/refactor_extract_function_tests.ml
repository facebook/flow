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

let assert_relevant_defs ~ctxt ~expected source extracted_statements_loc =
  let ast = parse source in
  let scope_info = Scope_builder.program ~with_types:false ast in
  let actual =
    Refactor_extract_function.collect_relevant_defs_with_scope ~scope_info ~extracted_statements_loc
    |> List.map
         (fun ({ Scope_api.With_Loc.Def.actual_name; _ }, { Scope_api.With_Loc.Scope.defs; _ }) ->
           assert_bool "scope does not contain def" (SMap.mem actual_name defs);
           actual_name)
    |> List.sort String.compare
  in
  let expected = List.sort String.compare expected in
  assert_equal ~ctxt ~printer:(String.concat ", ") expected actual

let collect_relevant_defs_with_scope_tests =
  [
    ( "used_defs_toplevel" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        let b = 4;
        console.log(a + b);
      |}
      in
      assert_relevant_defs
        ~ctxt
        ~expected:["a"; "b"]
        source
        {
          Loc.source = None;
          start = { Loc.line = 4; column = 8 };
          _end = { Loc.line = 4; column = 27 };
        } );
    ( "used_defs_in_function" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        let b = 4;
        function test() {
          return a + b;
        }
        console.log("I should not be selected");
      |}
      in
      assert_relevant_defs
        ~ctxt
        ~expected:["a"; "b"]
        source
        {
          Loc.source = None;
          start = { Loc.line = 5; column = 10 };
          _end = { Loc.line = 5; column = 23 };
        } );
    ( "used_defs_in_many_scopes" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        function level1() {
          const b = 4;
          function level2() {
            function level3() {
              const d = 6;
              function level4() {
                const e = 7;
                return a + b + c + d + e;
              }
            }
            var c = 5;
          }
        }
        console.log("I should not be selected");
      |}
      in
      assert_relevant_defs
        ~ctxt
        ~expected:["a"; "b"; "c"; "d"; "e"]
        source
        {
          Loc.source = None;
          start = { Loc.line = 10; column = 16 };
          _end = { Loc.line = 10; column = 49 };
        } );
  ]

let assert_undefined_variables
    ~ctxt ~expected ~source ~new_function_target_scope_loc ~extracted_statements_loc =
  let ast = parse source in
  let scope_info = Scope_builder.program ~with_types:false ast in
  let relevant_defs_with_scope =
    Refactor_extract_function.collect_relevant_defs_with_scope ~scope_info ~extracted_statements_loc
  in
  let actual =
    Refactor_extract_function.undefined_variables_after_extraction
      ~scope_info
      ~relevant_defs_with_scope
      ~new_function_target_scope_loc
      ~extracted_statements_loc
    |> List.sort String.compare
  in
  let expected = List.sort String.compare expected in
  assert_equal ~ctxt ~printer:(String.concat ", ") expected actual

let undefined_variables_after_extraction_tests =
  [
    ( "basic_function_extract_to_toplevel" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        let b = a;
        function test() {
          const c = 4;
          console.log(b + c);
        }
      |}
      in
      assert_undefined_variables
        ~ctxt
        ~expected:["c"]
        ~source
        ~new_function_target_scope_loc:
          {
            Loc.source = None;
            start = { Loc.line = 2; column = 8 };
            _end = { Loc.line = 7; column = 9 };
          }
        ~extracted_statements_loc:
          {
            Loc.source = None;
            start = { Loc.line = 6; column = 10 };
            _end = { Loc.line = 6; column = 29 };
          } );
    ( "basic_function_extract_to_function" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        let b = a;
        function test() {
          const c = 4;
          console.log(b + c);
        }
      |}
      in
      assert_undefined_variables
        ~ctxt
        ~expected:[]
        ~source
        ~new_function_target_scope_loc:
          {
            Loc.source = None;
            start = { Loc.line = 4; column = 24 };
            _end = { Loc.line = 7; column = 9 };
          }
        ~extracted_statements_loc:
          {
            Loc.source = None;
            start = { Loc.line = 6; column = 10 };
            _end = { Loc.line = 6; column = 29 };
          } );
    ( "deeply_nested_extract_to_inner_function" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        let b = a;
        function level1() {
          const c = 4;
          function level2() {
            const e = d;
            {
              const f = 3;
              const level3 = () => {
                return b + c + d + e + f + level2() + level3();
              }
            }
          }
          var d = b + c;
          return level2();
        }
      |}
      in
      assert_undefined_variables
        ~ctxt
        ~expected:["f"; "level3"]
        ~source
        ~new_function_target_scope_loc:
          {
            Loc.source = None;
            start = { Loc.line = 6; column = 28 };
            _end = { Loc.line = 14; column = 11 };
          }
        ~extracted_statements_loc:
          {
            Loc.source = None;
            start = { Loc.line = 11; column = 16 };
            _end = { Loc.line = 11; column = 63 };
          } );
  ]

let assert_escaping_locally_defined_variables ~ctxt ~expected source extracted_statements_loc =
  let ast = parse source in
  let scope_info = Scope_builder.program ~with_types:false ast in
  let actual =
    Refactor_extract_function.collect_escaping_local_defs ~scope_info ~extracted_statements_loc
    |> List.sort String.compare
  in
  let expected = List.sort String.compare expected in
  assert_equal ~ctxt ~printer:(String.concat ", ") expected actual

let escaping_locally_defined_variables_tests =
  [
    ( "non_escaping" >:: fun ctxt ->
      let source =
        {|
        // selected start
        const a = 3;
        const b = a;
        // selected end
        console.log("not using a and b");
      |}
      in
      assert_escaping_locally_defined_variables
        ~ctxt
        ~expected:[]
        source
        {
          Loc.source = None;
          start = { Loc.line = 2; column = 8 };
          _end = { Loc.line = 4; column = 20 };
        } );
    ( "all_escaping" >:: fun ctxt ->
      let source =
        {|
        // selected start
        const a = 3;
        const b = a;
        // selected end
        console.log(a+b);
      |}
      in
      assert_escaping_locally_defined_variables
        ~ctxt
        ~expected:["a"; "b"]
        source
        {
          Loc.source = None;
          start = { Loc.line = 2; column = 8 };
          _end = { Loc.line = 4; column = 20 };
        } );
    ( "escaping_with_hoisting" >:: fun ctxt ->
      let source =
        {|
        console.log(test());
        // selected start
        const a = 3;
        function test() { return a; }
        // selected end
      |}
      in
      assert_escaping_locally_defined_variables
        ~ctxt
        ~expected:["test"]
        source
        {
          Loc.source = None;
          start = { Loc.line = 3; column = 8 };
          _end = { Loc.line = 5; column = 37 };
        } );
    ( "escaping_with_shadowing" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        function test() { const a = 3; return a; }
      |}
      in
      assert_escaping_locally_defined_variables
        ~ctxt
        ~expected:[]
        source
        {
          Loc.source = None;
          start = { Loc.line = 2; column = 8 };
          _end = { Loc.line = 2; column = 20 };
        } );
    ( "escaping_with_hoising_shadowing_nested_scopes" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        function level1() {
          const b = 4;
          const c = b + 4;
          const d = c + b;
          {
            return c + level2();
          }

          function level2() {
            const d = 5;
            return c + d;
          }
        }
      |}
      in
      assert_escaping_locally_defined_variables
        ~ctxt
        ~expected:["c"]
        source
        {
          Loc.source = None;
          start = { Loc.line = 4; column = 22 };
          _end = { Loc.line = 6; column = 26 };
        } );
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
    ( "very_nested_extract_with_variables" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        function level1() {
          let b = 4;
          function level2() {
            let c = 5;
            function level3() {
              let d = 6;
              {
                let e = 7;
                function level4() {
                  let f = 8;
                  console.log(a + b + c + d + e + f);
                }
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
const a = 3;
function level1() {
  let b = 4;
  function level2() {
    let c = 5;
    function level3() {
      let d = 6;
      {
        let e = 7;
        function level4() {
          let f = 8;
          newFunction(b, c, d, e, f);
        }
      }
    }
  }
}

function newFunction(b, c, d, e, f) {
  console.log(a + b + c + d + e + f);
}
|}
          );
          ( "Extract to inner function in function 'level4'",
            {|
const a = 3;
function level1() {
  let b = 4;
  function level2() {
    let c = 5;
    function level3() {
      let d = 6;
      {
        let e = 7;
        function level4() {
          let f = 8;
          newFunction();
          function newFunction() {
            console.log(a + b + c + d + e + f);
          }
        }
      }
    }
  }
}
|}
          );
          ( "Extract to inner function in function 'level3'",
            {|
const a = 3;
function level1() {
  let b = 4;
  function level2() {
    let c = 5;
    function level3() {
      let d = 6;
      {
        let e = 7;
        function level4() {
          let f = 8;
          newFunction(e, f);
        }
      }
      function newFunction(e, f) {
        console.log(a + b + c + d + e + f);
      }
    }
  }
}
|}
          );
          ( "Extract to inner function in function 'level2'",
            {|
const a = 3;
function level1() {
  let b = 4;
  function level2() {
    let c = 5;
    function level3() {
      let d = 6;
      {
        let e = 7;
        function level4() {
          let f = 8;
          newFunction(d, e, f);
        }
      }
    }
    function newFunction(d, e, f) {
      console.log(a + b + c + d + e + f);
    }
  }
}
|}
          );
          ( "Extract to inner function in function 'level1'",
            {|
const a = 3;
function level1() {
  let b = 4;
  function level2() {
    let c = 5;
    function level3() {
      let d = 6;
      {
        let e = 7;
        function level4() {
          let f = 8;
          newFunction(c, d, e, f);
        }
      }
    }
  }
  function newFunction(c, d, e, f) {
    console.log(a + b + c + d + e + f);
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
          start = { Loc.line = 13; column = 18 };
          _end = { Loc.line = 13; column = 53 };
        } );
  ]

let tests =
  "refactor_extract_function"
  >::: [
         "extract_statements" >::: extract_statements_tests;
         "collect_relevant_defs_with_scope" >::: collect_relevant_defs_with_scope_tests;
         "undefined_variables_after_extraction" >::: undefined_variables_after_extraction_tests;
         "escaping_locally_defined_variables" >::: escaping_locally_defined_variables_tests;
         "provide_available_refactor" >::: provide_available_refactor_tests;
       ]
