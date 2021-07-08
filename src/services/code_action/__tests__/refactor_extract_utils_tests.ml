(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Refactor_extract_utils

let parse contents =
  let (ast, _) =
    Parser_flow.program ~parse_options:(Some Parser_env.default_parse_options) contents
  in
  ast

let pretty_print layout =
  let source = Pretty_printer.print ~source_maps:None ~skip_endline:true layout in
  Source.contents source

let extract_statements_tests =
  let assert_extracted ~ctxt expected source extract_range =
    let ast = parse source in
    let extracted_statements_str =
      match StatementsExtractor.extract ast extract_range with
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
  in
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

let find_closest_enclosing_class_scope_tests =
  let assert_closest_enclosing_class_scope ~ctxt ?expected source extracted_statements_loc =
    let ast = parse source in
    let actual =
      LocationCollectors.find_closest_enclosing_class_scope ~ast ~extracted_statements_loc
    in
    let printer = function
      | None -> "None"
      | Some (id, loc) ->
        Printf.sprintf "%s: %s" (Option.value ~default:"anonymous" id) (Loc.show loc)
    in
    assert_equal ~ctxt ~printer expected actual
  in
  let source =
    {|
console.log("0");
export default class {
  test1() {
    console.log("1");
    class Level2 {
      test2() {
        console.log("2");
      }
    }
  }
}
  |}
  in
  [
    ( "without_enclosing_class_scope" >:: fun ctxt ->
      assert_closest_enclosing_class_scope
        ~ctxt
        source
        {
          Loc.source = None;
          start = { Loc.line = 2; column = 0 };
          _end = { Loc.line = 2; column = 17 };
        } );
    ( "mid_enclosing_class_scope" >:: fun ctxt ->
      assert_closest_enclosing_class_scope
        ~ctxt
        ~expected:
          ( None,
            {
              Loc.source = None;
              start = { Loc.line = 3; column = 21 };
              _end = { Loc.line = 12; column = 1 };
            } )
        source
        {
          Loc.source = None;
          start = { Loc.line = 5; column = 4 };
          _end = { Loc.line = 10; column = 5 };
        } );
    ( "inner_most_enclosing_class_scope" >:: fun ctxt ->
      assert_closest_enclosing_class_scope
        ~ctxt
        ~expected:
          ( Some "Level2",
            {
              Loc.source = None;
              start = { Loc.line = 6; column = 17 };
              _end = { Loc.line = 10; column = 5 };
            } )
        source
        {
          Loc.source = None;
          start = { Loc.line = 8; column = 8 };
          _end = { Loc.line = 8; column = 25 };
        } );
  ]

let collect_relevant_defs_with_scope_tests =
  let assert_relevant_defs
      ~ctxt
      ~expected_defs_of_local_uses
      ~expected_vars_with_shadowed_local_reassignments
      source
      extracted_statements_loc =
    let ast = parse source in
    let (scope_info, ssa_values) = Ssa_builder.program_with_scope ast in
    let { VariableAnalysis.defs_with_scopes_of_local_uses; vars_with_shadowed_local_reassignments }
        =
      VariableAnalysis.collect_relevant_defs_with_scope
        ~scope_info
        ~ssa_values
        ~extracted_statements_loc
    in
    let actual_defs_of_local_uses =
      defs_with_scopes_of_local_uses
      |> List.map (fun ({ Scope_api.Def.actual_name; _ }, { Scope_api.Scope.defs; _ }) ->
             assert_bool "scope does not contain def" (SMap.mem actual_name defs);
             actual_name)
      |> List.sort String.compare
    in
    let actual_vars_with_shadowed_local_reassignments =
      List.sort String.compare vars_with_shadowed_local_reassignments
    in
    let expected_defs_of_local_uses = List.sort String.compare expected_defs_of_local_uses in
    let expected_vars_with_shadowed_local_reassignments =
      List.sort String.compare expected_vars_with_shadowed_local_reassignments
    in
    let printer = String.concat ", " in
    assert_equal ~ctxt ~printer expected_defs_of_local_uses actual_defs_of_local_uses;
    assert_equal
      ~ctxt
      ~printer
      expected_vars_with_shadowed_local_reassignments
      actual_vars_with_shadowed_local_reassignments
  in
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
        ~expected_defs_of_local_uses:["a"; "b"]
        ~expected_vars_with_shadowed_local_reassignments:[]
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
        ~expected_defs_of_local_uses:["a"; "b"]
        ~expected_vars_with_shadowed_local_reassignments:[]
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
        ~expected_defs_of_local_uses:["a"; "b"; "c"; "d"; "e"]
        ~expected_vars_with_shadowed_local_reassignments:[]
        source
        {
          Loc.source = None;
          start = { Loc.line = 10; column = 16 };
          _end = { Loc.line = 10; column = 49 };
        } );
    ( "basic_def_with_shadowed_local_reassignment" >:: fun ctxt ->
      let source =
        {|
        let a = 4;
        a = 3; // selected
        console.log(a);
      |}
      in
      assert_relevant_defs
        ~ctxt
        ~expected_defs_of_local_uses:["a"]
        ~expected_vars_with_shadowed_local_reassignments:["a"]
        source
        {
          Loc.source = None;
          start = { Loc.line = 3; column = 8 };
          _end = { Loc.line = 3; column = 26 };
        } );
    ( "basic_def_without_shadowed_local_reassignment" >:: fun ctxt ->
      let source =
        {|
        let a = 4; // selected
        a = 3; // selected
        console.log(a);
      |}
      in
      assert_relevant_defs
        ~ctxt
        ~expected_defs_of_local_uses:["a"]
        ~expected_vars_with_shadowed_local_reassignments:[]
        source
        {
          Loc.source = None;
          start = { Loc.line = 2; column = 8 };
          _end = { Loc.line = 3; column = 26 };
        } );
  ]

let undefined_variables_after_extraction_tests =
  let assert_undefined_variables
      ~ctxt ~expected ~source ~new_function_target_scope_loc ~extracted_statements_loc =
    let ast = parse source in
    let (scope_info, ssa_values) = Ssa_builder.program_with_scope ast in
    let { VariableAnalysis.defs_with_scopes_of_local_uses; _ } =
      VariableAnalysis.collect_relevant_defs_with_scope
        ~scope_info
        ~ssa_values
        ~extracted_statements_loc
    in
    let actual =
      VariableAnalysis.undefined_variables_after_extraction
        ~scope_info
        ~defs_with_scopes_of_local_uses
        ~new_function_target_scope_loc
        ~extracted_statements_loc
      |> List.sort String.compare
    in
    let expected = List.sort String.compare expected in
    assert_equal ~ctxt ~printer:(String.concat ", ") expected actual
  in
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

let escaping_locally_defined_variables_tests =
  let assert_escaping_locally_defined_variables
      ~ctxt ~expected_variables ~expected_has_external_writes source extracted_statements_loc =
    let ast = parse source in
    let (scope_info, ssa_values) = Ssa_builder.program_with_scope ast in
    let { VariableAnalysis.escaping_variables; has_external_writes = actual_has_external_writes } =
      VariableAnalysis.collect_escaping_local_defs ~scope_info ~ssa_values ~extracted_statements_loc
    in
    let actual_variables = List.sort String.compare escaping_variables in
    let expected_variables = List.sort String.compare expected_variables in
    assert_equal ~ctxt ~printer:(String.concat ", ") expected_variables actual_variables;
    assert_equal
      ~ctxt
      ~printer:string_of_bool
      expected_has_external_writes
      actual_has_external_writes
  in
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
        ~expected_variables:[]
        ~expected_has_external_writes:false
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
        ~expected_variables:["a"; "b"]
        ~expected_has_external_writes:false
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
        ~expected_variables:["test"]
        ~expected_has_external_writes:false
        source
        {
          Loc.source = None;
          start = { Loc.line = 3; column = 8 };
          _end = { Loc.line = 5; column = 37 };
        } );
    ( "escaping_with_hoisting_in_function" >:: fun ctxt ->
      let source =
        {|
        function foo() {
          console.log(test());
          // selected start
          const a = 3;
          function test() { return a; }
          // selected end
        }
      |}
      in
      assert_escaping_locally_defined_variables
        ~ctxt
        ~expected_variables:["test"]
        ~expected_has_external_writes:false
        source
        {
          Loc.source = None;
          start = { Loc.line = 4; column = 10 };
          _end = { Loc.line = 6; column = 39 };
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
        ~expected_variables:[]
        ~expected_has_external_writes:false
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
        ~expected_variables:["c"]
        ~expected_has_external_writes:false
        source
        {
          Loc.source = None;
          start = { Loc.line = 4; column = 22 };
          _end = { Loc.line = 6; column = 26 };
        } );
    ( "escaping_with_external_assignments" >:: fun ctxt ->
      let source =
        {|
        let a = 3, b = 2; // selected
        a = 2;
        console.log(b);
      |}
      in
      assert_escaping_locally_defined_variables
        ~ctxt
        ~expected_variables:["a"; "b"]
        ~expected_has_external_writes:true
        source
        {
          Loc.source = None;
          start = { Loc.line = 2; column = 8 };
          _end = { Loc.line = 2; column = 37 };
        } );
  ]

let tests =
  "refactor_extract_utils"
  >::: [
         "extract_statements" >::: extract_statements_tests;
         "find_closest_enclosing_class_scope" >::: find_closest_enclosing_class_scope_tests;
         "collect_relevant_defs_with_scope" >::: collect_relevant_defs_with_scope_tests;
         "undefined_variables_after_extraction" >::: undefined_variables_after_extraction_tests;
         "escaping_locally_defined_variables" >::: escaping_locally_defined_variables_tests;
       ]
