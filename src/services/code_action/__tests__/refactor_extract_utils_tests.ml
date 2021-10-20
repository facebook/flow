(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Refactor_extract_utils
open Loc_collections

let parse contents =
  let (ast, _) =
    Parser_flow.program
      ~parse_options:
        (Some Parser_env.{ default_parse_options with esproposal_class_instance_fields = true })
      contents
  in
  ast

let pretty_print layout =
  let source = Pretty_printer.print ~source_maps:None ~skip_endline:true layout in
  Source.contents source

let stub_metadata ~root ~checked =
  {
    Context.checked (* local *);
    munge_underscores = false;
    verbose = None;
    weak = false;
    jsx = Options.Jsx_react;
    strict = false;
    strict_local = false;
    include_suppressions = false;
    (* global *)
    automatic_require_default = false;
    babel_loose_array_spread = false;
    max_literal_length = 100;
    enable_const_params = false;
    enable_enums = true;
    enable_indexed_access = true;
    enable_relay_integration = false;
    enforce_local_inference_annotations = false;
    local_inference_annotation_dirs = [];
    experimental_infer_indexers = false;
    enforce_strict_call_arity = true;
    exact_by_default = false;
    facebook_fbs = None;
    facebook_fbt = None;
    facebook_module_interop = false;
    haste_module_ref_prefix = None;
    ignore_non_literal_requires = false;
    max_trace_depth = 0;
    max_workers = 0;
    react_runtime = Options.ReactRuntimeClassic;
    react_server_component_exts = SSet.empty;
    recursion_limit = 10000;
    relay_integration_module_prefix = None;
    reorder_checking = Options.Lexical;
    root;
    run_post_inference_implicit_instantiation = false;
    strict_es6_import_export = false;
    strict_es6_import_export_excludes = [];
    strip_root = true;
    suppress_types = SSet.empty;
    trust_mode = Options.NoTrust;
    type_asserts = false;
    env_mode = Options.ClassicEnv [];
    env_mode_constrain_write_dirs = [];
  }

let dummy_filename = File_key.SourceFile ""

let file_sig_of_ast ast =
  match File_sig.With_Loc.program ~ast ~opts:File_sig.With_Loc.default_opts with
  | Ok (a, _) -> File_sig.abstractify_locs a
  | Error _ -> failwith "failed to construct file signature"

let dummy_context =
  let root = Path.dummy_path in
  let master_cx = Context.empty_master_cx () in
  let () =
    let reason =
      let loc = ALoc.none in
      let desc = Reason.RCustom "Explicit any used in refactor_extract_functioon tests" in
      Reason.mk_reason desc loc
    in
    (* Add builtins that will be used by tests. *)
    Builtins.set_builtin
      ~flow_t:(fun _ -> ())
      master_cx.Context.builtins
      (Reason.OrdinaryName "console")
      (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)));
    Builtins.set_builtin
      ~flow_t:(fun _ -> ())
      master_cx.Context.builtins
      (Reason.OrdinaryName "Object")
      (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)));
    Builtins.set_builtin
      ~flow_t:(fun _ -> ())
      master_cx.Context.builtins
      (Reason.OrdinaryName "Generator")
      (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)));
    Builtins.set_builtin
      ~flow_t:(fun _ -> ())
      master_cx.Context.builtins
      (Reason.OrdinaryName "Promise")
      (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)));
    Builtins.set_builtin
      ~flow_t:(fun _ -> ())
      master_cx.Context.builtins
      (Reason.OrdinaryName "promise")
      (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)));
    Builtins.set_builtin
      ~flow_t:(fun _ -> ())
      master_cx.Context.builtins
      (Reason.OrdinaryName "$await")
      (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)));
    Builtins.set_builtin
      ~flow_t:(fun _ -> ())
      master_cx.Context.builtins
      (Reason.OrdinaryName "$AsyncIterable")
      (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)))
  in
  let ccx = Context.make_ccx master_cx in
  let metadata = stub_metadata ~root ~checked:true in
  let aloc_table = lazy (ALoc.empty_table dummy_filename) in
  let module_ref = Reason.OrdinaryName (Files.module_ref dummy_filename) in
  Context.make ccx metadata dummy_filename aloc_table module_ref Context.Checking

let typed_ast_of_ast ast =
  let (_, { Flow_ast.Program.all_comments = comments; _ }) = ast in
  let aloc_ast = Ast_loc_utils.loc_to_aloc_mapper#program ast in
  Type_inference_js.infer_ast
    ~lint_severities:LintSettings.empty_severities
    dummy_context
    dummy_filename
    comments
    aloc_ast

let mk_loc = Loc.mk_loc

let extract_tests =
  let assert_extracted
      ~ctxt ?expected_statements ?expected_expression ?expected_type source extract_range =
    let ast = parse source in
    let { AstExtractor.extracted_statements; extracted_expression; extracted_type } =
      AstExtractor.extract ast extract_range
    in
    let extracted_statements_str =
      match extracted_statements with
      | None -> None
      | Some extracted_statements ->
        Some
          (extracted_statements
          |> Js_layout_generator.statement_list ~opts:Js_layout_generator.default_opts
          |> List.map pretty_print
          |> String.concat ""
          |> String.trim
          )
    in
    let actual_extracted_expression =
      match extracted_expression with
      | None -> None
      | Some { AstExtractor.constant_insertion_points; expression } ->
        Some
          ( Nel.to_list constant_insertion_points,
            expression
            |> Js_layout_generator.expression ~opts:Js_layout_generator.default_opts
            |> pretty_print
            |> String.trim
          )
    in
    let expected_extracted_expression =
      match expected_expression with
      | None -> None
      | Some (constant_insertion_points, expression) ->
        Some (constant_insertion_points, String.trim expression)
    in
    let actual_extracted_type =
      match extracted_type with
      | None -> None
      | Some { AstExtractor.directly_containing_statement_loc; type_ } ->
        Some
          ( directly_containing_statement_loc,
            type_
            |> Js_layout_generator.type_ ~opts:Js_layout_generator.default_opts
            |> pretty_print
            |> String.trim
          )
    in
    let expected_extracted_type =
      match expected_type with
      | None -> None
      | Some (directly_containing_statement_loc, type_) ->
        Some (directly_containing_statement_loc, String.trim type_)
    in
    assert_equal
      ~ctxt
      ~printer:(Option.value ~default:"statement not allowed")
      (Base.Option.map ~f:String.trim expected_statements)
      extracted_statements_str;
    assert_equal
      ~ctxt
      ~printer:
        (Base.Option.value_map ~default:"expression not allowed" ~f:(fun (points, e) ->
             (points |> List.map AstExtractor.show_constant_insertion_point |> String.concat ",")
             ^ "\n"
             ^ e
         )
        )
      expected_extracted_expression
      actual_extracted_expression;
    assert_equal
      ~ctxt
      ~printer:
        (Base.Option.value_map ~default:"type not allowed" ~f:(fun (loc, t) ->
             Loc.debug_to_string loc ^ ": " ^ t
         )
        )
      expected_extracted_type
      actual_extracted_type
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
      assert_extracted ~ctxt ~expected_statements:"const a = 3;" source (mk_loc (2, 8) (2, 20))
    );
    (* Exactly select the first two statements. *)
    ( "extract_statements_linear_exact_multiple_statements" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        let b = 4;
        console.log("I should not be selected");
      |}
      in
      let expected_statements = {|
const a = 3;
let b = 4;
      |} in
      assert_extracted ~ctxt ~expected_statements source (mk_loc (2, 8) (3, 18))
    );
    (* Exactly select the first two statements, but with some whitespaces. *)
    ( "extract_statements_linear_multiple_statements_with_whitespaces" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        let b = 4;
        console.log("I should not be selected");
      |}
      in
      let expected_statements = {|
const a = 3;
let b = 4;
      |} in
      assert_extracted ~ctxt ~expected_statements source (mk_loc (2, 0) (4, 0))
    );
    (* Partially select a statement is not alllowed. *)
    ( "extract_statements_linear_partial" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        let b = 4;
        console.log("I should not be selected");
      |}
      in
      assert_extracted ~ctxt source (mk_loc (2, 10) (3, 18))
    );
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
      let expected_statements = {|
const a = 3;
{
  let b = 4;
}
foo(a + b);
      |} in
      assert_extracted ~ctxt ~expected_statements source (mk_loc (2, 8) (6, 19))
    );
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
      assert_extracted ~ctxt ~expected_statements:"let b = 4;" source (mk_loc (4, 0) (4, 20))
    );
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
      assert_extracted ~ctxt source (mk_loc (2, 8) (4, 20))
    );
    ( "extract_expression_basic" >:: fun ctxt ->
      assert_extracted
        ~ctxt
        ~expected_expression:
          ( [
              {
                AstExtractor.title = "Extract to constant in module scope";
                function_body_loc = None;
                statement_loc = mk_loc (1, 0) (1, 11);
              };
            ],
            "1"
          )
        "const a = 1"
        (mk_loc (1, 10) (1, 11))
    );
    (* When statement selection is empty, the chosen expression is good. *)
    ( "extract_expression_nested" >:: fun ctxt ->
      assert_extracted
        ~ctxt
        ~expected_expression:
          ( [
              {
                AstExtractor.title = "Extract to constant in module scope";
                function_body_loc = None;
                statement_loc = mk_loc (1, 0) (1, 15);
              };
            ],
            "1 + 1"
          )
        "const a = 1 + 1"
        (mk_loc (1, 10) (1, 15))
    );
    (* When the selection is both an expression and a statement *)
    ( "extract_expression_statement_without_semicolon" >:: fun ctxt ->
      assert_extracted
        ~ctxt
        ~expected_statements:"\"hello\";"
        ~expected_expression:
          ( [
              {
                AstExtractor.title = "Extract to constant in module scope";
                function_body_loc = None;
                statement_loc = mk_loc (1, 0) (1, 7);
              };
            ],
            "\"hello\""
          )
        "'hello'"
        (mk_loc (1, 0) (1, 7))
    );
    (* Same as above case, but adding a semicolon makes the selection only statement. *)
    ( "extract_expression_statement_with_semicolon" >:: fun ctxt ->
      assert_extracted ~ctxt ~expected_statements:"\"hello\";" "'hello';" (mk_loc (1, 0) (1, 8))
    );
    ( "extract_expression_ban_multiple" >:: fun ctxt ->
      assert_extracted ~ctxt "const a = 1; const b = 2;" (mk_loc (1, 9) (1, 25))
    );
    (* Selecting `1;`.
       Although expression is contained, illegal selection of statement invalidates it. *)
    ( "extract_expression_ban_partial_statement" >:: fun ctxt ->
      assert_extracted ~ctxt "const a = 1; const b = 2;" (mk_loc (1, 8) (1, 11));
      assert_extracted ~ctxt "const a = 1; const b = 2;" (mk_loc (1, 10) (1, 16))
    );
    (* Expression only exists in a class, not in any statement. *)
    ( "ban_expression_only_in_class" >:: fun ctxt ->
      assert_extracted
        ~ctxt
        "class A { a = 3 }"
        ~expected_expression:
          ( [
              {
                AstExtractor.title = "Extract to constant in module scope";
                function_body_loc = None;
                statement_loc = mk_loc (1, 0) (1, 17);
              };
            ],
            "3"
          )
        (mk_loc (1, 14) (1, 15));
      assert_extracted
        ~ctxt
        "class A { constructor(a = 3) {} }"
        ~expected_expression:
          ( [
              {
                AstExtractor.title = "Extract to constant in module scope";
                function_body_loc = None;
                statement_loc = mk_loc (1, 0) (1, 33);
              };
            ],
            "3"
          )
        (mk_loc (1, 26) (1, 27))
    );
    (* Expression exists in a class, but also in a statement. *)
    ( "expression_in_class_statements" >:: fun ctxt ->
      assert_extracted
        ~ctxt
        "class A { foo() { const a = 3; } }"
        ~expected_expression:
          ( [
              {
                AstExtractor.title = "Extract to constant in method 'foo'";
                function_body_loc = Some (mk_loc (1, 16) (1, 32));
                statement_loc = mk_loc (1, 18) (1, 30);
              };
              {
                AstExtractor.title = "Extract to constant in module scope";
                function_body_loc = None;
                statement_loc = mk_loc (1, 0) (1, 34);
              };
            ],
            "3"
          )
        (mk_loc (1, 28) (1, 29))
    );
    (* Expression in many containing scopes with different names *)
    ( "expression_in_many_scopes" >:: fun ctxt ->
      let source =
        {|
        class A {
          foo() {
            function bar() {
              const a = 3;
            }
          }
        }
        |}
      in
      assert_extracted
        ~ctxt
        source
        ~expected_expression:
          ( [
              {
                AstExtractor.title = "Extract to constant in function 'bar'";
                function_body_loc = Some (mk_loc (4, 27) (6, 13));
                statement_loc = mk_loc (5, 14) (5, 26);
              };
              {
                AstExtractor.title = "Extract to constant in method 'foo'";
                function_body_loc = Some (mk_loc (3, 16) (7, 11));
                statement_loc = mk_loc (4, 12) (6, 13);
              };
              {
                AstExtractor.title = "Extract to constant in module scope";
                function_body_loc = None;
                statement_loc = mk_loc (2, 8) (8, 9);
              };
            ],
            "3"
          )
        (mk_loc (5, 24) (5, 25))
    );
    ( "extract_types" >:: fun ctxt ->
      assert_extracted
        ~ctxt
        ~expected_type:(mk_loc (1, 0) (1, 20), "number")
        "const a: number = 1;"
        (mk_loc (1, 9) (1, 15));
      assert_extracted
        ~ctxt
        ~expected_type:(mk_loc (1, 0) (1, 30), "number")
        "const a: [number, number] = 1;"
        (mk_loc (1, 10) (1, 16));
      assert_extracted
        ~ctxt
        ~expected_type:(mk_loc (1, 0) (1, 30), "[number, number]")
        "const a: [number, number] = 1;"
        (mk_loc (1, 9) (1, 25))
    );
  ]

let collect_function_method_inserting_points_tests =
  [
    ( "basic_test" >:: fun ctxt ->
      let source =
        {|
console.log("0");
function foo<A>() {
  function bar<B>() {
    class Foo {
      baz1 = () => {
        class Bar {
          baz2() {
            console.log("2"); // selected
          }
        }
      }
    }
  }
}
        |}
      in
      let typed_ast = source |> parse |> typed_ast_of_ast in
      let extracted_loc = mk_loc (9, 12) (9, 42) in
      let reader = State_reader.create () in
      let actual =
        InsertionPointCollectors.collect_function_method_inserting_points
          ~typed_ast
          ~reader
          ~extracted_loc
        |> List.map
             (fun { InsertionPointCollectors.function_name; body_loc; is_method; tparams_rev } ->
               ( function_name,
                 body_loc,
                 is_method,
                 tparams_rev |> List.map (fun { Type.name; _ } -> name)
               )
           )
      in
      let expected =
        [
          ("baz2", mk_loc (8, 17) (10, 11), true, ["B"; "A"]);
          ("baz1", mk_loc (6, 19) (12, 7), true, ["B"; "A"]);
          ("bar", mk_loc (4, 20) (14, 3), false, ["B"; "A"]);
          ("foo", mk_loc (3, 18) (15, 1), false, ["A"]);
        ]
      in
      let printer result =
        result
        |> List.map (fun (id, loc, is_method, typeparams) ->
               Printf.sprintf
                 "%s: %s: <%s>. is_method: %b"
                 id
                 (Loc.debug_to_string loc)
                 (String.concat "," typeparams)
                 is_method
           )
        |> String.concat "\n"
      in
      assert_equal ~ctxt ~printer expected actual
    );
  ]

let find_closest_enclosing_class_tests =
  let assert_closest_enclosing_class_scope ~ctxt ?expected source extracted_loc =
    let typed_ast = source |> parse |> typed_ast_of_ast in
    let reader = State_reader.create () in
    let actual =
      match
        InsertionPointCollectors.find_closest_enclosing_class ~typed_ast ~reader ~extracted_loc
      with
      | None -> None
      | Some { InsertionPointCollectors.class_name; body_loc; tparams_rev } ->
        Some (class_name, body_loc, tparams_rev |> List.map (fun { Type.name; _ } -> name))
    in
    let printer = function
      | None -> "None"
      | Some (id, loc, typeparams) ->
        Printf.sprintf
          "%s: %s: <%s>"
          (Option.value ~default:"anonymous" id)
          (Loc.show loc)
          (String.concat "," typeparams)
    in
    assert_equal ~ctxt ~printer expected actual
  in
  let source =
    {|
console.log("0");
export default class <A> {
  test1() {
    console.log("1");
    class Level2<B> {
      test2(v: B) {
        console.log("2");
      }
    }
  }
}
  |}
  in
  [
    ( "without_enclosing_class_scope" >:: fun ctxt ->
      assert_closest_enclosing_class_scope ~ctxt source (mk_loc (2, 0) (2, 17))
    );
    ( "mid_enclosing_class_scope" >:: fun ctxt ->
      assert_closest_enclosing_class_scope
        ~ctxt
        ~expected:(None, mk_loc (3, 25) (12, 1), ["A"])
        source
        (mk_loc (5, 4) (10, 5))
    );
    ( "inner_most_enclosing_class_scope" >:: fun ctxt ->
      assert_closest_enclosing_class_scope
        ~ctxt
        ~expected:(Some "Level2", mk_loc (6, 20) (10, 5), ["B"; "A"])
        source
        (mk_loc (8, 8) (8, 25))
    );
  ]

let collect_relevant_defs_with_scope_tests =
  let assert_relevant_defs
      ~ctxt
      ~expected_defs_of_local_uses
      ~expected_vars_with_shadowed_local_reassignments
      source
      extracted_loc =
    let ast = parse source in
    let (_abnormal_completion_state, (scope_info, ssa_values, _possible_globals)) =
      Ssa_builder.program_with_scope ast
    in
    let { VariableAnalysis.defs_with_scopes_of_local_uses; vars_with_shadowed_local_reassignments }
        =
      VariableAnalysis.collect_relevant_defs_with_scope ~scope_info ~ssa_values ~extracted_loc
    in
    let actual_defs_of_local_uses =
      defs_with_scopes_of_local_uses
      |> List.map (fun ({ Scope_api.Def.actual_name; _ }, { Scope_api.Scope.defs; _ }) ->
             assert_bool "scope does not contain def" (SMap.mem actual_name defs);
             actual_name
         )
      |> List.sort String.compare
    in
    let actual_vars_with_shadowed_local_reassignments =
      vars_with_shadowed_local_reassignments |> List.map fst |> List.sort String.compare
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
        (mk_loc (4, 8) (4, 27))
    );
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
        (mk_loc (5, 10) (5, 23))
    );
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
        (mk_loc (10, 16) (10, 49))
    );
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
        (mk_loc (3, 8) (3, 26))
    );
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
        (mk_loc (2, 8) (3, 26))
    );
  ]

let undefined_variables_after_extraction_tests =
  let assert_undefined_variables
      ~ctxt ~expected ~source ~new_function_target_scope_loc ~extracted_loc =
    let ast = parse source in
    let (_abnormal_completion_state, (scope_info, ssa_values, _possible_globals)) =
      Ssa_builder.program_with_scope ast
    in
    let { VariableAnalysis.defs_with_scopes_of_local_uses; _ } =
      VariableAnalysis.collect_relevant_defs_with_scope ~scope_info ~ssa_values ~extracted_loc
    in
    let actual =
      VariableAnalysis.undefined_variables_after_extraction
        ~scope_info
        ~defs_with_scopes_of_local_uses
        ~new_function_target_scope_loc
        ~extracted_loc
      |> List.map fst
      |> List.sort String.compare
    in
    let expected = List.sort String.compare expected in
    assert_equal ~ctxt ~printer:(String.concat ", ") expected actual
  in
  [
    ( "toplevel_only_extract" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        let b = a;
        console.log(b + c); // selected
      |}
      in
      assert_undefined_variables
        ~ctxt
        ~expected:[]
        ~source
        ~new_function_target_scope_loc:None
        ~extracted_loc:(mk_loc (4, 2) (4, 28))
    );
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
        ~new_function_target_scope_loc:None
        ~extracted_loc:(mk_loc (6, 10) (6, 29))
    );
    ( "extract_use_of_toplevel_function_parameter_to_toplevel" >:: fun ctxt ->
      let source =
        {|
        function test(a: number) {
          console.log(a);
        }
      |}
      in
      assert_undefined_variables
        ~ctxt
        ~expected:["a"]
        ~source
        ~new_function_target_scope_loc:None
        ~extracted_loc:(mk_loc (3, 10) (3, 25))
    );
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
        ~new_function_target_scope_loc:(Some (mk_loc (4, 24) (7, 9)))
        ~extracted_loc:(mk_loc (6, 10) (6, 29))
    );
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
        ~new_function_target_scope_loc:(Some (mk_loc (6, 28) (14, 11)))
        ~extracted_loc:(mk_loc (11, 16) (11, 63))
    );
  ]

let escaping_locally_defined_variables_tests =
  let assert_escaping_locally_defined_variables
      ~ctxt ~expected_variables ~expected_has_external_writes source extracted_statements_loc =
    let ast = parse source in
    let (_abnormal_completion_state, (scope_info, ssa_values, _possible_globals)) =
      Ssa_builder.program_with_scope ast
    in
    let { VariableAnalysis.escaping_variables; has_external_writes = actual_has_external_writes } =
      VariableAnalysis.collect_escaping_local_defs ~scope_info ~ssa_values ~extracted_statements_loc
    in
    let actual_variables = escaping_variables |> List.map fst |> List.sort String.compare in
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
        (mk_loc (2, 8) (4, 20))
    );
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
        (mk_loc (2, 8) (4, 20))
    );
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
        (mk_loc (3, 8) (5, 37))
    );
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
        (mk_loc (4, 10) (6, 39))
    );
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
        (mk_loc (2, 8) (2, 20))
    );
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
        (mk_loc (4, 22) (6, 26))
    );
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
        (mk_loc (2, 8) (2, 37))
    );
  ]

let type_synthesizer_tests =
  let reader = State_reader.create () in
  let create_context source locs =
    let ast = parse source in
    let typed_ast = typed_ast_of_ast ast in
    let file_sig = file_sig_of_ast ast in
    let locs = LocSet.of_list locs in
    TypeSynthesizer.create_synthesizer_context
      ~full_cx:dummy_context
      ~file:dummy_filename
      ~file_sig
      ~typed_ast
      ~reader
      ~locs
  in
  let pretty_print_type type_param_synthesizer = function
    | Ok (Some (tparams_rev, type_)) ->
      let type_string =
        type_ |> Js_layout_generator.type_ ~opts:Js_layout_generator.default_opts |> pretty_print
      in
      let typeparams_string =
        match tparams_rev with
        | [] -> ""
        | _ ->
          let pretty_print_typeparam tparam =
            match type_param_synthesizer tparams_rev tparam with
            | Ok synthesized ->
              synthesized
              |> Js_layout_generator.type_param ~opts:Js_layout_generator.default_opts
              |> pretty_print
            | Error expected ->
              Printf.sprintf "<Error: %s>" Insert_type.(error_to_string (Expected expected))
          in
          "<" ^ (tparams_rev |> List.map pretty_print_typeparam |> String.concat ", ") ^ ">: "
      in
      typeparams_string ^ type_string
    | Ok None -> "<missing>"
    | Error expected ->
      Printf.sprintf "<Error: %s>" Insert_type.(error_to_string (Expected expected))
  in
  [
    ( "basic_test" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        const b = "2";
        class C<T> {}
        const c = new C<number>();
        function foo<A, B, C:B, D:B=C>(d: D): D {
          return d;
        }
        |}
      in
      let loc_of_a = mk_loc (2, 14) (2, 15) in
      let loc_of_b = mk_loc (3, 14) (3, 15) in
      let loc_of_c = mk_loc (5, 14) (5, 15) in
      let loc_of_d = mk_loc (6, 39) (6, 40) in
      let loc_of_missing = mk_loc (100, 14) (100, 15) in
      let context =
        create_context source [loc_of_a; loc_of_b; loc_of_c; loc_of_d; loc_of_missing]
      in
      let { TypeSynthesizer.type_param_synthesizer; type_synthesizer; _ } =
        TypeSynthesizer.create_type_synthesizer_with_import_adder context
      in
      assert_equal
        ~ctxt
        ~printer:Base.Fn.id
        "number"
        (loc_of_a |> type_synthesizer |> pretty_print_type type_param_synthesizer);
      assert_equal
        ~ctxt
        ~printer:Base.Fn.id
        "string"
        (loc_of_b |> type_synthesizer |> pretty_print_type type_param_synthesizer);
      assert_equal
        ~ctxt
        ~printer:Base.Fn.id
        "C<number>"
        (loc_of_c |> type_synthesizer |> pretty_print_type type_param_synthesizer);
      assert_equal
        ~ctxt
        ~printer:Base.Fn.id
        "<D: B = C, C: B, B>: D"
        (loc_of_d |> type_synthesizer |> pretty_print_type type_param_synthesizer);
      assert_equal
        ~ctxt
        ~printer:Base.Fn.id
        "<missing>"
        (loc_of_missing |> type_synthesizer |> pretty_print_type type_param_synthesizer)
    );
  ]

let tests =
  "refactor_extract_utils"
  >::: [
         "extract" >::: extract_tests;
         "function_method_inserting_points" >::: collect_function_method_inserting_points_tests;
         "find_closest_enclosing_class" >::: find_closest_enclosing_class_tests;
         "collect_relevant_defs_with_scope" >::: collect_relevant_defs_with_scope_tests;
         "undefined_variables_after_extraction" >::: undefined_variables_after_extraction_tests;
         "escaping_locally_defined_variables" >::: escaping_locally_defined_variables_tests;
         "type_synthesizer" >::: type_synthesizer_tests;
       ]
