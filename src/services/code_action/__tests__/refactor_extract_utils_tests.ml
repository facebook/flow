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
    Parser_flow.program ~parse_options:(Some Parser_env.default_parse_options) contents
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
    check_updates_against_providers = false;
    max_literal_length = 100;
    enable_const_params = false;
    enable_enums = true;
    enable_enums_with_unknown_members = true;
    enable_indexed_access = true;
    enforce_local_inference_annotations = false;
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
    reorder_checking = Options.Lexical;
    root;
    run_post_inference_implicit_instantiation = false;
    strict_es6_import_export = false;
    strict_es6_import_export_excludes = [];
    strip_root = true;
    suppress_types = SSet.empty;
    trust_mode = Options.NoTrust;
    type_asserts = false;
  }

let dummy_filename = File_key.SourceFile ""

let file_sig_of_ast ast =
  match File_sig.With_Loc.program ~ast ~module_ref_prefix:None with
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
      |> List.map fst
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
  let pretty_print_type = function
    | Some type_ ->
      type_ |> Js_layout_generator.type_ ~opts:Js_layout_generator.default_opts |> pretty_print
    | None -> "<missing>"
  in
  [
    ( "basic_test" >:: fun ctxt ->
      let source =
        {|
        const a = 3;
        const b = "2";
        class C<T> {}
        const c = new C<number>();
        |}
      in
      let loc_of_a =
        {
          Loc.source = None;
          start = { Loc.line = 2; column = 14 };
          _end = { Loc.line = 2; column = 15 };
        }
      in
      let loc_of_b =
        {
          Loc.source = None;
          start = { Loc.line = 3; column = 14 };
          _end = { Loc.line = 3; column = 15 };
        }
      in
      let loc_of_c =
        {
          Loc.source = None;
          start = { Loc.line = 5; column = 14 };
          _end = { Loc.line = 5; column = 15 };
        }
      in
      let loc_of_missing =
        {
          Loc.source = None;
          start = { Loc.line = 6; column = 14 };
          _end = { Loc.line = 6; column = 15 };
        }
      in
      let context = create_context source [loc_of_a; loc_of_b; loc_of_c; loc_of_missing] in
      let { TypeSynthesizer.type_synthesizer; _ } =
        TypeSynthesizer.create_type_synthesizer_with_import_adder context
      in
      assert_equal
        ~ctxt
        ~printer:Base.Fn.id
        "number"
        (loc_of_a |> type_synthesizer |> pretty_print_type);
      assert_equal
        ~ctxt
        ~printer:Base.Fn.id
        "string"
        (loc_of_b |> type_synthesizer |> pretty_print_type);
      assert_equal
        ~ctxt
        ~printer:Base.Fn.id
        "C<number>"
        (loc_of_c |> type_synthesizer |> pretty_print_type);
      assert_equal
        ~ctxt
        ~printer:Base.Fn.id
        "<missing>"
        (loc_of_missing |> type_synthesizer |> pretty_print_type) );
  ]

let tests =
  "refactor_extract_utils"
  >::: [
         "extract_statements" >::: extract_statements_tests;
         "find_closest_enclosing_class_scope" >::: find_closest_enclosing_class_scope_tests;
         "collect_relevant_defs_with_scope" >::: collect_relevant_defs_with_scope_tests;
         "undefined_variables_after_extraction" >::: undefined_variables_after_extraction_tests;
         "escaping_locally_defined_variables" >::: escaping_locally_defined_variables_tests;
         "type_synthesizer" >::: type_synthesizer_tests;
       ]
