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

let assert_refactored ~ctxt expected source extract_range =
  let ast = parse source in
  let typed_ast = typed_ast_of_ast ast in
  let parsing_heap_reader = State_reader.create () in
  let actual =
    Refactor_extract_function.provide_available_refactors
      ~ast
      ~typed_ast
      ~parsing_heap_reader
      ~extract_range
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
    ( "single_escaping_def_extract" >:: fun ctxt ->
      let source = {|
        const a = 3;
        console.log(a);
      |} in
      let expected =
        [
          ( "Extract to function in module scope",
            {|
const a = newFunction();
console.log(a);

function newFunction() {
  const a = 3;
  return a;
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
          start = { Loc.line = 2; column = 8 };
          _end = { Loc.line = 2; column = 20 };
        } );
    ( "local_reassignment_single_return_extract" >:: fun ctxt ->
      let source =
        {|
        let fooo = 3;
        const a = 3;
        fooo = a + 2; // selected
        console.log(a + fooo);
      |}
      in
      let expected =
        [
          ( "Extract to function in module scope",
            {|
let fooo = 3;
const a = 3;
fooo = newFunction();
console.log(a + fooo);

function newFunction() {
  fooo = a + 2; // selected
  return fooo;
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
          start = { Loc.line = 4; column = 8 };
          _end = { Loc.line = 4; column = 33 };
        } );
    ( "local_reassignment_mixed_return_extract" >:: fun ctxt ->
      let source =
        {|
        let fooo = 3;
        const a = 3; // selected
        fooo = a + 2; // selected
        console.log(a + fooo);
      |}
      in
      let expected =
        [
          ( "Extract to function in module scope",
            {|
let fooo = 3;
let a;

({a, fooo} = newFunction());
console.log(a + fooo);

function newFunction() {
  const a = 3; // selected
  fooo = a + 2; // selected
  return { a, fooo };
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
          start = { Loc.line = 3; column = 8 };
          _end = { Loc.line = 4; column = 33 };
        } );
    ( "external_reassignment_single_return_extract" >:: fun ctxt ->
      let source =
        {|
        let fooo = 3; // selected
        const a = 3;
        fooo = a + 2;
      |}
      in
      let expected =
        [
          ( "Extract to function in module scope",
            {|
let fooo = newFunction();
const a = 3;
fooo = a + 2;

function newFunction() {
  let fooo = 3; // selected
  return fooo;
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
          start = { Loc.line = 2; column = 8 };
          _end = { Loc.line = 2; column = 33 };
        } );
    ( "external_reassignment_multiple_returns_extract" >:: fun ctxt ->
      let source =
        {|
        let fooo = 3; // selected
        const a = 3; // selected
        fooo = a + 2;
      |}
      in
      let expected =
        [
          ( "Extract to function in module scope",
            {|
let {a, fooo} = newFunction();
fooo = a + 2;

function newFunction() {
  let fooo = 3; // selected
  const a = 3; // selected
  return { a, fooo };
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
          start = { Loc.line = 2; column = 8 };
          _end = { Loc.line = 3; column = 32 };
        } );
    ( "async_expression_extract" >:: fun ctxt ->
      let source =
        {|
        const test = async () => {
          // selection start
          const a = 3;
          let b = 4;
          await b;
          let d = 6;
        };
      |}
      in
      let expected =
        [
          ( "Extract to function in module scope",
            {|
const test = (async () => {
  await newFunction();
});

async function newFunction() {
  // selection start
  const a = 3;
  let b = 4;
  await b;
  let d = 6;
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
    ( "async_for_of_extract" >:: fun ctxt ->
      let source =
        {|
        const test = async () => {
          // selection start
          const a = 3;
          { for await (const b of []) {} }
          let d = 6;
        };
      |}
      in
      let expected =
        [
          ( "Extract to function in module scope",
            {|
const test = (async () => {
  await newFunction();
});

async function newFunction() {
  // selection start
  const a = 3;
  {
    for await (const b of []) {}
  }
  let d = 6;
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
          _end = { Loc.line = 6; column = 20 };
        } );
    ( "await_in_async_function_extract" >:: fun ctxt ->
      let source = "const test = async () => await promise;" in
      let expected =
        [
          ( "Extract to function in module scope",
            {|
newFunction();

function newFunction() {
  const test = (async () => await promise);
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
          start = { Loc.line = 1; column = 0 };
          _end = { Loc.line = 1; column = 39 };
        } );
    ( "return_no_extract" >:: fun ctxt ->
      let source =
        {|
        function test() {
          // selection start
          const a = 1;
          const b = 2;
          {
            return;
          }
          // selection end
        }
      |}
      in
      assert_refactored
        ~ctxt
        []
        source
        {
          Loc.source = None;
          start = { Loc.line = 4; column = 10 };
          _end = { Loc.line = 8; column = 22 };
        } );
    ( "yield_no_extract" >:: fun ctxt ->
      let source =
        {|
        function* test() {
          // selection start
          const a = 1;
          {
            yield;
          }
          const b = 2;
          // selection end
        }
      |}
      in
      assert_refactored
        ~ctxt
        []
        source
        {
          Loc.source = None;
          start = { Loc.line = 4; column = 10 };
          _end = { Loc.line = 8; column = 22 };
        } );
    ( "label_no_extract" >:: fun ctxt ->
      assert_refactored
        ~ctxt
        []
        "const a = 1; {label:test();} function test() {}"
        {
          Loc.source = None;
          start = { Loc.line = 1; column = 0 };
          _end = { Loc.line = 1; column = 60 };
        } );
    ( "simple_break_continue_no_extract" >:: fun ctxt ->
      assert_refactored
        ~ctxt
        []
        "while (true) {break;}"
        {
          Loc.source = None;
          start = { Loc.line = 1; column = 12 };
          _end = { Loc.line = 1; column = 30 };
        };
      assert_refactored
        ~ctxt
        []
        "while (true) {continue;}"
        {
          Loc.source = None;
          start = { Loc.line = 1; column = 12 };
          _end = { Loc.line = 1; column = 30 };
        } );
    ( "continue_in_switch_no_extract" >:: fun ctxt ->
      let source =
        {|
      while (true) {
        // selection start
        switch (true) {default:continue;}
        // seletion end
      }

      |}
      in
      assert_refactored
        ~ctxt
        []
        source
        {
          Loc.source = None;
          start = { Loc.line = 3; column = 8 };
          _end = { Loc.line = 5; column = 23 };
        } );
    ( "wrapped_break_continue_with_label_no_extracts" >:: fun ctxt ->
      assert_refactored
        ~ctxt
        []
        "label:while (true) {break label;}"
        {
          Loc.source = None;
          start = { Loc.line = 1; column = 0 };
          _end = { Loc.line = 1; column = 50 };
        };
      assert_refactored
        ~ctxt
        []
        "label:while (true) {continue label;}"
        {
          Loc.source = None;
          start = { Loc.line = 1; column = 0 };
          _end = { Loc.line = 1; column = 50 };
        } );
    ( "wrapped_break_continue_switch_has_extracts" >:: fun ctxt ->
      let expected =
        [
          ( "Extract to function in module scope",
            {|
newFunction();

function newFunction() {
  while (true) {
    break;
  }
}
|} );
        ]
      in
      assert_refactored
        ~ctxt
        expected
        "while (true) {break;}"
        {
          Loc.source = None;
          start = { Loc.line = 1; column = 0 };
          _end = { Loc.line = 1; column = 30 };
        };
      let expected =
        [
          ( "Extract to function in module scope",
            {|
newFunction();

function newFunction() {
  while (true) {
    continue;
  }
}
|} );
        ]
      in
      assert_refactored
        ~ctxt
        expected
        "while (true) {continue;}"
        {
          Loc.source = None;
          start = { Loc.line = 1; column = 0 };
          _end = { Loc.line = 1; column = 30 };
        };
      let expected =
        [
          ( "Extract to function in module scope",
            {|
newFunction();

function newFunction() {
  switch (true) {
    default:
      break;
  }
}
|}
          );
        ]
      in
      assert_refactored
        ~ctxt
        expected
        "switch (true) {default:break;}"
        {
          Loc.source = None;
          start = { Loc.line = 1; column = 0 };
          _end = { Loc.line = 1; column = 40 };
        } );
    ( "basic_class_method_extract" >:: fun ctxt ->
      assert_refactored
        ~ctxt
        [
          ( "Extract to method in class 'A'",
            {|
class A {
  test1() {
    this.newMethod();
  }
  newMethod() {
    this.test1();
  }
}
        |}
          );
        ]
        "class A { test1() { this.test1(); } }"
        {
          Loc.source = None;
          start = { Loc.line = 1; column = 20 };
          _end = { Loc.line = 1; column = 33 };
        };
      assert_refactored
        ~ctxt
        [
          ( "Extract to method in class 'A'",
            {|
class A {
  test1() {
    this.newMethod();
  }
  newMethod() {
    super.test1();
  }
}
        |}
          );
        ]
        "class A { test1() { super.test1(); } }"
        {
          Loc.source = None;
          start = { Loc.line = 1; column = 20 };
          _end = { Loc.line = 1; column = 34 };
        };
      assert_refactored
        ~ctxt
        [
          ( "Extract to method in class 'A'",
            {|
class A {
  test1() {
    this.newMethod();
  }
  newMethod() {
    console.log();
  }
}
        |}
          );
          ( "Extract to function in module scope",
            {|
class A {
  test1() {
    newFunction();
  }
}

function newFunction() {
  console.log();
}
          |}
          );
        ]
        "class A { test1() { console.log(); } }"
        {
          Loc.source = None;
          start = { Loc.line = 1; column = 20 };
          _end = { Loc.line = 1; column = 34 };
        } );
    ( "class_method_with_parameters_and_return_extract" >:: fun ctxt ->
      let source =
        {|
        export default class {
          constructor() { this.v = 1; }
          test1() {
            class B {
              constructor(v1: number, v2: number) {
                console.log(v1, v2);
              }
              test2() {
                console.log('test');
              }
            }
            const a = 2;
            const b = new B(this.v, a); // selected
            b.test2();
          }
        }
      |}
      in
      let expected =
        [
          ( "Extract to method in anonymous class declaration",
            {|
export default class {
  constructor() {
    this.v = 1;
  }
  test1() {
    class B {
      constructor(v1: number, v2: number) {
        console.log(v1, v2);
      }
      test2() {
        console.log("test");
      }
    }
    const a = 2;
    const b = this.newMethod(B, a);
    b.test2();
  }
  newMethod(B, a) {
    const b = new B(this.v, a); // selected
    return b;
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
          start = { Loc.line = 14; column = 12 };
          _end = { Loc.line = 14; column = 51 };
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
                  const g = 3;
                  const h = 4;
                  console.log(a + b + c + d + e + f + g + h);
                  return f + g + h;
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
          const {g, h} = newFunction(b, c, d, e, f);
          return f + g + h;
        }
      }
    }
  }
}

function newFunction(b, c, d, e, f) {
  const g = 3;
  const h = 4;
  console.log(a + b + c + d + e + f + g + h);
  return { g, h };
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
          const {g, h} = newFunction();
          return f + g + h;
          function newFunction() {
            const g = 3;
            const h = 4;
            console.log(a + b + c + d + e + f + g + h);
            return { g, h };
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
          const {g, h} = newFunction(e, f);
          return f + g + h;
        }
      }
      function newFunction(e, f) {
        const g = 3;
        const h = 4;
        console.log(a + b + c + d + e + f + g + h);
        return { g, h };
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
          const {g, h} = newFunction(d, e, f);
          return f + g + h;
        }
      }
    }
    function newFunction(d, e, f) {
      const g = 3;
      const h = 4;
      console.log(a + b + c + d + e + f + g + h);
      return { g, h };
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
          const {g, h} = newFunction(c, d, e, f);
          return f + g + h;
        }
      }
    }
  }
  function newFunction(c, d, e, f) {
    const g = 3;
    const h = 4;
    console.log(a + b + c + d + e + f + g + h);
    return { g, h };
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
          _end = { Loc.line = 15; column = 61 };
        } );
  ]

let tests =
  "refactor_extract_function"
  >::: ["provide_available_refactor" >::: provide_available_refactor_tests]
