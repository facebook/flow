(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Refactor_extract_utils_tests

let assert_refactored ~ctxt expected source extract_range =
  let ast = parse source in
  let typed_ast = typed_ast_of_ast ast in
  let reader = State_reader.create () in
  let actual =
    Refactor_extract.provide_available_refactors
      ~ast
      ~full_cx:dummy_context
      ~file:dummy_filename
      ~file_sig:(file_sig_of_ast ast)
      ~typed_ast
      ~reader
      ~extract_range
    |> List.map (fun { Refactor_extract.title; new_ast; _ } ->
           ( title,
             new_ast
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
function newFunction(): void {
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
  function newFunction(): void {
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
function newFunction(): void {
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
  function newFunction(): void {
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
function newFunction(): number {
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
function newFunction(): number {
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
            {multiline|
let fooo = 3;
let a;

({a, fooo} = newFunction());
console.log(a + fooo);
function newFunction(): {| a: number, fooo: number |} {
  const a = 3; // selected
  fooo = a + 2; // selected
  return { a, fooo };
}
      |multiline}
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
function newFunction(): number {
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
            {multiline|
let {a, fooo} = newFunction();
fooo = a + 2;
function newFunction(): {| a: number, fooo: number |} {
  let fooo = 3; // selected
  const a = 3; // selected
  return { a, fooo };
}
      |multiline}
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
async function newFunction(): Promise<void> {
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
async function newFunction(): Promise<void> {
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
function newFunction(): void {
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
function newFunction(): void {
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
function newFunction(): void {
  while (true) {
    continue;
  }
}
|}
          );
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
function newFunction(): void {
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
  newMethod(): void {
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
  newMethod(): void {
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
  newMethod(): void {
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
function newFunction(): void {
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
  newMethod(B: typeof B, a: number): B {
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
function newFunction(): void {
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
        function newFunction(): void {
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
      function newFunction(): void {
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
    function newFunction(): void {
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
  function newFunction(): void {
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
            {multiline|
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
function newFunction(
  b: number,
  c: number,
  d: number,
  e: number,
  f: number,
): {| g: number, h: number |} {
  const g = 3;
  const h = 4;
  console.log(a + b + c + d + e + f + g + h);
  return { g, h };
}
|multiline}
          );
          ( "Extract to inner function in function 'level4'",
            {multiline|
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
          function newFunction(): {| g: number, h: number |} {
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
|multiline}
          );
          ( "Extract to inner function in function 'level3'",
            {multiline|
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
      function newFunction(e: number, f: number): {| g: number, h: number |} {
        const g = 3;
        const h = 4;
        console.log(a + b + c + d + e + f + g + h);
        return { g, h };
      }
    }
  }
}
|multiline}
          );
          ( "Extract to inner function in function 'level2'",
            {multiline|
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
    function newFunction(
      d: number,
      e: number,
      f: number,
    ): {| g: number, h: number |} {
      const g = 3;
      const h = 4;
      console.log(a + b + c + d + e + f + g + h);
      return { g, h };
    }
  }
}
|multiline}
          );
          ( "Extract to inner function in function 'level1'",
            {multiline|
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
  function newFunction(
    c: number,
    d: number,
    e: number,
    f: number,
  ): {| g: number, h: number |} {
    const g = 3;
    const h = 4;
    console.log(a + b + c + d + e + f + g + h);
    return { g, h };
  }
}
|multiline}
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
  "refactor_extract" >::: ["provide_available_refactor" >::: provide_available_refactor_tests]
