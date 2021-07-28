(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Refactor_extract_utils_tests

let assert_refactored
    ~ctxt ?(support_experimental_snippet_text_edit = false) expected source extract_range =
  let ast = parse source in
  let typed_ast = typed_ast_of_ast ast in
  let reader = State_reader.create () in
  let remove_blank_lines s =
    s
    |> String.split_on_char '\n'
    |> List.filter (fun s -> String.trim s <> "")
    |> String.concat "\n"
  in
  let actual =
    Refactor_extract.provide_available_refactors
      ~ast
      ~full_cx:dummy_context
      ~file:dummy_filename
      ~file_sig:(file_sig_of_ast ast)
      ~typed_ast
      ~reader
      ~support_experimental_snippet_text_edit
      ~extract_range
    |> List.map (fun { Refactor_extract.title; new_ast; _ } ->
           ( title,
             new_ast
             |> Js_layout_generator.program
                  ~opts:Js_layout_generator.default_opts
                  ~preserve_docblock:true
                  ~checksum:None
             |> pretty_print
             |> String.trim
             |> remove_blank_lines ))
  in
  let expected : (string * string) list =
    List.map (fun (title, s) -> (title, s |> String.trim |> remove_blank_lines)) expected
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
      assert_refactored ~ctxt expected source (mk_loc (4, 10) (4, 22)) );
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
      assert_refactored ~ctxt expected source (mk_loc (4, 10) (7, 20)) );
    ( "extract_use_toplevel_function_param" >:: fun ctxt ->
      let source =
        {|
        function foo(a: number) {
          console.log(a);
        }
        |}
      in
      let expected =
        [
          ( "Extract to function in module scope",
            {|
function foo(a: number) {
  newFunction(a);
}
function newFunction(a: number): void {
  console.log(a);
}
            |}
          );
          ( "Extract to inner function in function 'foo'",
            {|
function foo(a: number) {
  newFunction();
  function newFunction(): void {
    console.log(a);
  }
}
            |}
          );
        ]
      in
      assert_refactored ~ctxt expected source (mk_loc (3, 0) (3, 30)) );
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
      assert_refactored ~ctxt expected source (mk_loc (2, 8) (2, 20)) );
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
      assert_refactored ~ctxt expected source (mk_loc (4, 8) (4, 33)) );
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
      assert_refactored ~ctxt expected source (mk_loc (3, 8) (4, 33)) );
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
      assert_refactored ~ctxt expected source (mk_loc (2, 8) (2, 33)) );
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
      assert_refactored ~ctxt expected source (mk_loc (2, 8) (3, 32)) );
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
          ( "Extract to inner function in function 'test'",
            {|
const test = (async () => {
  await newFunction();
  async function newFunction(): Promise<void> {
    // selection start
    const a = 3;
    let b = 4;
    await b;
    let d = 6;
  }
});
            |}
          );
        ]
      in
      assert_refactored ~ctxt expected source (mk_loc (4, 10) (7, 20)) );
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
          ( "Extract to inner function in function 'test'",
            {|
const test = (async () => {
  await newFunction();
  async function newFunction(): Promise<void> {
    // selection start
    const a = 3;
    {
      for await (const b of []) {}
    }
    let d = 6;
  }
});
            |}
          );
        ]
      in
      assert_refactored ~ctxt expected source (mk_loc (4, 10) (6, 20)) );
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
      assert_refactored ~ctxt expected source (mk_loc (1, 0) (1, 39)) );
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
      assert_refactored ~ctxt [] source (mk_loc (4, 10) (8, 22)) );
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
      assert_refactored ~ctxt [] source (mk_loc (4, 10) (8, 22)) );
    ( "label_no_extract" >:: fun ctxt ->
      assert_refactored
        ~ctxt
        []
        "const a = 1; {label:test();} function test() {}"
        (mk_loc (1, 0) (1, 60)) );
    ( "simple_break_continue_no_extract" >:: fun ctxt ->
      assert_refactored ~ctxt [] "while (true) {break;}" (mk_loc (1, 12) (1, 30));
      assert_refactored ~ctxt [] "while (true) {continue;}" (mk_loc (1, 12) (1, 30)) );
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
      assert_refactored ~ctxt [] source (mk_loc (3, 8) (5, 23)) );
    ( "wrapped_break_continue_with_label_no_extracts" >:: fun ctxt ->
      assert_refactored ~ctxt [] "label:while (true) {break label;}" (mk_loc (1, 0) (1, 50));
      assert_refactored ~ctxt [] "label:while (true) {continue label;}" (mk_loc (1, 0) (1, 50)) );
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
      assert_refactored ~ctxt expected "while (true) {break;}" (mk_loc (1, 0) (1, 30));
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
      assert_refactored ~ctxt expected "while (true) {continue;}" (mk_loc (1, 0) (1, 30));
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
      assert_refactored ~ctxt expected "switch (true) {default:break;}" (mk_loc (1, 0) (1, 40)) );
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
        (mk_loc (1, 20) (1, 33));
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
        (mk_loc (1, 20) (1, 34));
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
          ( "Extract to inner function in method 'test1'",
            {|
class A {
  test1() {
    newFunction();
    function newFunction(): void {
      console.log();
    }
  }
}
            |}
          );
        ]
        "class A { test1() { console.log(); } }"
        (mk_loc (1, 20) (1, 34)) );
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
      assert_refactored ~ctxt expected source (mk_loc (14, 12) (14, 51)) );
    ( "type_parameters_extract" >:: fun ctxt ->
      (* Test that all the constraints of generic type parameters are added,
         and unused ones (A) are removed. *)
      let source =
        {multiline|
        function foo<A, B, C:B=B>(c: C) {
          function bar<D:C, E:D, F:D=E>(f: F): F {
            console.log(c); // selected
            const g = f; // selected
            return g;
          }
        }
        |multiline}
      in
      let expected =
        [
          ( "Extract to function in module scope",
            {multiline|
function foo<A, B, C: B = B>(c: C) {
  function bar<D: C, E: D, F: D = E>(f: F): F {
    const g = newFunction(c, f);
    return g;
  }
}
function newFunction<B, C: B = B, D: C, E: D, F: D = E>(c: C, f: F): F {
  console.log(c); // selected
  const g = f; // selected
  return g;
}
|multiline}
          );
          ( "Extract to inner function in function 'bar'",
            {multiline|
function foo<A, B, C: B = B>(c: C) {
  function bar<D: C, E: D, F: D = E>(f: F): F {
    const g = newFunction();
    return g;
    function newFunction(): F {
      console.log(c); // selected
      const g = f; // selected
      return g;
    }
  }
}
|multiline}
          );
          ( "Extract to inner function in function 'foo'",
            {multiline|
function foo<A, B, C: B = B>(c: C) {
  function bar<D: C, E: D, F: D = E>(f: F): F {
    const g = newFunction(f);
    return g;
  }
  function newFunction<D: C, E: D, F: D = E>(f: F): F {
    console.log(c); // selected
    const g = f; // selected
    return g;
  }
}
|multiline}
          );
        ]
      in
      assert_refactored ~ctxt expected source (mk_loc (4, 0) (5, 50)) );
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
      assert_refactored ~ctxt expected source (mk_loc (7, 16) (10, 26)) );
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
      assert_refactored ~ctxt expected source (mk_loc (13, 18) (15, 61)) );
    (* A simple constant extraction that can go to all possible scopes. *)
    ( "simple_constant_extract" >:: fun ctxt ->
      let source = "function test() { const a = 1; }" in
      let expected =
        [
          ( "Extract to constant in function 'test'",
            {|
function test() {
  const newLocal = 1;
  const a = newLocal;
}
            |} );
          ( "Extract to constant in module scope",
            {|
const newLocal = 1;
function test() {
  const a = newLocal;
}
            |} );
        ]
      in
      assert_refactored ~ctxt expected source (mk_loc (1, 28) (1, 29)) );
    (* Testing that we won't extract constant to scopes that will result in undefined variables. *)
    ( "constant_extract_with_scoping_issues" >:: fun ctxt ->
      let source =
        {|
        function foo() {
          const a = 3;
          function bar() {
            const b = 4;
            function baz() {
              const c = 5;
              // selected a + b + c
              return a + b + c;
            }
          }
        }
        |}
      in
      let expected =
        [
          ( "Extract to constant in function 'baz'",
            {|
function foo() {
  const a = 3;
  function bar() {
    const b = 4;
    function baz() {
      const c = 5;
      const newLocal = a + b + c;
      // selected a + b + c
      return newLocal;
    }
  }
}
          |}
          );
        ]
      in
      assert_refactored ~ctxt expected source (mk_loc (9, 21) (9, 30)) );
    (* A simple field extraction that can only go inside the class. *)
    ( "field_extract_class_only" >:: fun ctxt ->
      let source = "class A { test() { const a = this.test(); } }" in
      let expected =
        [
          ( "Extract to field in class 'A'",
            {|
class A {
  newProperty = this.test();
  test() {
    const a = this.newProperty;
  }
}
            |}
          );
          ( "Extract to constant in method 'test'",
            {|
class A {
  test() {
    const newLocal = this.test();
    const a = newLocal;
  }
}
            |}
          );
        ]
      in
      assert_refactored ~ctxt expected source (mk_loc (1, 29) (1, 40)) );
    (* A simple type alias extraction without any type variables. *)
    ( "simple_type_extract" >:: fun ctxt ->
      let source = "const a: number = 1;" in
      let expected = [("Extract to type alias", "type NewType = number;\nconst a: NewType = 1;")] in
      assert_refactored ~ctxt expected source (mk_loc (1, 9) (1, 15)) );
    (* Selected type uses generic type parameters *)
    ( "type_extract_with_generic_type_parameters" >:: fun ctxt ->
      let source =
        {|
        function foo<A, B>() {
          class Bar<C, D> {
            baz<E, F>(): [A, B, C, D, E, F] { }
          }
        }
        |}
      in
      let expected =
        [
          ( "Extract to type alias",
            {|
function foo<A, B>() {
  type NewType<C, D, E, F> = [A, B, C, D, E, F];
  class Bar<C, D> {
    baz<E, F>(): NewType<C, D, E, F> {}
  }
}
            |}
          );
        ]
      in
      assert_refactored ~ctxt expected source (mk_loc (4, 25) (4, 43)) );
    (* Generated name has conflicts. *)
    ( "generated_name_conflicts" >:: fun ctxt ->
      let source =
        {|
        const newFunction = 1, newFunction1 = 1;
        const newMethod = 1, newMethod1 = 1;
        const newLocal = 1, newLocal1 = 1;
        const newProperty = 1, newProperty1 = 1;
        class A {
          test() {
            1 + 1
          }
        }
        |}
      in
      let expected =
        [
          ( "Extract to method in class 'A'",
            {|
const newFunction = 1,
  newFunction1 = 1;
const newMethod = 1,
  newMethod1 = 1;
const newLocal = 1,
  newLocal1 = 1;
const newProperty = 1,
  newProperty1 = 1;
class A {
  test() {
    this.${0:newMethod2}();
  }
  ${0:newMethod2}(): void {
    1 + 1;
  }
}
            |}
          );
          ( "Extract to function in module scope",
            {|
const newFunction = 1,
  newFunction1 = 1;
const newMethod = 1,
  newMethod1 = 1;
const newLocal = 1,
  newLocal1 = 1;
const newProperty = 1,
  newProperty1 = 1;
class A {
  test() {
    ${0:newFunction2}();
  }
}
function ${0:newFunction2}(): void {
  1 + 1;
}
            |}
          );
          ( "Extract to inner function in method 'test'",
            {|
const newFunction = 1,
  newFunction1 = 1;
const newMethod = 1,
  newMethod1 = 1;
const newLocal = 1,
  newLocal1 = 1;
const newProperty = 1,
  newProperty1 = 1;
class A {
  test() {
    ${0:newFunction2}();
    function ${0:newFunction2}(): void {
      1 + 1;
    }
  }
}
            |}
          );
          ( "Extract to field in class 'A'",
            {|
const newFunction = 1,
  newFunction1 = 1;
const newMethod = 1,
  newMethod1 = 1;
const newLocal = 1,
  newLocal1 = 1;
const newProperty = 1,
  newProperty1 = 1;
class A {
  ${0:newProperty2} = 1 + 1;
  test() {
    this.${0:newProperty2};
  }
}
            |}
          );
          ( "Extract to constant in method 'test'",
            {|
const newFunction = 1,
  newFunction1 = 1;
const newMethod = 1,
  newMethod1 = 1;
const newLocal = 1,
  newLocal1 = 1;
const newProperty = 1,
  newProperty1 = 1;
class A {
  test() {
    const ${0:newLocal2} = 1 + 1;
    ${0:newLocal2};
  }
}
            |}
          );
          ( "Extract to constant in module scope",
            {|
const newFunction = 1,
  newFunction1 = 1;
const newMethod = 1,
  newMethod1 = 1;
const newLocal = 1,
  newLocal1 = 1;
const newProperty = 1,
  newProperty1 = 1;
const ${0:newLocal2} = 1 + 1;
class A {
  test() {
    ${0:newLocal2};
  }
}
            |}
          );
        ]
      in
      assert_refactored
        ~ctxt
        ~support_experimental_snippet_text_edit:true
        expected
        source
        (mk_loc (8, 12) (8, 17));
      let source = {|
        type NewType = number;
        type NewType1 = NewType;
      |} in
      let expected =
        [
          ( "Extract to type alias",
            {|
type ${0:NewType2} = number;
type NewType = ${0:NewType2};
type NewType1 = NewType;
            |}
          );
        ]
      in
      assert_refactored
        ~ctxt
        ~support_experimental_snippet_text_edit:true
        expected
        source
        (mk_loc (2, 23) (2, 29)) );
  ]

let tests =
  "refactor_extract" >::: ["provide_available_refactor" >::: provide_available_refactor_tests]
