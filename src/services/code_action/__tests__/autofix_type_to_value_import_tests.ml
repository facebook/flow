(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let indent_len str =
  let len = String.length str in
  let i = ref 0 in
  while !i < len && str.[!i] = ' ' do
    incr i
  done;
  !i

let dedent_trim str =
  let lines = String.split_on_char '\n' str in
  let non_empty_lines = List.filter (fun line -> String.trim line <> "") lines in
  let min_indent =
    List.fold_left (fun acc line -> min acc (indent_len line)) max_int non_empty_lines
  in
  let lines =
    List.map
      (fun line ->
        let len = String.length line in
        if len < min_indent then
          line
        else
          String.sub line min_indent (len - min_indent))
      lines
  in
  String.trim (String.concat "\n" lines) ^ "\n"

let parse contents =
  let parse_options = Some Parser_env.permissive_parse_options in
  let (ast, _errors) = Parser_flow.program ~parse_options contents in
  ast

let assert_patch ~ctxt ~f expected contents =
  let expected = dedent_trim expected in
  let contents = dedent_trim contents in
  let ast = parse contents in
  let new_ast = Base.Option.value_exn (f ast) in
  let patch =
    Replacement_printer.mk_patch_ast_differ
      ~opts:Js_layout_generator.default_opts
      (Insert_type.mk_diff ast new_ast)
      contents
  in
  let patched = Replacement_printer.print patch contents in
  assert_equal ~ctxt ~printer:(fun x -> x) expected patched

let assert_fixed ~ctxt ~expected ~loc ~contents =
  let expected = dedent_trim expected in
  let contents = dedent_trim contents in
  let ast = parse contents in
  let new_ast =
    Base.Option.value_exn (Autofix_type_to_value_import.convert_type_to_value_import ast loc)
  in
  let patch =
    Replacement_printer.mk_patch_ast_differ
      ~opts:Js_layout_generator.default_opts
      (Insert_type.mk_diff ast new_ast)
      contents
  in
  let patched = Replacement_printer.print patch contents in
  assert_equal ~ctxt ~printer:(fun x -> x) expected patched

let simple_type_imports_tests =
  [
    ( "named_specifiers_1" >:: fun ctxt ->
      let contents = {|
        import type { Foo } from "./a";
        Foo;
      |} in
      let expected = {|
        import { Foo } from "./a";
        Foo;
      |} in
      assert_fixed ~ctxt ~expected ~contents ~loc:(Loc.mk_loc (2, 0) (2, 3))
    );
    ( "named_specifiers_2" >:: fun ctxt ->
      let contents = {|
        import type { Foo, Bar } from "./a";
        Foo;
      |} in
      let expected =
        {|
        import type { Bar } from "./a";
        import { Foo } from "./a";
        Foo;
      |}
      in
      assert_fixed ~ctxt ~expected ~contents ~loc:(Loc.mk_loc (2, 0) (2, 3))
    );
    ( "named_specifiers_3" >:: fun ctxt ->
      let contents = {|
        import type Bar, { Foo } from "./a";
        Foo;
      |} in
      let expected =
        {|
        import type Bar from "./a";
        import { Foo } from "./a";
        Foo;
      |}
      in
      assert_fixed ~ctxt ~expected ~contents ~loc:(Loc.mk_loc (2, 0) (2, 3))
    );
    ( "named_specifiers_4" >:: fun ctxt ->
      let contents = {|
        import type Bar, { Foo, Baz } from "./a";
        Foo;
      |} in
      let expected =
        {|
        import type Bar, { Baz } from "./a";
        import { Foo } from "./a";
        Foo;
      |}
      in
      assert_fixed ~ctxt ~expected ~contents ~loc:(Loc.mk_loc (2, 0) (2, 3))
    );
    ( "default_1" >:: fun ctxt ->
      let contents = {|
        import type Foo from "./a";
        Foo;
      |} in
      let expected = {|
        import Foo from "./a";
        Foo;
      |} in
      assert_fixed ~ctxt ~expected ~contents ~loc:(Loc.mk_loc (2, 0) (2, 3))
    );
    ( "default_2" >:: fun ctxt ->
      let contents = {|
        import type Foo, { Bar } from "./a";
        Foo;
      |} in
      let expected =
        {|
        import type { Bar } from "./a";
        import Foo from "./a";
        Foo;
      |}
      in
      assert_fixed ~ctxt ~expected ~contents ~loc:(Loc.mk_loc (2, 0) (2, 3))
    );
  ]

let type_imports_in_value_imports_tests =
  [
    ( "named_specifiers_1" >:: fun ctxt ->
      let contents = {|
        import { type Foo } from "./a";
        Foo;
      |} in
      let expected = {|
        import { Foo } from "./a";
        Foo;
      |} in
      assert_fixed ~ctxt ~expected ~contents ~loc:(Loc.mk_loc (2, 0) (2, 3))
    );
    ( "named_specifiers_2" >:: fun ctxt ->
      let contents = {|
        import { type Foo, Bar } from "./a";
        Foo;
      |} in
      let expected = {|
        import { Foo, Bar } from "./a";
        Foo;
      |} in
      assert_fixed ~ctxt ~expected ~contents ~loc:(Loc.mk_loc (2, 0) (2, 3))
    );
  ]

[@@@ocamlformat "disable=false"]

let tests =
  "autofix_type_to_value_import_tests"
  >::: [
         "simple_type_imports_tests" >::: simple_type_imports_tests;
         "type_imports_in_value_imports_tests" >::: type_imports_in_value_imports_tests;
       ]
