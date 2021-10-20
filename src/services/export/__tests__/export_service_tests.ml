(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Export_service.For_test

let sf name = File_key.SourceFile name

let module_file file_key = Modulename.Filename file_key

let module_name name = Modulename.String name

let string_of_modulename_tests =
  [
    ( "string_dot" >:: fun ctxt ->
      let actual = string_of_modulename (module_name "FooBar.baz") in
      let expected = "FooBar" in
      assert_equal ~ctxt ~printer:(fun x -> x) expected actual
    );
    ( "filename_dot" >:: fun ctxt ->
      let actual = string_of_modulename (module_file (sf "/foo/bar/FooBar.baz.js")) in
      let expected = "FooBar" in
      assert_equal ~ctxt ~printer:(fun x -> x) expected actual
    );
    ( "string_multiple_dots" >:: fun ctxt ->
      let actual = string_of_modulename (module_name "FooBar.a.b") in
      let expected = "FooBar" in
      assert_equal ~ctxt ~printer:(fun x -> x) expected actual
    );
    ( "filename_multiple_dots" >:: fun ctxt ->
      let actual = string_of_modulename (module_file (sf "/foo/bar/FooBar.a.b.js")) in
      let expected = "FooBar" in
      assert_equal ~ctxt ~printer:(fun x -> x) expected actual
    );
    ( "string_camelcase_dashes" >:: fun ctxt ->
      let actual = string_of_modulename (module_name "foo-bar-baz") in
      let expected = "fooBarBaz" in
      assert_equal ~ctxt ~printer:(fun x -> x) expected actual
    );
    ( "filename_camelcase_dashes" >:: fun ctxt ->
      let actual = string_of_modulename (module_file (sf "/foo/bar/foo-bar-baz.js")) in
      let expected = "fooBarBaz" in
      assert_equal ~ctxt ~printer:(fun x -> x) expected actual
    );
    ( "scoped_package" >:: fun ctxt ->
      let actual = string_of_modulename (module_name "@example/xyz") in
      let expected = "xyz" in
      assert_equal ~ctxt ~printer:(fun x -> x) expected actual
    );
  ]

let suite = "export_service" >::: ["string_of_modulename" >::: string_of_modulename_tests]
