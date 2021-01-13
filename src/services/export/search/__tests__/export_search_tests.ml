(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Export_search

let named = Export_index.Named

let sf name = File_key.SourceFile name

let index =
  let open Export_index in
  empty
  |> add "F" (sf "/a/foo.js") named
  |> add "Fo" (sf "/a/foo.js") named
  |> add "Foo" (sf "/a/foo.js") named
  |> add "B" (sf "/a/bar.js") named
  |> add "Ba" (sf "/a/bar.js") named
  |> add "Bar" (sf "/a/bar.js") named
  |> add "FooBar" (sf "/a/foobar.js") named
  |> add "Baz" (sf "/a/baz.js") named

let mk_results ?(is_incomplete = false) results =
  {
    results = List.map (fun (name, file_key, kind) -> { name; file_key; kind }) results;
    is_incomplete;
  }

let tests =
  [
    ( "case_insensitive" >:: fun ctxt ->
      let t = init index in
      let results = search_values "foobar" t in
      let expected = mk_results [("FooBar", sf "/a/foobar.js", named)] in
      assert_equal ~ctxt ~printer:show_search_results expected results );
    ( "is_incomplete" >:: fun ctxt ->
      let t = init index in
      let options = { default_options with Fuzzy_path.max_results = 2 } in
      let { results; is_incomplete } = search_values ~options "f" t in
      assert_equal ~ctxt true is_incomplete;
      assert_equal ~ctxt ~printer:string_of_int 2 (List.length results);

      let options = { default_options with Fuzzy_path.max_results = 4 } in
      let { results; is_incomplete } = search_values ~options "f" t in
      assert_equal ~ctxt false is_incomplete;
      assert_equal ~ctxt ~printer:string_of_int 4 (List.length results);

      let options = { default_options with Fuzzy_path.max_results = 5 } in
      let { results; is_incomplete } = search_values ~options "f" t in
      assert_equal ~ctxt false is_incomplete;
      assert_equal ~ctxt ~printer:string_of_int 4 (List.length results) );
    ( "same_name_different_file" >:: fun ctxt ->
      let index = Export_index.add "FooBar" (sf "/a/f.js") named index in
      let t = init index in

      let results = search_values "FooBar" t in
      let expected =
        mk_results [("FooBar", sf "/a/f.js", named); ("FooBar", sf "/a/foobar.js", named)]
      in
      assert_equal ~ctxt ~printer:show_search_results expected results );
  ]

let suite = "export_search" >::: tests
