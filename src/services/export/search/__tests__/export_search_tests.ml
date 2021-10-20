(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Export_search

let default = Export_index.Default

let named = Export_index.Named

let named_type = Export_index.NamedType

let namespace = Export_index.Namespace

let sf name = Export_index.File_key (File_key.SourceFile name)

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
  { results = List.map (fun (name, source, kind) -> { name; source; kind }) results; is_incomplete }

let tests =
  [
    ( "case_insensitive" >:: fun ctxt ->
      let t = init index in
      let results = search_values "foobar" t in
      let expected = mk_results [("FooBar", sf "/a/foobar.js", named)] in
      assert_equal ~ctxt ~printer:show_search_results expected results
    );
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
      assert_equal ~ctxt ~printer:string_of_int 4 (List.length results)
    );
    ( "is_incomplete_multiple_files" >:: fun ctxt ->
      (* if there are 3 "Foo"s and "FooBar", but max_results = 2, make sure
         we don't add all 3 "Foo"s. *)
      let index = Export_index.add "Foo" (sf "/a/foo_2.js") named index in
      let index = Export_index.add "Foo" (sf "/a/foo_3.js") named index in
      let options = { default_options with Fuzzy_path.max_results = 2 } in
      let t = init index in
      let { results; is_incomplete } = search_values ~options "Foo" t in
      assert_equal ~ctxt true is_incomplete;
      assert_equal ~ctxt ~printer:string_of_int 2 (List.length results)
    );
    ( "same_name_different_file" >:: fun ctxt ->
      let index = Export_index.add "FooBar" (sf "/a/f.js") named index in
      let t = init index in

      let results = search_values "FooBar" t in
      let expected =
        mk_results [("FooBar", sf "/a/f.js", named); ("FooBar", sf "/a/foobar.js", named)]
      in
      assert_equal ~ctxt ~printer:show_search_results expected results
    );
    ( "filter_values_and_types" >:: fun ctxt ->
      let index = Export_index.add "FooBar" (sf "/a/f_type.js") named_type index in
      let t = init index in

      let results = search_values "FooBar" t in
      let expected = mk_results [("FooBar", sf "/a/foobar.js", named)] in
      assert_equal ~ctxt ~printer:show_search_results expected results;

      let results = search_types "FooBar" t in
      let expected = mk_results [("FooBar", sf "/a/f_type.js", named_type)] in
      assert_equal ~ctxt ~printer:show_search_results expected results
    );
    ( "max_results_filtered_by_kind" >:: fun ctxt ->
      let index = Export_index.add "FooBar" (sf "/a/foobar_a.js") named index in
      let index = Export_index.add "FooBar" (sf "/a/foobar_d.js") named index in
      let index = Export_index.add "FooBar" (sf "/a/foobar_b.js") named_type index in
      let index = Export_index.add "FooBar" (sf "/a/foobar_c.js") named_type index in
      let index = Export_index.add "FooBar" (sf "/a/foobar_e.js") named_type index in
      let t = init index in

      let options = { default_options with Fuzzy_path.max_results = 2 } in

      let results = search_values ~options "FooBar" t in
      let expected =
        mk_results
          ~is_incomplete:true
          [("FooBar", sf "/a/foobar.js", named); ("FooBar", sf "/a/foobar_a.js", named)]
      in
      assert_equal ~ctxt ~printer:show_search_results expected results;

      let results = search_types ~options "FooBar" t in
      let expected =
        mk_results
          ~is_incomplete:true
          [("FooBar", sf "/a/foobar_b.js", named_type); ("FooBar", sf "/a/foobar_c.js", named_type)]
      in
      assert_equal ~ctxt ~printer:show_search_results expected results
    );
    ( "default_before_named_before_namespace" >:: fun ctxt ->
      let index =
        Export_index.empty
        |> Export_index.add "FooBar" (sf "/a/FooBar.js") namespace
        |> Export_index.add "FooBar" (sf "/a/FooBar.js") default
        |> Export_index.add "FooBar" (sf "/a/FooBar.js") named
      in
      let t = init index in
      let results = search_values ~options:default_options "FooBar" t in

      let expected =
        mk_results
          ~is_incomplete:false
          [
            ("FooBar", sf "/a/FooBar.js", default);
            ("FooBar", sf "/a/FooBar.js", named);
            ("FooBar", sf "/a/FooBar.js", namespace);
          ]
      in
      assert_equal ~ctxt ~printer:show_search_results expected results
    );
  ]

let suite = "export_search" >::: tests
