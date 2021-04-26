(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let file_source name = Export_index.File_key (File_key.SourceFile name)

let file_lib name = Export_index.File_key (File_key.LibFile name)

let global = Export_index.Global

let declare_module name = Export_index.Builtin name

let assert_exports =
  let printer = [%show: Export_index.export list] in
  fun ~ctxt expected actual ->
    let actual = Export_index.ExportSet.elements actual in
    assert_equal ~ctxt ~printer expected actual

let find_tests =
  [
    ( "sorted_by_filename_ignoring_extension" >:: fun ctxt ->
      let file_a = file_source "path/to/a.js" in
      let file_a_foo = file_source "path/to/a.foo.js" in
      let file_b = file_source "path/to/b.js" in
      let lib = file_lib "path/to/a.bar.js" in

      (* - libs are mixed together with source files
         - a.js comes before a.bar.js which comes before a.foo.js, even
           though .j is lexographically after .b. *)
      let expected =
        [
          (file_a, Export_index.Default);
          (lib, Export_index.Default);
          (file_a_foo, Export_index.Default);
          (file_b, Export_index.Default);
        ]
      in

      let index =
        Export_index.empty
        |> Export_index.add "foo" file_a_foo Export_index.Default
        |> Export_index.add "foo" file_b Export_index.Default
        |> Export_index.add "foo" file_a Export_index.Default
        |> Export_index.add "foo" lib Export_index.Default
      in

      let actual = Export_index.find "foo" index in

      assert_exports ~ctxt expected actual );
    ( "builtins_before_sources_before_globals" >:: fun ctxt ->
      let file_a = file_source "path/to/a.js" in
      let builtin_z = declare_module "z" in

      (* builtins come first, even if their names are lexographically last *)
      let expected =
        [
          (builtin_z, Export_index.Default);
          (file_a, Export_index.Default);
          (global, Export_index.Named);
        ]
      in

      let index =
        Export_index.empty
        |> Export_index.add "foo" file_a Export_index.Default
        |> Export_index.add "foo" builtin_z Export_index.Default
        |> Export_index.add "foo" global Export_index.Named
      in

      let actual = Export_index.find "foo" index in

      assert_exports ~ctxt expected actual );
  ]

let suite = "export_index" >::: ["find" >::: find_tests]
