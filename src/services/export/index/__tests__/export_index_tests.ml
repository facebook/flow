(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let () = File_key.set_project_root "/"

let file_source name = Export_index.File_key (File_key.SourceFile name)

let file_lib name = Export_index.File_key (File_key.LibFile name)

let global = Export_index.Global

let declare_module name = Export_index.Builtin name

let assert_exports =
  let printer = [%show: Export_index.export list] in
  fun ~ctxt expected actual ->
    let actual = Export_index.ExportMap.keys actual in
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

      assert_exports ~ctxt expected actual
    );
    ( "compare_export" >:: fun ctxt ->
      let file_a = file_source "path/to/a.js" in
      let file_b = file_source "path/to/b.js" in
      let file_foo = file_source "path/to/foo.js" in
      let builtin_z = declare_module (Flow_import_specifier.userland "z") in

      (* defaults before named before namespace, then
         globals before builtins before source files *)
      let expected =
        [
          (builtin_z, Export_index.Default);
          (file_foo, Export_index.Default);
          (global, Export_index.Named);
          (file_a, Export_index.Named);
          (file_b, Export_index.Named);
          (file_foo, Export_index.Namespace);
        ]
      in

      let index =
        Export_index.empty
        |> Export_index.add "foo" builtin_z Export_index.Default
        |> Export_index.add "foo" file_a Export_index.Named
        |> Export_index.add "foo" file_b Export_index.Named
        |> Export_index.add "foo" file_foo Export_index.Default
        |> Export_index.add "foo" file_foo Export_index.Namespace
        |> Export_index.add "foo" global Export_index.Named
      in

      let actual = Export_index.find "foo" index in

      assert_exports ~ctxt expected actual
    );
  ]

(** Generate a synthetic Export_index.t with [num_names] unique export names,
    each having a single export from a unique source file. *)
let generate_index ~prefix ~num_names =
  let index = ref Export_index.empty in
  for i = 0 to num_names - 1 do
    let name = Printf.sprintf "export_%s_%d" prefix i in
    let source = file_source (Printf.sprintf "/path/to/%s_%d.js" prefix i) in
    index := Export_index.add name source Export_index.Named !index
  done;
  !index

let merge_all_tests =
  [
    ( "merge_all_empty" >:: fun _ctxt ->
      let result = Export_index.merge_all [] in
      assert_bool "empty merge_all should be empty" (Export_index.keys result = [])
    );
    ( "merge_all_singleton" >:: fun _ctxt ->
      let idx =
        Export_index.empty |> Export_index.add "foo" (file_source "a.js") Export_index.Named
      in
      let result = Export_index.merge_all [idx] in
      assert_bool
        "singleton merge_all should have foo"
        (Export_index.find "foo" result |> Export_index.ExportMap.is_empty |> not)
    );
    ( "merge_all_correctness" >:: fun ctxt ->
      (* Build multiple indices with overlapping and disjoint keys *)
      let idx1 =
        Export_index.empty
        |> Export_index.add "foo" (file_source "a.js") Export_index.Named
        |> Export_index.add "bar" (file_source "b.js") Export_index.Default
      in
      let idx2 =
        Export_index.empty
        |> Export_index.add "foo" (file_source "c.js") Export_index.Named
        |> Export_index.add "baz" (file_source "d.js") Export_index.NamedType
      in
      let idx3 =
        Export_index.empty
        |> Export_index.add "foo" (file_source "e.js") Export_index.Default
        |> Export_index.add "bar" (file_source "f.js") Export_index.Named
        |> Export_index.add "qux" (file_source "g.js") Export_index.Namespace
      in
      let indices = [idx1; idx2; idx3] in
      let merge_all_result = Export_index.merge_all indices in
      let fold_result =
        Base.List.fold indices ~init:Export_index.empty ~f:(fun acc i -> Export_index.merge i acc)
      in
      (* Both should produce the same keys *)
      let printer = [%show: string list] in
      assert_equal
        ~ctxt
        ~printer
        (Export_index.keys fold_result)
        (Export_index.keys merge_all_result);
      (* Verify exports under each key match *)
      List.iter
        (fun name ->
          let expected = Export_index.find name fold_result in
          let actual = Export_index.find name merge_all_result in
          assert_equal
            ~ctxt
            ~printer:[%show: Export_index.export list]
            (Export_index.ExportMap.keys expected)
            (Export_index.ExportMap.keys actual))
        (Export_index.keys fold_result)
    );
    ( "merge_all_benchmark" >:: fun _ctxt ->
      (* Generate ~700 indices with ~1500 names each = ~1M total entries
         This mimics production scale *)
      let num_indices = 700 in
      let names_per_index = 1500 in
      let indices =
        List.init num_indices (fun i ->
            generate_index ~prefix:(string_of_int i) ~num_names:names_per_index
        )
      in
      (* Benchmark fold-based merge (old approach) *)
      let t0 = Unix.gettimeofday () in
      let _fold_result =
        Base.List.fold indices ~init:Export_index.empty ~f:(fun acc i -> Export_index.merge i acc)
      in
      let t1 = Unix.gettimeofday () in
      let fold_time = t1 -. t0 in
      (* Benchmark merge_all (new approach) *)
      let t2 = Unix.gettimeofday () in
      let _merge_all_result = Export_index.merge_all indices in
      let t3 = Unix.gettimeofday () in
      let merge_all_time = t3 -. t2 in
      let speedup = fold_time /. merge_all_time in
      Printf.printf
        "\n=== BENCHMARK: %d indices x %d names ===\nfold merge:  %.3fs\nmerge_all:   %.3fs\nspeedup:     %.2fx\n"
        num_indices
        names_per_index
        fold_time
        merge_all_time
        speedup;
      (* Assert merge_all is faster *)
      assert_bool
        (Printf.sprintf
           "merge_all (%.3fs) should be faster than fold merge (%.3fs)"
           merge_all_time
           fold_time
        )
        (merge_all_time < fold_time)
    );
    ( "merge_all_benchmark_overlapping" >:: fun _ctxt ->
      (* Generate indices with heavily overlapping keys (more realistic).
         700 indices with 1500 shared names, each with unique source files.
         Also mix in some unique-per-worker names (10%) to mimic reality. *)
      let num_indices = 700 in
      let names_per_index = 1500 in
      let unique_per_index = 150 in
      let indices =
        List.init num_indices (fun i ->
            let index = ref Export_index.empty in
            for j = 0 to names_per_index - 1 do
              let name = Printf.sprintf "shared_export_%d" j in
              let source = file_source (Printf.sprintf "/path/to/file_%d_%d.js" i j) in
              index := Export_index.add name source Export_index.Named !index
            done;
            for j = 0 to unique_per_index - 1 do
              let name = Printf.sprintf "unique_%d_%d" i j in
              let source = file_source (Printf.sprintf "/path/to/unique_%d_%d.js" i j) in
              index := Export_index.add name source Export_index.Named !index
            done;
            !index
        )
      in
      let t0 = Unix.gettimeofday () in
      let _fold_result =
        Base.List.fold indices ~init:Export_index.empty ~f:(fun acc i -> Export_index.merge i acc)
      in
      let t1 = Unix.gettimeofday () in
      let fold_time = t1 -. t0 in
      let t2 = Unix.gettimeofday () in
      let _merge_all_result = Export_index.merge_all indices in
      let t3 = Unix.gettimeofday () in
      let merge_all_time = t3 -. t2 in
      let speedup = fold_time /. merge_all_time in
      Printf.printf
        "\n=== BENCHMARK (overlapping keys): %d indices x %d names ===\nfold merge:  %.3fs\nmerge_all:   %.3fs\nspeedup:     %.2fx\n"
        num_indices
        names_per_index
        fold_time
        merge_all_time
        speedup;
      assert_bool
        (Printf.sprintf
           "merge_all (%.3fs) should be faster than fold merge (%.3fs)"
           merge_all_time
           fold_time
        )
        (merge_all_time < fold_time)
    );
  ]

let suite = "export_index" >::: ["find" >::: find_tests; "merge_all" >::: merge_all_tests]
