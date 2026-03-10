(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

(* Set default roots so that the with_roots finally block restores to
   the same state. Tests that don't use with_roots are root-independent
   (sentinels short-circuit before dereferencing the root; compare uses
   suffix only). *)
let () =
  File_key.set_project_root "/project/";
  File_key.set_flowlib_root "/flowlib/"

(* Exception-safe bracket: sets roots before the test body and restores
   defaults afterwards, even if an assertion fails. *)
let with_roots ?(project = "/data/project/") ?(flowlib = "/usr/lib/flowlib/") f ctxt =
  File_key.set_project_root project;
  File_key.set_flowlib_root flowlib;
  Fun.protect
    ~finally:(fun () ->
      File_key.set_project_root "/project/";
      File_key.set_flowlib_root "/flowlib/")
    (fun () -> f ctxt)

let source_file_tests =
  [
    "in_root_suffix"
    >:: with_roots (fun ctxt ->
            let fk = File_key.source_file_of_absolute "/data/project/src/a.js" in
            assert_equal ~ctxt ~printer:Fun.id "src/a.js" (File_key.suffix fk)
        );
    "in_root_to_string_round_trip"
    >:: with_roots (fun ctxt ->
            let fk = File_key.source_file_of_absolute "/data/project/src/a.js" in
            assert_equal ~ctxt ~printer:Fun.id "/data/project/src/a.js" (File_key.to_string fk)
        );
    "out_of_root_suffix_is_absolute"
    >:: with_roots (fun ctxt ->
            let fk = File_key.source_file_of_absolute "/other/place/foo.js" in
            assert_equal ~ctxt ~printer:Fun.id "/other/place/foo.js" (File_key.suffix fk)
        );
    "out_of_root_to_string_round_trip"
    >:: with_roots (fun ctxt ->
            let fk = File_key.source_file_of_absolute "/other/place/foo.js" in
            assert_equal ~ctxt ~printer:Fun.id "/other/place/foo.js" (File_key.to_string fk)
        );
    "prefix_not_directory_boundary"
    >:: with_roots ~project:"/data/foo/" (fun ctxt ->
            (* /data/foo/ must NOT strip from /data/foobar/x.js *)
            let fk = File_key.source_file_of_absolute "/data/foobar/x.js" in
            assert_equal ~ctxt ~printer:Fun.id "/data/foobar/x.js" (File_key.suffix fk);
            assert_equal ~ctxt ~printer:Fun.id "/data/foobar/x.js" (File_key.to_string fk)
        );
    "root_string_at_non_prefix_position"
    >:: with_roots (fun ctxt ->
            (* Path contains root string but not at the start *)
            let fk = File_key.source_file_of_absolute "/other/data/project/file.js" in
            assert_equal ~ctxt ~printer:Fun.id "/other/data/project/file.js" (File_key.suffix fk);
            assert_equal ~ctxt ~printer:Fun.id "/other/data/project/file.js" (File_key.to_string fk)
        );
    "json_file_out_of_root"
    >:: with_roots (fun ctxt ->
            let fk = File_key.json_file_of_absolute "/other/package.json" in
            assert_equal ~ctxt ~printer:Fun.id "/other/package.json" (File_key.suffix fk);
            assert_equal ~ctxt ~printer:Fun.id "/other/package.json" (File_key.to_string fk)
        );
    "resource_file_out_of_root"
    >:: with_roots (fun ctxt ->
            let fk = File_key.resource_file_of_absolute "/other/image.png" in
            assert_equal ~ctxt ~printer:Fun.id "/other/image.png" (File_key.suffix fk);
            assert_equal ~ctxt ~printer:Fun.id "/other/image.png" (File_key.to_string fk)
        );
  ]

let lib_file_tests =
  [
    "lib_under_flowlib_root"
    >:: with_roots (fun ctxt ->
            let fk = File_key.lib_file_of_absolute "/usr/lib/flowlib/core.js" in
            (* suffix is the flowlib marker + relative path *)
            assert_equal
              ~ctxt
              ~printer:Fun.id
              (File_key.flowlib_marker ^ "core.js")
              (File_key.suffix fk);
            (* to_string reconstructs the absolute flowlib path *)
            assert_equal ~ctxt ~printer:Fun.id "/usr/lib/flowlib/core.js" (File_key.to_string fk)
        );
    "lib_under_project_root"
    >:: with_roots (fun ctxt ->
            let fk = File_key.lib_file_of_absolute "/data/project/lib/mylib.js" in
            assert_equal ~ctxt ~printer:Fun.id "lib/mylib.js" (File_key.suffix fk);
            assert_equal ~ctxt ~printer:Fun.id "/data/project/lib/mylib.js" (File_key.to_string fk)
        );
    "lib_outside_both_roots"
    >:: with_roots (fun ctxt ->
            let fk = File_key.lib_file_of_absolute "/somewhere/else/lib.js" in
            assert_equal ~ctxt ~printer:Fun.id "/somewhere/else/lib.js" (File_key.suffix fk);
            assert_equal ~ctxt ~printer:Fun.id "/somewhere/else/lib.js" (File_key.to_string fk)
        );
  ]

let sentinel_tests =
  [
    ( "empty_suffix" >:: fun ctxt ->
      let fk = File_key.SourceFile "" in
      assert_equal ~ctxt ~printer:Fun.id "" (File_key.suffix fk);
      assert_equal ~ctxt ~printer:Fun.id "" (File_key.to_string fk)
    );
    ( "stdin_sentinel" >:: fun ctxt ->
      let fk = File_key.SourceFile "-" in
      assert_equal ~ctxt ~printer:Fun.id "-" (File_key.suffix fk);
      assert_equal ~ctxt ~printer:Fun.id "-" (File_key.to_string fk)
    );
    "stdin_sentinel_via_of_absolute"
    >:: with_roots (fun ctxt ->
            let fk = File_key.source_file_of_absolute "-" in
            assert_equal ~ctxt ~printer:Fun.id "-" (File_key.suffix fk);
            assert_equal ~ctxt ~printer:Fun.id "-" (File_key.to_string fk)
        );
  ]

let root_tests =
  [
    "trailing_slash_enforced"
    >:: with_roots ~project:"/data/project" (fun ctxt ->
            (* Setting root without trailing slash should behave identically *)
            let fk = File_key.source_file_of_absolute "/data/project/src/a.js" in
            assert_equal ~ctxt ~printer:Fun.id "src/a.js" (File_key.suffix fk);
            assert_equal ~ctxt ~printer:Fun.id "/data/project/src/a.js" (File_key.to_string fk)
        );
    "trailing_slash_idempotent"
    >:: with_roots (fun ctxt ->
            (* Setting root with trailing slash should not add a double slash *)
            let fk = File_key.source_file_of_absolute "/data/project/src/a.js" in
            assert_equal ~ctxt ~printer:Fun.id "src/a.js" (File_key.suffix fk);
            assert_equal ~ctxt ~printer:Fun.id "/data/project/src/a.js" (File_key.to_string fk)
        );
  ]

let compare_tests =
  [
    "same_variant_in_root_vs_out_of_root"
    >:: with_roots (fun _ctxt ->
            let in_root = File_key.source_file_of_absolute "/data/project/src/a.js" in
            let out_root = File_key.source_file_of_absolute "/other/b.js" in
            (* out-of-root suffix starts with '/', which sorts before relative
               suffixes in ASCII ('/' = 47 < 'a'..'z' = 97..122) *)
            assert_bool
              "out-of-root should sort before in-root"
              (File_key.compare out_root in_root < 0)
        );
    ( "cross_variant_ordering" >:: fun _ctxt ->
      (* LibFile < SourceFile = JsonFile < ResourceFile *)
      let lib = File_key.LibFile "x" in
      let src = File_key.SourceFile "x" in
      let json = File_key.JsonFile "x" in
      let res = File_key.ResourceFile "x" in
      assert_bool "lib < source" (File_key.compare lib src < 0);
      assert_bool "source = json" (File_key.compare src json = 0);
      assert_bool "source < resource" (File_key.compare src res < 0);
      assert_bool "lib < resource" (File_key.compare lib res < 0)
    );
  ]

let tests =
  "file_key"
  >::: [
         "source_file" >::: source_file_tests;
         "lib_file" >::: lib_file_tests;
         "sentinels" >::: sentinel_tests;
         "roots" >::: root_tests;
         "compare" >::: compare_tests;
       ]
