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
    "out_of_root_suffix_is_relative"
    >:: with_roots (fun ctxt ->
            (* Out-of-root files get relative suffixes for saved state portability.
               /other/place/foo.js relative to /data/project/ = ../../other/place/foo.js *)
            let fk = File_key.source_file_of_absolute "/other/place/foo.js" in
            assert_equal ~ctxt ~printer:Fun.id "../../other/place/foo.js" (File_key.suffix fk)
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
            assert_equal ~ctxt ~printer:Fun.id "../foobar/x.js" (File_key.suffix fk);
            assert_equal ~ctxt ~printer:Fun.id "/data/foobar/x.js" (File_key.to_string fk)
        );
    "root_string_at_non_prefix_position"
    >:: with_roots (fun ctxt ->
            (* Path contains root string but not at the start *)
            let fk = File_key.source_file_of_absolute "/other/data/project/file.js" in
            assert_equal
              ~ctxt
              ~printer:Fun.id
              "../../other/data/project/file.js"
              (File_key.suffix fk);
            assert_equal ~ctxt ~printer:Fun.id "/other/data/project/file.js" (File_key.to_string fk)
        );
    "json_file_out_of_root"
    >:: with_roots (fun ctxt ->
            let fk = File_key.json_file_of_absolute "/other/package.json" in
            assert_equal ~ctxt ~printer:Fun.id "../../other/package.json" (File_key.suffix fk);
            assert_equal ~ctxt ~printer:Fun.id "/other/package.json" (File_key.to_string fk)
        );
    "resource_file_out_of_root"
    >:: with_roots (fun ctxt ->
            let fk = File_key.resource_file_of_absolute "/other/image.png" in
            assert_equal ~ctxt ~printer:Fun.id "../../other/image.png" (File_key.suffix fk);
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
            assert_equal ~ctxt ~printer:Fun.id "../../somewhere/else/lib.js" (File_key.suffix fk);
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

(* Windows-style path tests. Since Filename.is_relative and Filename.dir_sep
   are compile-time platform-specific, we test the underlying parameterized
   functions via File_key.For_tests with a Windows-style is_relative and
   dir_sep. This validates the Windows logic on any host platform. *)

(* Mimics OCaml's Win32 Filename.is_relative: a path is absolute if it
   starts with / or \, or has a drive letter like C:\ or C:/ *)
let win_is_relative s =
  let len = String.length s in
  if len >= 1 && (s.[0] = '/' || s.[0] = '\\') then
    false
  else if len >= 3 && s.[1] = ':' && (s.[2] = '/' || s.[2] = '\\') then
    false
  else
    true

let win_dir_sep = "\\"

let windows_path_tests =
  let open File_key.For_tests in
  [
    ( "enforce_trailing_sep_recognizes_backslash" >:: fun ctxt ->
      let result = enforce_trailing_sep ~dir_sep:win_dir_sep "C:\\Users\\project\\" in
      assert_equal ~ctxt ~printer:Fun.id "C:\\Users\\project\\" result
    );
    ( "enforce_trailing_sep_adds_backslash" >:: fun ctxt ->
      let result = enforce_trailing_sep ~dir_sep:win_dir_sep "C:\\Users\\project" in
      assert_equal ~ctxt ~printer:Fun.id "C:\\Users\\project\\" result
    );
    ( "enforce_trailing_sep_recognizes_forward_slash" >:: fun ctxt ->
      let result = enforce_trailing_sep ~dir_sep:win_dir_sep "C:\\Users\\project/" in
      assert_equal ~ctxt ~printer:Fun.id "C:\\Users\\project/" result
    );
    ( "resolve_root_recognizes_drive_letter" >:: fun ctxt ->
      let result =
        resolve_root_with
          ~is_relative:win_is_relative
          (fun () -> "C:\\Users\\project\\")
          "D:\\other\\file.js"
      in
      assert_equal ~ctxt ~printer:Fun.id "D:\\other\\file.js" result
    );
    ( "resolve_root_prepends_for_relative" >:: fun ctxt ->
      let result =
        resolve_root_with
          ~is_relative:win_is_relative
          (fun () -> "C:\\Users\\project\\")
          "src\\a.js"
      in
      assert_equal ~ctxt ~printer:Fun.id "C:\\Users\\project\\src\\a.js" result
    );
    ( "resolve_root_recognizes_unc_path" >:: fun ctxt ->
      let result =
        resolve_root_with
          ~is_relative:win_is_relative
          (fun () -> "C:\\Users\\project\\")
          "\\\\server\\share\\file.js"
      in
      assert_equal ~ctxt ~printer:Fun.id "\\\\server\\share\\file.js" result
    );
    ( "in_root_round_trip" >:: fun ctxt ->
      let root = enforce_trailing_sep ~dir_sep:win_dir_sep "C:\\Users\\project" in
      assert_equal ~ctxt ~printer:Fun.id "C:\\Users\\project\\" root;
      let path = "C:\\Users\\project\\src\\a.js" in
      let sfx = File_key.strip_prefix root path in
      assert_equal ~ctxt ~printer:Fun.id "src\\a.js" sfx;
      let resolved = resolve_root_with ~is_relative:win_is_relative (fun () -> root) sfx in
      assert_equal ~ctxt ~printer:Fun.id "C:\\Users\\project\\src\\a.js" resolved
    );
    ( "out_of_root_round_trip" >:: fun ctxt ->
      let root = enforce_trailing_sep ~dir_sep:win_dir_sep "C:\\Users\\project\\" in
      let path = "D:\\other\\file.js" in
      let sfx = File_key.strip_prefix root path in
      assert_equal ~ctxt ~printer:Fun.id "D:\\other\\file.js" sfx;
      let resolved = resolve_root_with ~is_relative:win_is_relative (fun () -> root) sfx in
      assert_equal ~ctxt ~printer:Fun.id "D:\\other\\file.js" resolved
    );
    (* relative_path_from tests with Windows paths *)
    (* resolve_root_with tests with ".." segments *)
    ( "resolve_root_dotdot_unix" >:: fun ctxt ->
      let result =
        resolve_root_with
          ~is_relative:Filename.is_relative
          (fun () -> "/data/project/")
          "../../other/file.js"
      in
      assert_equal ~ctxt ~printer:Fun.id "/other/file.js" result
    );
    ( "resolve_root_dotdot_deep" >:: fun ctxt ->
      let result =
        resolve_root_with
          ~is_relative:Filename.is_relative
          (fun () -> "/a/b/c/d/")
          "../../../x/y.js"
      in
      assert_equal ~ctxt ~printer:Fun.id "/a/x/y.js" result
    );
    ( "resolve_root_dotdot_with_leading_dot" >:: fun ctxt ->
      let result =
        resolve_root_with
          ~is_relative:Filename.is_relative
          (fun () -> "/data/project/")
          "./../other/file.js"
      in
      assert_equal ~ctxt ~printer:Fun.id "/data/other/file.js" result
    );
    (* Tests that exercise normalize_dir_sep and dirname pass ~dir_sep to
       simulate Windows backslash handling on any host platform, matching
       the pattern used by the other Windows tests above. *)
    ( "resolve_root_dotdot_windows" >:: fun ctxt ->
      let result =
        resolve_root_with
          ~is_relative:win_is_relative
          ~dir_sep:win_dir_sep
          (fun () -> "C:\\Users\\repo\\xplat\\js\\")
          "../../arvr/js/app.js"
      in
      assert_equal ~ctxt ~printer:Fun.id "C:/Users/repo/arvr/js/app.js" result
    );
    ( "resolve_root_dotdot_backslash_suffix" >:: fun ctxt ->
      let result =
        resolve_root_with
          ~is_relative:win_is_relative
          ~dir_sep:win_dir_sep
          (fun () -> "C:\\Users\\repo\\xplat\\js\\")
          "..\\..\\arvr\\js\\app.js"
      in
      assert_equal ~ctxt ~printer:Fun.id "C:/Users/repo/arvr/js/app.js" result
    );
    ( "relative_path_from_windows_out_of_root" >:: fun ctxt ->
      let result =
        relative_path_from
          ~dir_sep:win_dir_sep
          "C:\\Users\\dev\\project\\"
          "D:\\other\\repo\\file.js"
      in
      assert_equal ~ctxt ~printer:Fun.id "../../../../D:/other/repo/file.js" result
    );
    ( "relative_path_from_windows_in_root" >:: fun ctxt ->
      let result =
        relative_path_from
          ~dir_sep:win_dir_sep
          "C:\\Users\\dev\\project\\"
          "C:\\Users\\dev\\project\\src\\file.js"
      in
      assert_equal ~ctxt ~printer:Fun.id "src/file.js" result
    );
    ( "relative_path_from_mixed_separators" >:: fun ctxt ->
      let result =
        relative_path_from
          ~dir_sep:win_dir_sep
          "C:/Users/dev\\project/"
          "C:/Users/dev\\project/src\\file.js"
      in
      assert_equal ~ctxt ~printer:Fun.id "src/file.js" result
    );
    ( "relative_path_from_windows_portability" >:: fun ctxt ->
      let suffix_a =
        relative_path_from
          ~dir_sep:win_dir_sep
          "C:\\Users\\alice\\repo\\xplat\\js\\"
          "C:\\Users\\alice\\repo\\arvr\\js\\app.js"
      in
      let suffix_b =
        relative_path_from
          ~dir_sep:win_dir_sep
          "C:\\Users\\bob\\repo\\xplat\\js\\"
          "C:\\Users\\bob\\repo\\arvr\\js\\app.js"
      in
      assert_equal ~ctxt ~printer:Fun.id suffix_a suffix_b;
      assert_equal ~ctxt ~printer:Fun.id "../../arvr/js/app.js" suffix_a
    );
  ]

let compare_tests =
  [
    "same_variant_in_root_vs_out_of_root"
    >:: with_roots (fun _ctxt ->
            let in_root = File_key.source_file_of_absolute "/data/project/src/a.js" in
            let out_root = File_key.source_file_of_absolute "/other/b.js" in
            (* out-of-root suffix is now relative (../../other/b.js) which
               sorts before in-root suffix (src/a.js) in ASCII since '.' < 's' *)
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

(* S634538 regression: saved state portability for out-of-root files.
   When saved state is created on machine A (root /machine_a/project/) and
   loaded on machine B (root /machine_b/project/), File_key values for
   out-of-root files must compare equal. This requires suffixes to be
   portable relative paths (../../sibling/file.js), NOT absolute paths
   that embed the machine-specific root. *)
let saved_state_portability_tests =
  [
    ( "out_of_root_suffix_is_portable_across_machines" >:: fun ctxt ->
      (* Machine A creates a File_key for an out-of-root file *)
      File_key.set_project_root "/machine_a/repo/xplat/js/";
      File_key.set_flowlib_root "/machine_a/flowlib/";
      let fk_a = File_key.source_file_of_absolute "/machine_a/repo/arvr/js/app.js" in
      let suffix_a = File_key.suffix fk_a in

      (* Machine B creates a File_key for the same logical file *)
      File_key.set_project_root "/machine_b/repo/xplat/js/";
      File_key.set_flowlib_root "/machine_b/flowlib/";
      let fk_b = File_key.source_file_of_absolute "/machine_b/repo/arvr/js/app.js" in
      let suffix_b = File_key.suffix fk_b in

      (* Suffixes must be identical — this is what makes saved state portable *)
      assert_equal ~ctxt ~printer:Fun.id suffix_a suffix_b;

      (* Both must be relative paths, not absolute *)
      assert_bool "suffix should be relative" (Filename.is_relative suffix_a);

      (* Both must resolve to the correct absolute path on their machine *)
      assert_equal ~ctxt ~printer:Fun.id "/machine_b/repo/arvr/js/app.js" (File_key.to_string fk_b);

      (* Restore defaults *)
      File_key.set_project_root "/project/";
      File_key.set_flowlib_root "/flowlib/"
    );
    ( "out_of_root_sibling_directory" >:: fun ctxt ->
      (* Simulates xplat/js including files from ../../www/html/ *)
      File_key.set_project_root "/data/sandcastle/boxes/fbsource/xplat/js/";
      File_key.set_flowlib_root "/tmp/flowlib/";
      let fk =
        File_key.source_file_of_absolute
          "/data/sandcastle/boxes/fbsource/www/html/xplat-react/foo.js"
      in
      assert_equal ~ctxt ~printer:Fun.id "../../www/html/xplat-react/foo.js" (File_key.suffix fk);
      assert_equal
        ~ctxt
        ~printer:Fun.id
        "/data/sandcastle/boxes/fbsource/www/html/xplat-react/foo.js"
        (File_key.to_string fk);

      (* Restore defaults *)
      File_key.set_project_root "/project/";
      File_key.set_flowlib_root "/flowlib/"
    );
  ]

let tests =
  "file_key"
  >::: [
         "source_file" >::: source_file_tests;
         "lib_file" >::: lib_file_tests;
         "sentinels" >::: sentinel_tests;
         "roots" >::: root_tests;
         "windows_paths" >::: windows_path_tests;
         "compare" >::: compare_tests;
         "saved_state_portability" >::: saved_state_portability_tests;
       ]
