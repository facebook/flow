(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let candidates = [
  "";
  "a";
  "ab";
  "abC";
  "abcd";
  "alphabetacappa";
  "AlphaBetaCappa";
  "thisisatestdir";
  "/////ThisIsATestDir";
  "/this/is/a/test/dir";
  "/test/tiatd";
  "/zzz/path2/path3/path4";
  "/path1/zzz/path3/path4";
  "/path1/path2/zzz/path4";
  "/path1/path2/path3/zzz";
]

let string_list_printer items =
  Printf.sprintf "[%s]" (items |> List.map (Printf.sprintf "%S") |> String.concat "; ")

let values = List.map (fun { Fuzzy_path.value; _ } -> value)

let assert_values ?ctxt expected actual =
  assert_equal ?ctxt ~printer:string_list_printer expected (values actual)

let tests = "fuzzy-path" >::: [
  "can_match_strings" >:: (fun ctxt ->
    let matcher = Fuzzy_path.init candidates in

    let result = Fuzzy_path.search "abc" matcher in
    assert_values
      ~ctxt
      ["abC"; "abcd"; "AlphaBetaCappa"; "alphabetacappa"]
      result;

    let result = Fuzzy_path.search "t/i/a/t/d" matcher in
    assert_values
      ~ctxt
      ["/this/is/a/test/dir"]
      result;
  );

  "prefers_exact_over_abbr_over_others" >:: (fun ctxt ->
    let matcher = Fuzzy_path.init candidates in
    let result = Fuzzy_path.search "tiatd" matcher in
    assert_values
      ~ctxt
      [
        "/test/tiatd";
        "/this/is/a/test/dir";
        "/////ThisIsATestDir";
        "thisisatestdir";
      ]
      result;
  );

  "defaults_case_insensitive" >:: (fun ctxt ->
    let matcher = Fuzzy_path.init candidates in
    let options = Fuzzy_path.{ default_options with smart_case = true } in
    let result = Fuzzy_path.search ~options "ABC" matcher in
    assert_values
      ~ctxt
      [
        "AlphaBetaCappa";
        "abC";
        "abcd";
        "alphabetacappa";
      ]
      result;
  );

  "ignores_spaces" >:: (fun ctxt ->
    let matcher = Fuzzy_path.init candidates in
    let result = Fuzzy_path.search "a b\tcappa" matcher in
    assert_values
      ~ctxt
      [
        "AlphaBetaCappa";
        "alphabetacappa";
      ]
      result;
  );

  "does_not_match_with_excess_chars" >:: (fun ctxt ->
    (* There was a bug where this would result in a match. *)
    let matcher = Fuzzy_path.init candidates in
    let result = Fuzzy_path.search "abcc" matcher in
    assert_values ~ctxt [] result;
  );
]

let () = run_test_tt_main tests
