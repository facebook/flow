(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let candidates = [
  "", 0;
  "a", 0;
  "ab", 0;
  "abC", 0;
  "abcd", 0;
  "alphabetacappa", 0;
  "AlphaBetaCappa", 0;
  "thisisatestdir", 0;
  "/////ThisIsATestDir", 0;
  "/this/is/a/test/dir", 0;
  "/test/tiatd", 0;
  "/zzz/path2/path3/path4", 0;
  "/path1/zzz/path3/path4", 0;
  "/path1/path2/zzz/path4", 0;
  "/path1/path2/path3/zzz", 0;
]

let string_list_printer items =
  Printf.sprintf "[%s]" (items |> List.map (Printf.sprintf "%S") |> String.concat "; ")

let values = List.map (fun { Fuzzy_path.value; _ } -> value)

let assert_values ?ctxt expected actual =
  assert_equal ?ctxt ~printer:string_list_printer expected (values actual)

let options = Fuzzy_path.default_options

let tests = "fuzzy-path" >::: [
  "can_match_strings" >:: (fun ctxt ->
    let matcher = Fuzzy_path.init candidates in

    let result = Fuzzy_path.search "abc" matcher in
    assert_values
      ~ctxt
      ["abC"; "abcd"; "AlphaBetaCappa"; "alphabetacappa"]
      result;

    let result = Fuzzy_path.search "ABC" matcher in
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

  "finds_strong_match" >:: (fun ctxt ->
    let candidates = ["xabcabc", 0; "xAbcabc", 0; "xabcAbc", 0] in
    let matcher = Fuzzy_path.init candidates in
    let result = Fuzzy_path.search ~options "abc" matcher in
    assert_values ~ctxt ["xAbcabc"; "xabcAbc"; "xabcabc"] result;

    let options = { options with Fuzzy_path.first_match_can_be_weak = false } in
    let result = Fuzzy_path.search ~options "abc" matcher in
    assert_values ~ctxt ["xAbcabc"; "xabcAbc"] result
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

  "stable_ordering" >:: (fun ctxt ->
    let matcher = Fuzzy_path.init ["Foo", 0; "foo", 0; "far", 0; "foobar", 0] in
    let result = Fuzzy_path.search "f" matcher in
    (* "Foo", "foo" and "far" all have the same score, which is higher than
       the score for "foobar". If the scores and the lengths are the same,
       then sort lexicographically. *)
    assert_values ~ctxt ["Foo"; "far"; "foo"; "foobar"] result;
  );
]

let () = run_test_tt_main tests
