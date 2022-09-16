(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

(* helper *)
let write (fd : Unix.file_descr) (msg : string) : unit =
  let _ = Unix.write_substring fd msg 0 (String.length msg) in
  ()

let line_reader_tester f input =
  let (fd_in, fd_out) = Unix.pipe () in
  let reader = Buffered_line_reader.create fd_in in
  write fd_out input;
  let output = f reader in
  Unix.close fd_out;
  Unix.close fd_in;
  output

let read_headers_tests =
  let assert_headers ~ctxt expected input =
    let actual = line_reader_tester Http_lite.read_headers input in
    assert_equal ~ctxt ~printer:[%show: string list] expected actual
  in
  [
    ("crlf_terminated" >:: fun ctxt -> assert_headers ~ctxt ["A"; "B"] "A\r\nB\r\n\r\n");
    ("lf_terminated" >:: fun ctxt -> assert_headers ~ctxt ["C"; "D"] "C\nD\n\n");
    ("mixed_termination" >:: fun ctxt -> assert_headers ~ctxt ["E"] "E\r\n\n");
    ("empty_headers" >:: fun ctxt -> assert_headers ~ctxt [] "\n");
  ]

let parse_headers_tests =
  let printer = SMap.show Format.pp_print_string in
  [
    ( "lowercase_keys" >:: fun ctxt ->
      let actual = Http_lite.parse_headers_to_lowercase_map ["A:B"; "C:D"] in
      let expected = SMap.(empty |> add "a" "B" |> add "c" "D") in
      assert_equal ~ctxt ~printer expected actual
    );
    ( "colons_in_values" >:: fun ctxt ->
      let actual = Http_lite.parse_headers_to_lowercase_map ["a:B:C"] in
      let expected = SMap.(empty |> add "a" "B:C") in
      assert_equal ~ctxt ~printer expected actual
    );
    ( "trim_value_not_key" >:: fun ctxt ->
      let actual = Http_lite.parse_headers_to_lowercase_map ["a : b"] in
      let expected = SMap.(empty |> add "a " "b") in
      assert_equal ~ctxt ~printer expected actual
    );
    ( "duplicate_keys" >:: fun ctxt ->
      let actual = Http_lite.parse_headers_to_lowercase_map ["a:1"; "a:2"] in
      let expected = SMap.(empty |> add "a" "2") in
      assert_equal ~ctxt ~printer expected actual
    );
    ( "empty" >:: fun ctxt ->
      let actual = Http_lite.parse_headers_to_lowercase_map [] in
      let expected = SMap.(empty) in
      assert_equal ~ctxt ~printer expected actual
    );
    ( "malformed" >:: fun ctxt ->
      let actual = Http_lite.parse_headers_to_lowercase_map ["a"] in
      let expected = SMap.(empty) in
      assert_equal ~ctxt ~printer expected actual
    );
  ]

let parse_charset_tests =
  let printer = function
    | Some x -> x
    | None -> "(none)"
  in
  let assert_charset ~ctxt expected actual =
    assert_equal ~ctxt ~printer expected (Http_lite.parse_charset actual)
  in
  [
    ("empty_string" >:: fun ctxt -> assert_charset ~ctxt None "");
    ("missing" >:: fun ctxt -> assert_charset ~ctxt None "text/plain");
    ("missing2" >:: fun ctxt -> assert_charset ~ctxt None "a;b");
    ("b" >:: fun ctxt -> assert_charset ~ctxt (Some "b") "a;charset=b");
    ("trim" >:: fun ctxt -> assert_charset ~ctxt (Some "b") "a; charset = b ");
    ("skip_mime_type" >:: fun ctxt -> assert_charset ~ctxt None "charset=a");
    ("b=c" >:: fun ctxt -> assert_charset ~ctxt (Some "b=c") "a;charset=b=c");
    ("abc1" >:: fun ctxt -> assert_charset ~ctxt (Some "c") "a;b;charset=c");
    ("abc2" >:: fun ctxt -> assert_charset ~ctxt (Some "b") "a;charset=b; c");
    ("empty" >:: fun ctxt -> assert_charset ~ctxt None "a;charset=;c");
    ("dupe" >:: fun ctxt -> assert_charset ~ctxt (Some "b") "a;charset=b;charset=c");
  ]

let read_request_tests =
  let assert_message ~ctxt expected msg =
    let actual = line_reader_tester Http_lite.read_message_utf8 msg in
    assert_equal ~ctxt ~printer:Fun.id expected actual
  in
  let body = "{\"jsonrpc\":\"2.0\", \"method\":\"method_name\"}" in
  let len = String.length body in
  [
    ( "normal_reading" >:: fun ctxt ->
      let msg = Printf.sprintf "Content-length: %n\n\n%s" len body in
      assert_message ~ctxt body msg
    );
    ( "case_insensitive_content_length" >:: fun ctxt ->
      let msg = Printf.sprintf "content-length: %n\n\n%s" len body in
      assert_message ~ctxt body msg
    );
    ( "missing_content_length" >:: fun _ctxt ->
      try
        let msg = Printf.sprintf "\r\n%s" body in
        let _ = line_reader_tester Http_lite.read_message_utf8 msg in
        assert_failure "should fail if missing content-length"
      with
      | Http_lite.Malformed _ -> assert_bool "ok" true
    );
    ( "explicit_charset" >:: fun ctxt ->
      let msg =
        Printf.sprintf "content-length: %n\ncontent-type: text/plain; utf-8\n\n%s" len body
      in
      assert_message ~ctxt body msg
    );
    ( "wrong_charset" >:: fun _ctxt ->
      try
        let msg =
          Printf.sprintf
            "content-length: %n\ncontent-type: text/plain; charset=ascii\n\n%s"
            len
            body
        in
        let _ = line_reader_tester Http_lite.read_message_utf8 msg in
        assert_failure "should fail on non-utf8"
      with
      | Http_lite.Malformed _ -> assert_bool "ok" true
    );
  ]

let tests =
  [
    "read_headers" >::: read_headers_tests;
    "parse_headers" >::: parse_headers_tests;
    "parse_charset" >::: parse_charset_tests;
    "read_request" >::: read_request_tests;
  ]

let () = run_test_tt_main ("http_lite" >::: tests)
