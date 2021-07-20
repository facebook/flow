(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* helper *)
let write (fd : Unix.file_descr) (msg : string) : unit =
  let _ = Unix.write_substring fd msg 0 (String.length msg) in
  ()

(* helper: read_message only takes a buffered_line_reader, so when we want
   to test the parsing of a message string, we need to feed it through that. *)
let read_message_tester (msg : string) : string =
  let (fd_in, fd_out) = Unix.pipe () in
  let reader = Buffered_line_reader.create fd_in in
  write fd_out msg;
  let got = Http_lite.read_message_utf8 reader in
  Unix.close fd_out;
  Unix.close fd_in;
  got

let test_read_headers () =
  let (fd_in, fd_out) = Unix.pipe () in
  let reader = Buffered_line_reader.create fd_in in
  write fd_out "A\r\nB\r\n\r\n";
  if Http_lite.read_headers reader <> ["A"; "B"] then failwith "CRLF-terminated";
  write fd_out "C\nD\n\n";
  if Http_lite.read_headers reader <> ["C"; "D"] then failwith "LF-terminated";
  write fd_out "E\r\n\n";
  if Http_lite.read_headers reader <> ["E"] then failwith "mixed-termination";
  write fd_out "\n";
  if Http_lite.read_headers reader <> [] then failwith "empty-headers";
  true

let test_parse_headers () =
  (* lowercase keys *)
  let actual1 = Http_lite.parse_headers_to_lowercase_map ["A:B"; "C:D"] in
  let expected1 = SMap.(empty |> add "a" "B" |> add "c" "D") in
  if actual1 <> expected1 then failwith "aB;cD";

  (* colons in values *)
  let actual2 = Http_lite.parse_headers_to_lowercase_map ["a:B:C"] in
  let expected2 = SMap.(empty |> add "a" "B:C") in
  if actual2 <> expected2 then failwith "B:C";

  (* trim value not key *)
  let actual3 = Http_lite.parse_headers_to_lowercase_map ["a : b"] in
  let expected3 = SMap.(empty |> add "a " "b") in
  if actual3 <> expected3 then failwith "trim value not key";

  (* duplicate keys *)
  let actual4 = Http_lite.parse_headers_to_lowercase_map ["a:1"; "a:2"] in
  let expected4 = SMap.(empty |> add "a" "2") in
  if actual4 <> expected4 then failwith "duplicate values";

  (* empty *)
  let actual5 = Http_lite.parse_headers_to_lowercase_map [] in
  let expected5 = SMap.(empty) in
  if actual5 <> expected5 then failwith "empty";

  (* malformed *)
  let actual6 = Http_lite.parse_headers_to_lowercase_map ["a"] in
  let expected6 = SMap.(empty) in
  if actual6 <> expected6 then failwith "missing key";
  true

let test_parse_charset () =
  Http_lite.(
    if parse_charset "" <> None then failwith "empty-string";
    if parse_charset "text/plain" <> None then failwith "missing";
    if parse_charset "a;b" <> None then failwith "missing2";
    if parse_charset "a;charset=b" <> Some "b" then failwith "b";
    if parse_charset "a; charset = b " <> Some "b" then failwith "trim";
    if parse_charset "charset=a" <> None then failwith "skip mime-type";
    if parse_charset "a;charset=b=c" <> Some "b=c" then failwith "b=c";
    if parse_charset "a;b;charset=c" <> Some "c" then failwith "abc1";
    if parse_charset "a;charset=b; c" <> Some "b" then failwith "abc2";
    if parse_charset "a;charset=;c" <> None then failwith "empty";
    if parse_charset "a;charset=b;charset=c" <> Some "b" then failwith "dupe";
    true)

let test_read_request () =
  let body = "{\"jsonrpc\":\"2.0\", \"method\":\"method_name\"}" in
  let len = String.length body in
  (* normal reading *)
  let msg1 = Printf.sprintf "Content-length: %n\n\n%s" len body in
  if read_message_tester msg1 <> body then failwith "normal reading";

  (* case-insensitive content-length *)
  let msg2 = Printf.sprintf "content-length: %n\n\n%s" len body in
  if read_message_tester msg2 <> body then failwith "case insensitive";

  (* missing content-length *)
  (try
     let msg3 = Printf.sprintf "\r\n%s" body in
     let _ = read_message_tester msg3 in
     failwith "missing content-length"
   with
  | Http_lite.Malformed _ -> ());

  (* explicit charset *)
  let msg4 = Printf.sprintf "content-length: %n\ncontent-type: text/plain; utf-8\n\n%s" len body in
  if read_message_tester msg4 <> body then failwith "explicit charset";

  (* wrong charset *)
  (try
     let msg5 =
       Printf.sprintf "content-length: %n\ncontent-type: text/plain; charset=ascii\n\n%s" len body
     in
     let _ = read_message_tester msg5 in
     failwith "non-utf8"
   with
  | Http_lite.Malformed _ -> ());
  true

let tests =
  [
    ("test_read_headers", test_read_headers);
    ("test_parse_headers", test_parse_headers);
    ("test_parse_charset", test_parse_charset);
    ("test_read_request", test_read_request);
  ]

let () = Unit_test.run_all tests
