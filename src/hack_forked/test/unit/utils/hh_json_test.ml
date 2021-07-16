(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

let throws f =
  try
    let _ = f () in
    false
  with
  | _ -> true

let test_escape_unescape_data =
  [
    "newline\n";
    "\"quoted string\"";
    "tab\t";
    "carriage return\r";
    "backslash\\";
    "magic char" ^ String.make 1 (Char.of_int_exn 8);
    "magic_char_with_hexadecimal_digit" ^ String.make 1 (Char.of_int_exn 26);
  ]

let test_escape_unescape () =
  List.for_all test_escape_unescape_data (fun s ->
      let json = Hh_json.JSON_String s in
      let encoded = Hh_json.json_to_string json in
      let decoded = Hh_json.json_of_string encoded in
      let result = Hh_json.get_string_exn decoded in
      String.equal result s)

let test_empty_string () =
  try
    ignore (Hh_json.json_of_string "");
    false
  with
  | Hh_json.Syntax_error _ -> true

let test_whitespace_string () =
  match Hh_json.json_of_string "\" \"" with
  | Hh_json.JSON_String " " -> true
  | _ -> false

let test_access_string () =
  let json_string = Hh_json.json_of_string "{ \"foo\": \"hello\" }" in
  let json_number = Hh_json.json_of_string "{ \"foo\": 1 }" in
  let json_null = Hh_json.json_of_string "{ \"foo\": null }" in
  let json_absent = Hh_json.json_of_string "{ }" in
  Hh_json.Access.(
    let r1 =
      match return json_string >>= get_string "foo" with
      | Ok ("hello", _) -> true
      | _ -> false
    in
    let r2 =
      match return json_number >>= get_string "foo" with
      | Error (Wrong_type_error (["foo"], Hh_json.String_t)) -> true
      | _ -> false
    in
    let r3 =
      match return json_null >>= get_string "foo" with
      | Error (Wrong_type_error (["foo"], Hh_json.String_t)) -> true
      | _ -> false
    in
    let r4 =
      match return json_absent >>= get_string "foo" with
      | Error (Missing_key_error ("foo", [])) -> true
      | _ -> false
    in
    r1 && r2 && r3 && r4)

let test_jget_string () =
  let json_string = Some (Hh_json.json_of_string "{ \"foo\": \"hello\" }") in
  let json_number = Some (Hh_json.json_of_string "{ \"foo\": 1 }") in
  let json_null = Some (Hh_json.json_of_string "{ \"foo\": null }") in
  let json_absent = Some (Hh_json.json_of_string "{ }") in
  let json_none = None in
  Hh_json_helpers.(
    let results = "" in
    let str = Jget.string_opt json_string "foo" |> Base.Option.equal String.equal (Some "hello") in
    let num = Jget.string_opt json_number "foo" |> Base.Option.is_none in
    let nul = Jget.string_opt json_number "foo" |> Base.Option.is_none in
    let abs = Jget.string_opt json_absent "foo" |> Base.Option.is_none in
    let non = Jget.string_opt json_none "foo" |> Base.Option.is_none in
    let results =
      results
      ^ Printf.sprintf "string_opt: str=%B num=%B nul=%B abs=%B non=%B\n" str num nul abs non
    in
    let str = Jget.string_d json_string "foo" ~default:"d" |> String.equal "hello" in
    let num = Jget.string_d json_absent "foo" ~default:"d" |> String.equal "d" in
    let nul = Jget.string_d json_absent "foo" ~default:"d" |> String.equal "d" in
    let abs = Jget.string_d json_absent "foo" ~default:"d" |> String.equal "d" in
    let non = Jget.string_d json_none "foo" ~default:"d" |> String.equal "d" in
    let results =
      results ^ Printf.sprintf "string_d: str=%B num=%B nul=%B abs=%B non=%B\n" str num nul abs non
    in
    let str = Jget.string_exn json_string "foo" |> String.equal "hello" in
    let num = throws (fun () -> Jget.string_exn json_number "foo") in
    let nul = throws (fun () -> Jget.string_exn json_null "foo") in
    let abs = throws (fun () -> Jget.string_exn json_absent "foo") in
    let non = throws (fun () -> Jget.string_exn json_none "foo") in
    let results =
      results
      ^ Printf.sprintf "string_exn: str=%B num=%B nul=%B abs=%B non=%B\n" str num nul abs non
    in
    let failed = String_utils.is_substring "false" results in
    if failed then Caml.Printf.eprintf "%s" results;
    not failed)

let test_jget_number () =
  let json_int = Some (Hh_json.json_of_string "{ \"foo\": 1 }") in
  let json_float = Some (Hh_json.json_of_string "{ \"foo\": 1.0 }") in
  let json_string = Some (Hh_json.json_of_string "{ \"foo\": \"hello\" }") in
  Hh_json_helpers.(
    let results = "" in
    let iint = Jget.int_opt json_int "foo" |> Option.equal ( = ) (Some 1) in
    let ifloat = throws (fun () -> Jget.int_opt json_float "foo") in
    let istring = Jget.int_opt json_string "foo" |> Base.Option.is_none in
    let results =
      results ^ Printf.sprintf "int_opt: int=%B float=%B string=%B\n" iint ifloat istring
    in
    let fint = Jget.float_opt json_int "foo" |> Option.equal Float.equal (Some 1.0) in
    let ffloat = Jget.float_opt json_float "foo" |> Option.equal Float.equal (Some 1.0) in
    let fstring = Jget.float_opt json_string "foo" |> Base.Option.is_none in
    let results =
      results ^ Printf.sprintf "float_opt: int=%B float=%B string=%B\n" fint ffloat fstring
    in
    let failed = String_utils.is_substring "false" results in
    if failed then Caml.Printf.eprintf "%s" results;
    not failed)

let test_access_object_string () =
  let json = Hh_json.json_of_string "{ \"foo\": { \"bar\": { \"baz\": \"hello\" } } }" in
  Hh_json.Access.(
    let result = return json >>= get_obj "foo" >>= get_obj "bar" >>= get_string "baz" in
    match result with
    | Ok ("hello", _) -> true
    | _ -> false)

let test_access_object_bool () =
  let json = Hh_json.json_of_string "{ \"foo\": { \"bar\": { \"baz\": true } } }" in
  Hh_json.Access.(
    let result = return json >>= get_obj "foo" >>= get_obj "bar" >>= get_bool "baz" in
    match result with
    | Ok (true, _) -> true
    | _ -> false)

let test_access_object_number () =
  let json = Hh_json.json_of_string "{ \"foo\": { \"bar\": { \"baz\": 5 } } }" in
  Hh_json.Access.(
    let result = return json >>= get_obj "foo" >>= get_obj "bar" >>= get_number "baz" in
    match result with
    | Ok ("5", _) -> true
    | _ -> false)

let test_access_object_val () =
  let json = Hh_json.json_of_string "{ \"foo\": { \"bar\": { \"baz\": 5 } } }" in
  Hh_json.Access.(
    let result = return json >>= get_obj "foo" >>= get_obj "bar" >>= get_val "baz" in
    match result with
    | Ok (Hh_json.JSON_Number "5", _) -> true
    | _ -> false)

let test_access_object_key_doesnt_exist () =
  let json = Hh_json.json_of_string "{ \"foo\": { \"bar\": { \"baz\": 5 } } }" in
  Hh_json.Access.(
    let result = return json >>= get_obj "foo" >>= get_obj "bar" >>= get_number "oops" in
    match result with
    | Error (Missing_key_error ("oops", ["bar"; "foo"])) -> true
    | _ -> false)

let test_access_object_type_invalid () =
  let json = Hh_json.json_of_string "{ \"foo\": { \"bar\": { \"baz\": 5 } } }" in
  Hh_json.Access.(
    let result = return json >>= get_obj "foo" >>= get_obj "bar" >>= get_string "baz" in
    match result with
    | Error (Wrong_type_error (["baz"; "bar"; "foo"], Hh_json.String_t)) -> true
    | _ -> false)

(** Hit an error when accessing the third key, in this JSON object
  * of depth 4. *)
let test_access_object_error_in_middle () =
  let json = Hh_json.json_of_string "{ \"foo\": { \"bar\": { \"baz\": { \"qux\" : 5 } } } }" in
  Hh_json.Access.(
    let result =
      return json
      >>= get_obj "foo"
      >>= get_obj "bar"
      >>= get_obj "oops"
      >>= get_obj "baz"
      >>= get_number "qux"
    in
    match result with
    | Error (Missing_key_error ("oops", ["bar"; "foo"])) -> true
    | _ -> false)

type fbz_record = {
  foo: bool;
  bar: string;
  baz: int;
}

let test_access_3_keys_one_object () =
  let json =
    Hh_json.json_of_string
      ("{\n" ^ "  \"foo\" : true,\n" ^ "  \"bar\" : \"hello\",\n" ^ "  \"baz\" : 5\n" ^ "}")
  in
  Hh_json.Access.(
    let accessor = return json in
    let result =
      accessor >>= get_bool "foo" >>= fun (foo, _) ->
      accessor >>= get_string "bar" >>= fun (bar, _) ->
      accessor >>= get_number_int "baz" >>= fun (baz, _) -> return { foo; bar; baz }
    in
    match result with
    | Error access_failure ->
      Caml.Printf.eprintf
        "Error failed to parse. See: %s\n"
        (access_failure_to_string access_failure);
      false
    | Ok (v, _) ->
      Asserter.Bool_asserter.assert_equals v.foo true "foo value mismatch";
      Asserter.String_asserter.assert_equals v.bar "hello" "bar value mismatch";
      Asserter.Int_asserter.assert_equals v.baz 5 "baz value mismatch";
      true)

(** We access exactly as we do above, but "bar" actually is an array instead
 * of a string, so we should expect to get a Error. *)
let test_access_3_keys_one_object_wrong_type_middle () =
  let json =
    Hh_json.json_of_string
      ("{\n" ^ "  \"foo\" : true,\n" ^ "  \"bar\" : [],\n" ^ "  \"baz\" : 5\n" ^ "}")
  in
  Hh_json.Access.(
    let accessor = return json in
    let result =
      accessor >>= get_bool "foo" >>= fun (foo, _) ->
      accessor >>= get_string "bar" >>= fun (bar, _) ->
      accessor >>= get_number_int "baz" >>= fun (baz, _) -> return { foo; bar; baz }
    in
    match result with
    | Error access_failure ->
      Asserter.String_asserter.assert_equals
        "Value expected to be String (at field `bar`)"
        (access_failure_to_string access_failure)
        "Not the access failure we expected";
      true
    | Ok (_, _) ->
      Caml.Printf.eprintf "Expected failure, but successfully traversed json.\n";
      false)

let test_truncate () =
  let s = {|{ "a":{"a1":{"a1x":"hello","a1y":42},"a2":true},"b":null}|} in
  let actual = Hh_json.json_truncate_string s in
  let exp = s in
  (* we expect it to preserve the leading space! *)
  Asserter.String_asserter.assert_equals exp actual "unchanged truncate";

  let actual = Hh_json.json_truncate_string s ~max_string_length:1 in
  let exp = {|{"a":{"a1":{"a1x":"h...","a1y":42},"a2":true},"b":null}|} in
  Asserter.String_asserter.assert_equals exp actual "max_string_length truncate";

  let actual = Hh_json.json_truncate_string s ~max_child_count:1 in
  let exp = {|{"a":{"a1":{"a1x":"hello"}}}|} in
  Asserter.String_asserter.assert_equals exp actual "max_child_count truncate";

  let actual = Hh_json.json_truncate_string s ~max_depth:1 in
  let exp = {|{"a":{},"b":null}|} in
  Asserter.String_asserter.assert_equals exp actual "max_depth truncate";

  let actual = Hh_json.json_truncate_string s ~max_total_count:1 in
  let exp = {|{"a":{}}|} in
  Asserter.String_asserter.assert_equals exp actual "max_total_count truncate 1";

  let actual = Hh_json.json_truncate_string s ~max_total_count:2 in
  let exp = {|{"a":{"a1":{}}}|} in
  Asserter.String_asserter.assert_equals exp actual "max_total_count truncate 2";
  true

let test_hex_escape () =
  let input = "\"\\u003C\"" in
  let exp = "<" in
  Asserter.Hh_json_json_asserter.assert_equals
    (Hh_json.JSON_String exp)
    (Hh_json.json_of_string input)
    "unicode escape with caps";
  true

let tests =
  [
    ("test_escape_unescape", test_escape_unescape);
    ("test_empty_string", test_empty_string);
    ("test_whitespace_string", test_whitespace_string);
    ("test_access_string", test_access_string);
    ("test_jget_string", test_jget_string);
    ("test_jget_number", test_jget_number);
    ("test_access_object_string", test_access_object_string);
    ("test_access_object_bool", test_access_object_bool);
    ("test_access_object_number", test_access_object_number);
    ("test_access_object_val", test_access_object_val);
    ("test_access_object_key_doesnt_exit", test_access_object_key_doesnt_exist);
    ("test_access_object_type_invalid", test_access_object_type_invalid);
    ("test_access_object_error_in_middle", test_access_object_error_in_middle);
    ("test_access_3_keys_on_object", test_access_3_keys_one_object);
    ( "test_access_3_keys_one_object_wrong_type_middle",
      test_access_3_keys_one_object_wrong_type_middle );
    ("test_truncate", test_truncate);
    ("test_hex_escape", test_hex_escape);
  ]

let () = Unit_test.run_all tests
