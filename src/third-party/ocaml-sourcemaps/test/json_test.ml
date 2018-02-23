(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

type json =
  | JSON_String of string
  | JSON_Object of (string * json) list
  | JSON_Array of json list
  | JSON_Number of string
  | JSON_Null
type json' = json

module Json_writer : (
  Sourcemap.Json_serializer_intf with type t = json
) = struct
  type t = json
  let string x = JSON_String x
  let obj props = JSON_Object props
  let array arr = JSON_Array arr
  let number x = JSON_Number x
  let null = JSON_Null
end

module Json = Sourcemap.Make_json (Json_writer)

let tests = "json" >::: [
  "json_of_sourcemap_empty" >:: begin fun ctxt ->
    let map = Sourcemap.create () in
    let actual = Json.json_of_sourcemap map in
    let expected = JSON_Object [
      "version", JSON_Number "3";
      "sources", JSON_Array [];
      "names", JSON_Array [];
      "mappings", JSON_String "";
    ] in
    assert_equal ~ctxt expected actual
  end;

  "json_of_sourcemap_with_file" >:: begin fun ctxt ->
    let map = Sourcemap.create ~file:"foo.js" () in
    let actual = Json.json_of_sourcemap map in
    let expected = JSON_Object [
      "version", JSON_Number "3";
      "sources", JSON_Array [];
      "names", JSON_Array [];
      "mappings", JSON_String "";
      "file", JSON_String "foo.js";
    ] in
    assert_equal ~ctxt expected actual
  end;

  "json_of_sourcemap_with_source_root" >:: begin fun ctxt ->
    let map = Sourcemap.create ~source_root:"http://example.com/" () in
    let actual = Json.json_of_sourcemap map in
    let expected = JSON_Object [
      "version", JSON_Number "3";
      "sources", JSON_Array [];
      "names", JSON_Array [];
      "mappings", JSON_String "";
      "sourceRoot", JSON_String "http://example.com/";
    ] in
    assert_equal ~ctxt expected actual
  end;

  "json_of_sourcemap" >:: begin fun ctxt ->
    let original = { Sourcemap.line = 1; col = 1 } in
    let generated = { Sourcemap.line = 3; col = 1 } in
    let map =
      Sourcemap.create ()
      |> Sourcemap.add_mapping ~source:"bar.js" ~original ~generated
      |> Sourcemap.add_mapping ~source:"foo.js" ~original ~generated
    in
    let actual = Json.json_of_sourcemap map in
    let expected = JSON_Object [
      "version", JSON_Number "3";
      "sources", JSON_Array [JSON_String "bar.js"; JSON_String "foo.js"];
      "names", JSON_Array [];
      "mappings", JSON_String ";;CAAC,ACAA";
    ] in
    assert_equal ~ctxt expected actual
  end;
]
