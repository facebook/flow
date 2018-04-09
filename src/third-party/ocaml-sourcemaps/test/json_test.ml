(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
(* open Sourcemaps *)
open Test_utils

type json =
  | JSON_String of string
  | JSON_Object of (string * json) list
  | JSON_Array of json list
  | JSON_Number of string
  | JSON_Null
type json' = json

exception Json_error

module Json_writer : (
  Sourcemap.Json_writer_intf with type t = json
) = struct
  type t = json
  let of_string x = JSON_String x
  let of_obj props = JSON_Object props
  let of_array arr = JSON_Array arr
  let of_number x = JSON_Number x
  let null = JSON_Null
end

module Json_reader : (
  Sourcemap.Json_reader_intf with type t = json
) = struct
  type t = json

  let to_string t =
    match t with
    | JSON_String x -> x
    | _ -> raise Json_error

  let to_obj t =
    match t with
    | JSON_Object x -> x
    | _ -> raise Json_error

  let to_array t =
    match t with
    | JSON_Array x -> x
    | _ -> raise Json_error

  let to_number t =
    match t with
    | JSON_Number x -> x
    | _ -> raise Json_error

  let is_null t = t = JSON_Null
end

module W = Sourcemap.Make_json_writer (Json_writer)
open W
module R = Sourcemap.Make_json_reader (Json_reader)
open R

let tests = "json" >::: [
  "json_of_sourcemap_empty" >:: begin fun ctxt ->
    let map = Sourcemap.create () in
    let actual = json_of_sourcemap map in
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
    let actual = json_of_sourcemap map in
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
    let actual = json_of_sourcemap map in
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
    let bar = Sourcemap.({
      source = "bar.js";
      original_loc = { line = 1; col = 1 };
      name = None;
    }) in
    let foo = Sourcemap.({
      source = "foo.js";
      original_loc = { line = 1; col = 1 };
      name = None;
    }) in
    let map =
      Sourcemap.create ()
      |> Sourcemap.add_mapping ~original:bar ~generated:{ Sourcemap.line = 3; col = 1 }
      |> Sourcemap.add_mapping ~original:foo ~generated:{ Sourcemap.line = 3; col = 2 }
    in
    let actual = json_of_sourcemap map in
    let expected = JSON_Object [
      "version", JSON_Number "3";
      "sources", JSON_Array [JSON_String "bar.js"; JSON_String "foo.js"];
      "names", JSON_Array [];
      "mappings", JSON_String ";;CAAC,CCAA";
    ] in
    assert_equal ~ctxt expected actual
  end;

  "sourcemap_of_json" >:: begin fun ctxt ->
    let json = JSON_Object [
      "version", JSON_Number "3";
      "sources", JSON_Array [JSON_String "bar.js"; JSON_String "foo.js"];
      "names", JSON_Array [JSON_String "y"];
      "mappings", JSON_String ";;CAAC,CCAAA";
    ] in
    let expected =
      let bar = Sourcemap.({
        source = "bar.js";
        original_loc = { line = 1; col = 1 };
        name = None;
      }) in
      let foo = Sourcemap.({
        source = "foo.js";
        original_loc = { line = 1; col = 1 };
        name = Some "y";
      }) in
      Sourcemap.create ()
      |> Sourcemap.add_mapping ~original:bar ~generated:{ Sourcemap.line = 3; col = 1 }
      |> Sourcemap.add_mapping ~original:foo ~generated:{ Sourcemap.line = 3; col = 2 }
    in
    let actual = sourcemap_of_json json in

    assert_equal_sourcemaps ~ctxt expected actual
  end
]
