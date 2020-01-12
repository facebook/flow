(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Lsp
open Lsp_fmt
open Hh_json

let assert_equals (exp : 'a) (actual : 'a) (failure_msg : string) =
  if exp <> actual then failwith failure_msg

(* TODO: write a generic "dump" routine *)

let assert_json_equals (exp : string) (actual : json) (failure_msg : string) =
  let exp = exp |> json_of_string |> json_to_string in
  (* canonical *)
  let actual = actual |> json_to_string in
  if exp <> actual then failwith (failure_msg ^ " - expected " ^ exp ^ ", got " ^ actual)

let test_position () =
  let p = { line = 1; character = 3 } in
  let json = print_position p in
  assert_json_equals {|{ "line": 1, "character": 3 }|} json "print_position";
  let p' = parse_position (Some json) in
  assert_equals p p' "parse_position";
  true

(* TODO: write a load more printing+parsing tests *)

let tests = [("test_position", test_position)]

let () = Unit_test.run_all tests
