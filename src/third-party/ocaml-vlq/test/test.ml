(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let cases = [
  (* input, output *)
  0, "A";
  1, "C";
  -1, "D";
  123, "2H";
  123456789, "qxmvrH";
]

let test_encode ctxt =
  List.iter (fun (input, expected) ->
    let buf = Buffer.create 10 in
    Vlq.Base64.encode buf input;
    let actual = Buffer.contents buf in
    assert_equal ~ctxt ~printer:(fun x -> x)
      ~msg:(Printf.sprintf "Vql.encode buf %d:" input)
      expected actual
  ) cases

let tests = "vlq" >::: [
  "encode" >:: test_encode;
]

let () = run_test_tt_main tests
