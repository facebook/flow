(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let split_nth_printer result =
  match result with
  | Some (pre, x, post) -> Printf.sprintf "Some (%S, %S, %S)" pre x post
  | None -> "None"

let tests =
  "line"
  >::: [
         "split_nth"
         >::: [
                ( "last_line" >:: fun ctxt ->
                  assert_equal
                    ~ctxt
                    ~printer:split_nth_printer
                    (Some ("foo\n", "bar", ""))
                    (Line.split_nth "foo\nbar" 1) );
              ];
       ]
