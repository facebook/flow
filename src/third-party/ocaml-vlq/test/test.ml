(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let cases = [(* input, output *) (0, "A"); (1, "C"); (-1, "D"); (123, "2H"); (123456789, "qxmvrH")]

let tests =
  "vlq"
  >::: [
         ( "encode" >:: fun ctxt ->
           List.iter
             (fun (input, expected) ->
               let buf = Buffer.create 10 in
               Vlq.Base64.encode buf input;
               let actual = Buffer.contents buf in
               assert_equal
                 ~ctxt
                 ~printer:(fun x -> x)
                 ~msg:(Printf.sprintf "Vql.encode buf %d:" input)
                 expected
                 actual)
             cases
         );
         ( "decode" >:: fun ctxt ->
           List.iter
             (fun (input, expected) ->
               let stream = Stream.of_string expected in
               let actual = Vlq.Base64.decode stream in
               assert_equal
                 ~ctxt
                 ~printer:string_of_int
                 ~msg:(Printf.sprintf "Vql.decode %S:" expected)
                 input
                 actual)
             cases
         );
         ( "decode_extra" >:: fun ctxt ->
           let stream = Stream.of_string "qxmvrH the rest is ignored" in
           let actual = Vlq.Base64.decode stream in
           assert_equal ~ctxt ~printer:string_of_int 123456789 actual
         );
         ( "decode_eof" >:: fun _ctxt ->
           let stream = Stream.of_string "qxmvr" in
           assert_raises Vlq.Unexpected_eof (fun () -> Vlq.Base64.decode stream)
         );
         ( "decode_invalid" >:: fun _ctxt ->
           let stream = Stream.of_string "qx." in
           assert_raises (Vlq.Invalid_base64 '.') (fun () -> Vlq.Base64.decode stream)
         );
       ]

let () = run_test_tt_main tests
