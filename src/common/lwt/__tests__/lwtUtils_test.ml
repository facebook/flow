(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Lwt_test_utils

let tests =
  "LwtUtils.all"
  >::: [
         ( "includes_all_results" %>:: fun ctxt ->
           let (p1, r1) = Lwt.wait () in
           let (p2, r2) = Lwt.wait () in
           Lwt.wakeup r1 1;
           let p = LwtUtils.all [p1; p2] in
           Lwt.wakeup r2 2;
           let%lwt lst = p in
           (* This illustrates the difference between LwtUtils.all and Lwt.nchoose, which would resolve to
            * [1] *)
           assert_equal ~ctxt lst [1; 2];
           Lwt.return_unit );
         ( "fails_early" %>:: fun ctxt ->
           let (p1, _) = Lwt.wait () in
           let (p2, r2) = Lwt.wait () in
           Lwt.wakeup_exn r2 (Failure "did not work");
           let p = LwtUtils.all [p1; p2] in
           let%lwt exn =
             try%lwt
               let%lwt _lst = p in
               Lwt.return_none
             with
             | Failure s -> Lwt.return_some s
           in
           assert_equal ~ctxt exn (Some "did not work");
           Lwt.return_unit );
       ]
