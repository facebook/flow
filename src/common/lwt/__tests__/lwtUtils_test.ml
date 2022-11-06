(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Lwt_test_utils

let all_tests =
  [
    ( "includes_all_results" %>:: fun ctxt ->
      let (p1, r1) = Lwt.wait () in
      let (p2, r2) = Lwt.wait () in
      Lwt.wakeup r1 1;
      let p = LwtUtils.all [p1; p2] in
      Lwt.wakeup r2 2;
      let%lwt lst = p in
      (* This illustrates the difference between LwtUtils.all and Lwt.nchoose, which would resolve to
       * [1] *)
      assert_equal ~ctxt [1; 2] lst;
      Lwt.return_unit
    );
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
      assert_equal ~ctxt (Some "did not work") exn;
      Lwt.return_unit
    );
  ]

let both_tests =
  [
    ( "includes_both_results" %>:: fun ctxt ->
      let (p1, r1) = Lwt.wait () in
      let (p2, r2) = Lwt.wait () in
      Lwt.wakeup r1 1;
      let p = LwtUtils.both p1 p2 in
      Lwt.wakeup r2 2;
      let%lwt (x1, x2) = p in
      assert_equal ~ctxt 1 x1;
      assert_equal ~ctxt 2 x2;
      Lwt.return_unit
    );
    ( "fails_early" %>:: fun ctxt ->
      let (p1, _) = Lwt.wait () in
      let (p2, r2) = Lwt.wait () in
      Lwt.wakeup_exn r2 (Failure "did not work");
      (* Lwt.both would just hang because p1 never resolves *)
      let p = LwtUtils.both p1 p2 in
      let%lwt exn =
        try%lwt
          let%lwt _ = p in
          Lwt.return_none
        with
        | Failure s -> Lwt.return_some s
      in
      assert_equal ~ctxt (Some "did not work") exn;
      Lwt.return_unit
    );
  ]

let both_result_tests =
  [
    ( "includes_both_results" %>:: fun ctxt ->
      let (p1, r1) = Lwt.wait () in
      let (p2, r2) = Lwt.wait () in
      Lwt.wakeup r1 (Ok 1);
      let p = LwtUtils.both_result p1 p2 in
      Lwt.wakeup r2 (Ok 2);
      let%lwt r = p in
      assert_equal ~ctxt (Ok (1, 2)) r;
      Lwt.return_unit
    );
    ( "fails_early_exn" %>:: fun ctxt ->
      let (p1, _) = Lwt.wait () in
      let (p2, r2) = Lwt.wait () in
      Lwt.wakeup_exn r2 (Failure "did not work");
      (* Lwt_result.both would just hang because p1 never resolves *)
      let p = LwtUtils.both_result p1 p2 in
      let%lwt exn =
        try%lwt
          let%lwt _ = p in
          Lwt.return_none
        with
        | Failure s -> Lwt.return_some s
      in
      assert_equal ~ctxt (Some "did not work") exn;
      Lwt.return_unit
    );
    ( "fails_early_error" %>:: fun ctxt ->
      let (p1, _) = Lwt.wait () in
      let (p2, r2) = Lwt.wait () in
      Lwt.wakeup r2 (Error "did not work");
      (* Lwt_result.both would just hang because p1 never resolves *)
      let%lwt r = LwtUtils.both_result p1 p2 in
      assert_equal ~ctxt (Error "did not work") r;
      Lwt.return_unit
    );
  ]

let tests =
  "LwtUtils"
  >::: ["all" >::: all_tests; "both" >::: both_tests; "both_result" >::: both_result_tests]
