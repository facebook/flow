(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Lwt_test_utils

let tests =
  "LwtTimeout.with_timeout"
  >::: [
         ( "no_timeout" %>:: fun ctxt ->
           let on_timeout_called = ref false in
           let on_timeout () =
             on_timeout_called := true;
             Lwt.return_unit
           in
           let workload () = Lwt.return (Ok 5) in
           let%lwt result = LwtTimeout.with_timeout ~on_timeout 10.0 workload in
           assert_equal ~ctxt result (Ok 5);
           assert_equal ~ctxt !on_timeout_called false;
           Lwt.return_unit
         );
         ( "no_timeout_error" %>:: fun ctxt ->
           let on_timeout_called = ref false in
           let on_timeout () =
             on_timeout_called := true;
             Lwt.return_unit
           in
           let workload () = Lwt.return (Error "foo") in
           let%lwt result = LwtTimeout.with_timeout ~on_timeout 10.0 workload in
           assert_equal ~ctxt result (Error "foo");
           assert_equal ~ctxt !on_timeout_called false;
           Lwt.return_unit
         );
         ( "timeout" %>:: fun ctxt ->
           let on_timeout_called = ref false in
           let on_timeout () =
             on_timeout_called := true;
             Lwt.return_unit
           in
           let workload () =
             let%lwt () = Lwt_unix.sleep 10.0 in
             Lwt.return (Ok 5)
           in
           let%lwt result = LwtTimeout.with_timeout ~on_timeout 0.1 workload in
           assert_equal ~ctxt result (Error "Operation timed out");
           assert_equal ~ctxt !on_timeout_called true;
           Lwt.return_unit
         );
         ( "timeout_custom_msg" %>:: fun ctxt ->
           let timeout_msg = "Custom message" in
           let on_timeout_called = ref false in
           let on_timeout () =
             on_timeout_called := true;
             Lwt.return_unit
           in
           let workload () =
             let%lwt () = Lwt_unix.sleep 10.0 in
             Lwt.return (Ok 5)
           in
           let%lwt result = LwtTimeout.with_timeout ~timeout_msg ~on_timeout 0.1 workload in
           assert_equal ~ctxt result (Error timeout_msg);
           assert_equal ~ctxt !on_timeout_called true;
           Lwt.return_unit
         );
         ( "timeout_no_callback" %>:: fun ctxt ->
           let workload () =
             let%lwt () = Lwt_unix.sleep 10.0 in
             Lwt.return (Ok 5)
           in
           let%lwt result = LwtTimeout.with_timeout 0.1 workload in
           assert_equal ~ctxt result (Error "Operation timed out");
           Lwt.return_unit
         );
       ]
