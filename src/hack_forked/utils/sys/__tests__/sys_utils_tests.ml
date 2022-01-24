(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let tests =
  [
    ( "total_ram" >:: fun ctxt ->
      if Sys.unix then
        assert_bool "total_ram should be set on linux and macOS" (Sys_utils.total_ram > 0)
      else
        (* on other systems, it should return 0 and not raise *)
        assert_equal ~ctxt ~printer:string_of_int 0 Sys_utils.total_ram
    );
  ]

let () = run_test_tt_main ("sys_utils" >::: tests)
