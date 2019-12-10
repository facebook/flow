(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let tests =
  "semver"
  >::: [
         "satisfies"
         >::: [
                ( "greater_than_major" >:: fun ctxt ->
                  let cases =
                    [
                      (">2", "2", false);
                      (">2", "2.0", false);
                      (">2", "2.1", true);
                      (">2", "3", true);
                      (">2", "3.0", true);
                      (">2", "11.0", true);
                    ]
                  in
                  List.iter
                    (fun (range, version, satisfies) ->
                      assert_equal ~ctxt satisfies (Semver.satisfies range version))
                    cases );
              ];
         Semver_parser_test.tests;
         Semver_range_test.tests;
         Semver_version_test.tests;
       ]

let () = run_test_tt_main tests
