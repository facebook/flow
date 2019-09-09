(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Semver_version

let v major minor patch prerelease build = { major; minor; patch; prerelease; build }

let v1_0_0_alpha = v 1 0 0 [Str "alpha"] []

let v1_0_0_alpha_1 = v 1 0 0 [Str "alpha"; Int 1] []

let v1_0_0_alpha_beta = v 1 0 0 [Str "alpha"; Str "beta"] []

let v1_0_0_beta = v 1 0 0 [Str "beta"] []

let v1_0_0_beta_2 = v 1 0 0 [Str "beta"; Int 2] []

let v1_0_0_beta_11 = v 1 0 0 [Str "beta"; Int 11] []

let v1_0_0_rc_1 = v 1 0 0 [Str "rc"; Int 1] []

let v1_0_0 = v 1 0 0 [] []

let rec iter_pairs f = function
  | [] -> ()
  | [_] -> ()
  | a :: b :: rest ->
    f a b;
    iter_pairs f (b :: rest)

let tests =
  "version"
  >::: [ "compare_precedence"
         >::: [ ( "prerelease"
                >:: fun _ctxt ->
                let ordered =
                  [ v1_0_0_alpha;
                    v1_0_0_alpha_1;
                    v1_0_0_alpha_beta;
                    v1_0_0_beta;
                    v1_0_0_beta_2;
                    v1_0_0_beta_11;
                    v1_0_0_rc_1;
                    v1_0_0 ]
                in
                iter_pairs
                  (fun a b ->
                    let a_str = to_string a in
                    let b_str = to_string b in
                    assert_bool (a_str ^ " < " ^ b_str ^ " failed") (compare_precedence a b < 0);
                    assert_bool (b_str ^ " > " ^ a_str ^ " failed") (compare_precedence b a > 0))
                  ordered;
                List.iter
                  (fun a ->
                    let a_str = to_string a in
                    assert_bool (a_str ^ " not equal to itself") (compare_precedence a a = 0))
                  ordered );
                ( "build"
                >:: fun _ctxt ->
                let a = v 1 0 0 [] [Int 1] in
                let b = v 1 0 0 [] [Int 2] in
                assert_bool "1.0.0+1 should be = 1.0.0+2" (compare_precedence a b = 0) ) ];
         "compare"
         >::: [ ( "build"
                >:: fun _ctxt ->
                let a = v 1 0 0 [] [Int 1] in
                let b = v 1 0 0 [] [Int 2] in
                assert_bool "1.0.0+1 should NOT be = 1.0.0+2" (compare a b < 0) ) ] ]
