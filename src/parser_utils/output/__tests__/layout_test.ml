(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Layout
open Layout_test_utils

let tests =
  "layout"
  >::: [
         ( "fuse_list" >:: fun ctxt ->
           let a = Atom "a" in
           let b = Atom "b" in
           let c = Atom "c" in
           let d = Atom "d" in
           let sep = Atom "," in
           let actual = fuse_list [a; b] in
           let expected = Concat [a; pretty_space; b] in
           assert_layout ~ctxt expected actual;

           let actual = fuse_list [a; b; c] in
           let expected = Concat [a; pretty_space; b; pretty_space; c] in
           assert_layout ~ctxt expected actual;

           let actual = fuse_list ~sep [a; b] in
           let expected = Concat [a; sep; pretty_space; b] in
           assert_layout ~ctxt expected actual;

           let actual = fuse_list ~sep [a; b; c] in
           let expected = Concat [a; sep; pretty_space; b; sep; pretty_space; c] in
           assert_layout ~ctxt expected actual;

           let actual = fuse_list ~wrap:(c, d) [a; b] in
           let expected = Concat [c; a; pretty_space; b; d] in
           assert_layout ~ctxt expected actual;

           let actual = fuse_list ~sep ~wrap:(c, d) [a; b] in
           let expected = Concat [c; a; sep; pretty_space; b; d] in
           assert_layout ~ctxt expected actual );
       ]
