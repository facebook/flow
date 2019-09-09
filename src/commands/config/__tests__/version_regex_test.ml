(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let assert_regex ~ctxt ~regex exp act =
  assert_equal
    ~ctxt
    ~printer:(fun x -> x)
    ~msg:("regex for " ^ act ^ " didn't match")
    exp
    (regex act)

let tests =
  "version_regex"
  >::: [ ( "less_than_or_equal_to_zero"
         >:: fun ctxt ->
         let regex = Version_regex.less_than_or_equal_to_version in
         let assert_regex = assert_regex ~ctxt ~regex in
         assert_regex "0\\(\\.0\\(\\.0\\)?\\)?" "0.0.0" );
         ( "less_than_or_equal_to_pre_1_0"
         >:: fun ctxt ->
         let regex = Version_regex.less_than_or_equal_to_version in
         let assert_regex = assert_regex ~ctxt ~regex in
         assert_regex "0\\(\\.\\(1\\(\\.0\\)?\\|0\\(\\.[0-9]+\\)?\\)\\)?" "0.1.0";
         assert_regex "0\\(\\.\\(2\\(\\.[0-4]\\)?\\|[0-1]\\(\\.[0-9]+\\)?\\)\\)?" "0.2.4" );
         ( "less_than_or_equal_to_1_X"
         >:: fun ctxt ->
         let regex = Version_regex.less_than_or_equal_to_version in
         let assert_regex = assert_regex ~ctxt ~regex in
         assert_regex "\\(1\\(\\.0\\(\\.0\\)?\\)?\\|0\\(\\.[0-9]+\\)?\\(\\.[0-9]+\\)?\\)" "1.0.0";
         assert_regex
           "\\(1\\(\\.\\(1\\(\\.0\\)?\\|0\\(\\.[0-9]+\\)?\\)\\)?\\|0\\(\\.[0-9]+\\)?\\(\\.[0-9]+\\)?\\)"
           "1.1.0";
         assert_regex
           "\\(1\\(\\.\\(1\\(\\.[0-1]\\)?\\|0\\(\\.[0-9]+\\)?\\)\\)?\\|0\\(\\.[0-9]+\\)?\\(\\.[0-9]+\\)?\\)"
           "1.1.1";
         assert_regex
           "\\(1\\(\\.\\(2\\(\\.0\\)?\\|[0-1]\\(\\.[0-9]+\\)?\\)\\)?\\|0\\(\\.[0-9]+\\)?\\(\\.[0-9]+\\)?\\)"
           "1.2.0";
         assert_regex
           "\\(1\\(\\.\\(2\\(\\.[0-4]\\)?\\|[0-1]\\(\\.[0-9]+\\)?\\)\\)?\\|0\\(\\.[0-9]+\\)?\\(\\.[0-9]+\\)?\\)"
           "1.2.4" );
         ( "less_than_or_equal_to_10_X"
         >:: fun ctxt ->
         let regex = Version_regex.less_than_or_equal_to_version in
         let assert_regex = assert_regex ~ctxt ~regex in
         assert_regex
           "\\(10\\(\\.0\\(\\.0\\)?\\)?\\|[0-9]\\(\\.[0-9]+\\)?\\(\\.[0-9]+\\)?\\)"
           "10.0.0";
         assert_regex
           "\\(10\\(\\.\\(10\\(\\.0\\)?\\|[0-9]\\(\\.[0-9]+\\)?\\)\\)?\\|[0-9]\\(\\.[0-9]+\\)?\\(\\.[0-9]+\\)?\\)"
           "10.10.0";
         assert_regex
           "\\(10\\(\\.\\(10\\(\\.\\(10\\|[0-9]\\)\\)?\\|[0-9]\\(\\.[0-9]+\\)?\\)\\)?\\|[0-9]\\(\\.[0-9]+\\)?\\(\\.[0-9]+\\)?\\)"
           "10.10.10";
         assert_regex
           "\\(10\\(\\.\\(20\\(\\.0\\)?\\|\\(1[0-9]\\|[0-9]\\)\\(\\.[0-9]+\\)?\\)\\)?\\|[0-9]\\(\\.[0-9]+\\)?\\(\\.[0-9]+\\)?\\)"
           "10.20.0";
         assert_regex
           "\\(10\\(\\.\\(20\\(\\.[0-4]\\)?\\|\\(1[0-9]\\|[0-9]\\)\\(\\.[0-9]+\\)?\\)\\)?\\|[0-9]\\(\\.[0-9]+\\)?\\(\\.[0-9]+\\)?\\)"
           "10.20.4" ) ]
