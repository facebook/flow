(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Utils_js
open Dep_graph_test_utils

let assert_sets_equal ~ctxt expected actual =
  assert_equal
    ~ctxt
    ~cmp:FilenameSet.equal
    ~printer:Utils_js.debug_string_of_filename_set
    expected
    actual

let calc_all_dependents ~dependency_graph ~all_dependency_graph root_files =
  let dependency_graph = make_dependency_graph dependency_graph in
  let all_dependency_graph = make_dependency_graph all_dependency_graph in
  let root_files = make_filename_set root_files in
  Pure_dep_graph_operations.calc_all_dependents ~dependency_graph ~all_dependency_graph root_files

let tests =
  "pure_dep_graph_operations"
  >::: [
         "calc_all_dependents"
         >::: [
                ( "long_chain"
                >:: fun ctxt ->
                let dependency_graph =
                  [("a", []); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                in
                let all_dependency_graph =
                  [("a", []); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                in
                let root_files = ["b"] in
                let all_dependents =
                  calc_all_dependents ~dependency_graph ~all_dependency_graph root_files
                in
                let expected = make_filename_set ["b"; "c"; "d"; "e"] in
                assert_sets_equal ~ctxt expected all_dependents );
                ( "long_chain_no_sig_dependencies"
                >:: fun ctxt ->
                let dependency_graph = [("a", []); ("b", []); ("c", []); ("d", []); ("e", [])] in
                let all_dependency_graph =
                  [("a", []); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                in
                let root_files = ["b"] in
                let all_dependents =
                  calc_all_dependents ~dependency_graph ~all_dependency_graph root_files
                in
                (* See comments on `calc_all_dependents` for why this is expected behavior. *)
                let expected = make_filename_set ["b"; "c"] in
                assert_sets_equal ~ctxt expected all_dependents );
              ];
       ]
