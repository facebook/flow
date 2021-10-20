(*
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

let calc_all_dependents ~sig_dependency_graph ~implementation_dependency_graph root_files =
  let sig_dependency_graph = make_dependency_graph sig_dependency_graph in
  let implementation_dependency_graph = make_dependency_graph implementation_dependency_graph in
  let root_files = make_filename_set root_files in
  Pure_dep_graph_operations.calc_all_dependents
    ~sig_dependency_graph
    ~implementation_dependency_graph
    root_files

let tests =
  "pure_dep_graph_operations"
  >::: [
         "calc_all_dependents"
         >::: [
                ( "long_chain" >:: fun ctxt ->
                  let sig_dependency_graph =
                    [("a", []); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                  in
                  let implementation_dependency_graph =
                    [("a", []); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                  in
                  let root_files = ["b"] in
                  let (sig_dependents, all_dependents) =
                    calc_all_dependents
                      ~sig_dependency_graph
                      ~implementation_dependency_graph
                      root_files
                  in
                  let expected_all_dependents = make_filename_set ["b"; "c"; "d"; "e"] in
                  assert_sets_equal ~ctxt expected_all_dependents all_dependents;
                  let expected_sig_dependents = make_filename_set ["b"; "c"; "d"; "e"] in
                  assert_sets_equal ~ctxt expected_sig_dependents sig_dependents
                );
                ( "long_chain_no_sig_dependencies" >:: fun ctxt ->
                  let sig_dependency_graph =
                    [("a", []); ("b", []); ("c", []); ("d", []); ("e", [])]
                  in
                  let implementation_dependency_graph =
                    [("a", []); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                  in
                  let root_files = ["b"] in
                  let (sig_dependents, all_dependents) =
                    calc_all_dependents
                      ~sig_dependency_graph
                      ~implementation_dependency_graph
                      root_files
                  in
                  (* See comments on `calc_all_dependents` for why this is expected behavior. *)
                  let expected_all_dependents = make_filename_set ["b"; "c"] in
                  assert_sets_equal ~ctxt expected_all_dependents all_dependents;
                  let expected_sig_dependents = make_filename_set ["b"] in
                  assert_sets_equal ~ctxt expected_sig_dependents sig_dependents
                );
              ];
       ]
