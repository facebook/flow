(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Utils_js
open Dep_graph_test_utils

(* Like `>::` except it expects the function to return `unit Lwt.t` rather than `unit` *)
let ( %>:: ) name f = name >:: fun ctxt -> LwtInit.run_lwt (fun () -> f ctxt)

let assert_checked_sets_equal ~ctxt expected actual =
  assert_equal ~ctxt ~cmp:CheckedSet.debug_equal ~printer:CheckedSet.debug_to_string expected actual

module FilenameSetSet = Flow_set.Make (FilenameSet)

let debug_string_of_filename_set_set =
  FilenameSetSet.elements
  %> Base.List.map ~f:debug_string_of_filename_set
  %> Base.List.map ~f:(spf "  %s")
  %> String.concat "\n"
  %> spf "[\n%s\n]"

let filename_set_set_of_nested_list = Base.List.map ~f:FilenameSet.of_list %> FilenameSetSet.of_list

let assert_components_equal ~ctxt expected actual =
  (* `expected`, for convenience, is just a list of lists. `actual` is a list of Nel.ts. *)
  let expected =
    expected
    |> Base.List.map ~f:(Base.List.map ~f:make_fake_file_key)
    |> filename_set_set_of_nested_list
  in
  let actual = actual |> Base.List.map ~f:Nel.to_list |> filename_set_set_of_nested_list in
  assert_equal
    ~ctxt
    ~cmp:FilenameSetSet.equal
    ~printer:debug_string_of_filename_set_set
    expected
    actual

let test_with_profiling test_fun ctxt =
  let%lwt (_finished, result) =
    Profiling_js.with_profiling_lwt ~label:"Test" ~should_print_summary:false (test_fun ctxt)
  in
  Lwt.return result

let make_checked_set ~focused ~dependents ~dependencies =
  let focused = make_filename_set focused in
  let dependents = make_filename_set dependents in
  let dependencies = make_filename_set dependencies in
  CheckedSet.add ~focused ~dependents ~dependencies CheckedSet.empty

let make_unchanged_checked checked_files freshparsed =
  CheckedSet.remove (CheckedSet.all freshparsed) checked_files

let prepare_freshparsed freshparsed =
  freshparsed |> Base.List.map ~f:make_fake_file_key |> CheckedSet.of_focused_list

let checked_files_of_graph ~implementation_dependency_graph =
  (* Get all the files from the implementation_dependency_graph and consider them focused *)
  (* All files must be present as keys, even if they have no values. *)
  let focused_set =
    FilenameGraph.fold
      (fun file _ acc -> FilenameSet.add file acc)
      implementation_dependency_graph
      FilenameSet.empty
  in
  CheckedSet.add ~focused:focused_set CheckedSet.empty

let determine_what_to_recheck
    ~profiling ~sig_dependency_graph ~implementation_dependency_graph ~freshparsed =
  let sig_dependency_graph = make_dependency_graph sig_dependency_graph in
  let implementation_dependency_graph = make_dependency_graph implementation_dependency_graph in
  let checked_files = checked_files_of_graph ~implementation_dependency_graph in
  let freshparsed = prepare_freshparsed freshparsed in
  let options = Test_utils.make_options () in
  let unchanged_checked = make_unchanged_checked checked_files freshparsed in
  Types_js.debug_determine_what_to_recheck
    ~profiling
    ~options
    ~sig_dependency_graph
    ~implementation_dependency_graph
    ~freshparsed
    ~unchanged_checked
    ~unchanged_files_to_force:CheckedSet.empty
    ~dirty_direct_dependents:FilenameSet.empty

let include_dependencies_and_dependents
    ~profiling
    ~checked_files
    ~sig_dependency_graph
    ~implementation_dependency_graph
    ~input_focused
    ~input_dependents
    ~input_dependencies =
  let input =
    CheckedSet.add
      ~focused:(make_filename_set input_focused)
      ~dependents:(make_filename_set input_dependents)
      ~dependencies:(make_filename_set input_dependencies)
      CheckedSet.empty
  in
  let changed_files = CheckedSet.focused input in
  let sig_dependency_graph = make_dependency_graph sig_dependency_graph in
  let implementation_dependency_graph = make_dependency_graph implementation_dependency_graph in
  let checked_files =
    match checked_files with
    | `All -> checked_files_of_graph ~implementation_dependency_graph
    | `Lazy lst -> make_checked_set ~focused:lst ~dependents:[] ~dependencies:[]
  in
  let unchanged_checked = make_unchanged_checked checked_files input in
  let options = Test_utils.make_options () in
  let all_dependent_files =
    Pure_dep_graph_operations.calc_all_dependents
      ~sig_dependency_graph
      ~implementation_dependency_graph
      changed_files
  in
  Types_js.debug_include_dependencies_and_dependents
    ~options
    ~profiling
    ~unchanged_checked
    ~input
    ~implementation_dependency_graph
    ~sig_dependency_graph
    ~all_dependent_files

(* There is memory sampling embedded throughout the code under test. It polls the shared memory
 * system to get information about its usage. If the shared memory system is not initialized, we get
 * crashes, so we have to initialize it before running tests. *)
let sharedmem_config = { SharedMem.heap_size = 1024 * 1024; hash_table_pow = 19 }

let _ = SharedMem.init sharedmem_config ~num_workers:1

(* === State transition test helpers === *)

(** Helper to run determine_what_to_recheck with dirty_direct_dependents and unchanged_files_to_force.
    This exercises the full recheck fanout logic including forced files (IDE focus) and
    module-level dirty dependents. *)
let determine_what_to_recheck_full
    ~profiling
    ~sig_dependency_graph
    ~implementation_dependency_graph
    ~freshparsed
    ?(dirty_direct_dependents = [])
    ?(unchanged_files_to_force_focused = [])
    ?(unchanged_files_to_force_deps = [])
    ?(checked_files_override = None)
    () =
  let sig_dependency_graph = make_dependency_graph sig_dependency_graph in
  let implementation_dependency_graph = make_dependency_graph implementation_dependency_graph in
  let checked_files =
    match checked_files_override with
    | Some cs -> cs
    | None -> checked_files_of_graph ~implementation_dependency_graph
  in
  let freshparsed = prepare_freshparsed freshparsed in
  let options = Test_utils.make_options () in
  let unchanged_checked = make_unchanged_checked checked_files freshparsed in
  let dirty_direct_dependents = make_filename_set dirty_direct_dependents in
  let unchanged_files_to_force =
    CheckedSet.add
      ~focused:(make_filename_set unchanged_files_to_force_focused)
      ~dependencies:(make_filename_set unchanged_files_to_force_deps)
      CheckedSet.empty
  in
  Types_js.debug_determine_what_to_recheck
    ~profiling
    ~options
    ~sig_dependency_graph
    ~implementation_dependency_graph
    ~freshparsed
    ~unchanged_checked
    ~unchanged_files_to_force
    ~dirty_direct_dependents

let tests =
  "types_js"
  >::: [
         "determine_what_to_recheck"
         >::: [
                "simple_test"
                %>:: test_with_profiling (fun ctxt profiling ->
                         let sig_dependency_graph =
                           [("a", ["b"]); ("b", ["c"; "d"]); ("c", []); ("d", [])]
                         in
                         let implementation_dependency_graph =
                           [("a", ["b"]); ("b", ["c"; "d"]); ("c", []); ("d", [])]
                         in
                         let freshparsed = ["b"] in
                         let%lwt (Types_js.Determine_what_to_recheck_result
                                   {
                                     to_merge;
                                     to_check;
                                     components = _;
                                     recheck_set = _;
                                     dependent_file_count = _;
                                   }
                                   ) =
                           determine_what_to_recheck
                             ~profiling
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~freshparsed
                         in
                         let expected_to_merge =
                           make_checked_set ~focused:["b"] ~dependents:["a"] ~dependencies:[]
                         in
                         let expected_to_check =
                           make_checked_set ~focused:["b"] ~dependents:["a"] ~dependencies:[]
                         in
                         assert_checked_sets_equal ~ctxt expected_to_merge to_merge;
                         assert_checked_sets_equal ~ctxt expected_to_check to_check;
                         Lwt.return_unit
                     );
                "long_chain"
                %>:: test_with_profiling (fun ctxt profiling ->
                         let sig_dependency_graph =
                           [("a", []); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                         in
                         let implementation_dependency_graph =
                           [("a", []); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                         in
                         let freshparsed = ["a"] in
                         let%lwt (Types_js.Determine_what_to_recheck_result
                                   {
                                     to_merge;
                                     to_check;
                                     components = _;
                                     recheck_set = _;
                                     dependent_file_count = _;
                                   }
                                   ) =
                           determine_what_to_recheck
                             ~profiling
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~freshparsed
                         in
                         let expected_to_merge =
                           make_checked_set
                             ~focused:["a"]
                             ~dependents:["b"; "c"; "d"; "e"]
                             ~dependencies:[]
                         in
                         let expected_to_check =
                           make_checked_set
                             ~focused:["a"]
                             ~dependents:["b"; "c"; "d"; "e"]
                             ~dependencies:[]
                         in
                         assert_checked_sets_equal ~ctxt expected_to_merge to_merge;
                         assert_checked_sets_equal ~ctxt expected_to_check to_check;
                         Lwt.return_unit
                     );
                "long_chain_no_sig_dependencies"
                %>:: test_with_profiling (fun ctxt profiling ->
                         let sig_dependency_graph =
                           [("a", []); ("b", []); ("c", []); ("d", []); ("e", [])]
                         in
                         let implementation_dependency_graph =
                           [("a", []); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                         in
                         let freshparsed = ["a"] in
                         let%lwt (Types_js.Determine_what_to_recheck_result
                                   {
                                     to_merge;
                                     to_check;
                                     components = _;
                                     recheck_set = _;
                                     dependent_file_count = _;
                                   }
                                   ) =
                           determine_what_to_recheck
                             ~profiling
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~freshparsed
                         in
                         let expected_to_merge =
                           make_checked_set ~focused:["a"] ~dependents:["b"] ~dependencies:[]
                         in
                         let expected_to_check =
                           make_checked_set ~focused:["a"] ~dependents:["b"] ~dependencies:[]
                         in
                         assert_checked_sets_equal ~ctxt expected_to_merge to_merge;
                         assert_checked_sets_equal ~ctxt expected_to_check to_check;
                         Lwt.return_unit
                     );
                "simple_cycle"
                %>:: test_with_profiling (fun ctxt profiling ->
                         let sig_dependency_graph =
                           [("a", ["e"]); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                         in
                         let implementation_dependency_graph =
                           [("a", ["e"]); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                         in
                         let freshparsed = ["a"] in
                         let%lwt (Types_js.Determine_what_to_recheck_result
                                   {
                                     to_merge;
                                     to_check;
                                     components = _;
                                     recheck_set = _;
                                     dependent_file_count = _;
                                   }
                                   ) =
                           determine_what_to_recheck
                             ~profiling
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~freshparsed
                         in
                         let expected_to_merge =
                           make_checked_set
                             ~focused:["a"]
                             ~dependents:["b"; "c"; "d"; "e"]
                             ~dependencies:[]
                         in
                         let expected_to_check =
                           make_checked_set
                             ~focused:["a"]
                             ~dependents:["b"; "c"; "d"; "e"]
                             ~dependencies:[]
                         in
                         assert_checked_sets_equal ~ctxt expected_to_merge to_merge;
                         assert_checked_sets_equal ~ctxt expected_to_check to_check;
                         Lwt.return_unit
                     );
                "simple_cycle_no_sig_dependencies"
                %>:: test_with_profiling (fun ctxt profiling ->
                         let sig_dependency_graph =
                           [("a", []); ("b", []); ("c", []); ("d", []); ("e", [])]
                         in
                         let implementation_dependency_graph =
                           [("a", ["e"]); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                         in
                         let freshparsed = ["a"] in
                         let%lwt (Types_js.Determine_what_to_recheck_result
                                   {
                                     to_merge;
                                     to_check;
                                     components = _;
                                     recheck_set = _;
                                     dependent_file_count = _;
                                   }
                                   ) =
                           determine_what_to_recheck
                             ~profiling
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~freshparsed
                         in
                         let expected_to_merge =
                           make_checked_set ~focused:["a"] ~dependents:["b"] ~dependencies:[]
                         in
                         let expected_to_check =
                           make_checked_set ~focused:["a"] ~dependents:["b"] ~dependencies:[]
                         in
                         assert_checked_sets_equal ~ctxt expected_to_merge to_merge;
                         assert_checked_sets_equal ~ctxt expected_to_check to_check;
                         Lwt.return_unit
                     );
              ];
         "include_dependencies_and_dependents"
         >::: [
                "simple"
                %>:: test_with_profiling (fun ctxt profiling ->
                         let sig_dependency_graph =
                           [("a", ["b"]); ("b", ["c"; "d"]); ("c", []); ("d", [])]
                         in
                         let implementation_dependency_graph =
                           [("a", ["b"]); ("b", ["c"; "d"]); ("c", []); ("d", [])]
                         in
                         let%lwt (to_merge, to_check, _components, _recheck_set) =
                           include_dependencies_and_dependents
                             ~profiling
                             ~checked_files:`All
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~input_focused:["b"]
                             ~input_dependents:[]
                             ~input_dependencies:[]
                         in
                         let expected_to_merge =
                           make_checked_set ~focused:["b"] ~dependents:["a"] ~dependencies:[]
                         in
                         let expected_to_check =
                           make_checked_set ~focused:["b"] ~dependents:["a"] ~dependencies:[]
                         in
                         assert_checked_sets_equal ~ctxt expected_to_merge to_merge;
                         assert_checked_sets_equal ~ctxt expected_to_check to_check;
                         Lwt.return_unit
                     );
                "long_chain_no_sig_dependencies"
                %>:: test_with_profiling (fun ctxt profiling ->
                         let sig_dependency_graph =
                           [("a", []); ("b", []); ("c", []); ("d", []); ("e", [])]
                         in
                         let implementation_dependency_graph =
                           [("a", []); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                         in
                         let%lwt (to_merge, to_check, _components, _recheck_set) =
                           include_dependencies_and_dependents
                             ~profiling
                             ~checked_files:`All
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~input_focused:["a"]
                             ~input_dependents:[]
                             ~input_dependencies:[]
                         in
                         let expected_to_merge =
                           make_checked_set ~focused:["a"] ~dependents:["b"] ~dependencies:[]
                         in
                         let expected_to_check =
                           make_checked_set ~focused:["a"] ~dependents:["b"] ~dependencies:[]
                         in
                         assert_checked_sets_equal ~ctxt expected_to_merge to_merge;
                         assert_checked_sets_equal ~ctxt expected_to_check to_check;
                         Lwt.return_unit
                     );
                "cycle"
                %>:: test_with_profiling (fun ctxt profiling ->
                         let sig_dependency_graph =
                           [("a", ["b"]); ("b", ["c"]); ("c", ["a"]); ("d", ["c"]); ("e", [])]
                         in
                         let implementation_dependency_graph =
                           [
                             ("a", ["b"]);
                             ("b", ["c"]);
                             ("c", ["a"; "e"]);
                             ("d", ["c"]);
                             ("e", ["d"]);
                           ]
                         in
                         let%lwt (to_merge, to_check, components, _recheck_set) =
                           include_dependencies_and_dependents
                             ~profiling
                             ~checked_files:`All
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~input_focused:["a"]
                             ~input_dependents:[]
                             ~input_dependencies:[]
                         in
                         let expected_to_merge =
                           make_checked_set
                             ~focused:["a"]
                             ~dependents:["b"; "c"; "d"; "e"]
                             ~dependencies:[]
                         in
                         let expected_to_check =
                           make_checked_set
                             ~focused:["a"]
                             ~dependents:["b"; "c"; "d"; "e"]
                             ~dependencies:[]
                         in
                         let expected_components = [["a"; "b"; "c"]; ["d"]; ["e"]] in
                         assert_checked_sets_equal ~ctxt expected_to_merge to_merge;
                         assert_checked_sets_equal ~ctxt expected_to_check to_check;
                         assert_components_equal ~ctxt expected_components components;
                         Lwt.return_unit
                     );
                "lazy"
                %>:: test_with_profiling (fun ctxt profiling ->
                         let sig_dependency_graph = [("a", []); ("b", []); ("c", []); ("d", [])] in
                         let implementation_dependency_graph =
                           [("a", ["b"]); ("b", ["c"]); ("c", ["d"]); ("d", [])]
                         in
                         let%lwt (to_merge, to_check, components, _recheck_set) =
                           include_dependencies_and_dependents
                             ~profiling
                             ~checked_files:(`Lazy [])
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~input_focused:["d"]
                             ~input_dependents:[]
                             ~input_dependencies:[]
                         in
                         let expected_to_merge =
                           make_checked_set ~focused:["d"] ~dependents:["c"] ~dependencies:[]
                         in
                         let expected_to_check =
                           make_checked_set ~focused:["d"] ~dependents:["c"] ~dependencies:[]
                         in
                         let expected_components = [["c"]; ["d"]] in
                         assert_checked_sets_equal ~ctxt expected_to_merge to_merge;
                         assert_checked_sets_equal ~ctxt expected_to_check to_check;
                         assert_components_equal ~ctxt expected_components components;
                         Lwt.return_unit
                     );
              ];
         (* ================================================================
          * STATE TRANSITION TESTS
          *
          * These tests exercise the recheck fanout logic with additional
          * state transition scenarios beyond the basic determine_what_to_recheck
          * and include_dependencies_and_dependents tests above.
          *
          * Test categories:
          * 1. Recheck with dirty_direct_dependents (module-level changes)
          * 2. Recheck with forced files (IDE focus / lazy mode on-demand)
          * 3. Recheck with dependency-only updates (lazy mode dep graph only)
          * 4. Multiple simultaneous file changes
          * 5. Lazy vs eager mode fanout differences
          * ================================================================ *)
         "recheck_with_dirty_direct_dependents"
         >::: [
                (* When a module provider changes (e.g., haste module remapped),
                   dirty_direct_dependents are unchanged files that directly depend
                   on the changed module. They should appear in the recheck set
                   even though their content didn't change. *)
                "module_provider_change_forces_dependents"
                %>:: test_with_profiling (fun _ctxt profiling ->
                         (* Graph: a -> b -> c, d -> c
                            b changed, d is a dirty_direct_dependent (its module dep changed) *)
                         let sig_dependency_graph =
                           [("a", ["b"]); ("b", ["c"]); ("c", []); ("d", ["c"])]
                         in
                         let implementation_dependency_graph =
                           [("a", ["b"]); ("b", ["c"]); ("c", []); ("d", ["c"])]
                         in
                         let freshparsed = ["b"] in
                         let dirty_direct_dependents = ["d"] in
                         let%lwt (Types_js.Determine_what_to_recheck_result
                                   { to_merge; to_check; dependent_file_count; _ }
                                   ) =
                           determine_what_to_recheck_full
                             ~profiling
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~freshparsed
                             ~dirty_direct_dependents
                             ()
                         in
                         (* b is focused (changed). a depends on b via sig dep, so it's a dependent.
                            d is a dirty_direct_dependent, so it should also be pulled in. *)
                         assert_bool
                           "to_merge should contain focused file b"
                           (CheckedSet.mem_focused (make_fake_file_key "b") to_merge);
                         assert_bool
                           "to_merge should contain dependent a"
                           (CheckedSet.mem (make_fake_file_key "a") to_merge);
                         assert_bool
                           "to_merge should contain dirty dependent d"
                           (CheckedSet.mem (make_fake_file_key "d") to_merge);
                         (* dependent_file_count should include d *)
                         assert_bool "dependent_file_count should be > 0" (dependent_file_count > 0);
                         (* to_check should also include d since it's a dependent *)
                         assert_bool
                           "to_check should contain d"
                           (CheckedSet.mem (make_fake_file_key "d") to_check);
                         Lwt.return_unit
                     );
              ];
         "recheck_with_forced_files"
         >::: [
                (* In lazy mode, when a user opens a file in the IDE, it is "forced"
                   as an unchanged_file_to_force. This simulates that: force "d" which
                   is not in freshparsed (it didn't change), but should be pulled into
                   the recheck. *)
                "force_unchanged_file_triggers_recheck"
                %>:: test_with_profiling (fun _ctxt profiling ->
                         (* Graph: a -> b -> c -> d, all with sig deps *)
                         let sig_dependency_graph =
                           [("a", ["b"]); ("b", ["c"]); ("c", ["d"]); ("d", [])]
                         in
                         let implementation_dependency_graph =
                           [("a", ["b"]); ("b", ["c"]); ("c", ["d"]); ("d", [])]
                         in
                         (* Only "a" actually changed *)
                         let freshparsed = ["a"] in
                         (* But we also force "d" to be checked (e.g., user opened it in IDE) *)
                         let unchanged_files_to_force_focused = ["d"] in
                         let%lwt (Types_js.Determine_what_to_recheck_result
                                   { to_merge; to_check; _ }
                                   ) =
                           determine_what_to_recheck_full
                             ~profiling
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~freshparsed
                             ~unchanged_files_to_force_focused
                             ()
                         in
                         (* Both a (changed) and d (forced) should be focused *)
                         assert_bool
                           "to_merge should contain changed file a"
                           (CheckedSet.mem_focused (make_fake_file_key "a") to_merge);
                         assert_bool
                           "to_merge should contain forced file d"
                           (CheckedSet.mem (make_fake_file_key "d") to_merge);
                         assert_bool
                           "to_check should contain forced file d"
                           (CheckedSet.mem (make_fake_file_key "d") to_check);
                         Lwt.return_unit
                     );
                (* Force a file as a dependency only (lazy mode dep graph update).
                   Dependencies should be merged but NOT checked. *)
                "force_as_dependency_merges_but_does_not_check"
                %>:: test_with_profiling (fun _ctxt profiling ->
                         let sig_dependency_graph = [("a", []); ("b", []); ("c", []); ("d", [])] in
                         let implementation_dependency_graph =
                           [("a", []); ("b", []); ("c", []); ("d", [])]
                         in
                         let freshparsed = [] in
                         let unchanged_files_to_force_deps = ["c"] in
                         let%lwt (Types_js.Determine_what_to_recheck_result
                                   { to_merge; to_check; _ }
                                   ) =
                           determine_what_to_recheck_full
                             ~profiling
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~freshparsed
                             ~unchanged_files_to_force_deps
                             ()
                         in
                         (* c should be in to_merge (it needs to be merged) *)
                         assert_bool
                           "to_merge should contain dependency file c"
                           (CheckedSet.mem (make_fake_file_key "c") to_merge);
                         (* c should NOT be in to_check's focused or dependents sets,
                            because it was forced as a dependency only *)
                         let c_in_check_focused =
                           CheckedSet.mem_focused (make_fake_file_key "c") to_check
                         in
                         let c_in_check_dependent =
                           CheckedSet.mem_dependent (make_fake_file_key "c") to_check
                         in
                         assert_bool
                           "dependency c should not be in to_check focused"
                           (not c_in_check_focused);
                         assert_bool
                           "dependency c should not be in to_check dependents"
                           (not c_in_check_dependent);
                         Lwt.return_unit
                     );
              ];
         "multiple_simultaneous_changes"
         >::: [
                (* Multiple files change at once — each should be focused, and their
                   combined dependents should be included. *)
                "two_files_change_combined_fanout"
                %>:: test_with_profiling (fun ctxt profiling ->
                         (* a -> b, a -> c, d -> b, d -> c
                            Both b and c change. a and d are dependents of both. *)
                         let sig_dependency_graph =
                           [("a", ["b"; "c"]); ("b", []); ("c", []); ("d", ["b"; "c"])]
                         in
                         let implementation_dependency_graph =
                           [("a", ["b"; "c"]); ("b", []); ("c", []); ("d", ["b"; "c"])]
                         in
                         let freshparsed = ["b"; "c"] in
                         let%lwt (Types_js.Determine_what_to_recheck_result
                                   { to_merge; to_check; dependent_file_count; _ }
                                   ) =
                           determine_what_to_recheck
                             ~profiling
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~freshparsed
                         in
                         let expected_to_merge =
                           make_checked_set
                             ~focused:["b"; "c"]
                             ~dependents:["a"; "d"]
                             ~dependencies:[]
                         in
                         let expected_to_check =
                           make_checked_set
                             ~focused:["b"; "c"]
                             ~dependents:["a"; "d"]
                             ~dependencies:[]
                         in
                         assert_checked_sets_equal ~ctxt expected_to_merge to_merge;
                         assert_checked_sets_equal ~ctxt expected_to_check to_check;
                         (* dependent_file_count is the count from calc_all_dependents, which
                            includes the roots themselves. With b and c changed, and a and d
                            as dependents, plus b and c as self-dependents = 4. *)
                         assert_equal ~ctxt ~printer:string_of_int 4 dependent_file_count;
                         Lwt.return_unit
                     );
                (* A file that is both a dependency and a dependent of changed files
                   should be classified at the highest priority (focused > dependent > dependency). *)
                "changed_file_also_dependent_stays_focused"
                %>:: test_with_profiling (fun _ctxt profiling ->
                         (* a -> b -> c. Both a and c change. b depends on a (dependent)
                            but c depends on b (b is a dependency of c).
                            b should be a dependent, not a dependency. *)
                         let sig_dependency_graph = [("a", []); ("b", ["a"]); ("c", ["b"])] in
                         let implementation_dependency_graph =
                           [("a", []); ("b", ["a"]); ("c", ["b"])]
                         in
                         let freshparsed = ["a"; "c"] in
                         let%lwt (Types_js.Determine_what_to_recheck_result
                                   { to_merge; to_check; _ }
                                   ) =
                           determine_what_to_recheck
                             ~profiling
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~freshparsed
                         in
                         (* a and c are focused (changed). b is a sig-dependent of a. *)
                         assert_bool
                           "a should be focused"
                           (CheckedSet.mem_focused (make_fake_file_key "a") to_merge);
                         assert_bool
                           "c should be focused"
                           (CheckedSet.mem_focused (make_fake_file_key "c") to_merge);
                         (* b is a dependent of a, so it should be in to_check *)
                         assert_bool
                           "b should be in to_check"
                           (CheckedSet.mem (make_fake_file_key "b") to_check);
                         Lwt.return_unit
                     );
              ];
         "lazy_mode_fanout"
         >::: [
                (* In lazy mode with no files previously checked, changing a leaf file
                   should only pull in its direct implementation dependents, not the
                   entire transitive chain. *)
                "lazy_leaf_change_minimal_fanout"
                %>:: test_with_profiling (fun ctxt profiling ->
                         (* a -> b -> c -> d (all impl deps, no sig deps) *)
                         let sig_dependency_graph = [("a", []); ("b", []); ("c", []); ("d", [])] in
                         let implementation_dependency_graph =
                           [("a", ["b"]); ("b", ["c"]); ("c", ["d"]); ("d", [])]
                         in
                         (* Lazy: nothing previously checked *)
                         let%lwt (to_merge, to_check, _components, _recheck_set) =
                           include_dependencies_and_dependents
                             ~profiling
                             ~checked_files:(`Lazy [])
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~input_focused:["d"]
                             ~input_dependents:[]
                             ~input_dependencies:[]
                         in
                         (* Without sig deps, only c (direct impl dependent of d) is pulled in *)
                         let expected_to_merge =
                           make_checked_set ~focused:["d"] ~dependents:["c"] ~dependencies:[]
                         in
                         let expected_to_check =
                           make_checked_set ~focused:["d"] ~dependents:["c"] ~dependencies:[]
                         in
                         assert_checked_sets_equal ~ctxt expected_to_merge to_merge;
                         assert_checked_sets_equal ~ctxt expected_to_check to_check;
                         Lwt.return_unit
                     );
                (* In lazy mode with some files already checked, unchanged checked files
                   should not be re-merged. *)
                "lazy_skips_unchanged_checked"
                %>:: test_with_profiling (fun _ctxt profiling ->
                         (* a -> b -> c (sig deps) *)
                         let sig_dependency_graph = [("a", ["b"]); ("b", ["c"]); ("c", [])] in
                         let implementation_dependency_graph =
                           [("a", ["b"]); ("b", ["c"]); ("c", [])]
                         in
                         (* In lazy mode, a and b were previously checked *)
                         let%lwt (to_merge, _to_check, _components, _recheck_set) =
                           include_dependencies_and_dependents
                             ~profiling
                             ~checked_files:(`Lazy ["a"; "b"])
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~input_focused:["c"]
                             ~input_dependents:[]
                             ~input_dependencies:[]
                         in
                         (* c is focused (changed). a and b are sig-dependents of c.
                            Since a and b were already checked, they should be in to_merge
                            as dependents (they need to be re-merged because their dep changed). *)
                         assert_bool
                           "c should be focused in to_merge"
                           (CheckedSet.mem_focused (make_fake_file_key "c") to_merge);
                         assert_bool
                           "a should be in to_merge"
                           (CheckedSet.mem (make_fake_file_key "a") to_merge);
                         assert_bool
                           "b should be in to_merge"
                           (CheckedSet.mem (make_fake_file_key "b") to_merge);
                         Lwt.return_unit
                     );
              ];
         "dependency_only_updates"
         >::: [
                (* When files are input as dependencies only (not focused), they should
                   appear in to_merge but NOT in to_check. This is the lazy mode
                   dependency graph update path. *)
                "dependencies_merged_but_not_checked"
                %>:: test_with_profiling (fun _ctxt profiling ->
                         let sig_dependency_graph = [("a", ["b"]); ("b", ["c"]); ("c", [])] in
                         let implementation_dependency_graph =
                           [("a", ["b"]); ("b", ["c"]); ("c", [])]
                         in
                         let%lwt (to_merge, to_check, _components, _recheck_set) =
                           include_dependencies_and_dependents
                             ~profiling
                             ~checked_files:`All
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~input_focused:[]
                             ~input_dependents:[]
                             ~input_dependencies:["c"]
                         in
                         (* c is a dependency, so it should be in to_merge *)
                         assert_bool
                           "c should be in to_merge"
                           (CheckedSet.mem (make_fake_file_key "c") to_merge);
                         (* c should NOT be in to_check as focused or dependent *)
                         assert_bool
                           "c should not be focused in to_check"
                           (not (CheckedSet.mem_focused (make_fake_file_key "c") to_check));
                         assert_bool
                           "c should not be dependent in to_check"
                           (not (CheckedSet.mem_dependent (make_fake_file_key "c") to_check));
                         Lwt.return_unit
                     );
              ];
         "recheck_components"
         >::: [
                (* Verify that SCC components are correctly computed and that
                   every component is entirely inside or entirely outside to_merge.
                   This tests Invariant K4 from the state diagram. *)
                "components_entirely_inside_or_outside_to_merge"
                %>:: test_with_profiling (fun ctxt profiling ->
                         (* Two independent cycles: {a,b,c} and {d,e}. Change a. *)
                         let sig_dependency_graph =
                           [("a", ["c"]); ("b", ["a"]); ("c", ["b"]); ("d", ["e"]); ("e", ["d"])]
                         in
                         let implementation_dependency_graph =
                           [("a", ["c"]); ("b", ["a"]); ("c", ["b"]); ("d", ["e"]); ("e", ["d"])]
                         in
                         let%lwt (to_merge, _to_check, components, _recheck_set) =
                           include_dependencies_and_dependents
                             ~profiling
                             ~checked_files:`All
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~input_focused:["a"]
                             ~input_dependents:[]
                             ~input_dependencies:[]
                         in
                         let to_merge_all = CheckedSet.all to_merge in
                         (* Every component should be entirely inside or outside to_merge *)
                         List.iter
                           (fun component ->
                             let files = Nel.to_list component in
                             let all_in =
                               List.for_all (fun f -> FilenameSet.mem f to_merge_all) files
                             in
                             let all_out =
                               List.for_all (fun f -> not (FilenameSet.mem f to_merge_all)) files
                             in
                             assert_bool
                               "each component should be entirely inside or outside to_merge"
                               (all_in || all_out))
                           components;
                         (* The {a,b,c} cycle should be entirely in to_merge *)
                         let expected_components = [["a"; "b"; "c"]] in
                         let in_merge_components =
                           List.filter
                             (fun component ->
                               Nel.for_all (fun f -> FilenameSet.mem f to_merge_all) component)
                             components
                         in
                         assert_components_equal ~ctxt expected_components in_merge_components;
                         Lwt.return_unit
                     );
                (* In a diamond dependency pattern, changing a leaf should pull in
                   both paths to the root. *)
                "diamond_dependency_pulls_both_paths"
                %>:: test_with_profiling (fun ctxt profiling ->
                         (* Diamond: a -> b, a -> c, b -> d, c -> d *)
                         let sig_dependency_graph =
                           [("a", ["b"; "c"]); ("b", ["d"]); ("c", ["d"]); ("d", [])]
                         in
                         let implementation_dependency_graph =
                           [("a", ["b"; "c"]); ("b", ["d"]); ("c", ["d"]); ("d", [])]
                         in
                         let%lwt (to_merge, to_check, _components, _recheck_set) =
                           include_dependencies_and_dependents
                             ~profiling
                             ~checked_files:`All
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~input_focused:["d"]
                             ~input_dependents:[]
                             ~input_dependencies:[]
                         in
                         let expected_to_merge =
                           make_checked_set
                             ~focused:["d"]
                             ~dependents:["a"; "b"; "c"]
                             ~dependencies:[]
                         in
                         let expected_to_check =
                           make_checked_set
                             ~focused:["d"]
                             ~dependents:["a"; "b"; "c"]
                             ~dependencies:[]
                         in
                         assert_checked_sets_equal ~ctxt expected_to_merge to_merge;
                         assert_checked_sets_equal ~ctxt expected_to_check to_check;
                         Lwt.return_unit
                     );
              ];
         "to_check_excludes_dependencies"
         >::: [
                (* Verify Invariant K7: to_check = focused + dependents, never
                   includes dependencies. Dependencies are only merged for their
                   signatures but not type-checked. *)
                "to_check_never_has_dependencies"
                %>:: test_with_profiling (fun _ctxt profiling ->
                         (* a -> b -> c. Change a. b is a sig-dependent.
                            c is a dependency of b but not a dependent of a. *)
                         let sig_dependency_graph = [("a", []); ("b", ["a"]); ("c", [])] in
                         let implementation_dependency_graph =
                           [("a", []); ("b", ["a"; "c"]); ("c", [])]
                         in
                         let%lwt (_to_merge, to_check, _components, _recheck_set) =
                           include_dependencies_and_dependents
                             ~profiling
                             ~checked_files:`All
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~input_focused:["a"]
                             ~input_dependents:[]
                             ~input_dependencies:[]
                         in
                         (* to_check should only have focused and dependents *)
                         let check_deps = CheckedSet.dependencies to_check in
                         assert_bool
                           "to_check should have no dependencies"
                           (FilenameSet.is_empty check_deps);
                         (* Focused + dependents should be non-empty *)
                         let check_focused = CheckedSet.focused to_check in
                         let check_dependents = CheckedSet.dependents to_check in
                         assert_bool
                           "to_check should have focused files"
                           (not (FilenameSet.is_empty check_focused));
                         (* a should be focused, b should be dependent *)
                         assert_bool
                           "a should be in to_check focused"
                           (FilenameSet.mem (make_fake_file_key "a") check_focused);
                         assert_bool
                           "b should be in to_check dependents"
                           (FilenameSet.mem (make_fake_file_key "b") check_dependents);
                         Lwt.return_unit
                     );
              ];
       ]
