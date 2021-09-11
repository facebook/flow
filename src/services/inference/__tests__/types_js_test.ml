(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

let dummy_flowconfig_params =
  {
    CommandUtils.ignores = [];
    untyped = [];
    declarations = [];
    includes = [];
    libs = [];
    raw_lint_severities = [];
  }

let dummy_options_flags =
  {
    CommandUtils.Options_flags.all = false;
    debug = false;
    flowconfig_flags = dummy_flowconfig_params;
    include_warnings = false;
    max_warnings = None;
    max_workers = None;
    merge_timeout = None;
    munge_underscore_members = false;
    no_flowlib = false;
    profile = false;
    quiet = false;
    strip_root = false;
    temp_dir = None;
    traces = None;
    trust_mode = None;
    new_merge = false;
    abstract_locations = true;
    verbose = None;
    wait_for_recheck = None;
    weak = false;
    new_env = false;
    include_suppressions = false;
  }

let dummy_saved_state_flags =
  {
    CommandUtils.Saved_state_flags.saved_state_fetcher = None;
    saved_state_force_recheck = false;
    saved_state_no_fallback = false;
  }

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
  CheckedSet.add
    ~focused:(FilenameSet.diff (CheckedSet.all checked_files) freshparsed)
    CheckedSet.empty

let make_options () =
  let flowconfig = FlowConfig.empty_config in
  let root = Path.dummy_path in
  CommandUtils.make_options
    ~flowconfig_name:".flowconfig"
    ~flowconfig_hash:""
    ~flowconfig
    ~lazy_mode:None
    ~root
    ~options_flags:dummy_options_flags
    ~saved_state_options_flags:dummy_saved_state_flags

let prepare_freshparsed freshparsed =
  freshparsed |> Base.List.map ~f:make_fake_file_key |> FilenameSet.of_list

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
  let options = make_options () in
  let unchanged_checked = make_unchanged_checked checked_files freshparsed in
  (* This approximates the behavior of Dep_service.calc_direct_dependents. As of October 2019, it
   * includes all direct dependents, not just sig direct dependents. If
   * Dep_service.calc_direct_dependents changes, this should change too so that these tests more
   * accurately reflect reality. *)
  let direct_dependent_files =
    FilenameGraph.fold
      (fun file deps acc ->
        if FilenameSet.exists (fun x -> FilenameSet.mem x freshparsed) deps then
          FilenameSet.add file acc
        else
          acc)
      implementation_dependency_graph
      FilenameSet.empty
  in
  Types_js.debug_determine_what_to_recheck
    ~profiling
    ~options
    ~sig_dependency_graph
    ~implementation_dependency_graph
    ~checked_files
    ~freshparsed
    ~unparsed_set:FilenameSet.empty
    ~deleted:FilenameSet.empty
    ~unchanged_checked
    ~files_to_force:CheckedSet.empty
    ~unchanged_files_to_force:CheckedSet.empty
    ~direct_dependent_files

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
  let unchanged_checked = make_unchanged_checked checked_files changed_files in
  let options = make_options () in
  let (sig_dependent_files, all_dependent_files) =
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
    ~sig_dependent_files
    ~all_dependent_files

(* There is memory sampling embedded throughout the code under test. It polls the shared memory
 * system to get information about its usage. If the shared memory system is not initialized, we get
 * crashes, so we have to initialize it before running tests. *)
let sharedmem_config = { SharedMem.heap_size = 1024 * 1024; hash_table_pow = 19; log_level = 0 }

let _ = SharedMem.init sharedmem_config ~num_workers:1

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
                                     to_merge_or_check;
                                     components = _;
                                     recheck_set = _;
                                     sig_dependent_files = _;
                                     all_dependent_files = _;
                                   }) =
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
                         assert_checked_sets_equal
                           ~ctxt
                           (CheckedSet.union to_merge to_check)
                           to_merge_or_check;
                         Lwt.return_unit);
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
                                     to_merge_or_check;
                                     components = _;
                                     recheck_set = _;
                                     sig_dependent_files = _;
                                     all_dependent_files = _;
                                   }) =
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
                         assert_checked_sets_equal
                           ~ctxt
                           (CheckedSet.union to_merge to_check)
                           to_merge_or_check;
                         Lwt.return_unit);
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
                                     to_merge_or_check;
                                     components = _;
                                     recheck_set = _;
                                     sig_dependent_files = _;
                                     all_dependent_files = _;
                                   }) =
                           determine_what_to_recheck
                             ~profiling
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~freshparsed
                         in
                         let expected_to_merge =
                           (* We always include direct dependents, and compute them using the
                            * implementation dependency graph. *)
                           make_checked_set ~focused:["a"] ~dependents:["b"] ~dependencies:[]
                         in
                         let expected_to_check =
                           make_checked_set ~focused:["a"] ~dependents:["b"; "c"] ~dependencies:[]
                         in
                         assert_checked_sets_equal ~ctxt expected_to_merge to_merge;
                         assert_checked_sets_equal ~ctxt expected_to_check to_check;
                         assert_checked_sets_equal
                           ~ctxt
                           (CheckedSet.union to_merge to_check)
                           to_merge_or_check;
                         Lwt.return_unit);
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
                                     to_merge_or_check;
                                     components = _;
                                     recheck_set = _;
                                     sig_dependent_files = _;
                                     all_dependent_files = _;
                                   }) =
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
                         assert_checked_sets_equal
                           ~ctxt
                           (CheckedSet.union to_merge to_check)
                           to_merge_or_check;
                         Lwt.return_unit);
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
                                     to_merge_or_check;
                                     components = _;
                                     recheck_set = _;
                                     sig_dependent_files = _;
                                     all_dependent_files = _;
                                   }) =
                           determine_what_to_recheck
                             ~profiling
                             ~sig_dependency_graph
                             ~implementation_dependency_graph
                             ~freshparsed
                         in
                         let expected_to_merge =
                           (* We always include direct dependents, and compute them using the
                            * implementation dependency graph. *)
                           make_checked_set ~focused:["a"] ~dependents:["b"] ~dependencies:[]
                         in
                         let expected_to_check =
                           make_checked_set ~focused:["a"] ~dependents:["b"; "c"] ~dependencies:[]
                         in
                         assert_checked_sets_equal ~ctxt expected_to_merge to_merge;
                         assert_checked_sets_equal ~ctxt expected_to_check to_check;
                         assert_checked_sets_equal
                           ~ctxt
                           (CheckedSet.union to_merge to_check)
                           to_merge_or_check;
                         Lwt.return_unit);
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
                         let%lwt (to_merge, to_check, to_merge_or_check, _components, _recheck_set)
                             =
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
                         assert_checked_sets_equal
                           ~ctxt
                           (CheckedSet.union to_merge to_check)
                           to_merge_or_check;
                         Lwt.return_unit);
                "long_chain_no_sig_dependencies"
                %>:: test_with_profiling (fun ctxt profiling ->
                         let sig_dependency_graph =
                           [("a", []); ("b", []); ("c", []); ("d", []); ("e", [])]
                         in
                         let implementation_dependency_graph =
                           [("a", []); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                         in
                         let%lwt (to_merge, to_check, to_merge_or_check, _components, _recheck_set)
                             =
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
                           make_checked_set ~focused:["a"] ~dependents:[] ~dependencies:[]
                         in
                         let expected_to_check =
                           make_checked_set ~focused:["a"] ~dependents:["b"] ~dependencies:[]
                         in
                         assert_checked_sets_equal ~ctxt expected_to_merge to_merge;
                         assert_checked_sets_equal ~ctxt expected_to_check to_check;
                         assert_checked_sets_equal
                           ~ctxt
                           (CheckedSet.union to_merge to_check)
                           to_merge_or_check;
                         Lwt.return_unit);
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
                         let%lwt (to_merge, to_check, to_merge_or_check, components, _recheck_set) =
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
                             ~dependents:["b"; "c"; "d"]
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
                         assert_checked_sets_equal
                           ~ctxt
                           (CheckedSet.union to_merge to_check)
                           to_merge_or_check;
                         assert_components_equal ~ctxt expected_components components;
                         Lwt.return_unit);
                "lazy"
                %>:: test_with_profiling (fun ctxt profiling ->
                         let sig_dependency_graph = [("a", []); ("b", []); ("c", []); ("d", [])] in
                         let implementation_dependency_graph =
                           [("a", ["b"]); ("b", ["c"]); ("c", ["d"]); ("d", [])]
                         in
                         let%lwt (to_merge, to_check, to_merge_or_check, components, _recheck_set) =
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
                           (* TODO "c" should not be listed as a dependency *)
                           make_checked_set ~focused:["d"] ~dependents:[] ~dependencies:["c"]
                         in
                         let expected_to_check =
                           make_checked_set ~focused:["d"] ~dependents:["c"] ~dependencies:[]
                         in
                         let expected_components = [["c"]; ["d"]] in
                         assert_checked_sets_equal ~ctxt expected_to_merge to_merge;
                         assert_checked_sets_equal ~ctxt expected_to_check to_check;
                         assert_checked_sets_equal
                           ~ctxt
                           (CheckedSet.union to_merge to_check)
                           to_merge_or_check;
                         assert_components_equal ~ctxt expected_components components;
                         Lwt.return_unit);
              ];
       ]
