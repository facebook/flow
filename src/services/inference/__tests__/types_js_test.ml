(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Utils_js

(* Like `>::` except it expects the function to return `unit Lwt.t` rather than `unit` *)
let ( %>:: ) name f = name >:: (fun ctxt -> LwtInit.run_lwt (fun () -> f ctxt))

let assert_checked_sets_equal ~ctxt expected actual =
  assert_equal
    ~ctxt
    ~cmp:CheckedSet.debug_equal
    ~printer:CheckedSet.debug_to_string
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
    no_saved_state = true;
    profile = false;
    quiet = false;
    saved_state_fetcher = None;
    saved_state_force_recheck = false;
    saved_state_no_fallback = false;
    strip_root = false;
    temp_dir = None;
    traces = None;
    trust_mode = None;
    types_first = true;
    abstract_locations = true;
    verbose = None;
    wait_for_recheck = None;
    weak = false;
    include_suppressions = false;
  }

let test_with_profiling test_fun ctxt =
  let%lwt (_finished, result) =
    Profiling_js.with_profiling_lwt ~label:"Test" ~should_print_summary:false (test_fun ctxt)
  in
  Lwt.return result

let make_fake_file_key filename = File_key.SourceFile ("/tmp/fake/path/" ^ filename ^ ".js")

let make_filename_set filenames = filenames |> List.map make_fake_file_key |> FilenameSet.of_list

let make_checked_set ~focused ~dependents ~dependencies =
  let focused = make_filename_set focused in
  let dependents = make_filename_set dependents in
  let dependencies = make_filename_set dependencies in
  CheckedSet.add ~focused ~dependents ~dependencies CheckedSet.empty

let make_dependency_graph lst =
  List.fold_left
    (fun map (file, dependencies) ->
      let file = make_fake_file_key file in
      if FilenameMap.mem file map then failwith "Duplicate key when constructing map";
      let dependency_set = make_filename_set dependencies in
      FilenameMap.add file dependency_set map)
    FilenameMap.empty
    lst

let make_unchanged_checked checked_files freshparsed =
  CheckedSet.add
    ~focused:(FilenameSet.diff (CheckedSet.all checked_files) freshparsed)
    CheckedSet.empty

let make_options () =
  let flowconfig = FlowConfig.empty_config in
  let root = Path.dummy_path in
  CommandUtils.make_options
    ~flowconfig_name:".flowconfig"
    ~flowconfig
    ~lazy_mode:None
    ~root
    dummy_options_flags

let prepare_freshparsed freshparsed =
  freshparsed |> List.map make_fake_file_key |> FilenameSet.of_list

let make_checked_files ~all_dependency_graph =
  (* Get all the files from the all_dependency_graph and consider them focused *)
  (* All files must be present as keys, even if they have no values. *)
  let focused_set = all_dependency_graph |> FilenameMap.keys |> FilenameSet.of_list in
  CheckedSet.add ~focused:focused_set CheckedSet.empty

let determine_what_to_recheck ~profiling ~dependency_graph ~all_dependency_graph ~freshparsed =
  let dependency_graph = make_dependency_graph dependency_graph in
  let all_dependency_graph = make_dependency_graph all_dependency_graph in
  let checked_files = make_checked_files ~all_dependency_graph in
  let freshparsed = prepare_freshparsed freshparsed in
  let options = make_options () in
  let is_file_checked _ = true in
  let unchanged_checked = make_unchanged_checked checked_files freshparsed in
  (* This approximates the behavior of Dep_service.calc_direct_dependents. As of October 2019, it
   * includes all direct dependents, not just sig direct dependents. If
   * Dep_service.calc_direct_dependents changes, this should change too so that these tests more
   * accurately reflect reality. *)
  let direct_dependent_files =
    FilenameMap.fold
      (fun file deps acc ->
        if FilenameSet.exists (fun x -> FilenameSet.mem x freshparsed) deps then
          FilenameSet.add file acc
        else
          acc)
      all_dependency_graph
      FilenameSet.empty
  in
  Types_js.debug_determine_what_to_recheck
    ~profiling
    ~options
    ~is_file_checked
    ~ide_open_files:(lazy SSet.empty)
    ~dependency_graph
    ~all_dependency_graph
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
    ~dependency_graph
    ~all_dependency_graph
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
  let dependency_graph = make_dependency_graph dependency_graph in
  let all_dependency_graph = make_dependency_graph all_dependency_graph in
  let checked_files = make_checked_files ~all_dependency_graph in
  let unchanged_checked = make_unchanged_checked checked_files changed_files in
  let options = make_options () in
  let all_dependent_files =
    Pure_dep_graph_operations.calc_all_dependents
      ~dependency_graph
      ~all_dependency_graph
      changed_files
  in
  Types_js.debug_include_dependencies_and_dependents
    ~options
    ~profiling
    ~unchanged_checked
    ~input
    ~all_dependency_graph
    ~dependency_graph
    ~all_dependent_files

(* There is memory sampling embedded throughout the code under test. It polls the shared memory
 * system to get information about its usage. If the shared memory system is not initialized, we get
 * crashes, so we have to initialize it before running tests. *)
let sharedmem_config =
  {
    SharedMem_js.global_size = 0;
    heap_size = 1024 * 1024;
    dep_table_pow = 17;
    hash_table_pow = 19;
    shm_dirs = ["/dev/shm"];
    shm_min_avail = 1024 * 256;
    log_level = 0;
    sample_rate = 0.0;
  }

let _ = SharedMem_js.hh_shared_init ~config:sharedmem_config ~shm_dir:None ~num_workers:1

let tests =
  "types_js"
  >::: [
         "determine_what_to_recheck"
         >::: [
                "simple_test"
                %>:: test_with_profiling (fun ctxt profiling ->
                         let dependency_graph =
                           [("a", ["b"]); ("b", ["c"; "d"]); ("c", []); ("d", [])]
                         in
                         let all_dependency_graph =
                           [("a", ["b"]); ("b", ["c"; "d"]); ("c", []); ("d", [])]
                         in
                         let freshparsed = ["b"] in
                         let%lwt ( to_merge,
                                   to_check,
                                   to_merge_or_check,
                                   _components,
                                   _recheck_set,
                                   _all_dependent_files ) =
                           determine_what_to_recheck
                             ~profiling
                             ~dependency_graph
                             ~all_dependency_graph
                             ~freshparsed
                         in
                         let expected =
                           make_checked_set ~focused:["b"] ~dependents:["a"] ~dependencies:[]
                         in
                         assert_checked_sets_equal ~ctxt expected to_merge;
                         assert_checked_sets_equal ~ctxt to_merge to_check;
                         assert_checked_sets_equal ~ctxt to_merge to_merge_or_check;
                         Lwt.return_unit);
                "long_chain"
                %>:: test_with_profiling (fun ctxt profiling ->
                         let dependency_graph =
                           [("a", []); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                         in
                         let all_dependency_graph =
                           [("a", []); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                         in
                         let freshparsed = ["a"] in
                         let%lwt ( to_merge,
                                   to_check,
                                   to_merge_or_check,
                                   _components,
                                   _recheck_set,
                                   _all_dependent_files ) =
                           determine_what_to_recheck
                             ~profiling
                             ~dependency_graph
                             ~all_dependency_graph
                             ~freshparsed
                         in
                         let expected =
                           make_checked_set
                             ~focused:["a"]
                             ~dependents:["b"; "c"; "d"; "e"]
                             ~dependencies:[]
                         in
                         assert_checked_sets_equal ~ctxt expected to_merge;
                         assert_checked_sets_equal ~ctxt to_merge to_check;
                         assert_checked_sets_equal ~ctxt to_merge to_merge_or_check;
                         Lwt.return_unit);
                "long_chain_no_sig_dependencies"
                %>:: test_with_profiling (fun ctxt profiling ->
                         let dependency_graph =
                           [("a", []); ("b", []); ("c", []); ("d", []); ("e", [])]
                         in
                         let all_dependency_graph =
                           [("a", []); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                         in
                         let freshparsed = ["a"] in
                         let%lwt ( to_merge,
                                   to_check,
                                   to_merge_or_check,
                                   _components,
                                   _recheck_set,
                                   _all_dependent_files ) =
                           determine_what_to_recheck
                             ~profiling
                             ~dependency_graph
                             ~all_dependency_graph
                             ~freshparsed
                         in
                         let expected =
                           (* Currently, we have to include everything we need to *check* in the
                            * to_merge set, since we don't separately compute the set of files to
                            * merge and check. "a" changed, so it has to be rechecked. "b" has to be
                            * rechecked because it depends on "a". We also treat direct dependents
                            * as changed files when computing transitive dependents, so this also
                            * pulls in "c". *)
                           make_checked_set ~focused:["a"] ~dependents:["b"; "c"] ~dependencies:[]
                         in
                         assert_checked_sets_equal ~ctxt expected to_merge;
                         assert_checked_sets_equal ~ctxt to_merge to_check;
                         assert_checked_sets_equal ~ctxt to_merge to_merge_or_check;
                         Lwt.return_unit);
                "simple_cycle"
                %>:: test_with_profiling (fun ctxt profiling ->
                         let dependency_graph =
                           [("a", ["e"]); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                         in
                         let all_dependency_graph =
                           [("a", ["e"]); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                         in
                         let freshparsed = ["a"] in
                         let%lwt ( to_merge,
                                   to_check,
                                   to_merge_or_check,
                                   _components,
                                   _recheck_set,
                                   _all_dependent_files ) =
                           determine_what_to_recheck
                             ~profiling
                             ~dependency_graph
                             ~all_dependency_graph
                             ~freshparsed
                         in
                         let expected =
                           make_checked_set
                             ~focused:["a"]
                             ~dependents:["b"; "c"; "d"; "e"]
                             ~dependencies:[]
                         in
                         assert_checked_sets_equal ~ctxt expected to_merge;
                         assert_checked_sets_equal ~ctxt to_merge to_check;
                         assert_checked_sets_equal ~ctxt to_merge to_merge_or_check;
                         Lwt.return_unit);
                "simple_cycle_no_sig_dependencies"
                %>:: test_with_profiling (fun ctxt profiling ->
                         let dependency_graph =
                           [("a", []); ("b", []); ("c", []); ("d", []); ("e", [])]
                         in
                         let all_dependency_graph =
                           [("a", ["e"]); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                         in
                         let freshparsed = ["a"] in
                         let%lwt ( to_merge,
                                   to_check,
                                   to_merge_or_check,
                                   _components,
                                   _recheck_set,
                                   _all_dependent_files ) =
                           determine_what_to_recheck
                             ~profiling
                             ~dependency_graph
                             ~all_dependency_graph
                             ~freshparsed
                         in
                         let expected =
                           (* See the comment in the long_chain_no_sig_dependencies test (above) for
                            * an explanation. *)
                           make_checked_set ~focused:["a"] ~dependents:["b"; "c"] ~dependencies:[]
                         in
                         assert_checked_sets_equal ~ctxt expected to_merge;
                         assert_checked_sets_equal ~ctxt to_merge to_check;
                         assert_checked_sets_equal ~ctxt to_merge to_merge_or_check;
                         Lwt.return_unit);
              ];
         "include_dependencies_and_dependents"
         >::: [
                "simple"
                %>:: test_with_profiling (fun ctxt profiling ->
                         let dependency_graph =
                           [("a", ["b"]); ("b", ["c"; "d"]); ("c", []); ("d", [])]
                         in
                         let all_dependency_graph =
                           [("a", ["b"]); ("b", ["c"; "d"]); ("c", []); ("d", [])]
                         in
                         let%lwt (to_merge, to_check, to_merge_or_check, _components, _recheck_set)
                             =
                           include_dependencies_and_dependents
                             ~profiling
                             ~dependency_graph
                             ~all_dependency_graph
                             ~input_focused:["b"]
                             ~input_dependents:[]
                             ~input_dependencies:[]
                         in
                         let expected =
                           make_checked_set ~focused:["b"] ~dependents:["a"] ~dependencies:[]
                         in
                         assert_checked_sets_equal ~ctxt expected to_merge;
                         assert_checked_sets_equal ~ctxt to_merge to_check;
                         assert_checked_sets_equal ~ctxt to_merge to_merge_or_check;
                         Lwt.return_unit);
                "long_chain_no_sig_dependencies"
                %>:: test_with_profiling (fun ctxt profiling ->
                         let dependency_graph =
                           [("a", []); ("b", []); ("c", []); ("d", []); ("e", [])]
                         in
                         let all_dependency_graph =
                           [("a", []); ("b", ["a"]); ("c", ["b"]); ("d", ["c"]); ("e", ["d"])]
                         in
                         let%lwt (to_merge, to_check, to_merge_or_check, _components, _recheck_set)
                             =
                           include_dependencies_and_dependents
                             ~profiling
                             ~dependency_graph
                             ~all_dependency_graph
                             ~input_focused:["a"]
                             ~input_dependents:[]
                             ~input_dependencies:[]
                         in
                         let expected =
                           make_checked_set ~focused:["a"] ~dependents:["b"] ~dependencies:[]
                         in
                         assert_checked_sets_equal ~ctxt expected to_merge;
                         assert_checked_sets_equal ~ctxt to_merge to_check;
                         assert_checked_sets_equal ~ctxt to_merge to_merge_or_check;
                         Lwt.return_unit);
              ];
       ]
