(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This module drives the type checker *)

open Utils_js

(****************** typecheck job helpers *********************)

let clear_errors ?(debug=false) (files: FilenameSet.t) errors =
  FilenameSet.fold
    (fun file { ServerEnv.local_errors; merge_errors; suppressions; lint_settings; } ->
      if debug then prerr_endlinef "clear errors %s" (string_of_filename file);
      { ServerEnv.
        local_errors = FilenameMap.remove file local_errors;
        merge_errors = FilenameMap.remove file merge_errors;
        suppressions = FilenameMap.remove file suppressions;
        lint_settings = FilenameMap.remove file lint_settings;
      }
    ) files errors

let update_errset map file errset =
  if Errors.ErrorSet.is_empty errset then map
  else
    let errset = match FilenameMap.get file map with
    | Some prev_errset ->
      Errors.ErrorSet.union prev_errset errset
    | None -> errset
    in
    FilenameMap.add file errset map

let merge_error_maps = FilenameMap.union ~combine:(fun _ x y -> Some (Errors.ErrorSet.union x y))

(* Filter out duplicate provider error, if any, for the given file. *)
let filter_duplicate_provider map file =
  match FilenameMap.get file map with
  | Some prev_errset ->
    let new_errset = Errors.ErrorSet.filter (fun err ->
      not (Errors.is_duplicate_provider_error err)
    ) prev_errset in
    FilenameMap.add file new_errset map
  | None -> map

let update_suppressions map file errsup =
  if Error_suppressions.is_empty errsup
    then FilenameMap.remove file map
    else FilenameMap.add file errsup map

let update_lint_settings map file lintsett =
  FilenameMap.add file lintsett map

let with_timer ?options timer profiling f =
  let profiling = Profiling_js.start_timer ~timer profiling in
  let ret = f () in
  let profiling = Profiling_js.stop_timer ~timer profiling in

  (* If we're profiling then output timing information to stderr *)
  (match options with
  | Some options when Options.should_profile options ->
      (match Profiling_js.get_finished_timer ~timer profiling with
      | Some (start_wall_age, wall_duration) ->
          prerr_endlinef
            "TimingEvent `%s`: start_wall_age: %f; wall_duration: %f"
            timer
            start_wall_age
            wall_duration
      | _ -> ());
  | _ -> ());

  (profiling, ret)

let collate_parse_results { Parsing_service_js.parse_ok; parse_skips; parse_fails } =
  let local_errors = List.fold_left (fun errors (file, _, fail) ->
    let errset = match fail with
    | Parsing_service_js.Parse_error err ->
      Inference_utils.set_of_parse_error ~source_file:file err
    | Parsing_service_js.Docblock_errors errs ->
      Inference_utils.set_of_docblock_errors ~source_file:file errs
    in
    update_errset errors file errset
  ) FilenameMap.empty parse_fails in

  let unparsed = List.fold_left (fun unparsed (file, info, _) ->
    (file, info) :: unparsed
  ) parse_skips parse_fails in

  parse_ok, unparsed, local_errors

let parse ~options ~profiling ~workers parse_next =
  with_timer ~options "Parsing" profiling (fun () ->
    let results = Parsing_service_js.parse_with_defaults options workers parse_next in
    collate_parse_results results
  )

let reparse ~options ~profiling ~workers modified =
  with_timer ~options "Parsing" profiling (fun () ->
    let new_or_changed, results =
      Parsing_service_js.reparse_with_defaults options workers modified in
    let parse_ok, unparsed, local_errors = collate_parse_results results in
    new_or_changed, parse_ok, unparsed, local_errors
  )

let parse_contents ~options ~profiling ~check_syntax filename contents =
  with_timer "Parsing" profiling (fun () ->
    (* always enable types when checking an individual file *)
    let types_mode = Parsing_service_js.TypesAllowed in
    let use_strict = Options.modules_are_use_strict options in
    let max_tokens = Options.max_header_tokens options in

    let docblock_errors, info =
      Parsing_service_js.get_docblock ~max_tokens filename contents in
    let errors = Inference_utils.set_of_docblock_errors ~source_file:filename docblock_errors in
    let parse_result = Parsing_service_js.do_parse
      ~fail:check_syntax ~types_mode ~use_strict ~info
      contents filename
    in
    errors, parse_result, info
  )

(* commit providers for old and new modules, collect errors. *)
let commit_modules ~options profiling ~workers
    parsed unparsed ~old_modules local_errors new_or_changed =
  (* conservatively approximate set of modules whose providers will change *)
  (* register providers for modules, warn on dupes etc. *)
    with_timer ~options "CommitModules" profiling (fun () ->
      let new_modules = Module_js.introduce_files workers ~options parsed unparsed in
      let dirty_modules = List.rev_append old_modules new_modules in
      let providers, changed_modules, errmap =
        Module_js.commit_modules workers ~options new_or_changed dirty_modules in
  (* Providers might be new but not changed. This typically happens when old
     providers are deleted, and previously duplicate providers become new
     providers. In such cases, we must clear the old duplicate provider errors
     for the new providers.

     (Note that this is unncessary when the providers are changed, because in
     that case they are rechecked and *all* their errors are cleared. But we
     don't care about optimizing that case for now.) *)
      let errors = List.fold_left filter_duplicate_provider local_errors providers in
      changed_modules, FilenameMap.fold (fun file errors acc ->
        let errset = List.fold_left (fun acc err ->
          match err with
          | Module_js.ModuleDuplicateProviderError { Module_js.
              module_name; provider; conflict;
            } ->
            let msg = Flow_error.(EDuplicateModuleProvider { module_name; provider; conflict }) in
            let error = Flow_error.error_of_msg ~trace_reasons:[] ~op:None ~source_file:file msg in
            Errors.ErrorSet.add error acc
        ) Errors.ErrorSet.empty errors in
        update_errset acc file errset
      ) errmap errors
    )

let resolve_requires ~options profiling ~workers parsed =
  let node_modules_containers = !Files.node_modules_containers in
  with_timer ~options "ResolveRequires" profiling (fun () ->
    MultiWorker.call workers
      ~job: (List.fold_left (fun errors_acc filename ->
        let require_loc = Parsing_service_js.get_requires_unsafe filename in
        let errors =
          Module_js.add_parsed_resolved_requires ~audit:Expensive.ok ~options
            ~node_modules_containers
            filename require_loc in
        if Errors.ErrorSet.is_empty errors
        then errors_acc
        else FilenameMap.add filename errors errors_acc
      )
      )
      ~neutral: FilenameMap.empty
      ~merge: FilenameMap.union
      ~next:(MultiWorker.next workers parsed)
  )

let commit_modules_and_resolve_requires
  ~options
  ~profiling
  ~workers
  ~old_modules
  ~parsed
  ~unparsed
  ~new_or_changed
  ~errors =
  (* TODO remove after lookup overhaul *)
  Module_js.clear_filename_cache ();

  let { ServerEnv.local_errors; merge_errors; suppressions; lint_settings } = errors in

  let profiling, (changed_modules, local_errors) = commit_modules
    ~options profiling ~workers parsed unparsed ~old_modules local_errors new_or_changed in

  let profiling, resolve_errors = resolve_requires
    ~options profiling ~workers parsed in
  let local_errors = FilenameMap.union resolve_errors local_errors in

  profiling, changed_modules, { ServerEnv.local_errors; merge_errors; suppressions; lint_settings }

let error_set_of_merge_exception file exc =
  let loc = Loc.({ none with source = Some file }) in
  let msg = Flow_error.(EInternal (loc, MergeJobException exc)) in
  let error = Flow_error.error_of_msg ~trace_reasons:[] ~op:None ~source_file:file msg in
  Errors.ErrorSet.singleton error

let infer ~options ~profiling ~workers ~suppressions ~lint_settings infer_input =
  with_timer ~options "Infer" profiling (fun () ->
    let infer_results = Infer_service.infer ~options ~workers infer_input in
    List.fold_left (fun (errset, suppressions, lint_settings) (file, errs, supps, lint_setts) ->
      (* TODO update_errset may be able to be replaced by a simpler operation, since infer_results
       * may only have one entry per file *)
      let errset = update_errset errset file errs in
      let suppressions = update_suppressions suppressions file supps in
      let lint_settings = update_lint_settings lint_settings file lint_setts in
      errset, suppressions, lint_settings
    ) (FilenameMap.empty, suppressions, lint_settings) infer_results
  )

let calc_deps ~options ~profiling ~workers to_merge =
  with_timer ~options "CalcDeps" profiling (fun () ->
    let dependency_graph = Dep_service.calc_dependency_graph workers to_merge in
    let partition = Sort_js.topsort dependency_graph in
    if Options.should_profile options then Sort_js.log partition;
    let component_map = Sort_js.component_map partition in
    dependency_graph, component_map
  )

let merge
    ~intermediate_result_callback
    ~options
    ~profiling
    ~workers
    ~merge_errors
    dependency_graph
    component_map
    recheck_map =
  with_timer ~options "Merge" profiling (fun () ->
    let merged = Merge_service.merge_strict
      ~intermediate_result_callback ~options ~workers dependency_graph component_map recheck_map
    in
    List.fold_left (fun merge_errors (file, errors_or_exn) ->
      let merge_errors = List.fold_left (fun merge_errors file ->
        FilenameMap.remove file merge_errors
      ) merge_errors (FilenameMap.find_unsafe file component_map) in
      let errors = match errors_or_exn with
      | Ok errors -> errors
      | Error exc -> error_set_of_merge_exception file exc
      in
      if Errors.ErrorSet.is_empty errors then merge_errors
      else FilenameMap.add file errors merge_errors
    ) merge_errors merged
  )

(* helper *)
let typecheck
  ~options
  ~profiling
  ~workers
  ~errors
  ~unchanged_checked
  ~infer_input
  ~parsed
  ~all_dependent_files
  ~make_merge_input
  ~persistent_connections =
  let { ServerEnv.local_errors; merge_errors; suppressions; lint_settings } = errors in

  (* local inference populates context heap, resolved requires heap *)
  Hh_logger.info "Running local inference";

  let infer_input =
    if Options.is_lazy_mode options
    then
      let new_files = FilenameSet.of_list infer_input in
      let roots = FilenameSet.union new_files all_dependent_files in
      let dependency_graph = Dep_service.calc_dependency_graph workers parsed in
      let all_dependencies = Dep_service.calc_all_dependencies dependency_graph roots in
      let to_infer = FilenameSet.diff all_dependencies unchanged_checked in
      FilenameSet.elements to_infer
    else
      infer_input
  in

  let profiling, (new_local_errors, suppressions, lint_settings) =
    infer ~options ~profiling ~workers ~suppressions ~lint_settings infer_input in

  let send_errors_over_connection = match persistent_connections with
    | None -> fun _ -> ()
    | Some conns ->
        let open Errors in
        let current_errors = ref ErrorSet.empty in
        let suppressions = Error_suppressions.union_suppressions suppressions in
        let lint_settings = SuppressionMap.union_settings lint_settings in
        function lazy results ->
          let new_errors = List.fold_left
            (fun acc (_, errs) -> Errors.ErrorSet.union acc errs)
            Errors.ErrorSet.empty
            results
          in
          let new_errors, _, _ =
            Error_suppressions.filter_suppressed_errors suppressions lint_settings new_errors
          in
          let new_errors = ErrorSet.diff new_errors !current_errors in
          current_errors := ErrorSet.union new_errors !current_errors;
          if not (ErrorSet.is_empty new_errors) then
            Persistent_connection.update_clients conns new_errors
  in

  let () =
    let new_errors = lazy (FilenameMap.bindings new_local_errors) in
    send_errors_over_connection new_errors
  in

  let local_errors = merge_error_maps new_local_errors local_errors in

  (* call supplied function to calculate closure of modules to merge *)
  let profiling, merge_input =
    with_timer ~options "MakeMergeInput" profiling (fun () ->
      make_merge_input infer_input
    ) in

  match merge_input with
  | Some (to_merge, recheck_map) ->
    (* to_merge is the union of inferred (newly inferred files) and the
       transitive closure of all dependents.

       recheck_map maps each file in to_merge to whether it should be rechecked
       initially.
    *)
    Hh_logger.info "Calculating dependencies";
    let profiling, (dependency_graph, component_map) =
      calc_deps ~options ~profiling ~workers to_merge in

    Hh_logger.info "Merging";
    let profiling, merge_errors = try
      let intermediate_result_callback results =
        let errors = lazy (
          List.map (fun (file, errors_or_exc) ->
            match errors_or_exc with
            | Ok errors -> file, errors
            | Error exc -> file, error_set_of_merge_exception file exc
          ) (Lazy.force results)
        ) in
        send_errors_over_connection errors
      in

      let profiling, merge_errors =
        merge
          ~intermediate_result_callback
          ~options
          ~profiling
          ~workers
          ~merge_errors
          dependency_graph
          component_map
          recheck_map
      in
      if Options.should_profile options then Gc.print_stat stderr;
      Hh_logger.info "Done";
      profiling, merge_errors
    with
    (* Unrecoverable exceptions *)
    | SharedMem_js.Out_of_shared_memory
    | SharedMem_js.Heap_full
    | SharedMem_js.Hash_table_full
    | SharedMem_js.Dep_table_full as exn -> raise exn
    (* A catch all suppression is probably a bad idea... *)
    | exc ->
        prerr_endline (Printexc.to_string exc);
        profiling, merge_errors
    in

    profiling,
    FilenameSet.union unchanged_checked (FilenameSet.of_list to_merge),
    { ServerEnv.local_errors; merge_errors; suppressions; lint_settings; }

  | None ->
    profiling, unchanged_checked,
      { ServerEnv.local_errors; merge_errors; suppressions; lint_settings }

(* When checking contents, ensure that dependencies are checked. Might have more
   general utility. *)
let ensure_checked_dependencies ~options ~workers ~env resolved_requires =
  if Options.is_lazy_mode options
  then begin
    let infer_input = Module_js.(NameSet.fold (fun m acc ->
      match get_file m ~audit:Expensive.warn with
      | Some f ->
        if FilenameSet.mem f !env.ServerEnv.files then f :: acc
        else acc
      | None -> acc (* complain elsewhere about required module not found *)
    ) resolved_requires []) in
    let profiling = Profiling_js.empty in
    let errors = !env.ServerEnv.errors in
    let unchanged_checked = !env.ServerEnv.checked_files in
    let parsed = FilenameSet.elements !env.ServerEnv.files in
    let all_dependent_files = FilenameSet.empty in
    let persistent_connections = Some (!env.ServerEnv.connections) in
    let _profiling, checked, errors = typecheck ~options ~profiling ~workers ~errors
      ~unchanged_checked ~infer_input
      ~parsed ~all_dependent_files
      ~persistent_connections
      ~make_merge_input:(fun inferred ->
        Some (inferred,
              inferred |> List.fold_left (fun map f ->
                FilenameMap.add f true map
              ) FilenameMap.empty)
      ) in
    env := { !env with ServerEnv.
      checked_files = checked;
      errors
    }
  end

(* Another special case, similar assumptions as above. *)
(** TODO: handle case when file+contents don't agree with file system state **)
let typecheck_contents ~options ~workers ~env ?(check_syntax=false) contents filename =
  let profiling = Profiling_js.empty in

  let profiling, (errors, parse_result, info) =
    parse_contents ~options ~profiling ~check_syntax filename contents in

  match parse_result with
  | Parsing_service_js.Parse_ok ast ->
      (* defaults *)
      let metadata = Context.metadata_of_options options in
      let metadata =
        let checked, weak = Docblock.(
          match flow info with
          | None ->
            (* If the file does not specify a @flow pragma, we still want to try
               to infer something, but the file might be huge and unannotated,
               which can cause performance issues (including non-termination).
               To avoid this case, we infer the file using "weak mode." *)
            true, true
          | Some OptIn ->
            (* Respect @flow pragma *)
            true, false
          | Some OptInWeak ->
            (* Respect @flow weak pragma *)
            true, true
          | Some OptOut ->
            (* Respect @noflow, which `apply_docblock_overrides` does not by
               default. Again, large files can cause non-termination, so
               respecting this pragma gives programmers a way to tell Flow to
               avoid inference on such files. *)
            false, false
        ) in
        let local_metadata = { metadata.Context.local_metadata with Context.checked; weak } in
        { metadata with Context.local_metadata }
      in
      (* apply overrides from the docblock *)
      let metadata = Infer_service.apply_docblock_overrides metadata info in
      let require_loc_map =
        Parsing_service_js.calc_requires ~default_jsx:(info.Docblock.jsx = None) ~ast
      in

      let lint_settings = Some options.Options.opt_lint_settings in

      (* infer *)
      let profiling, cx = with_timer "Infer" profiling (fun () ->
        Type_inference_js.infer_ast ~metadata ~filename ~lint_settings ast ~require_loc_map
      ) in

      (* write graphml of (unmerged) types, if requested *)
      if Options.output_graphml options then begin
        let fn = Loc.string_of_filename filename in
        let graphml = spf "%s.graphml"
          (if fn = "-" then "contents" else fn) in
        let lines = Graph.format cx in
        let oc = open_out graphml in
        List.iter (output_string oc) lines;
        close_out oc
      end;

      (* merge *)
      let profiling, () = with_timer "Merge" profiling (fun () ->
        let ensure_checked_dependencies = ensure_checked_dependencies ~options ~workers ~env in
        Merge_service.merge_contents_context ~options cx require_loc_map
           ~ensure_checked_dependencies
      ) in

      (* Filter out suppressed errors *)
      let error_suppressions = Context.error_suppressions cx in
      let lint_settings = Context.lint_settings cx in
      let errors = Errors.ErrorSet.fold (fun err errors ->
        let suppressed, _, _ =
          Error_suppressions.check err lint_settings error_suppressions in
        if not suppressed
        then Errors.ErrorSet.add err errors
        else errors
      ) (Context.errors cx) errors in

      profiling, Some cx, errors, info

  | Parsing_service_js.Parse_fail fails ->
      let errors = match fails with
      | Parsing_service_js.Parse_error err ->
          let err = Inference_utils.error_of_parse_error ~source_file:filename err in
          Errors.ErrorSet.add err errors
      | Parsing_service_js.Docblock_errors errs ->
          List.fold_left (fun errors err ->
            let err = Inference_utils.error_of_docblock_error ~source_file:filename err in
            Errors.ErrorSet.add err errors
          ) errors errs
      in
      profiling, None, errors, info

  | Parsing_service_js.Parse_skip
     (Parsing_service_js.Skip_non_flow_file
    | Parsing_service_js.Skip_resource_file) ->
      (* should never happen *)
      profiling, None, errors, info


let init_package_heap ~options ~profiling parsed =
  let profiling, () = with_timer ~options "PackageHeap" profiling (fun () ->
    FilenameSet.iter (fun filename ->
      match filename with
      | Loc.JsonFile str when Filename.basename str = "package.json" ->
        let ast = Parsing_service_js.get_ast_unsafe filename in
        Module_js.add_package str ast
      | _ -> ()
    ) parsed;
  ) in
  profiling

let init_libs ~options ~profiling ~local_errors ~suppressions ~lint_settings ordered_libs =
  with_timer ~options "InitLibs" profiling (fun () ->
    let lib_files = Init_js.init ~options ordered_libs in
    List.fold_left (fun acc (lib_file, ok, errs, suppressions, lint_settings) ->
      let all_ok, errors_acc, suppressions_acc, lint_settings_acc = acc in
      let all_ok = if ok then all_ok else false in
      let errors_acc = update_errset errors_acc lib_file errs in
      let suppressions_acc =
        update_suppressions suppressions_acc lib_file suppressions in
      let lint_settings_acc =
        update_lint_settings lint_settings_acc lib_file lint_settings in
      all_ok, errors_acc, suppressions_acc, lint_settings_acc
    ) (true, local_errors, suppressions, lint_settings) lib_files
  )

(* We maintain the following invariant across rechecks: The set of
   `files` contains files that parsed successfully in the previous
   phase (which could be the init phase or a previous recheck phase)
*)
let recheck ~options ~workers ~updates env =
  let errors = env.ServerEnv.errors in
  let debug = Options.is_debug_mode options in

  (* If foo.js is updated and foo.js.flow exists, then mark foo.js.flow as
   * updated too. This is because sometimes we decide what foo.js.flow
   * provides based on the existence of foo.js *)
  let updates = FilenameSet.fold (fun file updates ->
    if not (Loc.check_suffix file Files.flow_ext) &&
      Parsing_service_js.has_ast (Loc.with_suffix file Files.flow_ext)
    then FilenameSet.add (Loc.with_suffix file Files.flow_ext) updates
    else updates
  ) updates updates in

  let profiling = Profiling_js.empty in

  (* split updates into deleted files and modified files *)
  (** NOTE: We use the term "modified" in the same sense as the underlying file
      system: a modified file exists, and in relation to an old file system
      state, a modified file could be any of "new," "changed," or "unchanged."
  **)
  let modified, deleted = FilenameSet.partition (fun f ->
    Sys.file_exists (string_of_filename f)
  ) updates in
  let deleted_count = FilenameSet.cardinal deleted in
  let modified_count = FilenameSet.cardinal modified in

  (* log modified and deleted files *)
  if deleted_count + modified_count > 0 then (
    prerr_endlinef "recheck %d modified, %d deleted files"
      modified_count deleted_count;
    let log_files files msg n =
      prerr_endlinef "%s files:" msg;
      let _ = FilenameSet.fold (fun f i ->
        prerr_endlinef "%d/%d: %s" i n (string_of_filename f);
        i + 1
      ) files 1
      in ()
    in
    if modified_count > 0 then log_files modified "modified" modified_count;
    if deleted_count > 0 then log_files deleted "deleted" deleted_count
  );

  (* clear errors, asts for deleted files *)
  Parsing_service_js.remove_batch deleted;
  SharedMem_js.collect options `gentle;

  Hh_logger.info "Parsing";
  (* reparse modified files, updating modified to new_or_changed to reflect
     removal of unchanged files *)
  let profiling, (new_or_changed, freshparsed, unparsed, new_local_errors) =
     reparse ~options ~profiling ~workers modified in
  let new_or_changed_count = FilenameSet.cardinal new_or_changed in

  (* clear errors for new, changed and deleted files *)
  let errors =
    errors
    |> clear_errors ~debug new_or_changed
    |> clear_errors ~debug deleted
  in

  (* record reparse errors *)
  let errors =
    let () =
      let error_set: Errors.ErrorSet.t =
        FilenameMap.fold (fun _ -> Errors.ErrorSet.union) new_local_errors Errors.ErrorSet.empty
      in
      Persistent_connection.update_clients env.ServerEnv.connections error_set;
    in
    let local_errors = merge_error_maps new_local_errors errors.ServerEnv.local_errors in
    { errors with ServerEnv.local_errors }
  in

  (* get old (unchanged, undeleted) files that were parsed successfully *)
  let old_parsed = env.ServerEnv.files in
  let new_or_changed_or_deleted = FilenameSet.union new_or_changed deleted in
  let unchanged = FilenameSet.diff old_parsed new_or_changed_or_deleted in

  if debug then prerr_endlinef
    "recheck: old = %d, del = %d, fresh = %d, unmod = %d"
    (FilenameSet.cardinal old_parsed)
    (FilenameSet.cardinal deleted)
    (FilenameSet.cardinal freshparsed)
    (FilenameSet.cardinal unchanged);

  (** Here's where the interesting part of rechecking begins. Before diving into
      code, let's think through the problem independently.

      Note that changing a file can be conceptually thought of as deleting the
      file and then adding it back as a new file. While such a reduction might
      miss optimization opportunities (so we don't actually implement it), it
      simplifies thinking about correctness.

      We focus on dependency management. Specifically, we discuss how to
      correctly update InfoHeap and NameHeap, and calculate the set of unchanged
      files whose imports might resolve to different files. (With these results,
      the remaining part of rechecking is relatively simple.)

      Recall that InfoHeap maps file names in FS to module names in MS, where
      each file name in FS must exist, different file names may map to the same
      module name, and every module name in MS is mapped to by at least one file
      name; and NameHeap maps module names in MS to file names in FS, where the
      file name mapped to by a module name must map back to the same module name
      in InfoHeap. A file's imports might resolve to different files if the
      corresponding modules map to different files in NameHeap.

      Deleting a file
      ===============

      Suppose that a file D is deleted. Let D |-> m in InfoHeap, and m |-> F in
      NameHeap.

      Remove D |-> m from InfoHeap.

      If F = D, then remove m |-> F from NameHeap and mark m "dirty": any file
      importing m will be affected. If other files map to m in InfoHeap, map m
      to one of those files in NameHeap.

      Adding a file
      =============

      Suppose that a new file N is added.

      Map N to some module name, say m, in InfoHeap. If m is not mapped to any
      file in NameHeap, add m |-> N to NameHeap and mark m "dirty." Otherwise,
      decide whether to replace the existing mapping to m |-> N in NameHeap, and
      pessimistically assuming it might be, mark m "dirty."

      Adding a file
      =============

      What happens when a file C is changed? Suppose that C |-> m in InfoHeap,
      and m |-> F in NameHeap.

      Optimistically, C continues to map to m in InfoHeap and we do nothing.

      However, let's pessimistically assume that C maps to a different m' in
      InfoHeap. Considering C deleted and added back as new, we must remove C
      |-> m from InfoHeap and add C |-> m' to InfoHeap. If F = C, then remove m
      |-> F from NameHeap and mark m "dirty." If other files map to m in
      InfoHeap, map m to one of those files in NameHeap. If m' is not mapped to
      any file in NameHeap, add m' |-> C to NameHeap and mark m' "dirty."
      Otherwise, decide whether to replace the existing mapping to m' |-> C in
      NameHeap, and mark m' "dirty."

      Summary
      =======

      Summarizing, if an existing file F1 is changed or deleted, and F1 |-> m in
      InfoHeap and m |-> F in NameHeap, and F1 = F, then mark m "dirty." And if
      a new file or a changed file F2 now maps to m' in InfoHeap, mark m' "dirty."

      Ideally, any module name that does not map to a different file in NameHeap
      should not be considered "dirty."

      In terms of implementation:

      Deleted file
      ============

      Say it pointed to module OLD_M

      1. need to repick a provider for OLD_M *if OLD_M's current provider is this
      file*
      2. files that depend on OLD_M need to be rechecked if:
        a. the provider for OLD_M is **replaced** or **removed**; or
        b. the provider for OLD_M is **unchanged**, but is a _changed file_

      New file
      ========

      Say it points to module NEW_M

      1. need to repick a provider for NEW_M
      2. files that depend on NEW_M need to be rechecked if:
        a. the provider for NEW_M is **added** or **replaced**; or
        b. the provider for NEW_M is **unchanged**, but is a _changed file_

      Changed file
      ============

      Say it pointed to module OLD_M, now points to module NEW_M

      * Is OLD_M the same as NEW_M? *(= delete the file, then add it back)*

      1. need to repick providers for OLD_M *if OLD_M's current provider is this
      file*.
      2. files that depend on OLD_M need to be rechecked if:
        a. the provider for OLD_M is **replaced** or **removed**; or
        b. the provider for OLD_M is **unchanged**, but is a _changed file_
      3. need to repick a provider for NEW_M
      4. files that depend on NEW_M need to be rechecked if:
        a. the provider for NEW_M is **added** or **replaced**; or
        b. the provider for NEW_M is **unchanged**, but is a _changed file_

      * TODO: Is OLD_M the same as NEW_M?

      1. *don't repick a provider!*
      2. files that depend on OLD_M need to be rechecked if: OLD_M's current provider
      is a _changed file_

**)

  (* clear contexts and module registrations for new, changed, and deleted files *)
  (* remember old modules *)
  Context_cache.remove_batch new_or_changed_or_deleted;
  let unchanged_checked = FilenameSet.diff env.ServerEnv.checked_files new_or_changed_or_deleted in
  (* clear out records of files, and names of modules provided by those files *)
  let old_modules = Module_js.clear_files workers ~options new_or_changed_or_deleted in

  let dependent_file_count = ref 0 in

  let freshparsed_list = FilenameSet.elements freshparsed in
  let profiling, changed_modules, errors =
    commit_modules_and_resolve_requires
      ~options
      ~profiling
      ~workers
      ~old_modules
      ~parsed:freshparsed_list
      ~unparsed
      ~new_or_changed:(FilenameSet.elements new_or_changed)
      ~errors in

  (* direct_dependent_files are unchanged files which directly depend on changed modules,
     or are new / changed files that are phantom dependents. dependent_files are
     direct_dependent_files plus their dependents (transitive closure) *)
  let all_dependent_files, direct_dependent_files = Dep_service.dependent_files
    workers
    ~unchanged
    ~new_or_changed
    ~changed_modules
  in

  Hh_logger.info "Re-resolving directly dependent files";
  (** TODO [perf] Consider oldifying **)
  Module_js.remove_batch_resolved_requires direct_dependent_files;
  SharedMem_js.collect options `gentle;

  let node_modules_containers = !Files.node_modules_containers in
  (* requires in direct_dependent_files must be re-resolved before merging. *)
  MultiWorker.call workers
    ~job: (fun () files ->
      List.iter (fun f ->
        let require_loc = Parsing_service_js.get_requires_unsafe f in
        let errors = Module_js.add_parsed_resolved_requires ~audit:Expensive.ok ~options
          ~node_modules_containers f require_loc in
        ignore errors (* TODO: why, FFS, why? *)
      ) files
    )
    ~neutral: ()
    ~merge: (fun () () -> ())
    ~next:(MultiWorker.next workers (FilenameSet.elements direct_dependent_files));

  let parsed = FilenameSet.union freshparsed unchanged in

  (* recheck *)
  let profiling, checked, errors = typecheck
    ~options
    ~profiling
    ~workers
    ~errors
    ~unchanged_checked
    ~infer_input:freshparsed_list
    ~parsed:(FilenameSet.elements parsed)
    ~all_dependent_files
    ~persistent_connections:(Some env.ServerEnv.connections)
    ~make_merge_input:(fun inferred ->
      (* need to merge the closure of inferred files and their deps *)

      let n = FilenameSet.cardinal all_dependent_files in
      if n > 0
      then Hh_logger.info "remerge %d dependent files:" n;
      dependent_file_count := n;

      let _ = FilenameSet.fold (fun f i ->
        Hh_logger.info "%d/%d: %s" i n (string_of_filename f);
        i + 1
      ) all_dependent_files 1 in
      Hh_logger.info "Merge prep";

      (* merge errors for unchanged dependents will be cleared lazily *)

      (* to_merge is inferred files plus all dependents. prep for re-merge *)
      (* NOTE: Non-@flow files don't have entries in ResolvedRequiresHeap, so
         don't add then to the set of files to merge! Only inferred files (along
         with dependents) should be merged: see below. *)
      let inferred = FilenameSet.of_list inferred in
      let to_merge = FilenameSet.union all_dependent_files inferred in
      Context_cache.oldify_merge_batch to_merge;
      (** TODO [perf]: Consider `aggressive **)
      SharedMem_js.collect options `gentle;

      let to_merge = FilenameSet.elements to_merge in

      (* Definitely recheck inferred and direct_dependent_files. As merging proceeds, other
         files in to_merge may or may not be rechecked. *)
      let recheck_map = to_merge |> List.fold_left (
        let roots = FilenameSet.union direct_dependent_files inferred in
        fun recheck_map file ->
          FilenameMap.add file (FilenameSet.mem file roots) recheck_map
      ) FilenameMap.empty in

      Some (to_merge, recheck_map)
    )
  in

  FlowEventLogger.recheck
    (** TODO: update log to reflect current terminology **)
    ~modified_count:new_or_changed_count
    ~deleted_count
    ~dependent_file_count:!dependent_file_count
    ~profiling;

  (* NOTE: unused fields are left in their initial empty state *)
  { env with ServerEnv.
    files = parsed;
    checked_files = checked;
    errors;
  }

let files_to_infer ~workers ~focus_target parsed_list =
  match focus_target with
  | Some f ->
    if Module_js.is_tracked_file f (* otherwise, f is probably a directory *)
      && Module_js.checked_file ~audit:Expensive.warn f
    then
      (* Calculate the set of files to check. This set includes not only the
         files to be "rechecked", which is f and all its dependents, but also
         the dependencies of such files since they may not already be
         checked. *)
      let { Module_js._module; _ } = Module_js.get_info_unsafe ~audit:Expensive.warn f in
      let all_dependent_files, _ = Dep_service.dependent_files workers
        ~unchanged:(FilenameSet.(remove f (of_list parsed_list)))
        ~new_or_changed:(FilenameSet.singleton f)
        (* TODO: isn't it possible that _module is not provided by f? *)
        ~changed_modules:(Module_js.NameSet.singleton _module) in
      let dependency_graph = Dep_service.calc_dependency_graph workers parsed_list in
      let roots = FilenameSet.add f all_dependent_files in
      let to_infer = Dep_service.calc_all_dependencies dependency_graph roots in
      FilenameSet.elements to_infer
    else (* terminate *)
      []
  | _ -> parsed_list

(* creates a closure that lists all files in the given root, returned in chunks *)
let make_next_files ~libs ~file_options root =
  let make_next_raw =
    Files.make_next_files ~root ~all:false ~subdir:None ~options:file_options ~libs in
  fun () ->
    make_next_raw ()
    |> List.map (Files.filename_from_string ~options:file_options)
    |> Bucket.of_list

let init ~profiling ~workers options =
  let file_options = Options.file_options options in
  let ordered_libs, libs = Files.init file_options in
  let next_files = make_next_files ~libs ~file_options (Options.root options) in

  Hh_logger.info "Parsing";
  let profiling, (parsed, unparsed, local_errors) =
    parse ~options ~profiling ~workers next_files in

  Hh_logger.info "Building package heap";
  let profiling = init_package_heap ~options ~profiling parsed in

  Hh_logger.info "Loading libraries";
  let profiling, (libs_ok, local_errors, suppressions, lint_settings) =
    let suppressions = FilenameMap.empty in
    let lint_settings = FilenameMap.empty in
    init_libs ~options ~profiling ~local_errors ~suppressions ~lint_settings ordered_libs in

  Hh_logger.info "Resolving dependencies";
  let profiling, _, errors =
    let parsed_list = FilenameSet.elements parsed in
    let errors = { ServerEnv.
      local_errors;
      merge_errors = FilenameMap.empty;
      suppressions;
      lint_settings;
    } in
    let all_files = List.fold_left (fun acc (filename, _) ->
      filename::acc
    ) parsed_list unparsed in
    commit_modules_and_resolve_requires
      ~options
      ~profiling
      ~workers
      ~old_modules:[]
      ~parsed:parsed_list
      ~unparsed
      ~new_or_changed:all_files
      ~errors
  in

  profiling, parsed, libs, libs_ok, errors

let full_check ~profiling ~options ~workers ~focus_target ~should_merge parsed errors =
  let infer_input = files_to_infer ~workers ~focus_target parsed in
  typecheck
    ~options
    ~profiling
    ~workers
    ~errors
    ~unchanged_checked:FilenameSet.empty
    ~infer_input
    ~parsed
    ~all_dependent_files:FilenameSet.empty
    ~persistent_connections:None
    ~make_merge_input:(fun inferred ->
      if not should_merge then None
      else Some (inferred,
        inferred |> List.fold_left (fun map f ->
          FilenameMap.add f true map
        ) FilenameMap.empty)
    )
