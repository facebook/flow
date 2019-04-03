(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module drives the type checker *)

open Utils_js

(****************** typecheck job helpers *********************)

let clear_errors (files: FilenameSet.t) errors =
  FilenameSet.fold
    (fun file { ServerEnv.local_errors; merge_errors; warnings; suppressions; } ->
      Hh_logger.debug "clear errors %s" (File_key.to_string file);
      { ServerEnv.
        local_errors = FilenameMap.remove file local_errors;
        merge_errors = FilenameMap.remove file merge_errors;
        warnings = FilenameMap.remove file warnings;
        suppressions = Error_suppressions.remove file suppressions;
      }
    ) files errors

let update_errset map file errset =
  if Flow_error.ErrorSet.is_empty errset then map
  else
    let errset = match FilenameMap.get file map with
    | Some prev_errset ->
      Flow_error.ErrorSet.union prev_errset errset
    | None -> errset
    in
    FilenameMap.add file errset map

let merge_error_maps = FilenameMap.union ~combine:(fun _ x y -> Some (Flow_error.ErrorSet.union x y))

(* We just want to replace the old coverage with the new one *)
let update_coverage = FilenameMap.union ~combine:(fun _ _ -> Option.return)

(* Filter out duplicate provider error, if any, for the given file. *)
let filter_duplicate_provider map file =
  match FilenameMap.get file map with
  | Some prev_errset ->
    let new_errset = Flow_error.ErrorSet.filter (fun err ->
      not (Flow_error.kind_of_error err = Errors.DuplicateProviderError)
    ) prev_errset in
    FilenameMap.add file new_errset map
  | None -> map

let with_timer_lwt ?options timer profiling f =
  let should_print = Option.value_map options ~default:false ~f:(Options.should_profile) in
  Profiling_js.with_timer_lwt ~should_print ~timer ~f profiling

let collate_parse_results ~options parse_results =
  let { Parsing_service_js.
    parse_ok; parse_skips; parse_hash_mismatch_skips; parse_fails; parse_unchanged
  } = parse_results in
  (* No one who is calling collate_parse_results is skipping files with hash mismatches *)
  assert (FilenameSet.is_empty parse_hash_mismatch_skips);
  let local_errors = List.fold_left (fun errors (file, _, fail) ->
    let errset = match fail with
    | Parsing_service_js.Parse_error err ->
      Inference_utils.set_of_parse_error ~source_file:file err
    | Parsing_service_js.Docblock_errors errs ->
      Inference_utils.set_of_docblock_errors ~source_file:file errs
    | Parsing_service_js.File_sig_error err ->
      Inference_utils.set_of_file_sig_error ~source_file:file err
    in
    update_errset errors file errset
  ) FilenameMap.empty parse_fails in

  let local_errors =
    (* In practice, the only `tolerable_errors` are related to well formed exports. If this flag
     * were not temporary in nature, it would be worth adding some complexity to avoid conflating
     * them. *)
    Inference_utils.fold_whitelisted_well_formed_exports ~f:(
      fun file file_sig_errors errors ->
        let file_sig_errors = File_sig.abstractify_tolerable_errors file_sig_errors in
        let errset = Inference_utils.set_of_file_sig_tolerable_errors
          ~source_file:file file_sig_errors in
        update_errset errors file errset
    ) options parse_ok local_errors
  in

  let unparsed = List.fold_left (fun unparsed (file, info, _) ->
    (file, info) :: unparsed
  ) parse_skips parse_fails in

  let parse_ok = parse_ok |> FilenameMap.keys |> FilenameSet.of_list in

  parse_ok, unparsed, parse_unchanged, local_errors

let parse ~options ~profiling ~workers ~reader parse_next =
  with_timer_lwt ~options "Parsing" profiling (fun () ->
    let%lwt results = Parsing_service_js.parse_with_defaults ~reader options workers parse_next in
    Lwt.return (collate_parse_results ~options results)
  )

let reparse ~options ~profiling ~transaction ~reader ~workers ~modified ~deleted =
  with_timer_lwt ~options "Parsing" profiling (fun () ->
    let%lwt new_or_changed, results =
      Parsing_service_js.reparse_with_defaults
        ~transaction ~reader ~with_progress:true ~workers ~modified ~deleted options
    in
    let parse_ok, unparsed, unchanged, local_errors = collate_parse_results ~options results in
    Lwt.return (new_or_changed, parse_ok, unparsed, unchanged, local_errors)
  )

let parse_contents ~options ~profiling ~check_syntax filename contents =
  with_timer_lwt ~options "Parsing" profiling (fun () ->
    (* always enable types when checking an individual file *)
    let types_mode = Parsing_service_js.TypesAllowed in
    let use_strict = Options.modules_are_use_strict options in
    let max_tokens = Options.max_header_tokens options in
    let module_ref_prefix = Options.haste_module_ref_prefix options in
    let facebook_fbt = Options.facebook_fbt options in
    let arch = options.Options.opt_arch in

    let docblock_errors, info =
      Parsing_service_js.parse_docblock ~max_tokens filename contents in
    let errors = Inference_utils.set_of_docblock_errors ~source_file:filename docblock_errors in
    let parse_result = Parsing_service_js.do_parse
      ~fail:check_syntax ~types_mode ~use_strict ~info ~module_ref_prefix
      ~facebook_fbt ~arch contents filename
    in
    Lwt.return (errors, parse_result, info)
  )

(* commit providers for old and new modules, collect errors. *)
let commit_modules, commit_modules_from_saved_state =
  let commit_modules_generic ~introduce_files ~transaction ~reader ~all_providers_mutator ~options
      ~is_init ~profiling ~workers ~parsed ~parsed_set ~unparsed ~unparsed_set ~old_modules ~deleted
      ~local_errors ~new_or_changed =
    (* conservatively approximate set of modules whose providers will change *)
    (* register providers for modules, warn on dupes etc. *)
      with_timer_lwt ~options "CommitModules" profiling (fun () ->
        let all_files_set = FilenameSet.union (FilenameSet.union parsed_set unparsed_set) deleted in
        let mutator = Module_heaps.Introduce_files_mutator.create transaction all_files_set in

        let%lwt new_modules =
          introduce_files
            ~mutator ~all_providers_mutator ~workers ~options ~parsed ~unparsed
        in
        let dirty_modules = List.rev_append old_modules new_modules in
        let%lwt providers, changed_modules, errmap =
          Module_js.commit_modules
            ~transaction ~workers ~options ~reader ~is_init new_or_changed dirty_modules
        in
    (* Providers might be new but not changed. This typically happens when old
      providers are deleted, and previously duplicate providers become new
      providers. In such cases, we must clear the old duplicate provider errors
      for the new providers.

      (Note that this is unncessary when the providers are changed, because in
      that case they are rechecked and *all* their errors are cleared. But we
      don't care about optimizing that case for now.) *)
        let errors = List.fold_left filter_duplicate_provider local_errors providers in
        Lwt.return (
          changed_modules, FilenameMap.fold (fun file errors acc ->
            let errset = List.fold_left (fun acc err ->
              match err with
              | Module_js.ModuleDuplicateProviderError { module_name; provider; conflict; } ->
                let error = Error_message.(EDuplicateModuleProvider { module_name; provider; conflict })
                |> Flow_error.error_of_msg ~trace_reasons:[] ~source_file:file in
                Flow_error.ErrorSet.add error acc
            ) Flow_error.ErrorSet.empty errors in
            update_errset acc file errset
          ) errmap errors
      )
    )
  in
  let commit_modules ~transaction ~reader =
    commit_modules_generic ~introduce_files:(Module_js.introduce_files ~reader) ~transaction ~reader
  in
  let commit_modules_from_saved_state ~transaction ~reader =
    commit_modules_generic
      ~introduce_files:Module_js.introduce_files_from_saved_state ~transaction ~reader
  in
  commit_modules, commit_modules_from_saved_state

let resolve_requires ~transaction ~reader ~options ~profiling ~workers ~parsed ~parsed_set =
  let node_modules_containers = !Files.node_modules_containers in
  let mutator = Module_heaps.Resolved_requires_mutator.create transaction parsed_set in
  with_timer_lwt ~options "ResolveRequires" profiling (fun () ->
    MultiWorkerLwt.call workers
      ~job: (List.fold_left (fun errors_acc filename ->
        let errors = Module_js.add_parsed_resolved_requires filename
          ~mutator ~reader ~options ~node_modules_containers in
        if Flow_error.ErrorSet.is_empty errors
        then errors_acc
        else FilenameMap.add filename errors errors_acc
      )
      )
      ~neutral: FilenameMap.empty
      ~merge: FilenameMap.union
      ~next:(MultiWorkerLwt.next workers parsed)
  )

let commit_modules_and_resolve_requires
  ~transaction
  ~reader
  ~all_providers_mutator
  ~options
  ~profiling
  ~workers
  ~old_modules
  ~parsed_set
  ~unparsed
  ~unparsed_set
  ~new_or_changed
  ~deleted
  ~errors
  ~is_init =
  (* TODO remove after lookup overhaul *)
  Module_js.clear_filename_cache ();

  let { ServerEnv.local_errors; merge_errors; warnings; suppressions } = errors in

  let parsed = FilenameSet.elements parsed_set in

  let%lwt changed_modules, local_errors = commit_modules
    ~transaction ~reader ~all_providers_mutator ~options ~is_init ~profiling ~workers ~parsed
    ~parsed_set ~unparsed ~unparsed_set ~old_modules ~deleted ~local_errors ~new_or_changed
  in

  let%lwt resolve_errors =
    resolve_requires ~transaction ~reader ~options ~profiling ~workers ~parsed ~parsed_set
  in
  let local_errors = FilenameMap.union resolve_errors local_errors in

  Lwt.return (
    changed_modules, { ServerEnv.local_errors; merge_errors; warnings; suppressions }
  )

let error_set_of_merge_error file =
  Flow_error.error_of_msg ~trace_reasons:[] ~source_file:file
  %> Flow_error.ErrorSet.singleton


let calc_deps ~options ~profiling ~dependency_graph ~components to_merge =
  with_timer_lwt ~options "CalcDeps" profiling (fun () ->
    let dependency_graph = Dep_service.filter_dependency_graph dependency_graph to_merge in
    let components = List.filter (Nel.exists (fun f -> FilenameSet.mem f to_merge)) components in
    if Options.should_profile options then Sort_js.log components;
    let component_map = List.fold_left (fun component_map component ->
      let file = Nel.hd component in
      FilenameMap.add file component component_map
    ) FilenameMap.empty components in
    Lwt.return (dependency_graph, component_map)
  )

(* The infer_input passed in basically tells us what the caller wants to typecheck.
 * However, due to laziness, it's possible that certain dependents or dependencies have not been
 * checked yet. So we need to calculate all the transitive dependents and transitive dependencies
 * and add them to infer_input, unless they're already checked and in unchanged_checked
 *
 * Note that we do not want to add all_dependent_files to infer_input directly! We only want to
 * pass the dependencies, and later add dependent files as needed. This is important for recheck
 * optimizations. We create the recheck map which indicates whether a given file needs to be
 * rechecked. Dependent files only need to be rechecked if their dependencies change.
 *)
let include_dependencies_and_dependents
    ~options
    ~profiling
    ~unchanged_checked
    ~infer_input
    ~dependency_info
    ~all_dependent_files
    ~direct_dependent_files =
  let%lwt infer_input, components = with_timer_lwt ~options "PruneDeps" profiling (fun () ->
    (* Don't just look up the dependencies of the focused or dependent modules. Also look up
     * the dependencies of dependencies, since we need to check transitive dependencies *)
    let preliminary_to_merge = CheckedSet.all
      (CheckedSet.add ~dependents:all_dependent_files infer_input) in
    let preliminary_to_merge = Dep_service.calc_direct_dependencies dependency_info
      preliminary_to_merge in
    (* So we want to prune our dependencies to only the dependencies which changed. However,
     * two dependencies A and B might be in a cycle. If A changed and B did not, we still need to
     * check both of them. So we need to calculate components before we can prune *)
    (* Grab the subgraph containing all our dependencies and sort it into the strongly connected
     * cycles *)
    let dependency_graph = Dependency_info.dependency_graph dependency_info in
    let components = Sort_js.topsort ~roots:preliminary_to_merge dependency_graph in
    let dependencies = List.fold_left (fun dependencies component ->
      if Nel.exists (fun fn -> not (CheckedSet.mem fn unchanged_checked)) component
      (* If at least one member of the component is not unchanged, then keep the component *)
      then Nel.fold_left (fun acc fn -> FilenameSet.add fn acc) dependencies component
      (* If every element is unchanged, drop the component *)
      else dependencies
    ) FilenameSet.empty components in
    Lwt.return (CheckedSet.add ~dependencies infer_input, components)
  ) in

  (* NOTE: An important invariant here is that if we recompute Sort_js.topsort with infer_input +
     all_dependent_files (which is = to_merge later) on dependency_graph, we would get exactly the
     same components. Later, we will filter dependency_graph to just to_merge, and correspondingly
     filter components as well. This will work out because every component is either entirely inside
     to_merge or entirely outside. *)

  let to_merge = CheckedSet.add ~dependents:all_dependent_files infer_input in

  let recheck_map =
    let roots = CheckedSet.add ~dependents:direct_dependent_files infer_input in
    (* Definitely recheck inferred and direct_dependent_files. As merging proceeds, other
       files in to_merge may or may not be rechecked. *)
    CheckedSet.fold (fun recheck_map file ->
      FilenameMap.add file (CheckedSet.mem file roots) recheck_map
    ) FilenameMap.empty to_merge
  in

  Lwt.return (to_merge, components, recheck_map)

let run_merge_service
    ~master_mutator
    ~worker_mutator
    ~reader
    ~intermediate_result_callback
    ~options
    ~profiling
    ~workers
    dependency_graph
    component_map
    recheck_map
    acc
    =
  with_timer_lwt ~options "Merge" profiling (fun () ->
    let%lwt merged, skipped_count = Merge_service.merge_strict
      ~master_mutator ~worker_mutator ~reader ~intermediate_result_callback ~options ~workers
      dependency_graph component_map recheck_map
    in
    let errs, warnings, suppressions, coverage = List.fold_left (fun acc (file, result) ->
      let component = FilenameMap.find_unsafe file component_map in
      (* remove all errors, suppressions for rechecked component *)
      let errors, warnings, suppressions, coverage =
        Nel.fold_left (fun (errors, warnings, suppressions, coverage) file ->
          FilenameMap.remove file errors,
          FilenameMap.remove file warnings,
          Error_suppressions.remove file suppressions,
          FilenameMap.remove file coverage
        ) acc component
      in
      match result with
      | Ok (new_errors, new_warnings, new_suppressions, new_coverage) ->
        update_errset errors file new_errors,
        update_errset warnings file new_warnings,
        Error_suppressions.update_suppressions suppressions new_suppressions,
        update_coverage coverage new_coverage
      | Error msg ->
        let new_errors = error_set_of_merge_error file msg in
        update_errset errors file new_errors, warnings, suppressions, coverage
    ) acc merged
    in
    Lwt.return (errs, warnings, suppressions, coverage, skipped_count)
  )

(* This function does some last minute preparation and then calls into the merge service, which
 * typechecks the code. By the time this function is called, we know exactly what we want to merge
 * (though we may later decline to typecheck some files due to recheck optimizations) *)
let merge
  ~transaction
  ~reader
  ~options
  ~profiling
  ~workers
  ~errors
  ~unchanged_checked
  ~to_merge
  ~components
  ~recheck_map
  ~dependency_graph
  ~deleted
  ~persistent_connections
  ~prep_merge =
  let { ServerEnv.local_errors; merge_errors; warnings; suppressions; } = errors in

  let%lwt send_errors_over_connection =
    match persistent_connections with
    | None -> Lwt.return (fun _ -> ())
    | Some clients -> with_timer_lwt ~options "MakeSendErrors" profiling (fun () ->
      (* Each merge step uncovers new errors, warnings, suppressions.

         While more suppressions may come in later steps, the suppressions we've seen so far are
         sufficient to filter the errors and warnings we've seen so far.

         Intuitively, we will not see an error (or warning) before we've seen all the files involved
         in that error, and thus all the suppressions which could possibly suppress the error. *)
      let open Errors in
      let curr_errors = ref ConcreteLocPrintableErrorSet.empty in
      let curr_warnings = ref ConcreteLocPrintableErrorSet.empty in
      let curr_suppressions = ref suppressions in
      let root = Options.root options in
      let file_options = Some (Options.file_options options) in
      let filter = Error_suppressions.filter_suppressed_errors ~root ~file_options in
      Lwt.return (function lazy results ->
        let new_errors, new_warnings, suppressions =
          List.fold_left (fun (errs_acc, warns_acc, supps_acc) result ->
            let file, old_errs, old_warns, supps = result in
            let supps_acc = Error_suppressions.union supps_acc supps in
            (* Filter errors and warnings based on suppressions we've seen so far. *)
            let errs, _, _ = filter supps_acc old_errs
              ~unused:Error_suppressions.empty (* TODO: track unused suppressions *)
            in
            (* Filter errors and warnings based on suppressions we've seen so far. *)
            let warns, _, _ = filter supps_acc old_warns
              ~unused:Error_suppressions.empty (* TODO: track unused suppressions *)
            in
            (* Only add errors we haven't seen before. *)
            let errs_acc = ConcreteLocPrintableErrorSet.fold (fun err acc ->
              if ConcreteLocPrintableErrorSet.mem err !curr_errors
              then acc
              else ConcreteLocPrintableErrorSet.add err acc
            ) errs errs_acc in
            (* Only add warnings we haven't seen before. Note that new warnings are stored by
               filename, because the clients only receive warnings for files they have open. *)
            let warns_acc =
              let acc = Option.value (FilenameMap.get file warns_acc) ~default:ConcreteLocPrintableErrorSet.empty in
              let acc = ConcreteLocPrintableErrorSet.fold (fun warn acc ->
                if ConcreteLocPrintableErrorSet.mem warn !curr_warnings
                then acc
                else ConcreteLocPrintableErrorSet.add warn acc
              ) warns acc in
              if ConcreteLocPrintableErrorSet.is_empty acc then warns_acc else FilenameMap.add file acc warns_acc
            in
            errs_acc, warns_acc, supps_acc
          ) (ConcreteLocPrintableErrorSet.empty, FilenameMap.empty, !curr_suppressions) results
        in

        curr_errors := ConcreteLocPrintableErrorSet.union new_errors !curr_errors;
        curr_warnings := FilenameMap.fold (fun _ -> ConcreteLocPrintableErrorSet.union) new_warnings !curr_warnings;
        curr_suppressions := suppressions;

        if not (ConcreteLocPrintableErrorSet.is_empty new_errors && FilenameMap.is_empty new_warnings)
        then Persistent_connection.update_clients
          ~clients
          ~calc_errors_and_warnings:(fun () -> new_errors, new_warnings)
      ))
  in

  let%lwt () = match prep_merge with
    | None -> Lwt.return_unit
    | Some callback ->
      (* call supplied function to calculate closure of modules to merge *)
      with_timer_lwt ~options "MakeMergeInput" profiling (fun () ->
        Lwt.return (callback to_merge)
      )
  in

  (* to_merge is the union of inferred (newly inferred files) and the
     transitive closure of all dependents.

     recheck_map maps each file in to_merge to whether it should be rechecked
     initially.
  *)
  Hh_logger.info "to_merge: %s" (CheckedSet.debug_counts_to_string to_merge);
  Hh_logger.info "unchanged_checked: %s" (CheckedSet.debug_counts_to_string unchanged_checked);
  Hh_logger.info "Calculating dependencies";
  MonitorRPC.status_update ~event:ServerStatus.Calculating_dependencies_progress;
  let files_to_merge = CheckedSet.all to_merge in
  let%lwt dependency_graph, component_map =
    calc_deps ~options ~profiling ~dependency_graph ~components files_to_merge in

  Hh_logger.info "Merging";
  let%lwt merge_errors, warnings, suppressions, coverage, skipped_count =
    let intermediate_result_callback results =
      let errors = lazy (
        Core_list.map ~f:(fun (file, result) ->
          match result with
          | Ok (errors, warnings, suppressions, _) ->
            let errors = Flow_error.make_errors_printable errors in
            let warnings = Flow_error.make_errors_printable warnings in
            file, errors, warnings, suppressions
          | Error msg ->
            let errors = error_set_of_merge_error file msg in
            let errors = Flow_error.make_errors_printable errors in
            let suppressions = Error_suppressions.empty in
            let warnings = Errors.ConcreteLocPrintableErrorSet.empty in
            file, errors, warnings, suppressions
        ) (Lazy.force results)
      ) in
      send_errors_over_connection errors
    in

    let master_mutator, worker_mutator =
      Context_heaps.Merge_context_mutator.create
        transaction (FilenameSet.union files_to_merge deleted)
    in

    let merge_start_time = Unix.gettimeofday () in

    let%lwt merge_errors, warnings, suppressions, coverage, skipped_count =
      run_merge_service
        ~master_mutator
        ~worker_mutator
        ~reader
        ~intermediate_result_callback
        ~options
        ~profiling
        ~workers
        dependency_graph
        component_map
        recheck_map
        (merge_errors, warnings, suppressions, FilenameMap.empty)
    in
    let%lwt () =
      if Options.should_profile options
      then with_timer_lwt ~options "PrintGCStats" profiling (fun () ->
        Lwt.return (Gc.print_stat stderr)
      )
      else Lwt.return_unit
    in

    let%lwt () = Recheck_stats.record_merge_time
      ~options
      ~total_time:(Unix.gettimeofday () -. merge_start_time)
      ~merged_files:(CheckedSet.cardinal to_merge);
    in

    Hh_logger.info "Done";
    Lwt.return (merge_errors, warnings, suppressions, coverage, skipped_count)
  in

  let checked = CheckedSet.union unchanged_checked to_merge in
  Hh_logger.info "Checked set: %s" (CheckedSet.debug_counts_to_string checked);

  let errors = { ServerEnv.local_errors; merge_errors; warnings; suppressions } in
  let cycle_leaders = component_map
    |> Utils_js.FilenameMap.elements
    |> Core_list.map ~f:(fun (leader, members) -> (leader, Nel.length members))
    |> Core_list.filter ~f:(fun (_, member_count) -> member_count > 1) in
  Lwt.return (checked, cycle_leaders, errors, coverage, skipped_count)

let check_files ~options ~reader ~workers errors coverage checked_files =
  let files = FilenameSet.union (CheckedSet.focused checked_files) (CheckedSet.dependents checked_files) in
  match options.Options.opt_arch with
  | Options.Classic -> Lwt.return (errors, coverage)
  | Options.TypesFirst ->
    let job = List.fold_left (fun (errors, warnings, suppressions, coverage) file ->
      let new_errors, new_warnings, new_suppressions, new_coverage =
        Merge_service.check_file options ~reader file in
      update_errset errors file new_errors,
      update_errset warnings file new_warnings,
      Error_suppressions.update_suppressions suppressions new_suppressions,
      update_coverage coverage new_coverage
    ) in
    let neutral =
      FilenameMap.empty,
      FilenameMap.empty,
      Error_suppressions.empty,
      FilenameMap.empty in
    let merge
        (errors, warnings, suppressions, coverage)
        (new_errors, new_warnings, new_suppressions, new_coverage) =
      FilenameMap.union errors new_errors,
      FilenameMap.union warnings new_warnings,
      Error_suppressions.update_suppressions suppressions new_suppressions,
      FilenameMap.union coverage new_coverage
      in

    Hh_logger.info "Checking files";
    let progress_fn ~total ~start ~length:_ =
      MonitorRPC.status_update
        ServerStatus.(Checking_progress { total = Some total; finished = start })
    in
    let%lwt merge_errors, warnings, suppressions, coverage = MultiWorkerLwt.call
        workers
        ~job
        ~neutral
        ~merge
        ~next:(MultiWorkerLwt.next ~progress_fn ~max_size:100 workers
                 (FilenameSet.elements files))
    in
    Hh_logger.info "Done";
    let errors = { errors with
      ServerEnv.merge_errors;
      warnings;
      suppressions;
    } in
    Lwt.return (errors, coverage)

let ensure_parsed ~options ~profiling ~workers ~reader files =
  with_timer_lwt ~options "EnsureParsed" profiling (fun () ->
    let%lwt parse_hash_mismatch_skips =
      Parsing_service_js.ensure_parsed ~reader options workers (CheckedSet.all files)
    in

    if FilenameSet.is_empty parse_hash_mismatch_skips
    then Lwt.return_unit
    else begin
      let files_to_recheck = FilenameSet.fold
        (fun f acc -> SSet.add (File_key.to_string f) acc)
        parse_hash_mismatch_skips
        SSet.empty
      in
      ServerMonitorListenerState.push_files_to_recheck files_to_recheck;
      raise Lwt.Canceled
    end
  )

(* When checking contents, ensure that dependencies are checked. Might have more
   general utility.
   TODO(ljw) CARE! This function calls "typecheck" which may emit errors over the
   persistent connection. It's reasonable that anything which checks new files
   should be able to emit errors, even places like propertyFindRefs.get_def_info
   that invoke this function. But it looks like this codepath fails to emit
   StartRecheck and EndRecheck messages. *)
let ensure_checked_dependencies ~options ~reader ~env file file_sig =
  let resolved_requires =
    let require_loc_map = File_sig.With_Loc.(require_loc_map file_sig.module_sig) in
    SMap.fold (fun r locs resolved_rs ->
      let locs = Nel.map ALoc.of_loc locs in
      let resolved_r = Module_js.imported_module
        ~options
        ~reader:(Abstract_state_reader.State_reader reader)
        ~node_modules_containers:!Files.node_modules_containers
        file locs r in
      Modulename.Set.add resolved_r resolved_rs
    ) require_loc_map Modulename.Set.empty
  in

  let infer_input = Modulename.Set.fold (fun m acc ->
    match Module_heaps.Reader.get_file ~reader m ~audit:Expensive.warn with
    | Some f ->
      let reader = Abstract_state_reader.State_reader reader in
      if FilenameSet.mem f env.ServerEnv.files &&
        Module_js.checked_file ~reader f ~audit:Expensive.warn
      then CheckedSet.add ~dependencies:(FilenameSet.singleton f) acc
      else acc
    | None -> acc (* complain elsewhere about required module not found *)
  ) resolved_requires CheckedSet.empty in
  let unchanged_checked = env.ServerEnv.checked_files in

  (* Often, all dependencies have already been checked, so infer_input contains no unchecked files.
   * In that case, let's short-circuit typecheck, since a no-op typecheck still takes time on
   * large repos *)
  let unchecked_dependencies = CheckedSet.diff infer_input unchanged_checked in
  if CheckedSet.is_empty unchecked_dependencies
  then Lwt.return_unit
  else begin
    Hh_logger.info "Canceling command due to %d unchecked dependencies"
      (CheckedSet.cardinal unchecked_dependencies);
    ServerMonitorListenerState.push_checked_set_to_force unchecked_dependencies;
    raise Lwt.Canceled
  end

(* Another special case, similar assumptions as above. *)
(** TODO: handle case when file+contents don't agree with file system state **)
let typecheck_contents_ ~options ~env ~check_syntax ~profiling contents filename =
  let%lwt errors, parse_result, info =
    parse_contents ~options ~profiling ~check_syntax filename contents in

  let reader = State_reader.create () in

  match parse_result with
  | Parsing_service_js.Parse_ok parse_ok ->
      (* override docblock info *)
      let ast, file_sig = Parsing_service_js.basic parse_ok in
      let info = Docblock.set_flow_mode_for_ide_command info in

      (* merge *)
      let%lwt cx, typed_ast = with_timer_lwt ~options "MergeContents" profiling (fun () ->
        let%lwt () =
          ensure_checked_dependencies ~options ~reader ~env filename file_sig
        in
        Lwt.return (Merge_service.merge_contents_context ~reader options filename ast info file_sig)
      ) in

      let errors = Context.errors cx in

      let errors =
        if Inference_utils.well_formed_exports_enabled options filename then
          File_sig.With_Loc.(file_sig.tolerable_errors)
          |> File_sig.abstractify_tolerable_errors
          |> Inference_utils.set_of_file_sig_tolerable_errors ~source_file:filename
          |> Flow_error.ErrorSet.union errors
        else
          errors
      in

      (* Suppressions for errors in this file can come from dependencies *)
      let suppressions =
        let open ServerEnv in
        let new_suppressions = Context.error_suppressions cx in
        let { suppressions; _ } = env.errors in
        Error_suppressions.update_suppressions suppressions new_suppressions
      in

      let severity_cover = Context.severity_cover cx in
      let include_suppressions = Context.include_suppressions cx in

      let errors, warnings, suppressions =
        Error_suppressions.filter_lints ~include_suppressions suppressions errors severity_cover in

      let errors = Flow_error.make_errors_printable errors in
      let warnings = Flow_error.make_errors_printable warnings in

      let root = Options.root options in
      let file_options = Some (Options.file_options options) in

      (* Filter out suppressed errors *)
      let errors, _, _ =
        Error_suppressions.filter_suppressed_errors ~root ~file_options suppressions errors
          ~unused:Error_suppressions.empty (* TODO: track unused suppressions *)
      in

      (* Filter out suppressed warnings *)
      let warnings, _, _ =
        Error_suppressions.filter_suppressed_errors ~root ~file_options suppressions warnings
          ~unused:Error_suppressions.empty (* TODO: track unused suppressions *)
      in

      let warnings = if Options.should_include_warnings options
        then warnings
        else Errors.ConcreteLocPrintableErrorSet.empty
      in

      Lwt.return (Some (cx, ast, file_sig, typed_ast), errors, warnings, info)

  | Parsing_service_js.Parse_fail fails ->
      let errors = match fails with
      | Parsing_service_js.Parse_error err ->
          let err = Inference_utils.error_of_parse_error ~source_file:filename err in
          Flow_error.ErrorSet.add err errors
      | Parsing_service_js.Docblock_errors errs ->
          List.fold_left (fun errors err ->
            let err = Inference_utils.error_of_docblock_error ~source_file:filename err in
            Flow_error.ErrorSet.add err errors
          ) errors errs
      | Parsing_service_js.File_sig_error err ->
          let err = Inference_utils.error_of_file_sig_error ~source_file:filename err in
          Flow_error.ErrorSet.add err errors
      in
      let errors = Flow_error.make_errors_printable errors in
      Lwt.return (None, errors, Errors.ConcreteLocPrintableErrorSet.empty, info)

  | Parsing_service_js.Parse_skip
     (Parsing_service_js.Skip_non_flow_file
    | Parsing_service_js.Skip_resource_file) ->
      (* should never happen *)
      let errors = Flow_error.make_errors_printable errors in
      Lwt.return (None, errors, Errors.ConcreteLocPrintableErrorSet.empty, info)

let typecheck_contents ~options ~env ~profiling contents filename =
  let%lwt cx_opt, errors, warnings, _info =
    typecheck_contents_ ~options ~env ~check_syntax:true ~profiling contents filename in
  Lwt.return (cx_opt, errors, warnings)

let basic_check_contents ~options ~env ~profiling contents filename =
  try%lwt
    let%lwt cx_opt, _errors, _warnings, info =
      typecheck_contents_
        ~options ~env ~check_syntax:false ~profiling contents filename in
    let cx, file_sig, typed_ast = match cx_opt with
      | Some (cx, _, file_sig, typed_ast) -> cx, file_sig, typed_ast
      | None -> failwith "Couldn't parse file" in
    Lwt.return (Ok (cx, info, file_sig, typed_ast))
  with
  | Lwt.Canceled as exn -> raise exn
  | exn ->
    let exn = Exception.wrap exn in
    let e = Exception.to_string exn in
    Hh_logger.error "Uncaught exception in basic_check_contents\n%s" e;
    Lwt.return (Error e)

let init_package_heap ~options ~profiling ~reader parsed =
  with_timer_lwt ~options "PackageHeap" profiling (fun () ->
    let errors = FilenameSet.fold (fun filename errors ->
      match filename with
      | File_key.JsonFile str when Filename.basename str = "package.json" ->
        let ast = Parsing_heaps.Mutator_reader.get_ast_unsafe ~reader filename in
        let package = Package_json.parse ast in
        Module_js.add_package str package;
        begin match package with
        | Ok _ ->
          errors
        | Error parse_err ->
          let errset = Inference_utils.set_of_package_json_error ~source_file:filename parse_err in
          update_errset errors filename errset
        end
      | _ ->
        errors
    ) parsed FilenameMap.empty in
    Lwt.return errors
  )

let init_libs
    ~options ~profiling ~local_errors ~warnings ~suppressions ~reader ordered_libs =
  with_timer_lwt ~options "InitLibs" profiling (fun () ->
    let%lwt lib_files =
      let options = match Options.verbose options with
        | Some { Verbose.enabled_during_flowlib = false; _; } ->
          (* Normally we disable verbosity while loading the libs. But if we're running with
           * --verbose-flowlib then we want to leave verbosity on *)
          { options with Options.opt_verbose = None; }
        | _ -> options
      in
      Init_js.init ~options ~reader ordered_libs
    in

    Lwt.return @@ List.fold_left (fun acc (lib_file, ok, errs, warnings, suppressions) ->
      let all_ok, errors_acc, warnings_acc, suppressions_acc = acc in
      let all_ok = if ok then all_ok else false in
      let errors_acc = update_errset errors_acc lib_file errs in
      let warnings_acc = update_errset warnings_acc lib_file warnings in
      let suppressions_acc =
        Error_suppressions.update_suppressions suppressions_acc suppressions in
      all_ok, errors_acc, warnings_acc, suppressions_acc
    ) (true, local_errors, warnings, suppressions) lib_files
  )

(* Given a CheckedSet of focused files and a dependency graph, calculate all the dependents and
 * dependencies and return them as a CheckedSet
 *
 * This is pretty darn expensive for large repos (on the order of a few seconds). What is taking
 * all that time?
 *
 * - Around 75% of the time is dependent_files looking up the dependents
 * - Around 20% of the time is calc_dependency_graph building the dependency graph
 *
 * There are no expected invariants for the input sets. The returned set has the following invariants
 * 1. Every recursive dependent of a focused file will be in the focused set or the dependent set
 **)
let focused_files_to_infer ~reader ~input_focused ~input_dependencies ~dependency_info =
  let input = CheckedSet.add
    ~focused:input_focused
    ~dependencies:(Option.value ~default:FilenameSet.empty input_dependencies)
    CheckedSet.empty in
  (* Filter unchecked files out of the input *)
  let input = CheckedSet.filter input ~f:(fun f ->
    Module_heaps.Mutator_reader.is_tracked_file ~reader f (* otherwise, f is probably a directory *)
    && Module_js.checked_file
      ~reader:(Abstract_state_reader.Mutator_state_reader reader)
      ~audit:Expensive.warn
      f
  ) in

  let focused = CheckedSet.focused input in

  (* Roots is the set of all focused files and all dependent files *)
  let roots = Dep_service.calc_all_reverse_dependencies dependency_info focused in
  let dependents = FilenameSet.diff roots focused in

  let dependencies = CheckedSet.dependencies input in

  Lwt.return (CheckedSet.add ~focused ~dependents ~dependencies CheckedSet.empty)

let filter_out_node_modules ~options =
  let root = Options.root options in
  let file_options = Options.file_options options in
  FilenameSet.filter (fun fn ->
    let filename_str = File_key.to_string fn in
    not (Files.is_within_node_modules ~root ~options:file_options filename_str)
  )

(* Filesystem lazy mode focuses on any file which changes. Non-lazy mode focuses on every file in
 * the repo. In both cases, we never want node_modules to appear in the focused sets.
 *
 * There are no expected invariants for the input sets. The returned set has the following invariants
 * 1. Node modules will only appear in the dependency set.
 * 2. Dependent files are empty.
 *)
let unfocused_files_to_infer ~options ~input_focused ~input_dependencies =
  let focused = filter_out_node_modules ~options input_focused in

  let dependencies = Option.value ~default:FilenameSet.empty input_dependencies in

  Lwt.return (CheckedSet.add ~focused ~dependencies CheckedSet.empty)

(* Called on initialization in non-lazy mode, with optional focus targets.

   When focus targets are not provided, the result is a checked set focusing on parsed files minus
   node modules, plus no dependents (because effectively any dependent is already focused), plus all
   their dependencies (minus those that are already focused). The set of dependencies might contain
   node modules.

   When focus targets are provided, the result is a checked set focusing on those files, plus their
   dependents, plus all their combined dependencies. All these sets might contain node modules.

   In either case, we can consider the result to be "closed" in terms of expected invariants.
*)
let files_to_infer ~options ~reader ?focus_targets ~profiling ~parsed ~dependency_info =
  with_timer_lwt ~options "FilesToInfer" profiling (fun () ->
    match focus_targets with
    | None ->
      unfocused_files_to_infer ~options ~input_focused:parsed ~input_dependencies:None
    | Some input_focused ->
      focused_files_to_infer ~reader ~input_focused ~input_dependencies:None ~dependency_info
  )

let restart_if_faster_than_recheck ~options ~env ~to_merge ~file_watcher_metadata =
  match Options.lazy_mode options with
  | None
  | Some Options.LAZY_MODE_FILESYSTEM
  | Some Options.LAZY_MODE_IDE ->
    (* Only watchman mode might restart *)
    Lwt.return_none
  | Some Options.LAZY_MODE_WATCHMAN ->
    let { MonitorProt.total_update_distance; changed_mergebase; } = file_watcher_metadata in
    Hh_logger.info "File watcher moved %d revisions and %s mergebase"
      total_update_distance
      (if changed_mergebase then "changed" else "did not change");

    if changed_mergebase then begin
      (* TODO (glevi) - One of the numbers we need to estimate is "If we restart how many files
       * would we merge". Currently we're looking at the number of already checked files. But a
       * better way would be to
       *
       * 1. When watchman notices the mergebase changing, also record the files which have changed
       *    since the mergebase
       * 2. Send these files to the server
       * 3. Calculate the fanout of these files (we should have an updated dependency graph by now)
       * 4. That should actually be the right number, instead of just an estimate. But it costs
       *    a little to compute the fanout
       *)
      let files_already_checked = CheckedSet.cardinal env.ServerEnv.checked_files in
      let files_about_to_recheck = CheckedSet.cardinal to_merge in

      Hh_logger.info "We've already checked %d files. We're about to recheck %d files"
        files_already_checked
        files_about_to_recheck;

      let init_time = Recheck_stats.get_init_time () in
      let per_file_time = Recheck_stats.get_per_file_time () in

      let time_to_restart =
        init_time +. per_file_time *. (float_of_int files_already_checked)
      in
      let time_to_recheck =
        per_file_time *. (float_of_int files_about_to_recheck)
      in

      let estimates = { Recheck_stats.
        estimated_time_to_recheck = time_to_recheck;
        estimated_time_to_restart = time_to_restart;
        estimated_time_to_init = init_time;
        estimated_time_to_merge_a_file = per_file_time;
        estimated_files_to_merge = files_about_to_recheck;
        estimated_files_to_init = files_already_checked;
      } in

      Hh_logger.debug "Estimated restart time: %fs to init + (%fs * %d files) = %fs"
        init_time per_file_time files_already_checked time_to_restart;
      Hh_logger.debug "Estimated recheck time: %fs * %d files = %fs"
        per_file_time files_about_to_recheck time_to_recheck;

      Hh_logger.info "Estimating a recheck would take %.2fs and a restart would take %.2fs"
        time_to_recheck
        time_to_restart;
      let%lwt () =
        if time_to_restart < time_to_recheck
        then
          let%lwt () = Recheck_stats.record_last_estimates ~options ~estimates in
          FlowExitStatus.(exit ~msg:"Restarting after a rebase to save time" Restart)
        else Lwt.return_unit
      in

      Lwt.return (Some estimates)
    end else
      Lwt.return_none

module Recheck: sig
  type recheck_result = {
    new_or_changed: Utils_js.FilenameSet.t;
    deleted: Utils_js.FilenameSet.t;
    all_dependent_files: Utils_js.FilenameSet.t;
    cycle_leaders: (Utils_js.FilenameMap.key * int) list;
    skipped_count: int;
    estimates: Recheck_stats.estimates option;
  }
  val full:
    profiling:Profiling_js.running ->
    transaction:Transaction.t ->
    reader:Parsing_heaps.Mutator_reader.reader ->
    options:Options.t ->
    workers:MultiWorkerLwt.worker list option ->
    updates:Utils_js.FilenameSet.t ->
    files_to_force:CheckedSet.t ->
    file_watcher_metadata:MonitorProt.file_watcher_metadata ->
    will_be_checked_files:CheckedSet.t ref ->
    env:ServerEnv.env ->
    (ServerEnv.env * recheck_result) Lwt.t

  val parse_and_update_dependency_info:
    profiling:Profiling_js.running ->
    transaction:Transaction.t ->
    reader:Parsing_heaps.Mutator_reader.reader ->
    options:Options.t ->
    workers:MultiWorkerLwt.worker list option ->
    updates:Utils_js.FilenameSet.t ->
    files_to_force:CheckedSet.t ->
    env:ServerEnv.env ->
    ServerEnv.env Lwt.t
end = struct
  type recheck_result = {
    new_or_changed: Utils_js.FilenameSet.t;
    deleted: Utils_js.FilenameSet.t;
    all_dependent_files: Utils_js.FilenameSet.t;
    cycle_leaders: (Utils_js.FilenameMap.key * int) list;
    skipped_count: int;
    estimates: Recheck_stats.estimates option;
  }

  (* This is the first part of the recheck. It parses the files and updates the dependency graph. It
   * does NOT figure out which files to merge or merge them.
   *
   * It returns an updated env and a bunch of intermediate values which `recheck_merge` can use to
   * calculate the to_merge and perform the merge *)
  let recheck_parse_and_update_dependency_info
      ~profiling ~transaction ~reader ~options ~workers ~updates ~files_to_force ~env =
    let errors = env.ServerEnv.errors in

    (* files_to_force is a request to promote certain files to be checked as a dependency, dependent,
     * or focused file. We can ignore a request if the file is already checked at the desired level
     * or at a more important level *)
    let files_to_force = CheckedSet.diff files_to_force env.ServerEnv.checked_files in

    (* If foo.js is updated and foo.js.flow exists, then mark foo.js.flow as
     * updated too. This is because sometimes we decide what foo.js.flow
     * provides based on the existence of foo.js *)
    let updates = FilenameSet.fold (fun file updates ->
      if not (File_key.check_suffix file Files.flow_ext) &&
        Parsing_heaps.Mutator_reader.has_ast ~reader (File_key.with_suffix file Files.flow_ext)
      then FilenameSet.add (File_key.with_suffix file Files.flow_ext) updates
      else updates
    ) updates updates in

    (* split updates into deleted files and modified files *)
    (** NOTE: We use the term "modified" in the same sense as the underlying file
        system: a modified file exists, and in relation to an old file system
        state, a modified file could be any of "new," "changed," or "unchanged."
    **)
    let modified, deleted = FilenameSet.partition (fun f ->
      Sys.file_exists (File_key.to_string f)
    ) updates in
    let deleted_count = FilenameSet.cardinal deleted in
    let modified_count = FilenameSet.cardinal modified in

    (* log modified and deleted files *)
    if deleted_count + modified_count > 0 then (
      Hh_logger.info "recheck %d modified, %d deleted files"
        modified_count deleted_count;
      let log_files files msg n =
        Hh_logger.info "%s files:" msg;
        let _ = FilenameSet.fold (fun f i ->
          Hh_logger.info "%d/%d: %s" i n (File_key.to_string f);
          i + 1
        ) files 1
        in ()
      in
      if modified_count > 0 then log_files modified "modified" modified_count;
      if deleted_count > 0 then log_files deleted "deleted" deleted_count
    );

    (* We don't need to delete things from the parsing heaps - they will be automatically oldified.
     * Oldifying something removes it from the heap (but keeps it around in case we need it back) *)

    Hh_logger.info "Parsing";
    (* reparse modified files, updating modified to new_or_changed to reflect
     * removal of unchanged files
     *
     * new_or_changed - Set of files which are not unchanged. This includes freshparsed, fails & skips
     * freshparsed - Set of files which parsed successfully
     * unparsed - Set of files which were skipped (e.g. no @flow) or which we failed to parse
     * unchanged_parse - Set of files who's file hash didn't changes
     * new_local_errors - Parse errors, docblock errors, etc
    *)
    let%lwt new_or_changed, freshparsed, unparsed, _unchanged_parse, new_local_errors =
       reparse ~options ~profiling ~transaction ~reader ~workers ~modified ~deleted
     in

    let unparsed_set =
      List.fold_left (fun set (fn, _) -> FilenameSet.add fn set) FilenameSet.empty unparsed
    in

    (* clear errors for new, changed and deleted files *)
    let errors =
      errors
      |> clear_errors new_or_changed
      |> clear_errors deleted
    in

    (* record reparse errors *)
    let errors =
      let () =
        let error_set: Flow_error.ErrorSet.t =
          FilenameMap.fold (fun _ -> Flow_error.ErrorSet.union) new_local_errors Flow_error.ErrorSet.empty
        in
        let error_set = Flow_error.make_errors_printable error_set in
        if Errors.ConcreteLocPrintableErrorSet.cardinal error_set > 0
        then Persistent_connection.update_clients
          ~clients:env.ServerEnv.connections
          ~calc_errors_and_warnings:(fun () -> error_set, FilenameMap.empty)
      in
      let local_errors = merge_error_maps new_local_errors errors.ServerEnv.local_errors in
      { errors with ServerEnv.local_errors }
    in

    (* get old (unchanged, undeleted) files that were parsed successfully *)
    let old_parsed = env.ServerEnv.files in
    let new_or_changed_or_deleted = FilenameSet.union new_or_changed deleted in
    let unchanged = FilenameSet.diff old_parsed new_or_changed_or_deleted in

    Hh_logger.debug
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

        Changing a file
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

        * Is OLD_M different from NEW_M? *(= delete the file, then add it back)*

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

    (* remember old modules *)
    let unchanged_checked = CheckedSet.remove new_or_changed_or_deleted env.ServerEnv.checked_files in

    let all_providers_mutator = Module_hashtables.All_providers_mutator.create transaction in

    (* clear out records of files, and names of modules provided by those files *)
    let%lwt old_modules = with_timer_lwt ~options "ModuleClearFiles" profiling (fun () ->
      Module_js.calc_old_modules
        ~reader workers ~all_providers_mutator ~options new_or_changed_or_deleted
    ) in

    (* We may be forcing a recheck on some unchanged files *)
    let unchanged_files_to_force = CheckedSet.filter files_to_force ~f:(fun fn ->
      not (FilenameSet.mem fn new_or_changed) && FilenameSet.mem fn old_parsed
    ) in

    MonitorRPC.status_update ServerStatus.Resolving_dependencies_progress;
    let%lwt changed_modules, errors =
      commit_modules_and_resolve_requires
        ~transaction
        ~reader
        ~all_providers_mutator
        ~options
        ~profiling
        ~workers
        ~old_modules
        ~parsed_set:freshparsed
        ~unparsed
        ~unparsed_set
        ~new_or_changed
        ~deleted
        ~errors
        ~is_init:false in


    (* We can ignore unchanged files which were forced as dependencies. We don't care about their
     * dependents *)
    let unchanged_files_with_dependents = FilenameSet.union
      (CheckedSet.focused unchanged_files_to_force)
      (CheckedSet.dependents unchanged_files_to_force)
    in

    (* Figure out which modules the unchanged forced files provide. We need these to figure out
     * which dependents need to be added to the checked set *)
    let%lwt unchanged_modules = with_timer_lwt ~options "CalcUnchangedModules" profiling (fun () ->
      Module_js.calc_unchanged_modules ~reader workers unchanged_files_with_dependents
    ) in

    let parsed = FilenameSet.union freshparsed unchanged in


    (* direct_dependent_files are unchanged files which directly depend on changed modules,
       or are new / changed files that are phantom dependents. dependent_files are
       direct_dependent_files plus their dependents (transitive closure) *)
    let%lwt all_dependent_files, direct_dependent_files =
      with_timer_lwt ~options "DependentFiles" profiling (fun () ->
        Dep_service.dependent_files
          ~reader:(Abstract_state_reader.Mutator_state_reader reader)
          workers
          ~candidates:(FilenameSet.diff unchanged unchanged_files_with_dependents)
          ~root_files:(FilenameSet.union new_or_changed unchanged_files_with_dependents)
          ~root_modules:(Modulename.Set.union unchanged_modules changed_modules)
      ) in

    Hh_logger.info "Re-resolving directly dependent files";

    let node_modules_containers = !Files.node_modules_containers in
    (* requires in direct_dependent_files must be re-resolved before merging. *)
    let mutator = Module_heaps.Resolved_requires_mutator.create transaction direct_dependent_files in
    let%lwt () = with_timer_lwt ~options "ReresolveDirectDependents" profiling (fun () ->
      MultiWorkerLwt.call workers
        ~job: (fun () files ->
          List.iter (fun filename ->
            let errors = Module_js.add_parsed_resolved_requires filename
              ~mutator ~reader ~options ~node_modules_containers in
            ignore errors (* TODO: why, FFS, why? *)
          ) files
        )
        ~neutral: ()
        ~merge: (fun () () -> ())
        ~next:(MultiWorkerLwt.next workers (FilenameSet.elements direct_dependent_files))
    ) in

    Hh_logger.info "Recalculating dependency graph";
    let%lwt dependency_info = with_timer_lwt ~options "CalcDepsTypecheck" profiling (fun () ->
      let files_to_include_in_dependency_info =
        freshparsed
        |> FilenameSet.union direct_dependent_files
      in
      let%lwt updated_dependency_info = Dep_service.calc_partial_dependency_info ~options ~reader workers
        files_to_include_in_dependency_info ~parsed in
      let old_dependency_info = env.ServerEnv.dependency_info in
      match old_dependency_info, updated_dependency_info with
        | Dependency_info.Classic old_map, Dependency_info.Classic updated_map ->
          Lwt.return (Dependency_info.Classic (
            old_map
            |> FilenameSet.fold FilenameMap.remove deleted
            |> FilenameMap.union updated_map
          ))
        | Dependency_info.TypesFirst old_map, Dependency_info.TypesFirst updated_map ->
          Lwt.return (Dependency_info.TypesFirst (
            old_map
            |> FilenameSet.fold FilenameMap.remove deleted
            |> FilenameMap.union updated_map
          ))
        | _ -> assert false
    ) in

    (* Here's how to update unparsed:
     * 1. Remove the parsed files. This removes any file which used to be unparsed but is now parsed
     * 2. Remove the deleted files. This removes any previously unparsed file which was deleted
     * 3. Add the newly unparsed files. This adds new unparsed files or files which became unparsed *)
    let unparsed =
      let to_remove = FilenameSet.union parsed deleted in
      FilenameSet.diff env.ServerEnv.unparsed to_remove
      |> FilenameSet.union unparsed_set
    in

    let env = { env with ServerEnv.
      files = parsed;
      unparsed;
      dependency_info;
    } in

    let intermediate_values = (
      all_dependent_files,
      deleted,
      direct_dependent_files,
      errors,
      files_to_force,
      freshparsed,
      new_or_changed,
      unchanged_checked,
      unchanged_files_to_force,
      unparsed_set
    ) in

    Lwt.return (env, intermediate_values)

  (* This function assumes it is called after recheck_parse_and_update_dependency_info. It uses some
   * of the info computed by recheck_parse_and_update_dependency_info to figure out which files to
   * merge. Then it merges them. *)
  let recheck_merge
      ~profiling ~transaction ~reader ~options ~workers
      ~will_be_checked_files ~file_watcher_metadata ~intermediate_values ~env =

    let (
      all_dependent_files,
      deleted,
      direct_dependent_files,
      errors,
      files_to_force,
      freshparsed,
      new_or_changed,
      unchanged_checked,
      unchanged_files_to_force,
      unparsed_set
    ) = intermediate_values in

    let dependency_info = env.ServerEnv.dependency_info in

    let acceptable_files_to_focus =
      FilenameSet.union freshparsed (CheckedSet.all unchanged_files_to_force)
    in

    let%lwt updated_checked_files, all_dependent_files =
      with_timer_lwt ~options "RecalcDepGraph" profiling (fun () ->
        match Options.lazy_mode options with
        | None (* Non lazy mode treats every file as focused. *)
        | Some Options.LAZY_MODE_WATCHMAN (* Watchman mode treats every modified file as focused *)
        | Some Options.LAZY_MODE_FILESYSTEM -> (* FS mode treats every modified file as focused *)
          let old_focus_targets = CheckedSet.focused env.ServerEnv.checked_files in
          let old_focus_targets = FilenameSet.diff old_focus_targets deleted in
          let old_focus_targets = FilenameSet.diff old_focus_targets unparsed_set in
          let focused = FilenameSet.union old_focus_targets freshparsed in
          let%lwt updated_checked_files = unfocused_files_to_infer ~options
              ~input_focused:(FilenameSet.union focused (CheckedSet.focused files_to_force))
              ~input_dependencies:(Some (CheckedSet.dependencies files_to_force)) in
          Lwt.return (updated_checked_files, all_dependent_files)
        | Some Options.LAZY_MODE_IDE -> (* IDE mode only treats opened files as focused *)
          (* Unfortunately, our checked_files set might be out of date. This update could have added
           * some new dependents or dependencies. So we need to recalculate those.
           *
           * To calculate dependents and dependencies, we need to know what are the focused files. We
           * define the focused files to be the union of
           *
           *   1. The files that were previously focused
           *   2. Modified files that are currently open in the IDE
           *   3. If this is a `flow force-recheck --focus A.js B.js C.js`, then A.js, B.js and C.js
           *
           * Remember that the IDE might open a new file or keep open a deleted file, so the focused
           * set might be missing that file. If that file reappears, we must remember to refocus on
           * it.
           **)
          let open_in_ide =
            let opened_files = Persistent_connection.get_opened_files env.ServerEnv.connections in
            FilenameSet.filter (function
              | File_key.SourceFile fn
              | File_key.LibFile fn
              | File_key.JsonFile fn
              | File_key.ResourceFile fn -> SSet.mem fn opened_files
              | File_key.Builtins -> false
            ) freshparsed
          in
          let input_focused = CheckedSet.focused files_to_force (* Files to force to be focused *)
              |> filter_out_node_modules ~options (* Never focus node modules *)
              |> FilenameSet.union (CheckedSet.focused env.ServerEnv.checked_files) (* old focused *)
              |> FilenameSet.union open_in_ide (* Files which are open in the IDE *)
          in
          let input_dependencies = Some (CheckedSet.dependencies files_to_force) in
          let%lwt updated_checked_files =
            focused_files_to_infer ~reader ~input_focused ~input_dependencies ~dependency_info
          in

          (* It's possible that all_dependent_files contains foo.js, which is a dependent of a
           * dependency. That's fine if foo.js is in the checked set. But if it's just some random
           * other dependent then we need to filter it out.
           *)
          let all_dependent_files =
            FilenameSet.inter all_dependent_files (CheckedSet.all updated_checked_files) in
          Lwt.return (updated_checked_files, all_dependent_files)
      )
    in

    (* Filter updated_checked_files down to the files which we just parsed or unchanged files which
     * will be focused *)
    let infer_input = CheckedSet.filter updated_checked_files ~f:(fun fn ->
      FilenameSet.mem fn acceptable_files_to_focus
    ) in

    let%lwt to_merge, components, recheck_map =
      include_dependencies_and_dependents
        ~options ~profiling ~unchanged_checked ~infer_input ~dependency_info ~all_dependent_files
        ~direct_dependent_files
    in

    (* This is a much better estimate of what checked_files will be after the merge finishes. We now
     * include the dependencies and dependents that are being implicitly included in the recheck. *)
    will_be_checked_files := CheckedSet.union env.ServerEnv.checked_files to_merge;

    let%lwt estimates =
      restart_if_faster_than_recheck ~options ~env ~to_merge ~file_watcher_metadata
    in

    let%lwt () = ensure_parsed ~options ~profiling ~workers ~reader to_merge in

    let dependency_graph = Dependency_info.dependency_graph dependency_info in
    (* recheck *)
    let%lwt checked_files, cycle_leaders, errors, coverage, skipped_count = merge
      ~transaction
      ~reader
      ~options
      ~profiling
      ~workers
      ~errors
      ~unchanged_checked
      ~to_merge
      ~components
      ~recheck_map
      ~dependency_graph
      ~deleted
      ~persistent_connections:(Some env.ServerEnv.connections)
      ~prep_merge:(Some (fun _to_merge ->
        (* need to merge the closure of inferred files and their deps *)

        let n = FilenameSet.cardinal all_dependent_files in
        if n > 0
        then Hh_logger.info "remerge %d dependent files:" n;

        let _ = FilenameSet.fold (fun f i ->
          Hh_logger.info "%d/%d: %s" i n (File_key.to_string f);
          i + 1
        ) all_dependent_files 1 in
        Hh_logger.info "Merge prep";

        (* merge errors for unchanged dependents will be cleared lazily *)

        (* to_merge is inferred files plus all dependents. prep for re-merge *)
        (* NOTE: Non-@flow files don't have entries in ResolvedRequiresHeap, so
           don't add then to the set of files to merge! Only inferred files (along
           with dependents) should be merged: see below. *)
        (* let _to_merge = CheckedSet.add ~dependents:all_dependent_files inferred in *)
        ()
      ))
    in

    let%lwt errors, coverage = check_files ~options ~reader ~workers errors coverage checked_files in

    (* NOTE: unused fields are left in their initial empty state *)
    env.ServerEnv.collated_errors := None;
    Lwt.return (
      { env with ServerEnv.
        checked_files;
        errors;
        coverage;
      },
      {
        new_or_changed;
        deleted;
        all_dependent_files;
        cycle_leaders;
        skipped_count;
        estimates;
      }
    )

  (* We maintain the following invariant across rechecks: The set of `files` contains files that
   * parsed successfully in the previous phase (which could be the init phase or a previous recheck
   * phase).
   *
   * This function has been split into two parts. This is because lazy saved state init needs to
   * update the dependency graph for files which have changed since the saved state was generated, but
   * doesn't want to merge those files yet.
   *)
  let full
      ~profiling ~transaction ~reader ~options ~workers ~updates
      ~files_to_force ~file_watcher_metadata ~will_be_checked_files ~env =
    let%lwt env, intermediate_values = recheck_parse_and_update_dependency_info
      ~profiling ~transaction ~reader ~options ~workers ~updates ~files_to_force ~env
    in
    recheck_merge
      ~profiling ~transaction ~reader ~options ~workers
      ~will_be_checked_files ~file_watcher_metadata ~intermediate_values ~env

  let parse_and_update_dependency_info
    ~profiling ~transaction ~reader ~options ~workers ~updates ~files_to_force ~env=
    let%lwt env, _intermediate_values = recheck_parse_and_update_dependency_info
      ~profiling ~transaction ~reader ~options ~workers ~updates ~files_to_force ~env
    in
    Lwt.return env
end

let with_transaction f =
  Transaction.with_transaction @@ fun transaction ->
    let reader = Mutator_state_reader.create transaction in
    f transaction reader

let recheck
    ~options ~workers ~updates env ~files_to_force ~file_watcher_metadata ~will_be_checked_files =
  let should_print_summary = Options.should_profile options in
  let%lwt profiling, (env, stats) =
    Profiling_js.with_profiling_lwt ~label:"Recheck" ~should_print_summary (fun profiling ->
      SharedMem_js.with_memory_profiling_lwt ~profiling ~collect_at_end:true (fun () ->
        with_transaction (fun transaction reader ->
          Recheck.full
            ~profiling ~transaction ~reader ~options ~workers ~updates ~env
            ~files_to_force ~file_watcher_metadata ~will_be_checked_files
        )
      )
    )
  in
  let { Recheck.
    new_or_changed = modified;
    deleted;
    all_dependent_files = dependent_files;
    cycle_leaders;
    skipped_count;
    estimates;
  } = stats in
  let (
    estimated_time_to_recheck,
    estimated_time_to_restart,
    estimated_time_to_init,
    estimated_time_to_merge_a_file,
    estimated_files_to_merge,
    estimated_files_to_init
  ) = Option.value_map estimates
    ~default:(None, None, None, None, None, None)
    ~f:(fun { Recheck_stats.
      estimated_time_to_recheck;
      estimated_time_to_restart;
      estimated_time_to_init;
      estimated_time_to_merge_a_file;
      estimated_files_to_merge;
      estimated_files_to_init;
    } -> (
      Some estimated_time_to_recheck,
      Some estimated_time_to_restart,
      Some estimated_time_to_init,
      Some estimated_time_to_merge_a_file,
      Some estimated_files_to_merge,
      Some estimated_files_to_init
    )
  ) in
  (** TODO: update log to reflect current terminology **)
  FlowEventLogger.recheck
    ~modified
    ~deleted
    ~dependent_files
    ~profiling
    ~skipped_count
    ~estimated_time_to_recheck
    ~estimated_time_to_restart
    ~estimated_time_to_init
    ~estimated_time_to_merge_a_file
    ~estimated_files_to_merge
    ~estimated_files_to_init
    ~scm_update_distance:file_watcher_metadata.MonitorProt.total_update_distance
    ~scm_changed_mergebase:file_watcher_metadata.MonitorProt.changed_mergebase;

  let duration = Profiling_js.get_profiling_duration profiling in
  let dependent_file_count = Utils_js.FilenameSet.cardinal dependent_files in
  let changed_file_count = (Utils_js.FilenameSet.cardinal modified)
    + (Utils_js.FilenameSet.cardinal deleted) in
  let top_cycle = Core_list.fold cycle_leaders ~init:None ~f:(fun top (f2, count2) ->
    match top with
    | Some (f1, count1) -> if f2 > f1 then Some (f2, count2) else Some (f1, count1)
    | None -> Some (f2, count2)) in
  let summary = ServerStatus.({
      duration;
      info = RecheckSummary {dependent_file_count; changed_file_count; top_cycle}; }) in
  Lwt.return (profiling, summary, env)

(* creates a closure that lists all files in the given root, returned in chunks *)
let make_next_files ~libs ~file_options root =
  let make_next_raw =
    Files.make_next_files ~root ~all:false ~subdir:None ~options:file_options ~libs in
  let total = ref 0 in
  fun () ->
    let files = make_next_raw () in

    let finished = !total in
    let length = List.length files in
    MonitorRPC.status_update ServerStatus.(Parsing_progress {
        finished;
        total = None;
    });
    total := finished + length;

    files
    |> Core_list.map ~f:(Files.filename_from_string ~options:file_options)
    |> Bucket.of_list

let mk_init_env ~files ~unparsed ~dependency_info ~ordered_libs ~libs ~errors ~coverage =
  { ServerEnv.
    files;
    unparsed;
    dependency_info;
    checked_files = CheckedSet.empty;
    ordered_libs;
    libs;
    errors;
    coverage;
    collated_errors = ref None;
    connections = Persistent_connection.empty;
  }

let init_from_saved_state ~profiling ~workers ~saved_state options =
  with_transaction @@ fun transaction reader ->

  let file_options = Options.file_options options in
  (* We don't want to walk the file system for the checked in files. But we still need to find the
   * flowlibs *)
  let ordered_flowlib_libs, _ = Files.init ~flowlibs_only:true file_options in

  let { Saved_state.
    flowconfig_hash=_;
    parsed_heaps;
    unparsed_heaps;
    ordered_non_flowlib_libs;
    local_errors;
    warnings;
    coverage;
    node_modules_containers;
  } = saved_state in

  Files.node_modules_containers := node_modules_containers;

  Hh_logger.info "Restoring heaps";
  let%lwt () = with_timer_lwt ~options "RestoreHeaps" profiling (fun () ->
    let%lwt () = MultiWorkerLwt.call workers
      ~job:(List.fold_left (fun () (fn, parsed_file_data) ->
        (* Every package.json file should have a Package_json.t. Use those to restore the
         * PackageHeap and the ReversePackageHeap *)
        begin match fn with
        | File_key.JsonFile str when Filename.basename str = "package.json" ->
          begin match parsed_file_data.Saved_state.package with
          | None -> failwith (Printf.sprintf "Saved state for `%s` missing Package_json.t data" str)
          | Some package -> Module_heaps.Package_heap_mutator.add_package_json str package
          end
        | _ -> ()
        end;

        (* Restore the FileSigHeap *)
        Parsing_heaps.From_saved_state.add_file_sig fn parsed_file_data.Saved_state.file_sig;

        (* Restore the FileHashHeap *)
        Parsing_heaps.From_saved_state.add_file_hash fn parsed_file_data.Saved_state.hash;

        (* Restore the ResolvedRequiresHeap *)
        Module_heaps.From_saved_state.add_resolved_requires
          fn parsed_file_data.Saved_state.resolved_requires
      ))
      ~merge:(fun () () -> ())
      ~neutral:()
      ~next:(MultiWorkerLwt.next workers (FilenameMap.bindings parsed_heaps))
    in

    MultiWorkerLwt.call workers
      ~job:(List.fold_left (fun () (fn, unparsed_file_data) ->
        (* Restore the FileHashHeap *)
        let hash = unparsed_file_data.Saved_state.unparsed_hash in
        Parsing_heaps.From_saved_state.add_file_hash fn hash;
      ))
      ~merge:(fun () () -> ())
      ~neutral:()
      ~next:(MultiWorkerLwt.next workers (FilenameMap.bindings unparsed_heaps))
  ) in

  Hh_logger.info "Loading libraries";
  (* We actually parse and typecheck the libraries, even though we're loading from saved state.
   * We'd need to check them anyway, as soon as any file is checked, since we don't track
   * dependents for libraries. And we don't really support incrementally checking libraries
   *
   * The order of libraries is significant. If two libraries define the same thing, the one
   * merged later wins. For this reason, the saved state stores the order in which the non-flowlib
   * libraries were merged. So all we need to guarantee here is:
   *
   * 1. The builtin libraries are merged first
   * 2. The non-builtin libraries are merged in the same order as before
   *)
  let ordered_libs = List.rev_append (List.rev ordered_flowlib_libs) ordered_non_flowlib_libs in
  let libs = SSet.of_list ordered_libs in

  let%lwt libs_ok, local_errors, warnings, suppressions =
    let suppressions = Error_suppressions.empty in
    init_libs
      ~options ~profiling ~local_errors ~warnings ~suppressions ~reader ordered_libs
  in

  Hh_logger.info "Resolving dependencies";
  MonitorRPC.status_update ServerStatus.Resolving_dependencies_progress;

  let%lwt parsed_set, unparsed_set, all_files, parsed, unparsed =
    with_timer_lwt ~options "PrepareCommitModules" profiling (fun () ->
      let parsed_set = parsed_heaps |> FilenameMap.keys |> FilenameSet.of_list in
      let unparsed_set = unparsed_heaps |> FilenameMap.keys |> FilenameSet.of_list in
      let all_files = FilenameSet.union parsed_set unparsed_set in
      let parsed = FilenameMap.fold
        (fun fn data acc -> (fn, data.Saved_state.info)::acc) parsed_heaps []
      in
      let unparsed = FilenameMap.fold
        (fun fn data acc -> (fn, data.Saved_state.unparsed_info)::acc) unparsed_heaps []
      in
      Lwt.return (parsed_set, unparsed_set, all_files, parsed, unparsed)
    )
  in

  let all_providers_mutator = Module_hashtables.All_providers_mutator.create transaction in

  (* This will restore InfoHeap, NameHeap, & all_providers hashtable *)
  let%lwt _ =
    commit_modules_from_saved_state
      ~transaction
      ~reader
      ~all_providers_mutator
      ~options
      ~is_init:true
      ~profiling
      ~workers
      ~parsed
      ~parsed_set
      ~unparsed
      ~unparsed_set
      ~old_modules:[]
      ~deleted:FilenameSet.empty
      ~local_errors
      ~new_or_changed:all_files
  in

  let errors = { ServerEnv.
    local_errors;
    merge_errors = FilenameMap.empty;
    warnings;
    suppressions;
  } in

  let%lwt dependency_info = with_timer_lwt ~options "CalcDepsTypecheck" profiling (fun () ->
    Dep_service.calc_dependency_info ~options ~reader workers ~parsed:parsed_set
  ) in

  let env = mk_init_env
    ~files:parsed_set ~unparsed:unparsed_set ~dependency_info ~ordered_libs ~libs ~errors ~coverage
  in

  Lwt.return (env, libs_ok)

let init ~profiling ~workers options =
  let file_options = Options.file_options options in

  with_transaction @@ fun transaction reader ->

  (* TODO - explicitly order the libs.
   *
   * Should we let the filesystem dictate the order that we merge libs? Are we sheep? No! We are
   * totally humans and we should deterministically order our library definitions. The order that
   * they are merged determines priority. We should be able to guarantee
   *
   * flowlibs are merged first (therefore have the lowest priority)
   * other libs are merged second (and therefore have the highest priority)
   *
   * However making this change is likely going to be a breaking change for people with conflicting
   * libraries
   *)
  let ordered_libs, libs = Files.init file_options in
  let next_files = make_next_files ~libs ~file_options (Options.root options) in


  Hh_logger.info "Parsing";
  let%lwt parsed, unparsed, unchanged, local_errors =
    parse ~options ~profiling ~workers ~reader next_files in

  (* Parsing won't raise warnings *)
  let warnings = FilenameMap.empty in

  (* Libdefs have no coverage *)
  let coverage = FilenameMap.empty in

  assert (FilenameSet.is_empty unchanged);

  Hh_logger.info "Building package heap";
  let%lwt package_errors = init_package_heap ~options ~profiling ~reader parsed in
  let local_errors = merge_error_maps package_errors local_errors in

  Hh_logger.info "Loading libraries";
  let%lwt libs_ok, local_errors, warnings, suppressions =
    let suppressions = Error_suppressions.empty in
    init_libs
      ~options ~profiling ~local_errors ~warnings ~suppressions ~reader ordered_libs
  in

  Hh_logger.info "Resolving dependencies";
  MonitorRPC.status_update ServerStatus.Resolving_dependencies_progress;

  let all_files, unparsed_set = List.fold_left (fun (all_files, unparsed_set) (filename, _) ->
    FilenameSet.add filename all_files, (FilenameSet.add filename unparsed_set)
  ) (parsed, FilenameSet.empty) unparsed in

  let all_providers_mutator = Module_hashtables.All_providers_mutator.create transaction in

  let%lwt _, errors =
    let errors = { ServerEnv.
      local_errors;
      merge_errors = FilenameMap.empty;
      warnings;
      suppressions;
    } in
    commit_modules_and_resolve_requires
      ~transaction
      ~reader
      ~all_providers_mutator
      ~options
      ~profiling
      ~workers
      ~old_modules:[]
      ~parsed_set:parsed
      ~unparsed
      ~unparsed_set
      ~new_or_changed:all_files
      ~deleted:FilenameSet.empty
      ~errors
      ~is_init:true
  in
  let%lwt dependency_info = with_timer_lwt ~options "CalcDepsTypecheck" profiling (fun () ->
    Dep_service.calc_dependency_info ~options ~reader workers ~parsed
  ) in
  let env =
    mk_init_env
      ~files:parsed ~unparsed:unparsed_set ~dependency_info ~ordered_libs ~libs ~errors ~coverage
  in
  Lwt.return (FilenameSet.empty, env, libs_ok)

(* Does a best-effort job to load a saved state. If it fails, returns None *)
let load_saved_state ~profiling ~workers options =
  let%lwt fetch_profiling, fetch_result = match Options.saved_state_fetcher options with
    | Options.Dummy_fetcher -> Saved_state_dummy_fetcher.fetch ~options
    | Options.Local_fetcher -> Saved_state_local_fetcher.fetch ~options
    | Options.Fb_fetcher -> Saved_state_fb_fetcher.fetch ~options
  in
  Profiling_js.merge ~from:fetch_profiling ~into:profiling;
  match fetch_result with
  | Saved_state_fetcher.No_saved_state ->
    Hh_logger.info "No saved state available";
    Lwt.return_none
  | Saved_state_fetcher.Saved_state { saved_state_filename; changed_files; } ->
    with_timer_lwt ~options "LoadSavedState" profiling (fun () ->
      let changed_files_count = SSet.cardinal changed_files in
      try%lwt
        let%lwt saved_state = Saved_state.load ~workers ~saved_state_filename ~options in
        let updates = Recheck_updates.process_updates
          ~options
          ~libs:(SSet.of_list saved_state.Saved_state.ordered_non_flowlib_libs)
          changed_files
        in
        let updates = match updates with
        | Core_result.Error ({ Recheck_updates.msg; _; }) ->
          Hh_logger.error "The saved state is no longer valid due to file changes: %s" msg;
          raise Saved_state.(Invalid_saved_state Changed_files)
        | Core_result.Ok updates -> updates in
        Hh_logger.info "Saved state script reports %d files changed & we care about %d of them"
          (SSet.cardinal changed_files)
          (FilenameSet.cardinal updates);
        FlowEventLogger.set_saved_state_filename (Path.to_string saved_state_filename);
        FlowEventLogger.load_saved_state_success ~changed_files_count;
        Lwt.return_some (saved_state, updates)
      with Saved_state.Invalid_saved_state invalid_reason ->
        let invalid_reason = Saved_state.invalid_reason_to_string invalid_reason in
        FlowEventLogger.load_saved_state_error
          ~saved_state_filename:(Path.to_string saved_state_filename)
          ~changed_files_count
          ~invalid_reason;
        if Options.saved_state_no_fallback options
        then
          let msg =
            spf "Failed to load saved state: %s" invalid_reason
          in
          FlowExitStatus.exit ~msg FlowExitStatus.Invalid_saved_state
        else Lwt.return_none
    )

let query_watchman_for_changed_files ~options =
  match Options.lazy_mode options with
  | None
  | Some Options.LAZY_MODE_FILESYSTEM
  | Some Options.LAZY_MODE_IDE -> Lwt.return (fun ~libs:_ -> Lwt.return FilenameSet.(empty, empty))
  | Some Options.LAZY_MODE_WATCHMAN -> begin
    let init_settings = {
      (* We're not setting up a subscription, we're just sending a single query *)
      Watchman_lwt.subscribe_mode = None;
      (* Hack makes this configurable in their local config. Apparently buck & hgwatchman also
       * use 10 seconds. *)
      init_timeout = 10;
      expression_terms = Watchman_expression_terms.make ~options;
      subscription_prefix = "flow_server_watcher";
      roots = Files.watched_paths (Options.file_options options);
      debug_logging = Options.is_debug_mode options;
    } in
    let%lwt watchman_env = Watchman_lwt.init init_settings () in
    let%lwt changed_files = match watchman_env with
    | None ->
      failwith "Failed to set up Watchman in order to get the changes since the mergebase"
    | Some watchman_env ->
      (* Huge timeout. We'll time this out ourselves after init if we need *)
      let%lwt changed_files =
        Watchman_lwt.get_changes_since_mergebase ~timeout:10000. watchman_env
      in
      let%lwt () = Watchman_lwt.close watchman_env in
      Lwt.return (SSet.of_list changed_files)
    in
    Lwt.return (fun ~libs ->
      let updates = Recheck_updates.process_updates
        ~skip_incompatible:true
        ~options
        ~libs
        changed_files
      in

      begin match updates with
      | Core_result.Error ({ Recheck_updates.msg; _; }) ->
        failwith
          (Printf.sprintf "skip_incompatible was set to true, how did we manage to error? %S" msg)
      | Core_result.Ok updates ->
        Hh_logger.info
          "Watchman reports %d files changed since mergebase & we care about %d of them"
          (SSet.cardinal changed_files)
          (FilenameSet.cardinal updates);
        (* We have to explicitly focus on these files, since we just parsed them and it will appear
         * to the rechecker that they're unchanged *)
        let files_to_focus = updates in
        Lwt.return (updates, files_to_focus)
      end
    )
  end

let init ~profiling ~workers options =
  let start_time = Unix.gettimeofday () in

  let%lwt get_watchman_updates = query_watchman_for_changed_files ~options
  and updates, env, libs_ok =
    match%lwt load_saved_state ~profiling ~workers options with
    | None ->
      (* Either there is no saved state or we failed to load it for some reason *)
      init ~profiling ~workers options
    | Some (saved_state, updates) ->
      (* We loaded a saved state successfully! We are awesome! *)
      let%lwt env, libs_ok = init_from_saved_state ~profiling ~workers ~saved_state options in
      (* We know that all the files in updates have changed since the saved state was generated. We
       * have two ways to deal with them: *)
      if Options.lazy_mode options = None
      then
        (* In non-lazy mode, we return updates here. They will immediately be rechecked. Due to
         * fanout, this can be a huge recheck, but it's sound. *)
        Lwt.return (updates, env, libs_ok)
      else begin
        (* In lazy mode, we try to avoid the fanout problem. All we really want to do in lazy mode
         * is to update the dependency graph and stuff like that. We don't actually want to merge
         * anything yet. *)
        with_transaction @@ fun transaction reader ->
          let%lwt env = Recheck.parse_and_update_dependency_info
            ~profiling ~transaction ~reader ~options ~workers ~updates
            ~files_to_force:CheckedSet.empty ~env
          in
          Lwt.return (FilenameSet.empty, env, libs_ok)
      end
  in

  let%lwt updates, files_to_focus =
    let now = Unix.gettimeofday () in
    (* If init took N seconds, let's give Watchman another max(15,N) seconds. *)
    let timeout = max 15.0 (now -. start_time) in
    let deadline = now +. timeout in
    MonitorRPC.status_update ~event:(ServerStatus.Watchman_wait_start deadline);
    let%lwt watchman_updates, files_to_focus =
      try%lwt
        Lwt_unix.with_timeout timeout @@ fun () -> get_watchman_updates ~libs:env.ServerEnv.libs
      with Lwt_unix.Timeout ->
        let msg = Printf.sprintf "Timed out after %ds waiting for Watchman."
          (Unix.gettimeofday () -. start_time |> int_of_float)
        in
        FlowExitStatus.(exit ~msg Watchman_error)
    in
    Lwt.return (FilenameSet.union updates watchman_updates, files_to_focus)
  in

  let init_time = Unix.gettimeofday () -. start_time in

  let%lwt last_estimates =
    Recheck_stats.init ~options ~init_time ~parsed_count:(FilenameSet.cardinal env.ServerEnv.files)
  in

  (* Don't recheck if the libs are not ok *)
  if (FilenameSet.is_empty updates && FilenameSet.is_empty files_to_focus) || not libs_ok
  then Lwt.return (libs_ok, env, last_estimates)
  else begin
    let files_to_force = CheckedSet.(add ~focused:files_to_focus empty) in
    let%lwt recheck_profiling, _summary, env =
      recheck
        ~options
        ~workers
        ~updates
        env
        ~files_to_force
        ~file_watcher_metadata:MonitorProt.empty_file_watcher_metadata
        ~will_be_checked_files:(ref files_to_force)
    in
    Profiling_js.merge ~from:recheck_profiling ~into:profiling;
    Lwt.return (true, env, last_estimates)
  end

let full_check ~profiling ~options ~workers ?focus_targets env =
  let { ServerEnv.files = parsed; dependency_info; errors; _; } = env in
  with_transaction (fun transaction reader ->
    let%lwt infer_input = files_to_infer
      ~options ~reader ?focus_targets ~profiling ~parsed ~dependency_info in

    let unchanged_checked = CheckedSet.empty in

    let%lwt to_merge, components, recheck_map =
      include_dependencies_and_dependents
        ~options ~profiling
        ~unchanged_checked
        ~infer_input
        ~dependency_info
        ~all_dependent_files:FilenameSet.empty
        ~direct_dependent_files:FilenameSet.empty
    in
    (* The values to_merge and recheck_map are essentially the same as infer_input, aggregated. This
       is not surprising because files_to_infer returns a closed checked set. Thus, the only purpose
       of calling include_dependencies_and_dependents is to compute components. *)

    let%lwt () = ensure_parsed ~options ~profiling ~workers ~reader to_merge in

    let dependency_graph = Dependency_info.dependency_graph dependency_info in
    let%lwt (checked_files, _, errors, coverage, _) = merge
      ~transaction
      ~reader
      ~options
      ~profiling
      ~workers
      ~errors
      ~unchanged_checked
      ~to_merge
      ~components
      ~recheck_map
      ~dependency_graph
      ~deleted:FilenameSet.empty
      ~persistent_connections:None
      ~prep_merge:None in
    let%lwt errors, coverage = check_files ~options ~reader ~workers errors coverage checked_files in
    Lwt.return { env with ServerEnv.checked_files; errors; coverage }
  )
