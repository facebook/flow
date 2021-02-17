(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module drives the type checker *)

open Utils_js

(****************** typecheck job helpers *********************)

let clear_errors (files : FilenameSet.t) errors =
  FilenameSet.fold
    (fun file { ServerEnv.local_errors; merge_errors; warnings; suppressions } ->
      Hh_logger.debug "clear errors %s" (File_key.to_string file);
      {
        ServerEnv.local_errors = FilenameMap.remove file local_errors;
        merge_errors = FilenameMap.remove file merge_errors;
        warnings = FilenameMap.remove file warnings;
        suppressions = Error_suppressions.remove file suppressions;
      })
    files
    errors

let update_errset map file errset =
  if Flow_error.ErrorSet.is_empty errset then
    map
  else
    let errset =
      match FilenameMap.find_opt file map with
      | Some prev_errset -> Flow_error.ErrorSet.union prev_errset errset
      | None -> errset
    in
    FilenameMap.add file errset map

let merge_error_maps =
  FilenameMap.union ~combine:(fun _ x y -> Some (Flow_error.ErrorSet.union x y))

(* We just want to replace the old coverage with the new one *)
let update_coverage coverage = function
  | None -> coverage
  | Some new_coverage ->
    FilenameMap.union ~combine:(fun _ _ -> Base.Option.return) coverage new_coverage

(* Filter out duplicate provider error, if any, for the given file. *)
let filter_duplicate_provider map file =
  match FilenameMap.find_opt file map with
  | Some prev_errset ->
    let new_errset =
      Flow_error.ErrorSet.filter
        (fun err -> not (Flow_error.kind_of_error err = Errors.DuplicateProviderError))
        prev_errset
    in
    FilenameMap.add file new_errset map
  | None -> map

let collate_parse_results parse_results =
  let {
    Parsing_service_js.parse_ok;
    parse_skips;
    parse_not_found_skips;
    parse_hash_mismatch_skips;
    parse_fails;
    parse_unchanged;
    parse_package_json;
  } =
    parse_results
  in
  (* No one who is calling collate_parse_results is skipping files with hash mismatches *)
  assert (FilenameSet.is_empty parse_hash_mismatch_skips);
  let local_errors =
    List.fold_left
      (fun errors (file, _, fail) ->
        let errset =
          match fail with
          | Parsing_service_js.Parse_error err ->
            Inference_utils.set_of_parse_error ~source_file:file err
          | Parsing_service_js.Docblock_errors errs ->
            Inference_utils.set_of_docblock_errors ~source_file:file errs
          | Parsing_service_js.File_sig_error err ->
            Inference_utils.set_of_file_sig_error ~source_file:file err
        in
        update_errset errors file errset)
      FilenameMap.empty
      parse_fails
  in
  let local_errors =
    (* In practice, the only `tolerable_errors` are related to well formed exports. If this flag
     * were not temporary in nature, it would be worth adding some complexity to avoid conflating
     * them. *)
    Utils_js.FilenameMap.fold
      (fun file file_sig_errors errors ->
        let file_sig_errors = File_sig.abstractify_tolerable_errors file_sig_errors in
        let errset =
          Inference_utils.set_of_file_sig_tolerable_errors ~source_file:file file_sig_errors
        in
        update_errset errors file errset)
      parse_ok
      local_errors
  in
  let unparsed =
    List.fold_left
      (fun unparsed (file, info, _) -> (file, info) :: unparsed)
      parse_skips
      parse_fails
    |> FilenameSet.fold
         (fun file unparsed -> (file, Docblock.default_info) :: unparsed)
         parse_not_found_skips
  in
  let parse_ok =
    FilenameMap.fold (fun k _ acc -> FilenameSet.add k acc) parse_ok FilenameSet.empty
  in
  (parse_ok, unparsed, parse_unchanged, local_errors, parse_package_json)

let parse ~options ~profiling ~workers ~reader parse_next =
  Memory_utils.with_memory_timer_lwt ~options "Parsing" profiling (fun () ->
      let%lwt results = Parsing_service_js.parse_with_defaults ~reader options workers parse_next in
      Lwt.return (collate_parse_results results))

let reparse ~options ~profiling ~transaction ~reader ~workers ~modified ~deleted =
  Memory_utils.with_memory_timer_lwt ~options "Parsing" profiling (fun () ->
      let%lwt (new_or_changed, results) =
        Parsing_service_js.reparse_with_defaults
          ~transaction
          ~reader
          ~with_progress:true
          ~workers
          ~modified
          ~deleted
          options
      in
      let (parse_ok, unparsed, unchanged, local_errors, _) = collate_parse_results results in
      Lwt.return (new_or_changed, parse_ok, unparsed, unchanged, local_errors))

(* TODO: make this always return errors and deal with check_syntax at the caller *)
let parse_contents ~options ~check_syntax filename contents =
  (* always enable types when checking an individual file *)
  let types_mode = Parsing_service_js.TypesAllowed in
  let max_tokens = Options.max_header_tokens options in
  let (docblock_errors, info) = Parsing_service_js.parse_docblock ~max_tokens filename contents in
  let errors = Inference_utils.set_of_docblock_errors ~source_file:filename docblock_errors in
  let parse_options =
    Parsing_service_js.make_parse_options ~fail:check_syntax ~types_mode info options
  in
  let parse_result = Parsing_service_js.do_parse ~info ~parse_options contents filename in
  match parse_result with
  | Parsing_service_js.Parse_ok { ast; file_sig; tolerable_errors; parse_errors; _ } ->
    (* NOTE: parse errors are ignored because we don't surface them when ~check_syntax:false,
       and they'll hit the Parse_fail case instead when ~check_syntax:true *)
    (* TODO: docblock errors get dropped *)
    (Ok (ast, file_sig, tolerable_errors, parse_errors), info)
  | Parsing_service_js.Parse_fail fails ->
    let errors =
      match fails with
      | Parsing_service_js.Parse_error err ->
        let err = Inference_utils.error_of_parse_error ~source_file:filename err in
        Flow_error.ErrorSet.add err errors
      | Parsing_service_js.Docblock_errors errs ->
        List.fold_left
          (fun errors err ->
            let err = Inference_utils.error_of_docblock_error ~source_file:filename err in
            Flow_error.ErrorSet.add err errors)
          errors
          errs
      | Parsing_service_js.File_sig_error err ->
        let err = Inference_utils.error_of_file_sig_error ~source_file:filename err in
        Flow_error.ErrorSet.add err errors
    in
    (Error errors, info)
  | Parsing_service_js.(Parse_skip (Skip_non_flow_file | Skip_resource_file | Skip_package_json _))
    ->
    (* should never happen *)
    (Error errors, info)

let flow_error_of_module_error file err =
  match err with
  | Module_js.ModuleDuplicateProviderError { module_name; provider; conflict } ->
    Error_message.(
      let provider =
        let pos = Loc.{ line = 1; column = 0 } in
        ALoc.of_loc Loc.{ source = Some provider; start = pos; _end = pos }
      in
      let conflict =
        let pos = Loc.{ line = 1; column = 0 } in
        ALoc.of_loc Loc.{ source = Some conflict; start = pos; _end = pos }
      in
      EDuplicateModuleProvider { module_name; provider; conflict })
    |> Flow_error.error_of_msg ~trace_reasons:[] ~source_file:file

(* commit providers for old and new modules, collect errors. *)
let (commit_modules, commit_modules_from_saved_state) =
  let commit_modules_generic
      ~introduce_files
      ~transaction
      ~reader
      ~all_providers_mutator
      ~options
      ~is_init
      ~profiling
      ~workers
      ~parsed
      ~parsed_set
      ~unparsed
      ~unparsed_set
      ~old_modules
      ~deleted
      ~local_errors
      ~new_or_changed =
    (* conservatively approximate set of modules whose providers will change *)
    (* register providers for modules, warn on dupes etc. *)
    Memory_utils.with_memory_timer_lwt ~options "CommitModules" profiling (fun () ->
        let all_files_set = FilenameSet.union (FilenameSet.union parsed_set unparsed_set) deleted in
        let mutator = Module_heaps.Introduce_files_mutator.create transaction all_files_set in
        let%lwt new_modules =
          introduce_files ~mutator ~all_providers_mutator ~workers ~options ~parsed ~unparsed
        in
        let dirty_modules = List.rev_append old_modules new_modules in
        let%lwt (providers, changed_modules, errmap) =
          Module_js.commit_modules
            ~transaction
            ~workers
            ~options
            ~reader
            ~is_init
            new_or_changed
            dirty_modules
        in

        (* Providers might be new but not changed. This typically happens when old
           providers are deleted, and previously duplicate providers become new
           providers. In such cases, we must clear the old duplicate provider errors
           for the new providers.

           (Note that this is unncessary when the providers are changed, because in
           that case they are rechecked and *all* their errors are cleared. But we
           don't care about optimizing that case for now.) *)
        let errors = List.fold_left filter_duplicate_provider local_errors providers in
        Lwt.return
          ( changed_modules,
            FilenameMap.fold
              (fun file errors acc ->
                let errset =
                  List.fold_left
                    (fun acc err ->
                      let error = flow_error_of_module_error file err in
                      Flow_error.ErrorSet.add error acc)
                    Flow_error.ErrorSet.empty
                    errors
                in
                update_errset acc file errset)
              errmap
              errors ))
  in
  let commit_modules ~transaction ~reader =
    commit_modules_generic ~introduce_files:(Module_js.introduce_files ~reader) ~transaction ~reader
  in
  let commit_modules_from_saved_state ~transaction ~reader =
    commit_modules_generic
      ~introduce_files:Module_js.introduce_files_from_saved_state
      ~transaction
      ~reader
  in
  (commit_modules, commit_modules_from_saved_state)

module DirectDependentFilesCache : sig
  val clear : unit -> unit

  val with_cache :
    root_files:FilenameSet.t -> on_miss:FilenameSet.t Lwt.t Lazy.t -> FilenameSet.t Lwt.t
end = struct
  let max_size = 100

  let cache : FilenameSet.t FilenameCache.t = FilenameCache.make ~max_size

  let clear () = FilenameCache.clear cache

  let with_cache ~root_files ~on_miss =
    match FilenameSet.elements root_files with
    | [root_file] ->
      let%lwt (result, _did_hit) = FilenameCache.with_cache root_file on_miss cache in
      Lwt.return result
    | _ ->
      (* Cache is only for when there is a single root file *)
      Lazy.force on_miss
end

let clear_cache_if_resolved_requires_changed resolved_requires_changed =
  if resolved_requires_changed then (
    Hh_logger.info "Resolved requires changed";
    DirectDependentFilesCache.clear ()
  ) else
    Hh_logger.info "Resolved requires are unchanged"

let resolve_requires ~transaction ~reader ~options ~profiling ~workers ~parsed ~parsed_set =
  let node_modules_containers = !Files.node_modules_containers in
  let mutator = Module_heaps.Resolved_requires_mutator.create transaction parsed_set in
  let merge (changed1, errors1) (changed2, errors2) =
    (changed1 || changed2, FilenameMap.union errors1 errors2)
  in
  let%lwt (resolved_requires_changed, errors) =
    Memory_utils.with_memory_timer_lwt ~options "ResolveRequires" profiling (fun () ->
        MultiWorkerLwt.call
          workers
          ~job:
            (List.fold_left (fun (changed, errors_acc) filename ->
                 let (resolved_requires_changed, errors) =
                   Module_js.add_parsed_resolved_requires
                     filename
                     ~mutator
                     ~reader
                     ~options
                     ~node_modules_containers
                 in
                 let changed = changed || resolved_requires_changed in
                 if Flow_error.ErrorSet.is_empty errors then
                   (changed, errors_acc)
                 else
                   (changed, FilenameMap.add filename errors errors_acc)))
          ~neutral:(false, FilenameMap.empty)
          ~merge
          ~next:(MultiWorkerLwt.next workers parsed))
  in
  clear_cache_if_resolved_requires_changed resolved_requires_changed;
  Lwt.return (errors, resolved_requires_changed)

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
  let%lwt (changed_modules, local_errors) =
    commit_modules
      ~transaction
      ~reader
      ~all_providers_mutator
      ~options
      ~is_init
      ~profiling
      ~workers
      ~parsed
      ~parsed_set
      ~unparsed
      ~unparsed_set
      ~old_modules
      ~deleted
      ~local_errors
      ~new_or_changed
  in
  let%lwt (resolve_errors, resolved_requires_changed) =
    resolve_requires ~transaction ~reader ~options ~profiling ~workers ~parsed ~parsed_set
  in
  let local_errors = FilenameMap.union resolve_errors local_errors in
  Lwt.return
    ( changed_modules,
      resolved_requires_changed,
      { ServerEnv.local_errors; merge_errors; warnings; suppressions } )

let error_set_of_internal_error file (loc, internal_error) =
  Error_message.EInternal (loc, internal_error)
  |> Flow_error.error_of_msg ~trace_reasons:[] ~source_file:file
  |> Flow_error.ErrorSet.singleton

let calc_deps ~options ~profiling ~sig_dependency_graph ~components to_merge =
  Memory_utils.with_memory_timer_lwt ~options "CalcDeps" profiling (fun () ->
      let sig_dependency_graph =
        Pure_dep_graph_operations.filter_dependency_graph sig_dependency_graph to_merge
      in
      let components = List.filter (Nel.exists (fun f -> FilenameSet.mem f to_merge)) components in
      if Options.should_profile options then Sort_js.log components;
      let component_map =
        List.fold_left
          (fun component_map component ->
            let file = Nel.hd component in
            FilenameMap.add file component component_map)
          FilenameMap.empty
          components
      in
      Lwt.return (sig_dependency_graph, component_map))

(* The input passed in basically tells us what the caller wants to typecheck.
 * However, due to laziness, it's possible that certain dependents or dependencies have not been
 * checked yet. So we need to calculate all the transitive dependents and transitive dependencies
 * and add them to input, unless they're already checked and in unchanged_checked
 *
 * Note that we do not want to add all_dependent_files to input directly! We only want to
 * pass the dependencies, and later add dependent files as needed. This is important for recheck
 * optimizations. We create the recheck map which indicates whether a given file needs to be
 * rechecked. Dependent files only need to be rechecked if their dependencies change.
 *)
let include_dependencies_and_dependents
    ~options
    ~profiling
    ~unchanged_checked
    ~input
    ~implementation_dependency_graph
    ~sig_dependency_graph
    ~sig_dependent_files
    ~all_dependent_files =
  Memory_utils.with_memory_timer_lwt ~options "PruneDeps" profiling (fun () ->
      (* We need to run the check phase on the entire input set as well as all_dependent_files.
       * We'll calculate the set of files we need to merge based on this. *)
      let to_check = CheckedSet.add ~dependents:all_dependent_files input in
      (* We need to make sure that signatures are available for the dependencies of the files we
       * are going to check. To accomplish this, we start by finding the direct *implementation*
       * dependencies of all the files we will check. Just the signature dependencies won't do,
       * since we need signatures available for all the files imported by the bodies of the files
       * we are going to check. *)
      let preliminary_dependencies =
        Pure_dep_graph_operations.calc_direct_dependencies
          implementation_dependency_graph
          (CheckedSet.all to_check)
      in
      (* So we want to prune our dependencies to only the dependencies which changed. However, two
       * dependencies A and B might be in a cycle. If A changed and B did not, we still need to
       * merge B. Likewise, a dependent A and a dependency B might be in a cycle. So we need to
       * calculate components before we can prune. *)
      let components =
        (* Grab the subgraph containing all our dependencies and sort it into the strongly connected
         * cycles *)
        Sort_js.topsort ~roots:preliminary_dependencies (FilenameGraph.to_map sig_dependency_graph)
      in
      let dependencies =
        let add_filename_to_set set filename = FilenameSet.add filename set in
        let add_nel_to_filenameset set nel = Nel.fold_left add_filename_to_set set nel in
        let is_in_unchanged_checked filename = CheckedSet.mem filename unchanged_checked in
        let all_in_unchanged_checked filenames = Nel.for_all is_in_unchanged_checked filenames in
        List.fold_left
          (fun dependencies component ->
            let dependencies =
              if all_in_unchanged_checked component then
                (* If every element is unchanged, drop the component *)
                dependencies
              else
                (* If some member of the component is not unchanged, then keep the component *)
                add_nel_to_filenameset dependencies component
            in
            dependencies)
          FilenameSet.empty
          components
      in
      (* Definitely recheck input and dependencies. As merging proceeds, dependents may or may not be
         rechecked. *)
      let definitely_to_merge = CheckedSet.add ~dependencies input in
      let to_merge = CheckedSet.add ~dependents:sig_dependent_files definitely_to_merge in
      (* This contains all of the files which may be merged or checked. Conveniently, this is
       * currently the same as to_check except for the addition of dependencies, so we can avoid
       * doing a costly union, but if we change how to_check or to_merge is computed we should be
       * sure to change this as well. *)
      let to_merge_or_check = CheckedSet.add ~dependencies to_check in
      (* NOTE: An important invariant here is that if we recompute Sort_js.topsort with
       * to_merge on sig_dependency_graph, we would get exactly the same components. Later, we
       * will filter sig_dependency_graph to just to_merge, and correspondingly filter
       * components as well. This will work out because every component is either entirely
       * inside to_merge or entirely outside. *)
      Lwt.return
        (to_merge, to_check, to_merge_or_check, components, CheckedSet.all definitely_to_merge))

let remove_old_results phase current_results file =
  let (errors, warnings, suppressions, coverage, first_internal_error) = current_results in
  let new_coverage =
    match phase with
    | Context.Merging
    | Context.Normalizing
    | Context.InitLib ->
      coverage
    | Context.Checking -> FilenameMap.remove file coverage
  in
  ( FilenameMap.remove file errors,
    FilenameMap.remove file warnings,
    Error_suppressions.remove file suppressions,
    new_coverage,
    first_internal_error )

let add_new_results
    ~record_slow_file (errors, warnings, suppressions, coverage, first_internal_error) file result =
  match result with
  | Ok (new_errors, new_warnings, new_suppressions, new_coverage_option, check_time) ->
    if check_time > 1. then record_slow_file file check_time;
    ( update_errset errors file new_errors,
      update_errset warnings file new_warnings,
      Error_suppressions.update_suppressions suppressions new_suppressions,
      update_coverage coverage new_coverage_option,
      first_internal_error )
  | Error (loc, internal_error) ->
    let new_errors = error_set_of_internal_error file (loc, internal_error) in
    let first_internal_error =
      match first_internal_error with
      | Some _ -> first_internal_error
      | None ->
        Some
          (spf
             "%s\n%s"
             (ALoc.debug_to_string ~include_source:true loc)
             (Error_message.string_of_internal_error internal_error))
    in
    (update_errset errors file new_errors, warnings, suppressions, coverage, first_internal_error)

let run_merge_service
    ~master_mutator
    ~worker_mutator
    ~reader
    ~intermediate_result_callback
    ~options
    ~profiling
    ~workers
    ~sig_dependency_graph
    ~component_map
    ~recheck_set
    acc =
  Memory_utils.with_memory_timer_lwt ~options "Merge" profiling (fun () ->
      let%lwt (merged, { Merge_service.skipped_count; sig_new_or_changed }) =
        Merge_service.merge
          ~master_mutator
          ~worker_mutator
          ~reader
          ~intermediate_result_callback
          ~options
          ~workers
          ~sig_dependency_graph
          ~component_map
          ~recheck_set
      in
      let (errs, warnings, suppressions, coverage, first_internal_error) =
        List.fold_left
          (fun acc (file, result) ->
            let component = FilenameMap.find file component_map in
            let acc = Nel.fold_left (remove_old_results Context.Merging) acc component in
            add_new_results ~record_slow_file:(fun _ _ -> ()) acc file result)
          acc
          merged
      in
      Lwt.return
        ( errs,
          warnings,
          suppressions,
          coverage,
          skipped_count,
          sig_new_or_changed,
          Base.Option.map first_internal_error ~f:(spf "First merge internal error:\n%s") ))

let mk_intermediate_result_callback
    ~reader ~options ~profiling ~persistent_connections ~recheck_reasons suppressions =
  let loc_of_aloc = Parsing_heaps.Mutator_reader.loc_of_aloc ~reader in
  let%lwt send_errors_over_connection =
    match persistent_connections with
    | None -> Lwt.return (fun _ -> ())
    | Some clients ->
      Memory_utils.with_memory_timer_lwt ~options "MakeSendErrors" profiling (fun () ->
          (* In classic, each merge step uncovers new errors, warnings, suppressions.
             While more suppressions may come in later steps, the suppressions we've seen so far are
             sufficient to filter the errors and warnings we've seen so far.
             Intuitively, we will not see an error (or warning) before we've seen all the files involved
             in that error, and thus all the suppressions which could possibly suppress the error.

             In types-first, we have already accumulated suppressions in the overall merge step, and
             each check step uses those suppressions to filter the errors and warnings uncovered.
          *)
          Errors.(
            let curr_errors = ref ConcreteLocPrintableErrorSet.empty in
            let curr_warnings = ref ConcreteLocPrintableErrorSet.empty in
            let curr_suppressions = ref suppressions in
            let root = Options.root options in
            let file_options = Some (Options.file_options options) in
            let filter = Error_suppressions.filter_suppressed_errors ~root ~file_options in
            Lwt.return (function (lazy results) ->
                let (new_errors, new_warnings, suppressions) =
                  List.fold_left
                    (fun (errs_acc, warns_acc, supps_acc) result ->
                      let (file, old_errs, old_warns, supps) = result in
                      let supps_acc = Error_suppressions.union supps_acc supps in
                      (* Filter errors and warnings based on suppressions we've seen so far. *)
                      let (errs, _, _) =
                        filter supps_acc old_errs ~unused:Error_suppressions.empty
                        (* TODO: track unused suppressions *)
                      in
                      (* Filter errors and warnings based on suppressions we've seen so far. *)
                      let (warns, _, _) =
                        filter supps_acc old_warns ~unused:Error_suppressions.empty
                        (* TODO: track unused suppressions *)
                      in
                      (* Only add errors we haven't seen before. *)
                      let errs_acc =
                        ConcreteLocPrintableErrorSet.fold
                          (fun err acc ->
                            if ConcreteLocPrintableErrorSet.mem err !curr_errors then
                              acc
                            else
                              ConcreteLocPrintableErrorSet.add err acc)
                          errs
                          errs_acc
                      in
                      (* Only add warnings we haven't seen before. Note that new warnings are stored by
                         filename, because the clients only receive warnings for files they have open. *)
                      let warns_acc =
                        let acc =
                          Base.Option.value
                            (FilenameMap.find_opt file warns_acc)
                            ~default:ConcreteLocPrintableErrorSet.empty
                        in
                        let acc =
                          ConcreteLocPrintableErrorSet.fold
                            (fun warn acc ->
                              if ConcreteLocPrintableErrorSet.mem warn !curr_warnings then
                                acc
                              else
                                ConcreteLocPrintableErrorSet.add warn acc)
                            warns
                            acc
                        in
                        if ConcreteLocPrintableErrorSet.is_empty acc then
                          warns_acc
                        else
                          FilenameMap.add file acc warns_acc
                      in
                      (errs_acc, warns_acc, supps_acc))
                    (ConcreteLocPrintableErrorSet.empty, FilenameMap.empty, !curr_suppressions)
                    results
                in
                curr_errors := ConcreteLocPrintableErrorSet.union new_errors !curr_errors;
                curr_warnings :=
                  FilenameMap.fold
                    (fun _ -> ConcreteLocPrintableErrorSet.union)
                    new_warnings
                    !curr_warnings;
                curr_suppressions := suppressions;

                if
                  not
                    ( ConcreteLocPrintableErrorSet.is_empty new_errors
                    && FilenameMap.is_empty new_warnings )
                then
                  let errors_reason = LspProt.Recheck_streaming { recheck_reasons } in
                  Persistent_connection.update_clients
                    ~clients
                    ~errors_reason
                    ~calc_errors_and_warnings:(fun () -> (new_errors, new_warnings)))))
  in
  let intermediate_result_callback results =
    let errors =
      lazy
        (Base.List.map
           ~f:(fun (file, result) ->
             match result with
             | Ok (errors, warnings, suppressions, _, _) ->
               let errors =
                 errors
                 |> Flow_error.concretize_errors loc_of_aloc
                 |> Flow_error.make_errors_printable
               in
               let warnings =
                 warnings
                 |> Flow_error.concretize_errors loc_of_aloc
                 |> Flow_error.make_errors_printable
               in
               (file, errors, warnings, suppressions)
             | Error msg ->
               let errors = error_set_of_internal_error file msg in
               let errors =
                 errors
                 |> Flow_error.concretize_errors loc_of_aloc
                 |> Flow_error.make_errors_printable
               in
               let suppressions = Error_suppressions.empty in
               let warnings = Errors.ConcreteLocPrintableErrorSet.empty in
               (file, errors, warnings, suppressions))
           (Lazy.force results))
    in
    send_errors_over_connection errors
  in
  Lwt.return intermediate_result_callback

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
    ~coverage
    ~to_merge
    ~components
    ~recheck_set
    ~sig_dependency_graph
    ~deleted
    ~unparsed_set
    ~recheck_reasons =
  let { ServerEnv.local_errors; merge_errors; warnings; suppressions } = errors in
  let%lwt intermediate_result_callback =
    mk_intermediate_result_callback
      ~reader
      ~options
      ~profiling
      ~persistent_connections:None
      ~recheck_reasons
      suppressions
  in

  (* to_merge is the union of inferred (newly inferred files) and the
     transitive closure of all dependents.

     recheck_set maps each file in to_merge to whether it should be rechecked
     initially.
  *)
  Hh_logger.info "to_merge: %s" (CheckedSet.debug_counts_to_string to_merge);
  Hh_logger.info "Calculating dependencies";
  MonitorRPC.status_update ~event:ServerStatus.Calculating_dependencies_progress;
  let files_to_merge = CheckedSet.all to_merge in
  let%lwt (sig_dependency_graph, component_map) =
    calc_deps ~options ~profiling ~sig_dependency_graph ~components files_to_merge
  in
  Hh_logger.info "Merging";
  let%lwt ( ( merge_errors,
              warnings,
              suppressions,
              coverage,
              skipped_count,
              sig_new_or_changed,
              first_internal_error ),
            time_to_merge ) =
    let (master_mutator, worker_mutator) =
      Context_heaps.Merge_context_mutator.create
        transaction
        (FilenameSet.union files_to_merge deleted |> FilenameSet.union unparsed_set)
    in
    let merge_start_time = Unix.gettimeofday () in
    let%lwt result =
      run_merge_service
        ~master_mutator
        ~worker_mutator
        ~reader
        ~intermediate_result_callback
        ~options
        ~profiling
        ~workers
        ~sig_dependency_graph
        ~component_map
        ~recheck_set
        (merge_errors, warnings, suppressions, coverage, None)
    in
    let%lwt () =
      if Options.should_profile options then
        Memory_utils.with_memory_timer_lwt ~options "PrintGCStats" profiling (fun () ->
            Lwt.return (Gc.print_stat stderr))
      else
        Lwt.return_unit
    in
    let time_to_merge = Unix.gettimeofday () -. merge_start_time in
    Hh_logger.info "Done";
    Lwt.return (result, time_to_merge)
  in
  (* It is not clear whether we want or need sig_new_or_changed to include deleted or unparsed
   * (i.e. failed to parse or non-@flow) files, but for the sake of consistency with previous
   * logic, we'll add them in here. It may be worth determining whether we can stop including these
   * files. *)
  let sig_new_or_changed =
    sig_new_or_changed |> FilenameSet.union deleted |> FilenameSet.union unparsed_set
  in
  let errors = { ServerEnv.local_errors; merge_errors; warnings; suppressions } in
  (* compute the largest cycle, for logging *)
  let top_cycle =
    Utils_js.FilenameMap.fold
      (fun leader members top ->
        let count = Nel.length members in
        if count = 1 then
          top
        else
          match top with
          | Some (_, top_count) ->
            if count > top_count then
              Some (leader, count)
            else
              top
          | None -> Some (leader, count))
      component_map
      None
  in
  Lwt.return
    ( errors,
      coverage,
      skipped_count,
      sig_new_or_changed,
      top_cycle,
      time_to_merge,
      first_internal_error )

module Check_files : sig
  val check_files :
    reader:Parsing_heaps.Mutator_reader.reader ->
    options:Options.t ->
    profiling:Profiling_js.running ->
    workers:MultiWorkerLwt.worker list option ->
    errors:ServerEnv.errors ->
    updated_errors:ServerEnv.errors ->
    coverage:Coverage_response.file_coverage Utils_js.FilenameMap.t ->
    to_check:CheckedSet.t ->
    direct_dependent_files:Utils_js.FilenameSet.t ->
    sig_new_or_changed:Utils_js.FilenameSet.t ->
    dependency_info:Dependency_info.t ->
    persistent_connections:Persistent_connection.t option ->
    recheck_reasons:LspProt.recheck_reason list ->
    cannot_skip_direct_dependents:bool ->
    ( ServerEnv.errors
    * Coverage_response.file_coverage Utils_js.FilenameMap.t
    * float
    * int
    * string option
    * int option
    * string option )
    Lwt.t
end = struct
  let out_of_time ~options ~start_time =
    Unix.gettimeofday () -. start_time > Options.max_seconds_for_check_per_worker options

  let out_of_memory ~options ~start_rss =
    match start_rss with
    | None -> false
    | Some start_rss ->
      (match ProcFS.status_for_pid (Unix.getpid ()) with
      | Ok { ProcFS.rss_total; _ } ->
        rss_total - start_rss > Options.max_rss_bytes_for_check_per_worker options
      | Error _ -> false)

  (* Check as many files as it can before it hits the timeout. The timeout is soft,
   * so the file which exceeds the timeout won't be canceled. We expect most buckets
   * to not hit the timeout *)
  let rec job_helper ~reader ~options ~start_time ~start_rss acc = function
    | [] -> (acc, [])
    | unfinished_files when out_of_time ~options ~start_time ->
      Hh_logger.debug
        "Bucket timed out! %d files finished, %d files unfinished"
        (List.length acc)
        (List.length unfinished_files);
      (acc, unfinished_files)
    | unfinished_files when out_of_memory ~options ~start_rss ->
      Hh_logger.debug
        "Bucket ran out of memory! %d files finished, %d files unfinished"
        (List.length acc)
        (List.length unfinished_files);
      (acc, unfinished_files)
    | file :: rest ->
      let result =
        match Merge_service.check options ~reader file with
        | Ok (_, acc) -> Ok acc
        | Error e -> Error e
      in
      job_helper ~reader ~options ~start_time ~start_rss ((file, result) :: acc) rest

  let job ~reader ~options acc files =
    let start_time = Unix.gettimeofday () in
    let start_rss =
      match ProcFS.status_for_pid (Unix.getpid ()) with
      | Ok { ProcFS.rss_total; _ } -> Some rss_total
      | Error _ -> None
    in
    job_helper ~reader ~options ~start_time ~start_rss acc files

  (* A stateful (next, merge) pair. This lets us re-queue unfinished files which are returned
   * when a bucket times out *)
  let mk_next ~intermediate_result_callback ~max_size ~workers ~files =
    let total_count = List.length files in
    let todo = ref (files, total_count) in
    let finished_count = ref 0 in
    let num_workers = max 1 (Base.Option.value_map workers ~default:1 ~f:List.length) in
    let status_update () =
      MonitorRPC.status_update
        ServerStatus.(Checking_progress { total = Some total_count; finished = !finished_count })
    in
    let next () =
      let (remaining_files, remaining_count) = !todo in
      (* When we get near the end of the file list, start using smaller buckets in order
       * to spread the work across the available workers *)
      let bucket_size =
        if remaining_count >= max_size * num_workers then
          max_size
        else
          (remaining_count / num_workers) + 1
      in
      let (bucket, remaining_files) = Base.List.split_n remaining_files bucket_size in
      let bucket_size = List.length bucket in
      todo := (remaining_files, remaining_count - bucket_size);
      if bucket_size = 0 then
        Bucket.Done
      else
        Bucket.Job bucket
    in
    let merge (finished_file_accs, unfinished_files) acc =
      intermediate_result_callback (lazy finished_file_accs);
      let (remaining_files, remaining_count) = !todo in
      todo :=
        ( List.rev_append unfinished_files remaining_files,
          remaining_count + List.length unfinished_files );
      finished_count := !finished_count + List.length finished_file_accs;
      status_update ();
      List.rev_append finished_file_accs acc
    in
    (next, merge)

  let check_files
      ~reader
      ~options
      ~profiling
      ~workers
      ~errors
      ~updated_errors
      ~coverage
      ~to_check
      ~direct_dependent_files
      ~sig_new_or_changed
      ~dependency_info
      ~persistent_connections
      ~recheck_reasons
      ~cannot_skip_direct_dependents =
    Memory_utils.with_memory_timer_lwt ~options "Check" profiling (fun () ->
        Hh_logger.info "Check prep";
        Hh_logger.info "new or changed signatures: %d" (FilenameSet.cardinal sig_new_or_changed);
        let focused_to_check = CheckedSet.focused to_check in
        let merged_dependents = CheckedSet.dependents to_check in
        let skipped_count = ref 0 in
        let (slowest_file, slowest_time, num_slow_files) = (ref None, ref 0., ref None) in
        let record_slow_file file time =
          (num_slow_files :=
             match !num_slow_files with
             | None -> Some 1
             | Some n -> Some (n + 1));
          if time > !slowest_time then (
            slowest_time := time;
            slowest_file := Some file
          )
        in
        let implementation_dependency_graph =
          Dependency_info.implementation_dependency_graph dependency_info
        in
        (* skip dependents whenever none of their dependencies have new or changed signatures *)
        let dependents_to_check =
          FilenameSet.filter (fun f ->
              (cannot_skip_direct_dependents && FilenameSet.mem f direct_dependent_files)
              || FilenameSet.exists (fun f' -> FilenameSet.mem f' sig_new_or_changed)
                 @@ FilenameGraph.find f implementation_dependency_graph
              ||
              ( incr skipped_count;
                false ))
          @@ merged_dependents
        in
        Hh_logger.info
          "Check will skip %d of %d files"
          !skipped_count
          (* We can just add these counts without worrying about files which are in both sets. We
           * got these both from a CheckedSet. CheckedSet's representation ensures that a single
           * file cannot have more than one kind. *)
          (FilenameSet.cardinal focused_to_check + FilenameSet.cardinal merged_dependents);
        let files = FilenameSet.union focused_to_check dependents_to_check in
        let%lwt intermediate_result_callback =
          mk_intermediate_result_callback
            ~reader
            ~options
            ~profiling
            ~persistent_connections
            ~recheck_reasons
            updated_errors.ServerEnv.suppressions
        in
        Hh_logger.info "Checking files";

        let check_start_time = Unix.gettimeofday () in
        let max_size = Options.max_files_checked_per_worker options in
        let (next, merge) =
          mk_next
            ~intermediate_result_callback
            ~max_size
            ~workers
            ~files:(FilenameSet.elements files)
        in
        let%lwt ret =
          MultiWorkerLwt.call workers ~job:(job ~reader ~options) ~neutral:[] ~merge ~next
        in
        let { ServerEnv.merge_errors; warnings; _ } = errors in
        let suppressions = updated_errors.ServerEnv.suppressions in
        let (merge_errors, warnings, suppressions, coverage, first_internal_error) =
          List.fold_left
            (fun acc (file, result) ->
              let acc = remove_old_results Context.Checking acc file in
              add_new_results ~record_slow_file acc file result)
            (merge_errors, warnings, suppressions, coverage, None)
            ret
        in
        let time_to_check_merged = Unix.gettimeofday () -. check_start_time in
        Hh_logger.info "Done";
        let errors = { errors with ServerEnv.merge_errors; warnings; suppressions } in
        Lwt.return
          ( errors,
            coverage,
            time_to_check_merged,
            !skipped_count,
            Base.Option.map ~f:File_key.to_string !slowest_file,
            !num_slow_files,
            Base.Option.map first_internal_error ~f:(spf "First check internal error:\n%s") ))
end

exception Unexpected_file_changes of File_key.t Nel.t

let handle_unexpected_file_changes changed_files =
  let filenames = Nel.map File_key.to_string changed_files in
  let reason =
    LspProt.(
      match filenames with
      | (filename, []) -> Single_file_changed { filename }
      | _ -> Many_files_changed { file_count = Nel.length filenames })
  in
  let filename_set = filenames |> Nel.to_list |> SSet.of_list in
  ServerMonitorListenerState.push_files_to_recheck ~reason filename_set;
  raise Lwt.Canceled

let ensure_parsed ~options ~profiling ~workers ~reader files =
  Memory_utils.with_memory_timer_lwt ~options "EnsureParsed" profiling (fun () ->
      (* The set of files that we expected to parse, but were skipped, either because they had
       * changed since the last recheck or no longer exist on disk. This is in contrast to files
       * that were skipped intentionally because they are not @flow, or because they are resource
       * files. *)
      let%lwt parse_unexpected_skips =
        Parsing_service_js.ensure_parsed ~reader options workers files
      in
      match FilenameSet.elements parse_unexpected_skips with
      | [] -> Lwt.return_unit
      | hd :: tl -> raise (Unexpected_file_changes (hd, tl)))

let ensure_parsed_or_trigger_recheck ~options ~profiling ~workers ~reader files =
  try%lwt ensure_parsed ~options ~profiling ~workers ~reader files
  with Unexpected_file_changes changed_files -> handle_unexpected_file_changes changed_files

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
    SMap.fold
      (fun r locs resolved_rs ->
        let resolved_r =
          Module_js.imported_module
            ~options
            ~reader:(Abstract_state_reader.State_reader reader)
            ~node_modules_containers:!Files.node_modules_containers
            file
            (Nel.hd locs |> ALoc.of_loc)
            r
        in
        Modulename.Set.add resolved_r resolved_rs)
      require_loc_map
      Modulename.Set.empty
  in
  let input =
    Modulename.Set.fold
      (fun m acc ->
        match Module_heaps.Reader.get_file ~reader m ~audit:Expensive.warn with
        | Some f ->
          let reader = Abstract_state_reader.State_reader reader in
          if
            FilenameSet.mem f env.ServerEnv.files
            && Module_js.checked_file ~reader f ~audit:Expensive.warn
          then
            CheckedSet.add ~dependencies:(FilenameSet.singleton f) acc
          else
            acc
        | None -> acc) (* complain elsewhere about required module not found *)
      resolved_requires
      CheckedSet.empty
  in
  let checked = env.ServerEnv.checked_files in
  (* Often, all dependencies have already been checked, so input contains no unchecked files.
   * In that case, let's short-circuit typecheck, since a no-op typecheck still takes time on
   * large repos *)
  let unchecked_dependencies = CheckedSet.diff input checked in
  if CheckedSet.is_empty unchecked_dependencies then
    Lwt.return_unit
  else (
    Hh_logger.info
      "Canceling command due to %d unchecked dependencies"
      (CheckedSet.cardinal unchecked_dependencies);
    let reason = LspProt.Unchecked_dependencies { filename = File_key.to_string file } in
    ServerMonitorListenerState.push_checked_set_to_force ~reason unchecked_dependencies;
    raise Lwt.Canceled
  )

(* Another special case, similar assumptions as above. *)

(** TODO: handle case when file+contents don't agree with file system state **)
let merge_contents ~options ~env ~reader filename info (ast, file_sig) =
  let%lwt () = ensure_checked_dependencies ~options ~reader ~env filename file_sig in
  Lwt.return (Merge_service.check_contents_context ~reader options filename ast info file_sig)

let errors_of_context ~options ~env ~loc_of_aloc filename tolerable_errors cx =
  let errors = Context.errors cx in
  let local_errors =
    tolerable_errors
    |> File_sig.abstractify_tolerable_errors
    |> Inference_utils.set_of_file_sig_tolerable_errors ~source_file:filename
  in
  (* Suppressions for errors in this file can come from dependencies *)
  let suppressions =
    ServerEnv.(
      let new_suppressions = Context.error_suppressions cx in
      let { suppressions; _ } = env.errors in
      Error_suppressions.update_suppressions suppressions new_suppressions)
  in
  let severity_cover = Context.severity_cover cx in
  let include_suppressions = Context.include_suppressions cx in
  let aloc_tables = Context.aloc_tables cx in
  let (errors, warnings, suppressions) =
    Error_suppressions.filter_lints
      ~include_suppressions
      suppressions
      errors
      aloc_tables
      severity_cover
  in
  let errors =
    errors
    |> Flow_error.ErrorSet.union local_errors
    |> Flow_error.concretize_errors loc_of_aloc
    |> Flow_error.make_errors_printable
  in
  let warnings =
    warnings |> Flow_error.concretize_errors loc_of_aloc |> Flow_error.make_errors_printable
  in
  let root = Options.root options in
  let file_options = Some (Options.file_options options) in
  (* Filter out suppressed errors *)
  let (errors, _, _) =
    Error_suppressions.filter_suppressed_errors
      ~root
      ~file_options
      suppressions
      errors
      ~unused:Error_suppressions.empty
    (* TODO: track unused suppressions *)
  in
  (* Filter out suppressed warnings *)
  let (warnings, _, _) =
    Error_suppressions.filter_suppressed_errors
      ~root
      ~file_options
      suppressions
      warnings
      ~unused:Error_suppressions.empty
    (* TODO: track unused suppressions *)
  in
  let warnings =
    if Options.should_include_warnings options then
      warnings
    else
      Errors.ConcreteLocPrintableErrorSet.empty
  in
  (errors, warnings)

let typecheck_contents ~options ~env ~profiling contents filename =
  let reader = State_reader.create () in
  let loc_of_aloc = Parsing_heaps.Reader.loc_of_aloc ~reader in
  let%lwt (parse_result, info) =
    Memory_utils.with_memory_timer_lwt ~options "Parsing" profiling (fun () ->
        Lwt.return (parse_contents ~options ~check_syntax:true filename contents))
  in
  (* override docblock info *)
  let info = Docblock.set_flow_mode_for_ide_command info in
  match parse_result with
  | Ok (ast, file_sig, tolerable_errors, _parse_errors) ->
    let%lwt (cx, typed_ast) =
      Memory_utils.with_memory_timer_lwt ~options "MergeContents" profiling (fun () ->
          merge_contents ~options ~env ~reader filename info (ast, file_sig))
    in
    let (errors, warnings) =
      errors_of_context ~options ~env ~loc_of_aloc filename tolerable_errors cx
    in
    Lwt.return (Some (cx, ast, file_sig, tolerable_errors, typed_ast), errors, warnings)
  | Error errors ->
    let errors =
      errors |> Flow_error.concretize_errors loc_of_aloc |> Flow_error.make_errors_printable
    in
    Lwt.return (None, errors, Errors.ConcreteLocPrintableErrorSet.empty)

let type_contents ~options ~env ~profiling contents filename =
  try%lwt
    let reader = State_reader.create () in
    let%lwt (parse_result, info) =
      Memory_utils.with_memory_timer_lwt ~options "Parsing" profiling (fun () ->
          Lwt.return (parse_contents ~options ~check_syntax:false filename contents))
    in
    (* override docblock info *)
    let info = Docblock.set_flow_mode_for_ide_command info in
    match parse_result with
    | Ok (ast, file_sig, tolerable_errors, parse_errors) ->
      let%lwt (cx, typed_ast) =
        Memory_utils.with_memory_timer_lwt ~options "MergeContents" profiling (fun () ->
            merge_contents ~options ~env ~reader filename info (ast, file_sig))
      in
      Lwt.return (Ok (cx, info, file_sig, tolerable_errors, ast, typed_ast, parse_errors))
    | Error _ -> failwith "Couldn't parse file"
  with
  | Lwt.Canceled as exn -> raise exn
  | exn ->
    let exn = Exception.wrap exn in
    let e = Exception.to_string exn in
    Hh_logger.error "Uncaught exception in type_contents\n%s" e;
    Lwt.return (Error e)

let init_libs ~options ~profiling ~local_errors ~warnings ~suppressions ~reader ordered_libs =
  Memory_utils.with_memory_timer_lwt ~options "InitLibs" profiling (fun () ->
      let options =
        match Options.verbose options with
        | Some { Verbose.enabled_during_flowlib = false; _ } ->
          (* Normally we disable verbosity while loading the libs. But if we're running with
           * --verbose-flowlib then we want to leave verbosity on *)
          { options with Options.opt_verbose = None }
        | _ -> options
      in
      let%lwt {
            Init_js.ok;
            errors = lib_errors;
            warnings = lib_warnings;
            suppressions = lib_suppressions;
            exports;
          } =
        Init_js.init ~options ~reader ordered_libs
      in
      Lwt.return
        ( ok,
          FilenameMap.union lib_errors local_errors,
          FilenameMap.union lib_warnings warnings,
          Error_suppressions.update_suppressions lib_suppressions suppressions,
          exports ))

let is_file_tracked_and_checked ~reader filename =
  Module_heaps.Reader_dispatcher.is_tracked_file ~reader filename
  (* otherwise, f is probably a directory *)
  && Module_js.checked_file ~reader ~audit:Expensive.warn filename

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
 *
 * `is_file_checked` should return a boolean indicating whether the file has @flow or is otherwise
 * considered to be a file that Flow should check. Unfortunately the term "checked" is overloaded in
 * this codebase. In some contexts it means the set of files that we are *currently* checking due to
 * lazy mode. In other contexts, it means the set of files which are eligible to be checked. In this
 * case, it has the latter meaning.
 * *)
let focused_files_and_dependents_to_infer
    ~is_file_checked
    ~implementation_dependency_graph
    ~sig_dependency_graph
    ~input_focused
    ~input_dependencies
    ~sig_dependent_files
    ~all_dependent_files =
  let input =
    CheckedSet.add
      ~focused:input_focused
      ~dependencies:(Base.Option.value ~default:FilenameSet.empty input_dependencies)
      CheckedSet.empty
  in
  (* Filter unchecked files out of the input *)
  let input = CheckedSet.filter input ~f:is_file_checked in
  let focused = CheckedSet.focused input in
  (* Roots is the set of all focused files and all dependent files. *)
  let (_sig_dependents, roots) =
    Pure_dep_graph_operations.calc_all_dependents
      ~sig_dependency_graph
      ~implementation_dependency_graph
      focused
  in
  let dependents = FilenameSet.diff roots focused in
  let dependencies = CheckedSet.dependencies input in
  let checked_files = CheckedSet.add ~focused ~dependents ~dependencies CheckedSet.empty in
  (* It's possible that all_dependent_files contains foo.js, which is a dependent of a
   * dependency. That's fine if foo.js is in the checked set. But if it's just some random
   * other dependent then we need to filter it out.
   *)
  let sig_dependent_files = FilenameSet.inter sig_dependent_files (CheckedSet.all checked_files) in
  let all_dependent_files = FilenameSet.inter all_dependent_files (CheckedSet.all checked_files) in
  Lwt.return (checked_files, sig_dependent_files, all_dependent_files)

let filter_out_node_modules ~options =
  let root = Options.root options in
  let file_options = Options.file_options options in
  FilenameSet.filter (fun fn ->
      let filename_str = File_key.to_string fn in
      not (Files.is_within_node_modules ~root ~options:file_options filename_str))

(* Filesystem lazy mode focuses on any file which changes. Non-lazy mode focuses on every file in
 * the repo. In both cases, we never want node_modules to appear in the focused sets.
 *
 * There are no expected invariants for the input sets. The returned set has the following invariants
 * 1. Node modules will only appear in the dependency set.
 * 2. Dependent files are empty.
 *)
let unfocused_files_and_dependents_to_infer
    ~options ~input_focused ~input_dependencies ~sig_dependent_files ~all_dependent_files =
  let focused = filter_out_node_modules ~options input_focused in
  let dependencies = Base.Option.value ~default:FilenameSet.empty input_dependencies in
  Lwt.return
    ( CheckedSet.add ~focused ~dependencies CheckedSet.empty,
      sig_dependent_files,
      all_dependent_files )

(* Called on initialization in non-lazy mode, with optional focus targets.

   When focus targets are not provided, the result is a checked set focusing on parsed files minus
   node modules, plus no dependents (because effectively any dependent is already focused), plus all
   their dependencies (minus those that are already focused). The set of dependencies might contain
   node modules.

   When focus targets are provided, we remove any unparsed (e.g. syntax error) targets, and then
   the result is a checked set focusing on those files, plus their dependents, plus all their
   combined dependencies. All these sets might contain node modules.

   In either case, we can consider the result to be "closed" in terms of expected invariants.
*)
let files_to_infer ~options ~profiling ~reader ~dependency_info ?focus_targets ~parsed =
  Memory_utils.with_memory_timer_lwt ~options "FilesToInfer" profiling (fun () ->
      match focus_targets with
      | None ->
        unfocused_files_and_dependents_to_infer
          ~options
          ~input_focused:parsed
          ~input_dependencies:None
          ~sig_dependent_files:FilenameSet.empty
          ~all_dependent_files:FilenameSet.empty
      | Some input_focused ->
        let implementation_dependency_graph =
          Dependency_info.implementation_dependency_graph dependency_info
        in
        let sig_dependency_graph = Dependency_info.sig_dependency_graph dependency_info in
        let is_file_checked =
          is_file_tracked_and_checked ~reader:(Abstract_state_reader.Mutator_state_reader reader)
        in
        (* only focus files that parsed successfully *)
        let input_focused = FilenameSet.inter input_focused parsed in
        focused_files_and_dependents_to_infer
          ~is_file_checked
          ~implementation_dependency_graph
          ~sig_dependency_graph
          ~input_focused
          ~input_dependencies:None
          ~sig_dependent_files:FilenameSet.empty
          ~all_dependent_files:FilenameSet.empty)

let restart_if_faster_than_recheck ~options ~env ~to_merge_or_check ~file_watcher_metadata =
  match Options.lazy_mode options with
  | Options.NON_LAZY_MODE
  | Options.LAZY_MODE_FILESYSTEM
  | Options.LAZY_MODE_IDE ->
    (* Only watchman mode might restart *)
    Lwt.return_none
  | Options.LAZY_MODE_WATCHMAN ->
    let { MonitorProt.total_update_distance; changed_mergebase } = file_watcher_metadata in
    Hh_logger.info
      "File watcher moved %d revisions and %s mergebase"
      total_update_distance
      ( if changed_mergebase then
        "changed"
      else
        "did not change" );

    if changed_mergebase then (
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
      let files_about_to_recheck = CheckedSet.cardinal to_merge_or_check in
      Hh_logger.info
        "We've already checked %d files. We're about to recheck %d files"
        files_already_checked
        files_about_to_recheck;

      let init_time = Recheck_stats.get_init_time () in
      let per_file_time = Recheck_stats.get_per_file_time () in
      let time_to_restart = init_time +. (per_file_time *. float_of_int files_already_checked) in
      let time_to_recheck = per_file_time *. float_of_int files_about_to_recheck in
      let estimates =
        {
          Recheck_stats.estimated_time_to_recheck = time_to_recheck;
          estimated_time_to_restart = time_to_restart;
          estimated_time_to_init = init_time;
          estimated_time_per_file = per_file_time;
          estimated_files_to_recheck = files_about_to_recheck;
          estimated_files_to_init = files_already_checked;
        }
      in
      Hh_logger.debug
        "Estimated restart time: %fs to init + (%fs * %d files) = %fs"
        init_time
        per_file_time
        files_already_checked
        time_to_restart;
      Hh_logger.debug
        "Estimated recheck time: %fs * %d files = %fs"
        per_file_time
        files_about_to_recheck
        time_to_recheck;

      Hh_logger.info
        "Estimating a recheck would take %.2fs and a restart would take %.2fs"
        time_to_recheck
        time_to_restart;
      let%lwt () =
        if time_to_restart < time_to_recheck then
          let%lwt () = Recheck_stats.record_last_estimates ~options ~estimates in
          FlowExitStatus.(exit ~msg:"Restarting after a rebase to save time" Restart)
        else
          Lwt.return_unit
      in
      Lwt.return (Some estimates)
    ) else
      Lwt.return_none

type determine_what_to_recheck_result =
  | Determine_what_to_recheck_result of {
      to_merge: CheckedSet.t;
      to_check: CheckedSet.t;
      (* union of to_merge and to_check *)
      to_merge_or_check: CheckedSet.t;
      components: File_key.t Nel.t list;
      recheck_set: FilenameSet.t;
      sig_dependent_files: FilenameSet.t;
      all_dependent_files: FilenameSet.t;
    }

module Recheck : sig
  type recheck_result = {
    new_or_changed: Utils_js.FilenameSet.t;
    deleted: Utils_js.FilenameSet.t;
    to_merge: CheckedSet.t;
    to_check: CheckedSet.t;
    sig_dependent_files: Utils_js.FilenameSet.t;
    all_dependent_files: Utils_js.FilenameSet.t;
    top_cycle: (File_key.t * int) option;
    merge_skip_count: int;
    check_skip_count: int;
    slowest_file: string option;
    num_slow_files: int option;
    estimates: Recheck_stats.estimates option;
  }

  val full :
    profiling:Profiling_js.running ->
    transaction:Transaction.t ->
    reader:Parsing_heaps.Mutator_reader.reader ->
    options:Options.t ->
    workers:MultiWorkerLwt.worker list option ->
    updates:Utils_js.FilenameSet.t ->
    files_to_force:CheckedSet.t ->
    file_watcher_metadata:MonitorProt.file_watcher_metadata ->
    recheck_reasons:LspProt.recheck_reason list ->
    will_be_checked_files:CheckedSet.t ref ->
    env:ServerEnv.env ->
    (ServerEnv.env * recheck_result * string option) Lwt.t

  (* Raises `Unexpected_file_changes` if it finds files unexpectedly changed when parsing. *)
  val parse_and_update_dependency_info :
    profiling:Profiling_js.running ->
    transaction:Transaction.t ->
    reader:Parsing_heaps.Mutator_reader.reader ->
    options:Options.t ->
    workers:MultiWorkerLwt.worker list option ->
    updates:Utils_js.FilenameSet.t ->
    files_to_force:CheckedSet.t ->
    recheck_reasons:LspProt.recheck_reason list ->
    env:ServerEnv.env ->
    ServerEnv.env Lwt.t

  (* Exposed only for testing purposes. Not meant for general consumption. *)
  val determine_what_to_recheck :
    profiling:Profiling_js.running ->
    options:Options.t ->
    is_file_checked:(File_key.t -> bool) ->
    ide_open_files:SSet.t Lazy.t ->
    sig_dependency_graph:FilenameGraph.t ->
    implementation_dependency_graph:FilenameGraph.t ->
    checked_files:CheckedSet.t ->
    freshparsed:FilenameSet.t ->
    unparsed_set:FilenameSet.t ->
    deleted:FilenameSet.t ->
    unchanged_checked:CheckedSet.t ->
    files_to_force:CheckedSet.t ->
    unchanged_files_to_force:CheckedSet.t ->
    direct_dependent_files:FilenameSet.t ->
    determine_what_to_recheck_result Lwt.t
end = struct
  type recheck_result = {
    new_or_changed: Utils_js.FilenameSet.t;
    deleted: Utils_js.FilenameSet.t;
    to_merge: CheckedSet.t;
    to_check: CheckedSet.t;
    sig_dependent_files: Utils_js.FilenameSet.t;
    all_dependent_files: Utils_js.FilenameSet.t;
    top_cycle: (File_key.t * int) option;
    merge_skip_count: int;
    check_skip_count: int;
    slowest_file: string option;
    num_slow_files: int option;
    estimates: Recheck_stats.estimates option;
  }

  (* This is the first part of the recheck. It parses the files and updates the dependency graph. It
   * does NOT figure out which files to merge or merge them.
   *
   * It returns an updated env and a bunch of intermediate values which `recheck_merge` can use to
   * calculate the to_merge and perform the merge.
   *
   * Raises `Unexpected_file_changes` if it finds files unexpectedly changed when parsing.
   *)
  let recheck_parse_and_update_dependency_info
      ~profiling
      ~transaction
      ~reader
      ~options
      ~workers
      ~updates
      ~files_to_force
      ~recheck_reasons
      ~env =
    let loc_of_aloc = Parsing_heaps.Mutator_reader.loc_of_aloc ~reader in
    let errors = env.ServerEnv.errors in
    (* files_to_force is a request to promote certain files to be checked as a dependency, dependent,
     * or focused file. We can ignore a request if the file is already checked at the desired level
     * or at a more important level *)
    let files_to_force = CheckedSet.diff files_to_force env.ServerEnv.checked_files in
    (* split updates into deleted files and modified files *)
    (* NOTE: We use the term "modified" in the same sense as the underlying file
       system: a modified file exists, and in relation to an old file system
       state, a modified file could be any of "new," "changed," or "unchanged."
    **)
    let (modified, deleted) =
      FilenameSet.partition (fun f -> Sys.file_exists (File_key.to_string f)) updates
    in
    let deleted_count = FilenameSet.cardinal deleted in
    let modified_count = FilenameSet.cardinal modified in
    (* log modified and deleted files *)
    if deleted_count + modified_count > 0 then (
      Hh_logger.info "recheck %d modified, %d deleted files" modified_count deleted_count;
      let log_files files msg n =
        Hh_logger.info "%s files:" msg;
        let _ =
          FilenameSet.fold
            (fun f i ->
              let cap = 500 in
              if i <= cap then
                Hh_logger.info "%d/%d: %s" i n (File_key.to_string f)
              else if Hh_logger.Level.(passes_min_level Debug) then
                Hh_logger.debug "%d/%d: %s" i n (File_key.to_string f)
              else if i = cap + 1 then
                Hh_logger.info "..."
              else
                ();
              i + 1)
            files
            1
        in
        ()
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
    let%lwt (new_or_changed, freshparsed, unparsed, _unchanged_parse, new_local_errors) =
      reparse ~options ~profiling ~transaction ~reader ~workers ~modified ~deleted
    in
    let unparsed_set =
      List.fold_left (fun set (fn, _) -> FilenameSet.add fn set) FilenameSet.empty unparsed
    in
    (* clear errors for new, changed and deleted files *)
    let errors = errors |> clear_errors new_or_changed |> clear_errors deleted in
    (* record reparse errors *)
    let errors =
      let () =
        let error_set : Flow_error.ErrorSet.t =
          FilenameMap.fold
            (fun _ -> Flow_error.ErrorSet.union)
            new_local_errors
            Flow_error.ErrorSet.empty
        in
        let error_set =
          error_set |> Flow_error.concretize_errors loc_of_aloc |> Flow_error.make_errors_printable
        in
        if not (Errors.ConcreteLocPrintableErrorSet.is_empty error_set) then
          Persistent_connection.update_clients
            ~clients:env.ServerEnv.connections
            ~errors_reason:(LspProt.Recheck_streaming { recheck_reasons })
            ~calc_errors_and_warnings:(fun () -> (error_set, FilenameMap.empty))
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

    (* Here's where the interesting part of rechecking begins. Before diving into
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
    let unchanged_checked =
      CheckedSet.remove new_or_changed_or_deleted env.ServerEnv.checked_files
    in
    let all_providers_mutator = Module_hashtables.All_providers_mutator.create transaction in
    (* clear out records of files, and names of modules provided by those files *)
    let%lwt old_modules =
      Memory_utils.with_memory_timer_lwt ~options "ModuleClearFiles" profiling (fun () ->
          Module_js.calc_old_modules
            ~reader
            workers
            ~all_providers_mutator
            ~options
            new_or_changed_or_deleted)
    in
    (* We may be forcing a recheck on some unchanged files *)
    let unchanged_files_to_force =
      CheckedSet.filter files_to_force ~f:(fun fn ->
          (not (FilenameSet.mem fn new_or_changed)) && FilenameSet.mem fn old_parsed)
    in
    MonitorRPC.status_update ServerStatus.Resolving_dependencies_progress;
    let%lwt (changed_modules, resolved_requires_changed_in_commit_modules, errors) =
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
        ~is_init:false
    in
    (* We can ignore unchanged files which were forced as dependencies. We don't care about their
     * dependents *)
    let unchanged_files_with_dependents =
      FilenameSet.union
        (CheckedSet.focused unchanged_files_to_force)
        (CheckedSet.dependents unchanged_files_to_force)
    in
    (* Figure out which modules the unchanged forced files provide. We need these to figure out
     * which dependents need to be added to the checked set *)
    let%lwt unchanged_modules =
      Memory_utils.with_memory_timer_lwt ~options "CalcUnchangedModules" profiling (fun () ->
          Module_js.calc_unchanged_modules ~reader workers unchanged_files_with_dependents)
    in
    let parsed = FilenameSet.union freshparsed unchanged in
    (* direct_dependent_files are unchanged files which directly depend on changed modules,
       or are new / changed files that are phantom dependents. all_dependent_files are
       direct_dependent_files plus their dependents (transitive closure) *)
    let%lwt direct_dependent_files =
      Memory_utils.with_memory_timer_lwt ~options "DirectDependentFiles" profiling (fun () ->
          let root_files = FilenameSet.union new_or_changed unchanged_files_with_dependents in
          DirectDependentFilesCache.with_cache
            ~root_files
            ~on_miss:
              ( lazy
                (Dep_service.calc_direct_dependents
                   ~reader:(Abstract_state_reader.Mutator_state_reader reader)
                   workers
                   ~candidates:(FilenameSet.diff unchanged unchanged_files_with_dependents)
                   ~root_files
                   ~root_modules:(Modulename.Set.union unchanged_modules changed_modules)) ))
    in
    Hh_logger.info "Re-resolving directly dependent files";
    let%lwt () = ensure_parsed ~options ~profiling ~workers ~reader direct_dependent_files in

    let node_modules_containers = !Files.node_modules_containers in
    (* requires in direct_dependent_files must be re-resolved before merging. *)
    let mutator =
      Module_heaps.Resolved_requires_mutator.create transaction direct_dependent_files
    in
    let%lwt resolved_requires_changed_in_reresolve_direct_dependents =
      Memory_utils.with_memory_timer_lwt ~options "ReresolveDirectDependents" profiling (fun () ->
          let%lwt resolved_requires_changed =
            MultiWorkerLwt.call
              workers
              ~job:(fun anything_changed files ->
                List.fold_left
                  (fun anything_changed filename ->
                    let (changed, errors) =
                      Module_js.add_parsed_resolved_requires
                        filename
                        ~mutator
                        ~reader
                        ~options
                        ~node_modules_containers
                    in
                    ignore errors;

                    (* TODO: why, FFS, why? *)
                    anything_changed || changed)
                  anything_changed
                  files)
              ~neutral:false
              ~merge:(fun changed1 changed2 -> changed1 || changed2)
              ~next:(MultiWorkerLwt.next workers (FilenameSet.elements direct_dependent_files))
          in
          clear_cache_if_resolved_requires_changed resolved_requires_changed;
          Lwt.return resolved_requires_changed)
    in
    Hh_logger.info "Recalculating dependency graph";
    let%lwt dependency_info =
      Memory_utils.with_memory_timer_lwt ~options "CalcDepsTypecheck" profiling (fun () ->
          let files_to_update_dependency_info =
            FilenameSet.union freshparsed direct_dependent_files
          in
          let%lwt partial_dependency_graph =
            Dep_service.calc_partial_dependency_graph
              ~options
              ~reader
              workers
              files_to_update_dependency_info
              ~parsed
          in
          let old_dependency_info = env.ServerEnv.dependency_info in
          let to_remove = FilenameSet.union unparsed_set deleted in
          Lwt.return (Dependency_info.update old_dependency_info partial_dependency_graph to_remove))
    in
    (* Here's how to update unparsed:
     * 1. Remove the parsed files. This removes any file which used to be unparsed but is now parsed
     * 2. Remove the deleted files. This removes any previously unparsed file which was deleted
     * 3. Add the newly unparsed files. This adds new unparsed files or files which became unparsed *)
    let unparsed =
      let to_remove = FilenameSet.union parsed deleted in
      FilenameSet.diff env.ServerEnv.unparsed to_remove |> FilenameSet.union unparsed_set
    in
    let cannot_skip_direct_dependents =
      resolved_requires_changed_in_commit_modules
      || resolved_requires_changed_in_reresolve_direct_dependents
      || deleted_count > 0
      || not (FilenameSet.is_empty unparsed_set)
    in

    Hh_logger.info "Updating index";
    let%lwt exports =
      Memory_utils.with_memory_timer_lwt ~options "Indexing" profiling (fun () ->
          Export_service.update
            ~workers
            ~reader
            ~update:new_or_changed
            ~remove:deleted
            env.ServerEnv.exports)
    in
    Hh_logger.info "Done updating index";

    let env = { env with ServerEnv.files = parsed; unparsed; dependency_info; exports } in
    let intermediate_values =
      ( deleted,
        direct_dependent_files,
        errors,
        files_to_force,
        freshparsed,
        new_or_changed,
        unchanged_checked,
        unchanged_files_to_force,
        unparsed_set,
        cannot_skip_direct_dependents )
    in
    Lwt.return (env, intermediate_values)

  let determine_what_to_recheck
      ~profiling
      ~options
      ~is_file_checked
      ~ide_open_files
      ~sig_dependency_graph
      ~implementation_dependency_graph
      ~checked_files
      ~freshparsed
      ~unparsed_set
      ~deleted
      ~unchanged_checked
      ~files_to_force
      ~unchanged_files_to_force
      ~direct_dependent_files =
    let%lwt (sig_dependent_files, all_dependent_files) =
      Memory_utils.with_memory_timer_lwt ~options "AllDependentFiles" profiling (fun () ->
          Lwt.return
            (Pure_dep_graph_operations.calc_all_dependents
               ~sig_dependency_graph
               ~implementation_dependency_graph
               direct_dependent_files))
    in
    let acceptable_files_to_focus =
      FilenameSet.union freshparsed (CheckedSet.all unchanged_files_to_force)
    in
    let%lwt (updated_checked_files, sig_dependent_files, all_dependent_files) =
      Memory_utils.with_memory_timer_lwt ~options "RecalcDepGraph" profiling (fun () ->
          match Options.lazy_mode options with
          | Options.NON_LAZY_MODE
          (* Non lazy mode treats every file as focused. *)
          | Options.LAZY_MODE_WATCHMAN
          (* Watchman mode treats every modified file as focused *)
          | Options.LAZY_MODE_FILESYSTEM ->
            (* FS mode treats every modified file as focused *)
            let old_focus_targets = CheckedSet.focused checked_files in
            let old_focus_targets = FilenameSet.diff old_focus_targets deleted in
            let old_focus_targets = FilenameSet.diff old_focus_targets unparsed_set in
            let focused = FilenameSet.union old_focus_targets freshparsed in
            unfocused_files_and_dependents_to_infer
              ~options
              ~input_focused:(FilenameSet.union focused (CheckedSet.focused files_to_force))
              ~input_dependencies:(Some (CheckedSet.dependencies files_to_force))
              ~sig_dependent_files
              ~all_dependent_files
          | Options.LAZY_MODE_IDE ->
            (* IDE mode only treats opened files as focused *)
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
             * *)
            let open_in_ide =
              let (lazy opened_files) = ide_open_files in
              FilenameSet.filter
                (function
                  | File_key.SourceFile fn
                  | File_key.LibFile fn
                  | File_key.JsonFile fn
                  | File_key.ResourceFile fn ->
                    SSet.mem fn opened_files
                  | File_key.Builtins -> false)
                freshparsed
            in
            let input_focused =
              CheckedSet.focused files_to_force
              (* Files to force to be focused *)
              |> filter_out_node_modules ~options
              (* Never focus node modules *)
              |> FilenameSet.union (CheckedSet.focused checked_files)
              (* old focused *)
              |> FilenameSet.union open_in_ide
              (* Files which are open in the IDE *)
            in
            let input_dependencies = Some (CheckedSet.dependencies files_to_force) in
            focused_files_and_dependents_to_infer
              ~is_file_checked
              ~implementation_dependency_graph
              ~sig_dependency_graph
              ~input_focused
              ~input_dependencies
              ~sig_dependent_files
              ~all_dependent_files)
    in
    (* Filter updated_checked_files down to the files which we just parsed or unchanged files which
     * will be focused *)
    let input =
      CheckedSet.filter updated_checked_files ~f:(fun fn ->
          FilenameSet.mem fn acceptable_files_to_focus)
    in
    let%lwt (to_merge, to_check, to_merge_or_check, components, recheck_set) =
      include_dependencies_and_dependents
        ~options
        ~profiling
        ~unchanged_checked
        ~input
        ~implementation_dependency_graph
        ~sig_dependency_graph
        ~sig_dependent_files
        ~all_dependent_files
    in
    Lwt.return
      (Determine_what_to_recheck_result
         {
           to_merge;
           to_check;
           to_merge_or_check;
           components;
           recheck_set;
           sig_dependent_files;
           all_dependent_files;
         })

  (* This function assumes it is called after recheck_parse_and_update_dependency_info. It uses some
   * of the info computed by recheck_parse_and_update_dependency_info to figure out which files to
   * merge. Then it merges them. *)
  let recheck_merge
      ~profiling
      ~transaction
      ~reader
      ~options
      ~workers
      ~will_be_checked_files
      ~file_watcher_metadata
      ~intermediate_values
      ~recheck_reasons
      ~env =
    let ( deleted,
          direct_dependent_files,
          errors,
          files_to_force,
          freshparsed,
          new_or_changed,
          unchanged_checked,
          unchanged_files_to_force,
          unparsed_set,
          cannot_skip_direct_dependents ) =
      intermediate_values
    in
    let dependency_info = env.ServerEnv.dependency_info in
    let implementation_dependency_graph =
      Dependency_info.implementation_dependency_graph dependency_info
    in
    let sig_dependency_graph = Dependency_info.sig_dependency_graph dependency_info in
    let is_file_checked =
      is_file_tracked_and_checked ~reader:(Abstract_state_reader.Mutator_state_reader reader)
    in
    let%lwt (Determine_what_to_recheck_result
              {
                to_merge;
                to_check;
                to_merge_or_check;
                components;
                recheck_set;
                sig_dependent_files;
                all_dependent_files;
              }) =
      determine_what_to_recheck
        ~profiling
        ~options
        ~is_file_checked
        ~ide_open_files:(lazy (Persistent_connection.get_opened_files env.ServerEnv.connections))
        ~sig_dependency_graph
        ~implementation_dependency_graph
        ~checked_files:env.ServerEnv.checked_files
        ~freshparsed
        ~unparsed_set
        ~deleted
        ~unchanged_checked
        ~files_to_force
        ~unchanged_files_to_force
        ~direct_dependent_files
    in
    (* This is a much better estimate of what checked_files will be after the merge finishes. We now
     * include the dependencies and dependents that are being implicitly included in the recheck. *)
    will_be_checked_files := CheckedSet.union env.ServerEnv.checked_files to_merge_or_check;

    let%lwt estimates =
      restart_if_faster_than_recheck ~options ~env ~to_merge_or_check ~file_watcher_metadata
    in
    let%lwt () =
      ensure_parsed_or_trigger_recheck
        ~options
        ~profiling
        ~workers
        ~reader
        (CheckedSet.all to_merge_or_check)
    in
    (* recheck *)
    let%lwt ( updated_errors,
              coverage,
              merge_skip_count,
              sig_new_or_changed,
              top_cycle,
              time_to_merge,
              merge_internal_error ) =
      let n = FilenameSet.cardinal all_dependent_files in
      if n > 0 then Hh_logger.info "recheck %d dependent files:" n;
      merge
        ~transaction
        ~reader
        ~options
        ~profiling
        ~workers
        ~errors
        ~coverage:env.ServerEnv.coverage
        ~to_merge
        ~components
        ~recheck_set
        ~sig_dependency_graph
        ~deleted
        ~unparsed_set
        ~recheck_reasons
    in
    Base.Option.iter merge_internal_error ~f:(Hh_logger.error "%s");

    let%lwt ( errors,
              coverage,
              time_to_check_merged,
              check_skip_count,
              slowest_file,
              num_slow_files,
              check_internal_error ) =
      Check_files.check_files
        ~reader
        ~options
        ~profiling
        ~workers
        ~errors
        ~updated_errors
        ~coverage
        ~to_check
        ~direct_dependent_files
        ~sig_new_or_changed
        ~dependency_info
        ~persistent_connections:(Some env.ServerEnv.connections)
        ~recheck_reasons
        ~cannot_skip_direct_dependents
    in
    Base.Option.iter check_internal_error ~f:(Hh_logger.error "%s");

    let%lwt () =
      Recheck_stats.record_recheck_time
        ~options
        ~total_time:(time_to_merge +. time_to_check_merged)
        ~rechecked_files:(CheckedSet.cardinal to_merge_or_check)
    in
    let checked_files = CheckedSet.union unchanged_checked to_merge_or_check in
    Hh_logger.info "Checked set: %s" (CheckedSet.debug_counts_to_string checked_files);

    (* NOTE: unused fields are left in their initial empty state *)
    env.ServerEnv.collated_errors := None;
    Lwt.return
      ( { env with ServerEnv.checked_files; errors; coverage },
        {
          new_or_changed;
          deleted;
          to_merge;
          to_check;
          sig_dependent_files;
          all_dependent_files;
          top_cycle;
          merge_skip_count;
          check_skip_count;
          slowest_file;
          num_slow_files;
          estimates;
        },
        Base.Option.first_some merge_internal_error check_internal_error )

  (* We maintain the following invariant across rechecks: The set of `files` contains files that
   * parsed successfully in the previous phase (which could be the init phase or a previous recheck
   * phase).
   *
   * This function has been split into two parts. This is because lazy saved state init needs to
   * update the dependency graph for files which have changed since the saved state was generated, but
   * doesn't want to merge those files yet.
   *)
  let full
      ~profiling
      ~transaction
      ~reader
      ~options
      ~workers
      ~updates
      ~files_to_force
      ~file_watcher_metadata
      ~recheck_reasons
      ~will_be_checked_files
      ~env =
    Transaction.add
      ~commit:(fun () ->
        (* We have to clear this at the end of the recheck, because it could have been populated with
         * now-out-of-date data in the middle of the recheck by parallelizable requests. *)
        Persistent_connection.clear_type_contents_caches ();
        Lwt.return_unit)
      ~rollback:(fun () -> Lwt.return_unit)
      transaction;

    let%lwt (env, intermediate_values) =
      try%lwt
        recheck_parse_and_update_dependency_info
          ~profiling
          ~transaction
          ~reader
          ~options
          ~workers
          ~updates
          ~files_to_force
          ~recheck_reasons
          ~env
      with Unexpected_file_changes changed_files -> handle_unexpected_file_changes changed_files
    in
    recheck_merge
      ~profiling
      ~transaction
      ~reader
      ~options
      ~workers
      ~will_be_checked_files
      ~file_watcher_metadata
      ~intermediate_values
      ~recheck_reasons
      ~env

  let parse_and_update_dependency_info
      ~profiling
      ~transaction
      ~reader
      ~options
      ~workers
      ~updates
      ~files_to_force
      ~recheck_reasons
      ~env =
    let%lwt (env, intermediate_values) =
      recheck_parse_and_update_dependency_info
        ~profiling
        ~transaction
        ~reader
        ~options
        ~workers
        ~updates
        ~files_to_force
        ~recheck_reasons
        ~env
    in
    let (_, _, errors, _, _, _, _, _, _, _) = intermediate_values in
    Lwt.return { env with ServerEnv.errors }
end

let with_transaction f =
  Transaction.with_transaction @@ fun transaction ->
  let reader = Mutator_state_reader.create transaction in
  f transaction reader

let recheck
    ~profiling
    ~options
    ~workers
    ~updates
    env
    ~files_to_force
    ~file_watcher_metadata
    ~recheck_reasons
    ~will_be_checked_files =
  let%lwt (env, stats, first_internal_error) =
    Memory_utils.with_memory_profiling_lwt ~profiling (fun () ->
        with_transaction (fun transaction reader ->
            Recheck.full
              ~profiling
              ~transaction
              ~reader
              ~options
              ~workers
              ~updates
              ~env
              ~files_to_force
              ~file_watcher_metadata
              ~recheck_reasons
              ~will_be_checked_files))
  in
  let {
    Recheck.new_or_changed = modified;
    deleted;
    to_merge;
    to_check;
    sig_dependent_files;
    all_dependent_files;
    top_cycle;
    merge_skip_count;
    check_skip_count;
    slowest_file;
    num_slow_files;
    estimates;
  } =
    stats
  in
  let ( estimated_time_to_recheck,
        estimated_time_to_restart,
        estimated_time_to_init,
        estimated_time_per_file,
        estimated_files_to_recheck,
        estimated_files_to_init ) =
    Base.Option.value_map
      estimates
      ~default:(None, None, None, None, None, None)
      ~f:(fun {
                Recheck_stats.estimated_time_to_recheck;
                estimated_time_to_restart;
                estimated_time_to_init;
                estimated_time_per_file;
                estimated_files_to_recheck;
                estimated_files_to_init;
              }
              ->
        ( Some estimated_time_to_recheck,
          Some estimated_time_to_restart,
          Some estimated_time_to_init,
          Some estimated_time_per_file,
          Some estimated_files_to_recheck,
          Some estimated_files_to_init ))
  in
  (* TODO: update log to reflect current terminology **)
  let log_recheck_event : profiling:Profiling_js.finished -> unit =
    FlowEventLogger.recheck
      ~recheck_reasons:(List.map LspProt.verbose_string_of_recheck_reason recheck_reasons)
      ~modified
      ~deleted
      ~to_merge
      ~to_check
      ~sig_dependent_files
      ~all_dependent_files
      ~merge_skip_count
      ~check_skip_count
      ~estimated_time_to_recheck
      ~estimated_time_to_restart
      ~estimated_time_to_init
      ~estimated_time_per_file
      ~estimated_files_to_recheck
      ~estimated_files_to_init
      ~slowest_file
      ~num_slow_files
      ~first_internal_error
      ~scm_update_distance:file_watcher_metadata.MonitorProt.total_update_distance
      ~scm_changed_mergebase:file_watcher_metadata.MonitorProt.changed_mergebase
  in

  let all_dependent_file_count = Utils_js.FilenameSet.cardinal all_dependent_files in
  let changed_file_count =
    Utils_js.FilenameSet.cardinal modified + Utils_js.FilenameSet.cardinal deleted
  in
  let summary_info =
    ServerStatus.RecheckSummary
      { dependent_file_count = all_dependent_file_count; changed_file_count; top_cycle }
  in
  Lwt.return (log_recheck_event, summary_info, env)

(* creates a closure that lists all files in the given root, returned in chunks *)
let make_next_files ~libs ~file_options root =
  let make_next_raw =
    Files.make_next_files ~root ~all:false ~subdir:None ~options:file_options ~libs
  in
  let total = ref 0 in
  fun () ->
    let files = make_next_raw () in
    let finished = !total in
    let length = List.length files in
    MonitorRPC.status_update ServerStatus.(Parsing_progress { finished; total = None });
    total := finished + length;

    files |> Base.List.map ~f:(Files.filename_from_string ~options:file_options) |> Bucket.of_list

let mk_init_env
    ~files ~unparsed ~package_json_files ~dependency_info ~ordered_libs ~libs ~errors ~exports =
  {
    ServerEnv.files;
    unparsed;
    dependency_info;
    checked_files = CheckedSet.empty;
    package_json_files;
    ordered_libs;
    libs;
    errors;
    coverage = FilenameMap.empty;
    collated_errors = ref None;
    connections = Persistent_connection.empty;
    exports;
  }

let init_from_saved_state ~profiling ~workers ~saved_state ~updates options =
  let%lwt (env, libs_ok) =
    with_transaction @@ fun transaction reader ->
    let file_options = Options.file_options options in
    (* We don't want to walk the file system for the checked in files. But we still need to find the
     * flowlibs *)
    let (ordered_flowlib_libs, _) = Files.init ~flowlibs_only:true file_options in
    let {
      Saved_state.flowconfig_hash = _;
      parsed_heaps;
      unparsed_heaps;
      package_heaps;
      ordered_non_flowlib_libs;
      local_errors;
      warnings;
      node_modules_containers;
      dependency_graph;
    } =
      saved_state
    in
    let root = Options.root options |> Path.to_string in
    Files.node_modules_containers := node_modules_containers;
    (* Restore PackageHeap and the ReversePackageHeap *)
    FilenameMap.iter (fun fn -> Module_js.add_package (File_key.to_string fn)) package_heaps;

    Hh_logger.info "Restoring heaps";
    let%lwt () =
      Memory_utils.with_memory_timer_lwt ~options "RestoreHeaps" profiling (fun () ->
          let%lwt () =
            MultiWorkerLwt.call
              workers
              ~job:
                (List.fold_left (fun () (fn, parsed_file_data) ->
                     let { Saved_state.hash; exports; resolved_requires } =
                       Saved_state.denormalize_file_data
                         ~root
                         parsed_file_data.Saved_state.normalized_file_data
                     in

                     (* Restore the FileHashHeap *)
                     Parsing_heaps.From_saved_state.add_file_hash fn hash;

                     (* Restore the ExportsHeap *)
                     Parsing_heaps.From_saved_state.add_exports fn exports;

                     (* Restore the ResolvedRequiresHeap *)
                     Module_heaps.From_saved_state.add_resolved_requires fn resolved_requires))
              ~merge:(fun () () -> ())
              ~neutral:()
              ~next:(MultiWorkerLwt.next workers parsed_heaps)
          in
          MultiWorkerLwt.call
            workers
            ~job:
              (List.fold_left (fun () (fn, unparsed_file_data) ->
                   (* Restore the FileHashHeap *)
                   let hash = unparsed_file_data.Saved_state.unparsed_hash in
                   Parsing_heaps.From_saved_state.add_file_hash fn hash))
            ~merge:(fun () () -> ())
            ~neutral:()
            ~next:(MultiWorkerLwt.next workers unparsed_heaps))
    in
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
    let%lwt (libs_ok, local_errors, warnings, suppressions, lib_exports) =
      let suppressions = Error_suppressions.empty in
      init_libs ~options ~profiling ~local_errors ~warnings ~suppressions ~reader ordered_libs
    in
    Hh_logger.info "Resolving dependencies";
    MonitorRPC.status_update ServerStatus.Resolving_dependencies_progress;

    let%lwt (parsed_set, unparsed_set, all_files, parsed, unparsed) =
      Memory_utils.with_memory_timer_lwt ~options "PrepareCommitModules" profiling (fun () ->
          let (parsed, parsed_set) =
            List.fold_left
              (fun (parsed, parsed_set) (fn, data) ->
                let parsed = (fn, data.Saved_state.info) :: parsed in
                let parsed_set = FilenameSet.add fn parsed_set in
                (parsed, parsed_set))
              ([], FilenameSet.empty)
              parsed_heaps
          in
          let (unparsed, unparsed_set) =
            List.fold_left
              (fun (unparsed, unparsed_set) (fn, data) ->
                let unparsed = (fn, data.Saved_state.unparsed_info) :: unparsed in
                let unparsed_set = FilenameSet.add fn unparsed_set in
                (unparsed, unparsed_set))
              ([], FilenameSet.empty)
              unparsed_heaps
          in
          let all_files = FilenameSet.union parsed_set unparsed_set in
          Lwt.return (parsed_set, unparsed_set, all_files, parsed, unparsed))
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
    let errors =
      { ServerEnv.local_errors; merge_errors = FilenameMap.empty; warnings; suppressions }
    in
    let dependency_info = Dependency_info.of_map dependency_graph in

    Hh_logger.info "Indexing files";
    let%lwt exports =
      Memory_utils.with_memory_timer_lwt ~options "Indexing" profiling (fun () ->
          Export_service.init ~workers ~reader ~libs:lib_exports parsed_set)
    in

    let env =
      mk_init_env
        ~files:parsed_set
        ~unparsed:unparsed_set
        ~package_json_files:(FilenameMap.keys package_heaps)
        ~dependency_info
        ~ordered_libs
        ~libs
        ~errors
        ~exports
    in
    Lwt.return (env, libs_ok)
  in

  let should_force_recheck = Options.saved_state_force_recheck options in
  (* We know that all the files in updates have changed since the saved state was generated. We
    * have two ways to deal with them: *)
  if Options.lazy_mode options = Options.NON_LAZY_MODE || should_force_recheck then
    (* In non-lazy mode, we return updates here. They will immediately be rechecked. Due to
      * fanout, this can be a huge recheck, but it's sound.
      *
      * We'll also hit this code path in lazy modes if the user has passed
      * --saved-state-force-recheck. These users want to force Flow to recheck all the files that
      * have changed since the saved state was generated*)
    Lwt.return (updates, env, libs_ok)
  else
    (* In lazy mode, we try to avoid the fanout problem. All we really want to do in lazy mode
      * is to update the dependency graph and stuff like that. We don't actually want to merge
      * anything yet. *)
    let recheck_reasons = [LspProt.Lazy_init_update_deps] in
    let%lwt env =
      let rec try_update updated_files =
        try%lwt
          with_transaction @@ fun transaction reader ->
          Recheck.parse_and_update_dependency_info
            ~profiling
            ~transaction
            ~reader
            ~options
            ~workers
            ~updates:updated_files
            ~files_to_force:CheckedSet.empty
            ~recheck_reasons
            ~env
        with Unexpected_file_changes changed_files ->
          let updated_files =
            changed_files |> Nel.to_list |> FilenameSet.of_list |> FilenameSet.union updated_files
          in
          try_update updated_files
      in
      try_update updates
    in
    Lwt.return (FilenameSet.empty, env, libs_ok)

let init_from_scratch ~profiling ~workers options =
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
  let (ordered_libs, libs) = Files.init file_options in
  let next_files = make_next_files ~libs ~file_options (Options.root options) in
  Hh_logger.info "Parsing";
  MonitorRPC.status_update ServerStatus.(Parsing_progress { finished = 0; total = None });
  let%lwt (parsed, unparsed, unchanged, local_errors, (package_json_files, package_json_errors)) =
    parse ~options ~profiling ~workers ~reader next_files
  in
  (* Parsing won't raise warnings *)
  let warnings = FilenameMap.empty in
  assert (FilenameSet.is_empty unchanged);

  let package_errors =
    List.fold_left
      (fun errors parse_err ->
        let filename = Base.Option.value_exn (Loc.source (fst parse_err)) in
        let errset = Inference_utils.set_of_package_json_error ~source_file:filename parse_err in
        update_errset errors filename errset)
      FilenameMap.empty
      package_json_errors
  in
  let local_errors = merge_error_maps package_errors local_errors in
  Hh_logger.info "Loading libraries";
  let%lwt (libs_ok, local_errors, warnings, suppressions, lib_exports) =
    let suppressions = Error_suppressions.empty in
    init_libs ~options ~profiling ~local_errors ~warnings ~suppressions ~reader ordered_libs
  in
  Hh_logger.info "Resolving dependencies";
  MonitorRPC.status_update ServerStatus.Resolving_dependencies_progress;

  let (all_files, unparsed_set) =
    List.fold_left
      (fun (all_files, unparsed_set) (filename, _) ->
        (FilenameSet.add filename all_files, FilenameSet.add filename unparsed_set))
      (parsed, FilenameSet.empty)
      unparsed
  in
  let all_providers_mutator = Module_hashtables.All_providers_mutator.create transaction in
  let%lwt (_, _, errors) =
    let errors =
      { ServerEnv.local_errors; merge_errors = FilenameMap.empty; warnings; suppressions }
    in
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
  let%lwt dependency_info =
    Memory_utils.with_memory_timer_lwt ~options "CalcDepsTypecheck" profiling (fun () ->
        Dep_service.calc_dependency_info ~options ~reader workers ~parsed)
  in

  Hh_logger.info "Indexing files";
  let%lwt exports =
    Memory_utils.with_memory_timer_lwt ~options "Indexing" profiling (fun () ->
        Export_service.init ~workers ~reader ~libs:lib_exports parsed)
  in
  Hh_logger.info "Done";

  let env =
    mk_init_env
      ~files:parsed
      ~unparsed:unparsed_set
      ~package_json_files
      ~dependency_info
      ~ordered_libs
      ~libs
      ~errors
      ~exports
  in
  Lwt.return (FilenameSet.empty, env, libs_ok)

let exit_if_no_fallback ?msg options =
  if Options.saved_state_no_fallback options then
    FlowExitStatus.exit ?msg FlowExitStatus.Invalid_saved_state

(* Does a best-effort job to load a saved state. If it fails, returns None *)
let load_saved_state ~profiling ~workers options =
  let%lwt (fetch_profiling, fetch_result) =
    match Options.saved_state_fetcher options with
    | Options.Dummy_fetcher -> Saved_state_dummy_fetcher.fetch ~options
    | Options.Local_fetcher -> Saved_state_local_fetcher.fetch ~options
    | Options.Fb_fetcher -> Saved_state_fb_fetcher.fetch ~options
  in
  Profiling_js.merge ~from:fetch_profiling ~into:profiling;
  match fetch_result with
  | Saved_state_fetcher.No_saved_state ->
    Hh_logger.info "No saved state available";
    Lwt.return_none
  | Saved_state_fetcher.Saved_state_error ->
    Hh_logger.info "Failed to load saved state";
    exit_if_no_fallback options;
    Lwt.return_none
  | Saved_state_fetcher.Saved_state { saved_state_filename; changed_files } ->
    let changed_files_count = SSet.cardinal changed_files in
    (try%lwt
       let%lwt (load_profiling, saved_state) =
         Saved_state.load ~workers ~saved_state_filename ~options
       in
       Profiling_js.merge load_profiling profiling;

       let updates =
         Recheck_updates.process_updates
           ~options
           ~libs:(SSet.of_list saved_state.Saved_state.ordered_non_flowlib_libs)
           changed_files
       in
       let updates =
         match updates with
         | Base.Result.Error { Recheck_updates.msg; _ } ->
           Hh_logger.error "The saved state is no longer valid due to file changes: %s" msg;
           raise Saved_state.(Invalid_saved_state Changed_files)
         | Base.Result.Ok updates -> updates
       in
       Hh_logger.info
         "Saved state script reports %d files changed & we care about %d of them"
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
       let msg = spf "Failed to load saved state: %s" invalid_reason in
       exit_if_no_fallback ~msg options;
       Lwt.return_none)

let init ~profiling ~workers options =
  let start_time = Unix.gettimeofday () in
  let%lwt (updates, env, libs_ok) =
    match%lwt load_saved_state ~profiling ~workers options with
    | None ->
      (* Either there is no saved state or we failed to load it for some reason *)
      init_from_scratch ~profiling ~workers options
    | Some (saved_state, updates) ->
      (* We loaded a saved state successfully! We are awesome! *)
      init_from_saved_state ~profiling ~workers ~saved_state ~updates options
  in
  let init_time = Unix.gettimeofday () -. start_time in
  let%lwt last_estimates =
    Recheck_stats.init ~options ~init_time ~parsed_count:(FilenameSet.cardinal env.ServerEnv.files)
  in
  (* Don't recheck if the libs are not ok *)
  if FilenameSet.is_empty updates || not libs_ok then
    Lwt.return (libs_ok, env, last_estimates)
  else
    let files_to_force = CheckedSet.empty in
    let recheck_reasons = [LspProt.Lazy_init_typecheck] in
    let%lwt (recheck_profiling, (log_recheck_event, _summary_info, env)) =
      let should_print_summary = Options.should_profile options in
      Profiling_js.with_profiling_lwt ~label:"Recheck" ~should_print_summary (fun profiling ->
          recheck
            ~profiling
            ~options
            ~workers
            ~updates
            env
            ~files_to_force
            ~file_watcher_metadata:MonitorProt.empty_file_watcher_metadata
            ~recheck_reasons
            ~will_be_checked_files:(ref files_to_force))
    in
    log_recheck_event ~profiling:recheck_profiling;
    Profiling_js.merge ~from:recheck_profiling ~into:profiling;
    Lwt.return (true, env, last_estimates)

let full_check ~profiling ~options ~workers ?focus_targets env =
  let { ServerEnv.files = parsed; dependency_info; errors; _ } = env in
  with_transaction (fun transaction reader ->
      let%lwt (input, sig_dependent_files, all_dependent_files) =
        files_to_infer ~options ~reader ?focus_targets ~profiling ~parsed ~dependency_info
      in
      let implementation_dependency_graph =
        Dependency_info.implementation_dependency_graph dependency_info
      in
      let sig_dependency_graph = Dependency_info.sig_dependency_graph dependency_info in
      let%lwt (to_merge, to_check, to_merge_or_check, components, recheck_set) =
        include_dependencies_and_dependents
          ~options
          ~profiling
          ~unchanged_checked:CheckedSet.empty
          ~input
          ~implementation_dependency_graph
          ~sig_dependency_graph
          ~sig_dependent_files
          ~all_dependent_files
      in
      (* The values to_merge and recheck_set are essentially the same as input, aggregated. This
         is not surprising because files_to_infer returns a closed checked set. Thus, the only purpose
         of calling include_dependencies_and_dependents is to compute components. *)
      let%lwt () =
        ensure_parsed_or_trigger_recheck
          ~options
          ~profiling
          ~workers
          ~reader
          (CheckedSet.all to_merge_or_check)
      in
      let recheck_reasons = [LspProt.Full_init] in
      let%lwt (updated_errors, coverage, _, sig_new_or_changed, _, _, merge_internal_error) =
        merge
          ~transaction
          ~reader
          ~options
          ~profiling
          ~workers
          ~errors
          ~coverage:env.ServerEnv.coverage
          ~to_merge
          ~components
          ~recheck_set
          ~sig_dependency_graph
          ~deleted:FilenameSet.empty
          ~unparsed_set:FilenameSet.empty
          ~recheck_reasons
      in
      Base.Option.iter merge_internal_error ~f:(Hh_logger.error "%s");

      let%lwt (errors, coverage, _, _, _, _, check_internal_error) =
        Check_files.check_files
          ~reader
          ~options
          ~profiling
          ~workers
          ~errors
          ~updated_errors
          ~coverage
          ~to_check
          ~direct_dependent_files:FilenameSet.empty
          ~sig_new_or_changed
          ~dependency_info
          ~persistent_connections:None
          ~recheck_reasons
          ~cannot_skip_direct_dependents:true
      in
      Base.Option.iter check_internal_error ~f:(Hh_logger.error "%s");

      let first_internal_error = Base.Option.first_some merge_internal_error check_internal_error in
      let checked_files = to_merge_or_check in
      Hh_logger.info "Checked set: %s" (CheckedSet.debug_counts_to_string checked_files);
      Lwt.return ({ env with ServerEnv.checked_files; errors; coverage }, first_internal_error))

let debug_determine_what_to_recheck = Recheck.determine_what_to_recheck

let debug_include_dependencies_and_dependents = include_dependencies_and_dependents
