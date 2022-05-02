(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module drives the type checker *)

open Utils_js

(****************** typecheck job helpers *********************)

let clear_errors files errors =
  let { ServerEnv.local_errors; duplicate_providers; merge_errors; warnings; suppressions } =
    errors
  in
  let (local_errors, merge_errors, warnings, suppressions) =
    FilenameSet.fold
      (fun file (local_errors, merge_errors, warnings, suppressions) ->
        ( FilenameMap.remove file local_errors,
          FilenameMap.remove file merge_errors,
          FilenameMap.remove file warnings,
          Error_suppressions.remove file suppressions
        ))
      files
      (local_errors, merge_errors, warnings, suppressions)
  in
  { ServerEnv.local_errors; duplicate_providers; merge_errors; warnings; suppressions }

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

let merge_error_maps = FilenameMap.union ~combine:(fun _ x y -> Some (Flow_error.ErrorSet.union x y))

let collate_parse_results parse_results =
  let {
    Parsing_service_js.parsed;
    unparsed;
    changed;
    failed = (failed, errors);
    unchanged;
    not_found;
    package_json;
    dirty_modules;
  } =
    parse_results
  in
  (* No one who is calling collate_parse_results is skipping files with hash mismatches *)
  assert (FilenameSet.is_empty changed);
  let local_errors =
    List.fold_left2
      (fun errors file error ->
        let errset =
          match error with
          | Parsing_service_js.Uncaught_exception exn ->
            Inference_utils.set_of_parse_exception ~source_file:file exn
          | Parsing_service_js.Parse_error err ->
            Inference_utils.set_of_parse_error ~source_file:file err
          | Parsing_service_js.Docblock_errors errs ->
            Inference_utils.set_of_docblock_errors ~source_file:file errs
        in
        update_errset errors file errset)
      FilenameMap.empty
      failed
      errors
  in
  let unparsed = FilenameSet.union (FilenameSet.of_list failed) unparsed in
  (parsed, unparsed, unchanged, not_found, dirty_modules, local_errors, package_json)

let parse ~options ~profiling ~workers ~reader parse_next =
  Memory_utils.with_memory_timer_lwt ~options "Parsing" profiling (fun () ->
      let%lwt results = Parsing_service_js.parse_with_defaults ~reader options workers parse_next in
      Lwt.return (collate_parse_results results)
  )

let reparse ~options ~profiling ~transaction ~reader ~workers ~modified =
  Memory_utils.with_memory_timer_lwt ~options "Parsing" profiling (fun () ->
      let%lwt results =
        Parsing_service_js.reparse_with_defaults
          ~transaction
          ~reader
          ~with_progress:true
          ~workers
          ~modified
          options
      in
      Lwt.return (collate_parse_results results)
  )

let commit_modules ~transaction ~options ~profiling ~workers ~duplicate_providers dirty_modules =
  Memory_utils.with_memory_timer_lwt ~options "CommitModules" profiling (fun () ->
      (* Clear duplicate provider errors for all dirty modules. *)
      let duplicate_providers =
        (* Avoid iterating over dirty modules when there are no duplicate
         * providers. This is most useful on init, when all modules are dirty,
         * but should also be true for most rechecks. *)
        if SMap.is_empty duplicate_providers then
          duplicate_providers
        else
          Modulename.Set.fold
            (fun m acc ->
              match m with
              | Modulename.String m -> SMap.remove m acc
              | Modulename.Filename _ -> acc)
            dirty_modules
            duplicate_providers
      in
      let%lwt (changed_modules, new_duplicate_providers) =
        Module_js.commit_modules ~transaction ~workers ~options dirty_modules
      in
      Lwt.return (changed_modules, SMap.union duplicate_providers new_duplicate_providers)
  )

module DirectDependentFilesCache : sig
  val clear : unit -> unit

  val with_cache :
    cache_key:FilenameSet.t -> on_miss:FilenameSet.t Lwt.t Lazy.t -> FilenameSet.t Lwt.t
end = struct
  let max_size = 100

  let cache : FilenameSet.t FilenameCache.t = FilenameCache.make ~max_size

  let clear () = FilenameCache.clear cache

  let with_cache ~cache_key ~on_miss =
    match FilenameSet.elements cache_key with
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
  let mutator = Parsing_heaps.Resolved_requires_mutator.create transaction parsed_set in
  let%lwt resolved_requires_changed =
    Memory_utils.with_memory_timer_lwt ~options "ResolveRequires" profiling (fun () ->
        MultiWorkerLwt.call
          workers
          ~job:
            (List.fold_left (fun acc filename ->
                 let changed =
                   Module_js.add_parsed_resolved_requires
                     filename
                     ~mutator
                     ~reader
                     ~options
                     ~node_modules_containers
                 in
                 acc || changed
             )
            )
          ~neutral:false
          ~merge:( || )
          ~next:(MultiWorkerLwt.next workers parsed)
    )
  in
  clear_cache_if_resolved_requires_changed resolved_requires_changed;
  Lwt.return resolved_requires_changed

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
      Lwt.return (sig_dependency_graph, component_map)
  )

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
        (to_merge, to_check, to_merge_or_check, components, CheckedSet.all definitely_to_merge)
  )

let update_first_internal_error first_internal_error (loc, internal_error) =
  match first_internal_error with
  | Some _ -> first_internal_error
  | None ->
    Some
      (spf
         "%s\n%s"
         (ALoc.debug_to_string ~include_source:true loc)
         (Error_message.string_of_internal_error internal_error)
      )

let add_internal_error errors file (loc, internal_error) =
  let new_errors = error_set_of_internal_error file (loc, internal_error) in
  update_errset errors file new_errors

let update_merge_results acc component result =
  match result with
  | None -> acc
  | Some (suppressions, _duration) ->
    let acc = Nel.fold_left (fun acc file -> Error_suppressions.remove file acc) acc component in
    Error_suppressions.update_suppressions acc suppressions

let update_slow_files acc file check_time =
  if check_time > 1. then
    let (num_slow_files, slowest_time, slowest_file) = acc in
    let (slowest_time, slowest_file) =
      if check_time > slowest_time then
        (check_time, Some file)
      else
        (slowest_time, slowest_file)
    in
    (num_slow_files + 1, slowest_time, slowest_file)
  else
    acc

let update_check_results (acc, slow_files) (file, result) =
  let (errors, warnings, suppressions, coverage, first_internal_error) = acc in
  let errors = FilenameMap.remove file errors in
  let warnings = FilenameMap.remove file warnings in
  let suppressions = Error_suppressions.remove file suppressions in
  let coverage = FilenameMap.remove file coverage in
  match result with
  | Ok None -> (acc, slow_files)
  | Ok (Some (new_errors, new_warnings, new_suppressions, new_coverage, check_time)) ->
    let errors = update_errset errors file new_errors in
    let warnings = update_errset warnings file new_warnings in
    let suppressions = Error_suppressions.update_suppressions suppressions new_suppressions in
    let coverage = FilenameMap.add file new_coverage coverage in
    let slow_files = update_slow_files slow_files file check_time in
    ((errors, warnings, suppressions, coverage, first_internal_error), slow_files)
  | Error e ->
    let first_internal_error = update_first_internal_error first_internal_error e in
    let errors = add_internal_error errors file e in
    ((errors, warnings, suppressions, coverage, first_internal_error), slow_files)

let run_merge_service
    ~master_mutator
    ~worker_mutator
    ~reader
    ~options
    ~profiling
    ~workers
    ~sig_dependency_graph
    ~component_map
    ~recheck_set
    suppressions =
  Memory_utils.with_memory_timer_lwt ~options "Merge" profiling (fun () ->
      let%lwt (results, { Merge_service.skipped_count; sig_new_or_changed }) =
        Merge_service.merge
          ~master_mutator
          ~worker_mutator
          ~reader
          ~options
          ~workers
          ~sig_dependency_graph
          ~component_map
          ~recheck_set
      in
      let suppressions =
        List.fold_left
          (fun acc (file, _diff, result) ->
            let component = FilenameMap.find file component_map in
            update_merge_results acc component result)
          suppressions
          results
      in
      Lwt.return (suppressions, skipped_count, sig_new_or_changed)
  )

let mk_intermediate_result_callback
    ~reader ~options ~persistent_connections ~recheck_reasons suppressions =
  let loc_of_aloc = Parsing_heaps.Mutator_reader.loc_of_aloc ~reader in
  let send_errors_over_connection =
    match persistent_connections with
    | None -> (fun _ -> ())
    | Some clients ->
      (* In types-first, we have already accumulated suppressions in the overall
         merge step, and each check step uses those suppressions to filter the
         errors and warnings uncovered. *)
      let open Errors in
      let curr_errors = ref ConcreteLocPrintableErrorSet.empty in
      let curr_warnings = ref ConcreteLocPrintableErrorSet.empty in
      let root = Options.root options in
      let file_options = Some (Options.file_options options) in
      let filter = Error_suppressions.filter_suppressed_errors ~root ~file_options in
      fun (lazy results) ->
        let (new_errors, new_warnings) =
          List.fold_left
            (fun (errs_acc, warns_acc) result ->
              let (file, old_errs, old_warns) = result in
              (* Filter errors and warnings.
               * TODO: track unused suppressions *)
              let (errs, _, _) = filter suppressions old_errs ~unused:Error_suppressions.empty in
              let (warns, _, _) = filter suppressions old_warns ~unused:Error_suppressions.empty in
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
              (errs_acc, warns_acc))
            (ConcreteLocPrintableErrorSet.empty, FilenameMap.empty)
            results
        in
        curr_errors := ConcreteLocPrintableErrorSet.union new_errors !curr_errors;
        curr_warnings :=
          FilenameMap.fold (fun _ -> ConcreteLocPrintableErrorSet.union) new_warnings !curr_warnings;

        if
          not (ConcreteLocPrintableErrorSet.is_empty new_errors && FilenameMap.is_empty new_warnings)
        then
          let errors_reason = LspProt.Recheck_streaming { recheck_reasons } in
          Persistent_connection.update_clients
            ~clients
            ~errors_reason
            ~calc_errors_and_warnings:(fun () -> (new_errors, new_warnings)
          )
  in
  let intermediate_result_callback results =
    let errors =
      lazy
        (Base.List.fold_left
           ~f:(fun acc (file, result) ->
             match result with
             | Ok None -> acc
             | Ok (Some (errors, warnings, _, _, _)) ->
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
               (file, errors, warnings) :: acc
             | Error msg ->
               let errors = error_set_of_internal_error file msg in
               let errors =
                 errors
                 |> Flow_error.concretize_errors loc_of_aloc
                 |> Flow_error.make_errors_printable
               in
               let warnings = Errors.ConcreteLocPrintableErrorSet.empty in
               (file, errors, warnings) :: acc)
           ~init:[]
           results
        )
    in
    send_errors_over_connection errors
  in
  intermediate_result_callback

(* This function does some last minute preparation and then calls into the merge service, which
 * typechecks the code. By the time this function is called, we know exactly what we want to merge
 * (though we may later decline to typecheck some files due to recheck optimizations) *)
let merge
    ~transaction
    ~reader
    ~options
    ~profiling
    ~workers
    ~suppressions
    ~to_merge
    ~components
    ~recheck_set
    ~sig_dependency_graph
    ~deleted
    ~unparsed_set =
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
  let%lwt ((suppressions, skipped_count, sig_new_or_changed), time_to_merge) =
    let (master_mutator, worker_mutator) =
      Parsing_heaps.Merge_context_mutator.create
        transaction
        (FilenameSet.union files_to_merge deleted |> FilenameSet.union unparsed_set)
    in
    let merge_start_time = Unix.gettimeofday () in
    let%lwt result =
      run_merge_service
        ~master_mutator
        ~worker_mutator
        ~reader
        ~options
        ~profiling
        ~workers
        ~sig_dependency_graph
        ~component_map
        ~recheck_set
        suppressions
    in
    let%lwt () =
      if Options.should_profile options then
        Memory_utils.with_memory_timer_lwt ~options "PrintGCStats" profiling (fun () ->
            Lwt.return (Gc.print_stat stderr)
        )
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
  Lwt.return (suppressions, skipped_count, sig_new_or_changed, top_cycle, time_to_merge)

module Check_files : sig
  val check_files :
    reader:Parsing_heaps.Mutator_reader.reader ->
    options:Options.t ->
    profiling:Profiling_js.running ->
    workers:MultiWorkerLwt.worker list option ->
    errors:ServerEnv.errors ->
    updated_suppressions:Error_suppressions.t ->
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
    * int
    * string option
    )
    Lwt.t
end = struct
  open Job_utils

  let check_files
      ~reader
      ~options
      ~profiling
      ~workers
      ~errors
      ~updated_suppressions
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
        let implementation_dependency_graph =
          Dependency_info.implementation_dependency_graph dependency_info
        in
        (* skip dependents whenever none of their dependencies have new or changed signatures *)
        let dependents_to_check =
          FilenameSet.filter
            (fun f ->
              (cannot_skip_direct_dependents && FilenameSet.mem f direct_dependent_files)
              || FilenameSet.exists (fun f' -> FilenameSet.mem f' sig_new_or_changed)
                 @@ FilenameGraph.find f implementation_dependency_graph
              ||
              ( incr skipped_count;
                false
              ))
            merged_dependents
        in
        Hh_logger.info
          "Check will skip %d of %d files"
          !skipped_count
          (* We can just add these counts without worrying about files which are in both sets. We
           * got these both from a CheckedSet. CheckedSet's representation ensures that a single
           * file cannot have more than one kind. *)
          (FilenameSet.cardinal focused_to_check + FilenameSet.cardinal merged_dependents);
        let files = FilenameSet.union focused_to_check dependents_to_check in
        let intermediate_result_callback =
          mk_intermediate_result_callback
            ~reader
            ~options
            ~persistent_connections
            ~recheck_reasons
            updated_suppressions
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
        let post_check _ x = x in
        let%lwt ret =
          MultiWorkerLwt.call
            workers
            ~job:(job ~post_check ~reader ~options)
            ~neutral:[]
            ~merge
            ~next
        in
        let { ServerEnv.merge_errors; warnings; _ } = errors in
        let ((merge_errors, warnings, suppressions, coverage, first_internal_error), slow_files) =
          List.fold_left
            update_check_results
            ((merge_errors, warnings, updated_suppressions, coverage, None), (0, 0., None))
            ret
        in
        let (num_slow_files, _, slowest_file) = slow_files in
        let time_to_check_merged = Unix.gettimeofday () -. check_start_time in
        Hh_logger.info "Done";
        let errors = { errors with ServerEnv.merge_errors; warnings; suppressions } in
        Lwt.return
          ( errors,
            coverage,
            time_to_check_merged,
            !skipped_count,
            Base.Option.map ~f:File_key.to_string slowest_file,
            num_slow_files,
            Base.Option.map first_internal_error ~f:(spf "First check internal error:\n%s")
          )
    )
end

exception Unexpected_file_changes of File_key.t Nel.t

let handle_unexpected_file_changes changed_files =
  let filename_set =
    Nel.fold_left (fun acc file -> SSet.add (File_key.to_string file) acc) SSet.empty changed_files
  in
  let file_count = SSet.cardinal filename_set in
  let reason =
    if file_count = 1 then
      LspProt.Single_file_changed { filename = SSet.choose filename_set }
    else
      LspProt.Many_files_changed { file_count }
  in
  Hh_logger.info "Canceling recheck due to %d unexpected file changes" file_count;
  ServerMonitorListenerState.push_files_to_prioritize ~reason filename_set;
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
      | hd :: tl -> raise (Unexpected_file_changes (hd, tl))
  )

let ensure_parsed_or_trigger_recheck ~options ~profiling ~workers ~reader files =
  try%lwt ensure_parsed ~options ~profiling ~workers ~reader files with
  | Unexpected_file_changes changed_files -> handle_unexpected_file_changes changed_files

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
          exports
        )
  )

(** Given a set of focused files and a dependency graph, calculates the recursive dependents and
    returns a CheckedSet containing both the focused and dependent files. *)
let focused_files_to_infer ~implementation_dependency_graph ~sig_dependency_graph focused =
  let (_sig_dependents, roots) =
    Pure_dep_graph_operations.calc_all_dependents
      ~sig_dependency_graph
      ~implementation_dependency_graph
      focused
  in
  (* [roots] is the set of all focused files and all dependent files.
     CheckedSet will ignore the focused files in [dependents] since they are
     also passed via [focused] and duplicates take the highest priority. *)
  let dependents = roots in
  CheckedSet.add ~focused ~dependents CheckedSet.empty

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
  let dependencies = input_dependencies in
  CheckedSet.add ~focused ~dependencies CheckedSet.empty

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
let files_to_infer ~options ~profiling ~dependency_info ~focus_targets ~parsed =
  Memory_utils.with_memory_timer_lwt ~options "FilesToInfer" profiling (fun () ->
      match focus_targets with
      | None ->
        let to_infer =
          unfocused_files_to_infer
            ~options
            ~input_focused:parsed
            ~input_dependencies:FilenameSet.empty
        in
        Lwt.return to_infer
      | Some input_focused ->
        let implementation_dependency_graph =
          Dependency_info.implementation_dependency_graph dependency_info
        in
        let sig_dependency_graph = Dependency_info.sig_dependency_graph dependency_info in
        (* Only focus files that parsed successfully. Parsed files are also
         * necessarily checked because we only parse checked files, so this also
         * serves to filter the input to checked files. *)
        let input_focused = FilenameSet.inter input_focused parsed in
        let to_infer =
          focused_files_to_infer
            ~implementation_dependency_graph
            ~sig_dependency_graph
            input_focused
        in
        Lwt.return to_infer
  )

let restart_if_faster_than_recheck ~options ~env ~to_merge_or_check ~file_watcher_metadata =
  match Options.lazy_mode options with
  | false ->
    (* it's never faster to do a full init than a recheck *)
    Lwt.return_none
  | true ->
    let { MonitorProt.changed_mergebase; missed_changes = _ } = file_watcher_metadata in
    (match changed_mergebase with
    | None ->
      (* Not tracking the mergebase, so we don't know one way or the other *)
      Lwt.return_none
    | Some false ->
      Hh_logger.info "File watcher did not change mergebase";
      Lwt.return_none
    | Some true ->
      Hh_logger.info "File watcher changed mergebase";
      (* TODO (glevi) - One of the numbers we need to estimate is "If we restart how many files
       * would we check". Currently we're looking at the number of already checked files. But a
       * better way would be to
       *
       * 1. When watchman notices the mergebase changing, also record the files which have changed
       *    since the mergebase
       * 2. Send these files to the server
       * 3. Calculate the fanout of these files (we should have an updated dependency graph by now)
       * 4. That should actually be the right number, instead of just an estimate. But it costs
       *    a little to compute the fanout
       *
       * This also treats dependencies, which are only merged, equally with focused/dependent
       * files which are also checked. So, doing a priority update, which merges but doesn't
       * check, throws off the estimate by making it seem like files get checked as quickly
       * as they get merged. This makes us underestimate how long it will take to recheck. *)
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
          Exit.(exit ~msg:"Restarting after a rebase to save time" Restart)
        else
          Lwt.return_unit
      in
      Lwt.return (Some estimates))

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
    num_slow_files: int;
    estimates: Recheck_stats.estimates option;
  }

  val full :
    profiling:Profiling_js.running ->
    transaction:Transaction.t ->
    reader:Parsing_heaps.Mutator_reader.reader ->
    options:Options.t ->
    workers:MultiWorkerLwt.worker list option ->
    updates:CheckedSet.t ->
    files_to_force:CheckedSet.t ->
    file_watcher_metadata:MonitorProt.file_watcher_metadata ->
    recheck_reasons:LspProt.recheck_reason list ->
    will_be_checked_files:CheckedSet.t ref ->
    env:ServerEnv.env ->
    (ServerEnv.env * recheck_result * ((* record_recheck_time *) unit -> unit Lwt.t) * string option)
    Lwt.t

  (* Raises `Unexpected_file_changes` if it finds files unexpectedly changed when parsing. *)
  val parse_and_update_dependency_info :
    profiling:Profiling_js.running ->
    transaction:Transaction.t ->
    reader:Parsing_heaps.Mutator_reader.reader ->
    options:Options.t ->
    workers:MultiWorkerLwt.worker list option ->
    updates:CheckedSet.t ->
    files_to_force:CheckedSet.t ->
    recheck_reasons:LspProt.recheck_reason list ->
    env:ServerEnv.env ->
    ServerEnv.env Lwt.t

  (* Exposed only for testing purposes. Not meant for general consumption. *)
  val determine_what_to_recheck :
    profiling:Profiling_js.running ->
    options:Options.t ->
    sig_dependency_graph:FilenameGraph.t ->
    implementation_dependency_graph:FilenameGraph.t ->
    freshparsed:CheckedSet.t ->
    unchanged_checked:CheckedSet.t ->
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
    num_slow_files: int;
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
      ~(updates : CheckedSet.t)
      ~files_to_force
      ~recheck_reasons
      ~env =
    let loc_of_aloc = Parsing_heaps.Mutator_reader.loc_of_aloc ~reader in
    let errors = env.ServerEnv.errors in
    (* files_to_force is a request to promote certain files to be checked as a dependency, dependent,
     * or focused file. We can ignore a request if the file is already checked at the desired level
     * or at a more important level *)
    let files_to_force = CheckedSet.diff files_to_force env.ServerEnv.checked_files in

    Hh_logger.info "Parsing";

    (* Reparse updates. Parsing splits the updates into the following
     * disjoint collections:
     *
     * parsed: files which should be typechecked and were successfully parsed
     * unparsed: files which either should not be parsed (resource files) or
     *   checked (not @flow), or which failed to parse
     * unchanged: files which have not actually changed since we last parsed
     *   them, and therefore do not need to be rechecked
     * deleted: files which could not be found on disk, for which we may have
     *   data that needs to be invalidated
     *
     * Parsing also gives us new local_errors, including parse errors, docblock
     * errors, etc. *)
    let%lwt (parsed_set, unparsed_set, unchanged_parse, deleted, dirty_modules, new_local_errors, _)
        =
      let modified = CheckedSet.all updates in
      reparse ~options ~profiling ~transaction ~reader ~workers ~modified
    in

    (* parsed + unparsed = new or changed files *)
    let new_or_changed = FilenameSet.union parsed_set unparsed_set in
    let new_or_changed_or_deleted = FilenameSet.union new_or_changed deleted in

    (* turn [parsed_set] into a CheckedSet with the same priorities as they came in from [updates] *)
    let freshparsed =
      CheckedSet.filter ~f:(fun file _ -> FilenameSet.mem file parsed_set) updates
    in

    (* clear errors for new, changed and deleted files *)
    let { ServerEnv.local_errors; duplicate_providers; merge_errors; warnings; suppressions } =
      clear_errors new_or_changed_or_deleted errors
    in
    (* clear stale coverage info *)
    let coverage =
      FilenameSet.fold FilenameMap.remove new_or_changed_or_deleted env.ServerEnv.coverage
    in

    (* record reparse errors *)
    let local_errors =
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
      merge_error_maps new_local_errors local_errors
    in

    (* get old (unchanged, undeleted) files that were parsed successfully *)
    let old_parsed = env.ServerEnv.files in
    let unchanged = FilenameSet.diff old_parsed new_or_changed_or_deleted in

    let deleted_count = FilenameSet.cardinal deleted in
    let modified_count = FilenameSet.cardinal new_or_changed in
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
      if modified_count > 0 then log_files new_or_changed "modified" modified_count;
      if deleted_count > 0 then log_files deleted "deleted" deleted_count
    );

    Hh_logger.debug
      "recheck: old = %d, del = %d, fresh = %d, unmod = %d"
      (FilenameSet.cardinal old_parsed)
      (FilenameSet.cardinal deleted)
      (CheckedSet.cardinal freshparsed)
      (FilenameSet.cardinal unchanged);

    (* "updates" is a CheckedSet where "focused" updates come from file system events.
       When a file changes, [reparse] notices and includes it in [new_or_changed], and
       that makes it get rechecked.

       Sometimes, we notice that a dependency changed before the file system event
       (via [ensure_parsed]); in that case, we have a "dependency" update, where we only
       want to update the dependency graph and merge, but not check.

       When we get the delayed file system event, it appears unchanged since we already
       reparsed it. We still want to check it and its dependents, since we skipped that
       before. We can detect this when a "focused" update is unchanged but was previously
       only a "dependency". *)
    let unchanged_files_to_upgrade =
      (* file is unchanged, should now be checked, and was previously merged but not checked *)
      CheckedSet.filter updates ~f:(fun file kind ->
          (not (CheckedSet.is_dependency kind))
          && FilenameSet.mem file unchanged_parse
          && CheckedSet.mem_dependency file env.ServerEnv.checked_files
      )
    in

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
    *)

    (* remember old modules *)
    let unchanged_checked =
      CheckedSet.remove new_or_changed_or_deleted env.ServerEnv.checked_files
    in
    (* We may be forcing a recheck on some unchanged files *)
    let unchanged_files_to_force =
      CheckedSet.filter files_to_force ~f:(fun fn _ -> FilenameSet.mem fn unchanged)
    in
    MonitorRPC.status_update ~event:ServerStatus.Resolving_dependencies_progress;
    let%lwt (changed_modules, duplicate_providers) =
      (* TODO remove after lookup overhaul *)
      Module_js.clear_filename_cache ();
      commit_modules ~transaction ~options ~profiling ~workers ~duplicate_providers dirty_modules
    in

    let unparsed_or_deleted = FilenameSet.union unparsed_set deleted in

    (* direct_dependent_files are unchanged files which directly depend on changed modules,
       or are new / changed files that are phantom dependents. *)
    let%lwt direct_dependent_files =
      Memory_utils.with_memory_timer_lwt ~options "DirectDependentFiles" profiling (fun () ->
          if not (FilenameSet.disjoint old_parsed unparsed_or_deleted) then
            (* unparsed/deleted files can't be direct dependents. a previously-parsed
               file may be cached as a direct dependent of some other file. so if any
               files are no longer parsed, we invalidate the cache to clear them out of
               any other files' caches.

               note: we could search the cache for entries containing these files. we
               could also just deal with bogus entries downstream, like by ignoring
               unparsed direct dependents in resolve_requires, but that's fragile.
               really, the solution is to make calc_direct_dependents fast so we don't
               need this cache. *)
            DirectDependentFilesCache.clear ();

          DirectDependentFilesCache.with_cache
            ~cache_key:new_or_changed_or_deleted
            ~on_miss:
              ( lazy
                (Dep_service.calc_direct_dependents
                   workers
                   ~candidates:unchanged
                   ~root_files:new_or_changed_or_deleted
                   ~root_modules:changed_modules
                )
                )
      )
    in
    Hh_logger.info "Re-resolving parsed and directly dependent files";
    let%lwt () = ensure_parsed ~options ~profiling ~workers ~reader direct_dependent_files in
    let%lwt resolved_requires_changed =
      let parsed_set = FilenameSet.union parsed_set direct_dependent_files in
      let parsed = FilenameSet.elements parsed_set in
      resolve_requires ~transaction ~reader ~options ~profiling ~workers ~parsed ~parsed_set
    in

    Hh_logger.info "Recalculating dependency graph";
    let parsed = FilenameSet.union parsed_set unchanged in
    let%lwt dependency_info =
      Memory_utils.with_memory_timer_lwt ~options "CalcDepsTypecheck" profiling (fun () ->
          let files_to_update_dependency_info =
            FilenameSet.union parsed_set direct_dependent_files
          in
          let%lwt partial_dependency_graph =
            Dep_service.calc_partial_dependency_graph
              ~reader
              workers
              files_to_update_dependency_info
              ~parsed
          in
          let old_dependency_info = env.ServerEnv.dependency_info in
          Lwt.return
            (Dependency_info.update old_dependency_info partial_dependency_graph unparsed_or_deleted)
      )
    in
    (* Here's how to update unparsed:
     * 1. Remove the parsed files. This removes any file which used to be unparsed but is now parsed
     * 2. Remove the deleted files. This removes any previously unparsed file which was deleted
     * 3. Add the newly unparsed files. This adds new unparsed files or files which became unparsed *)
    let unparsed =
      let to_remove = FilenameSet.union parsed deleted in
      FilenameSet.diff env.ServerEnv.unparsed to_remove |> FilenameSet.union unparsed_set
    in
    (* during check, we will try to skip checking dependents whose dependencies haven't changed.
       if the inputs are the same, the output should be too. however, there are a few cases that
       require a dependent to be checked, but which aren't reflected in the updated dependency
       graph:
       1) resolved requires changed: e.g. foo.js used to depend on a/bar.js but now depends on
          b/bar.js. even if b/bar.js didn't change, we still need to recheck foo.js
       2) a file is deleted: if foo.js used to depend on bar.js, and bar.js was deleted, then
          bar.js doesn't even appear in foo.js's dependencies to consider whether it changed.
          TODO: shouldn't this be captured by resolved_requires_changed because foo's
          require('bar') now resolves to nothing instead of bar.js?
       3) a file changed from @flow to non-@flow: if foo.js depends on bar.js and bar.js removes
          @flow, then we won't merge bar.js anymore, so it won't be in sig_new_or_changed,
          so we won't think we need to check foo.js.
          TODO: we should be able to track the specific files this applies to and avoid
          skipping all direct dependents, but the win here is probably small... how often
          do files become skipped? *)
    let cannot_skip_direct_dependents =
      resolved_requires_changed || deleted_count > 0 || not (FilenameSet.is_empty unparsed_set)
    in

    Hh_logger.info "Updating index";
    let%lwt exports =
      Memory_utils.with_memory_timer_lwt ~options "Indexing" profiling (fun () ->
          Export_service.update
            ~workers
            ~reader
            ~update:new_or_changed
            ~remove:deleted
            env.ServerEnv.exports
      )
    in
    Hh_logger.info "Done updating index";

    let env = { env with ServerEnv.files = parsed; unparsed; dependency_info; exports; coverage } in
    let errors =
      { ServerEnv.local_errors; duplicate_providers; merge_errors; warnings; suppressions }
    in
    let intermediate_values =
      ( deleted,
        direct_dependent_files,
        errors,
        freshparsed,
        new_or_changed,
        unchanged_checked,
        unchanged_files_to_force,
        unchanged_files_to_upgrade,
        unparsed_set,
        cannot_skip_direct_dependents
      )
    in
    Lwt.return (env, intermediate_values)

  (** [direct_dependents_to_recheck ~direct_dependent_files ~focused ~dependencies] computes the
      dependent files that directly depend on its inputs. a file directly depends on another if it
      literaly has an [import].

      Here's an example:

      - D depends on B, B and C depend on A

      ```
            A
           / \
          B   C
           \
            D
      ```

      Suppose we're rechecking B and notice that A changed unexpectedly.

      focused = {B}
      dependencies = {A}
      direct_dependent_files = { C, D }

      The dependents of the focused updates, {B}, are {D}. These are the dependents we want to
      check. We don't want to check C right now. We want to do it when we receive the file watcher
      update for A. How do we exclude C?

      Instead of using direct_dependent_files at all, we could recompute the direct dependents of
      {B} using `implementation_dependency_graph` and get {D}. win!

      But consider this expanded graph:

      ```
         E   A
        / \ / \
       F   B   C
            \
             D
      ```

      What if `E` is deleted (and A is changed, as before)?

      focused = {B}
      dependencies = {A, E}
      direct_dependent_files = {C, D, F}

      We actually have to recheck F. We won't want to -- it's not required to recheck B -- but as
      soon as the transaction commits, all record of the E <- B and E <- F edges are gone and we
      won't know to recheck F and B when we get the deletion event about E. Maybe we could keep
      track of this, but not today.

      implementation_dependency_graph doesn't contain E. Whereas before, we could recompute the
      direct dependents of B, we can't compute the direct dependents of E.

      Who knows what depended on E? direct_dependent_files!

      But recall that we don't want to just use direct_dependent_files, because it includes
      dependents of changed dependencies (C, because A changed).

      The result we're looking for is {D, F}.

      - direct dependents of focused updates = { D }
      - direct dependents of dependency updates = { B, C }
      - direct_dependent_files = { C, D, F }

      It's tempting to just remove the dependents of dependencies from direct_dependent_files:
      {C, D, F} - {B, C} = {D, F} -- win!

      Not so fast. If we even further complicate the dependency graph such that D also depends on A,
      then we get:

      ```
         E   A
        / \ /|\
       F   B | C
            \|
             D
      ```

      - direct dependents of focused updates = { D }
      - direct dependents of dependency updates = { B, C, D }
      - `direct_dependent_files` = { C, D, F }

      { C, D, F } - { B, C, D } = { F } -- fail!

      So we need to add the focused dependents back in:

      { D } + ({ C, D, F } - { B, C, D}) =
      { D } + { F } =
      { D, F }
  *)
  let direct_dependents_to_recheck
      ~implementation_dependency_graph ~direct_dependent_files ~focused ~dependencies =
    (* These are all the files that literally import the files we're focusing. *)
    let focused_direct_dependents =
      Pure_dep_graph_operations.calc_direct_dependents implementation_dependency_graph focused
    in
    (* These are all the files that import changed dependencies. We don't want to check
       dependents of dependencies. *)
    let dependency_direct_dependents =
      Pure_dep_graph_operations.calc_direct_dependents implementation_dependency_graph dependencies
    in
    (* These are files that import deleted files[1]. They need to be included now because
       we will forget they're dirty as soon as this recheck is over and we commit the
       updated dependency graph.

       We have to compute it this way because (a) the deleted edges are already removed
       from implementation_dependency_graph, so focused_direct_dependents doesn't include
       them, and (b) direct_dependent_files doesn't track which files a dependent depended
       on, so it doesn't tell us which files are dependents of deleted files.

       [1] strictly, we should also diff out focused_direct_dependents to get the orphaned
       dependents, but since we're about to union it with focused_direct_dependents, that's
       a waste. *)
    let orphaned_direct_dependents =
      FilenameSet.diff direct_dependent_files dependency_direct_dependents
    in
    FilenameSet.union focused_direct_dependents orphaned_direct_dependents

  let determine_what_to_recheck
      ~profiling
      ~options
      ~sig_dependency_graph
      ~implementation_dependency_graph
      ~freshparsed
      ~unchanged_checked
      ~unchanged_files_to_force
      ~direct_dependent_files =
    let input_focused =
      FilenameSet.union
        (CheckedSet.focused freshparsed)
        (CheckedSet.focused unchanged_files_to_force)
    in
    let input_dependencies =
      FilenameSet.union
        (CheckedSet.dependencies freshparsed)
        (CheckedSet.dependencies unchanged_files_to_force)
    in
    (* we will need the signature dependents and implementation dependents to compute the fanout
       from the changed files ([input_focused] + [input_dependencies]) to all of the files that
       could be impacted by those changes.

       [sig_dependent_files] is the set of files whose signatures transitively depend on the input.
       We re-merge these because their exports may have changed.

       [all_dependent_files] is the set of files whose implementations directly depend on the
       signature dependents. In other words, if a file F imports something from G where G's
       signature is impacted by C (a changed file), then G is a sig dependent and F needs to be
       checked because its implementation may use something from C through G. *)
    let%lwt (sig_dependent_files, all_dependent_files) =
      Memory_utils.with_memory_timer_lwt ~options "AllDependentFiles" profiling (fun () ->
          let implementation_dependents =
            direct_dependents_to_recheck
              ~implementation_dependency_graph
              ~direct_dependent_files
              ~focused:input_focused
              ~dependencies:input_dependencies
          in
          let (sig_dependent_files, all_dependent_files) =
            Pure_dep_graph_operations.calc_all_dependents
              ~sig_dependency_graph
              ~implementation_dependency_graph
              implementation_dependents
          in
          (* Prevent files in node_modules from being added to the checked set. *)
          let sig_dependent_files = filter_out_node_modules ~options sig_dependent_files in
          let all_dependent_files = filter_out_node_modules ~options all_dependent_files in
          Lwt.return (sig_dependent_files, all_dependent_files)
      )
    in
    let input = unfocused_files_to_infer ~options ~input_focused ~input_dependencies in
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
         }
      )

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
          freshparsed,
          new_or_changed,
          unchanged_checked,
          unchanged_files_to_force,
          unchanged_files_to_upgrade,
          unparsed_set,
          cannot_skip_direct_dependents
        ) =
      intermediate_values
    in
    let dependency_info = env.ServerEnv.dependency_info in
    let implementation_dependency_graph =
      Dependency_info.implementation_dependency_graph dependency_info
    in
    let sig_dependency_graph = Dependency_info.sig_dependency_graph dependency_info in
    let%lwt (Determine_what_to_recheck_result
              {
                to_merge;
                to_check;
                to_merge_or_check;
                components;
                recheck_set;
                sig_dependent_files;
                all_dependent_files;
              }
              ) =
      let unchanged_files_to_force =
        CheckedSet.union unchanged_files_to_force unchanged_files_to_upgrade
      in
      determine_what_to_recheck
        ~profiling
        ~options
        ~sig_dependency_graph
        ~implementation_dependency_graph
        ~freshparsed
        ~unchanged_checked
        ~unchanged_files_to_force
        ~direct_dependent_files
    in
    (* This is a much better estimate of what checked_files will be after the merge finishes. We now
     * include the dependencies and dependents that are being implicitly included in the recheck. *)
    will_be_checked_files := CheckedSet.union to_merge_or_check !will_be_checked_files;

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
    let%lwt (updated_suppressions, merge_skip_count, sig_new_or_changed, top_cycle, time_to_merge) =
      let n = FilenameSet.cardinal all_dependent_files in
      if n > 0 then Hh_logger.info "recheck %d dependent files:" n;
      merge
        ~transaction
        ~reader
        ~options
        ~profiling
        ~workers
        ~suppressions:errors.ServerEnv.suppressions
        ~to_merge
        ~components
        ~recheck_set
        ~sig_dependency_graph
        ~deleted
        ~unparsed_set
    in

    (* Merge_service.check_contents_cache contains type information for
     * dependencies. The set of files in sig_new_or_changed have meaningfully
     * changed and we should invalidate the caches for those files.
     *
     * Note that this invalidation happens after the transaction commits. While
     * a transaction is running, we maintain a consistent view of the type state
     * before this recheck started. *)
    Transaction.add
      ~commit:(fun () ->
        FilenameSet.iter (Check_cache.remove Merge_service.check_contents_cache) sig_new_or_changed;
        Lwt.return_unit)
      ~rollback:(fun () -> Lwt.return_unit)
      transaction;

    let%lwt ( errors,
              coverage,
              time_to_check_merged,
              check_skip_count,
              slowest_file,
              num_slow_files,
              check_internal_error
            ) =
      let sig_new_or_changed =
        (* include all files that were previously rechecked as "dependency" updates and are now
           being rechecked normally. their signatures are unchanged because we already saw them
           when they were rechecked as dependencies, but since we skipped checking their dependents
           before, we need to include them now. *)
        FilenameSet.union sig_new_or_changed (CheckedSet.all unchanged_files_to_upgrade)
      in
      Check_files.check_files
        ~reader
        ~options
        ~profiling
        ~workers
        ~errors
        ~updated_suppressions
        ~coverage:env.ServerEnv.coverage
        ~to_check
        ~direct_dependent_files
        ~sig_new_or_changed
        ~dependency_info
        ~persistent_connections:(Some env.ServerEnv.connections)
        ~recheck_reasons
        ~cannot_skip_direct_dependents
    in
    Base.Option.iter check_internal_error ~f:(Hh_logger.error "%s");

    let record_recheck_time () =
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
        record_recheck_time,
        check_internal_error
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
        Persistent_connection.clear_type_parse_artifacts_caches ();
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
      with
      | Unexpected_file_changes changed_files -> handle_unexpected_file_changes changed_files
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
  let%lwt (env, stats, record_recheck_time, first_internal_error) =
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
              ~will_be_checked_files
        )
    )
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
        estimated_files_to_init
      ) =
    Base.Option.value_map
      estimates
      ~default:(None, None, None, None, None, None)
      ~f:(fun
           {
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
          Some estimated_files_to_init
        )
    )
  in
  (* TODO: update log to reflect current terminology **)
  let log_recheck_event : profiling:Profiling_js.finished -> unit Lwt.t =
   fun ~profiling ->
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
      ~scm_changed_mergebase:file_watcher_metadata.MonitorProt.changed_mergebase
      ~profiling;
    record_recheck_time ()
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
    MonitorRPC.status_update ~event:ServerStatus.(Parsing_progress { finished; total = None });
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

    let restore_parsed (fns, dirty_modules) (fn, parsed_file_data) =
      let { Saved_state.module_name; normalized_file_data } = parsed_file_data in
      let { Saved_state.hash; exports; resolved_requires } =
        Saved_state.denormalize_file_data ~root normalized_file_data
      in

      (* Restore the FileHeap *)
      let ms =
        Parsing_heaps.From_saved_state.add_parsed fn hash module_name exports resolved_requires
      in

      (FilenameSet.add fn fns, Modulename.Set.union ms dirty_modules)
    in

    let restore_unparsed (fns, dirty_modules) (fn, unparsed_file_data) =
      let { Saved_state.unparsed_module_name; unparsed_hash } = unparsed_file_data in

      (* Restore the FileHeap *)
      let ms = Parsing_heaps.From_saved_state.add_unparsed fn unparsed_hash unparsed_module_name in

      (FilenameSet.add fn fns, Modulename.Set.union ms dirty_modules)
    in

    Hh_logger.info "Restoring heaps";
    let%lwt (parsed, unparsed, dirty_modules) =
      Memory_utils.with_memory_timer_lwt ~options "RestoreHeaps" profiling (fun () ->
          let neutral = (FilenameSet.empty, Modulename.Set.empty) in
          let merge (a1, a2) (b1, b2) = (FilenameSet.union a1 b1, Modulename.Set.union a2 b2) in
          let%lwt (parsed, dirty_modules_parsed) =
            MultiWorkerLwt.call
              workers
              ~job:(List.fold_left restore_parsed)
              ~merge
              ~neutral
              ~next:(MultiWorkerLwt.next workers parsed_heaps)
          in
          let%lwt (unparsed, dirty_modules_unparsed) =
            MultiWorkerLwt.call
              workers
              ~job:(List.fold_left restore_unparsed)
              ~merge
              ~neutral
              ~next:(MultiWorkerLwt.next workers unparsed_heaps)
          in
          let dirty_modules = Modulename.Set.union dirty_modules_parsed dirty_modules_unparsed in
          Lwt.return (parsed, unparsed, dirty_modules)
      )
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
    MonitorRPC.status_update ~event:ServerStatus.Resolving_dependencies_progress;

    (* This will restore InfoHeap, NameHeap, & all_providers hashtable *)
    let%lwt (_changed_modules, duplicate_providers) =
      commit_modules
        ~transaction
        ~options
        ~profiling
        ~workers
        ~duplicate_providers:SMap.empty
        dirty_modules
    in
    let errors =
      let merge_errors = FilenameMap.empty in
      { ServerEnv.local_errors; duplicate_providers; merge_errors; warnings; suppressions }
    in
    let dependency_info = Dependency_info.of_map dependency_graph in

    Hh_logger.info "Indexing files";
    let%lwt exports =
      Memory_utils.with_memory_timer_lwt ~options "Indexing" profiling (fun () ->
          Export_service.init ~workers ~reader ~libs:lib_exports parsed
      )
    in

    let env =
      mk_init_env
        ~files:parsed
        ~unparsed
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
  if (not (Options.lazy_mode options)) || should_force_recheck then begin
    if CheckedSet.is_empty updates || not libs_ok then
      (* Don't recheck if the libs are not ok *)
      Lwt.return (env, libs_ok)
    else
      (* In non-lazy mode, we return updates here. They will immediately be rechecked. Due to
       * fanout, this can be a huge recheck, but it's sound.
       *
       * We'll also hit this code path in lazy modes if the user has passed
       * --saved-state-force-recheck. These users want to force Flow to recheck all the files that
       * have changed since the saved state was generated *)
      let files_to_force = CheckedSet.empty in
      let recheck_reasons = [LspProt.Lazy_init_typecheck] in
      let%lwt (recheck_profiling, (_log_recheck_event, _summary_info, env)) =
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
              ~will_be_checked_files:(ref files_to_force)
        )
      in
      Profiling_js.merge ~from:recheck_profiling ~into:profiling;
      Lwt.return (env, libs_ok)
  end else
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
        with
        | Unexpected_file_changes changed_files ->
          let dependencies = changed_files |> Nel.to_list |> FilenameSet.of_list in
          let updated_files = CheckedSet.add ~dependencies updated_files in
          try_update updated_files
      in
      try_update updates
    in
    Lwt.return (env, libs_ok)

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
  MonitorRPC.status_update ~event:ServerStatus.(Parsing_progress { finished = 0; total = None });
  let%lwt ( parsed_set,
            unparsed_set,
            unchanged,
            _not_found,
            dirty_modules,
            local_errors,
            (package_json_files, package_json_errors)
          ) =
    parse ~options ~profiling ~workers ~reader next_files
  in
  assert (FilenameSet.is_empty unchanged);

  let parsed = FilenameSet.elements parsed_set in

  (* Parsing won't raise warnings *)
  let warnings = FilenameMap.empty in
  let package_errors =
    List.fold_left2
      (fun acc source_file -> function
        | None -> acc
        | Some err ->
          Inference_utils.set_of_parse_error ~source_file err |> update_errset acc source_file)
      FilenameMap.empty
      package_json_files
      package_json_errors
  in
  let local_errors = merge_error_maps package_errors local_errors in

  Hh_logger.info "Loading libraries";
  let%lwt (libs_ok, local_errors, warnings, suppressions, lib_exports) =
    let suppressions = Error_suppressions.empty in
    init_libs ~options ~profiling ~local_errors ~warnings ~suppressions ~reader ordered_libs
  in

  Hh_logger.info "Resolving dependencies";
  MonitorRPC.status_update ~event:ServerStatus.Resolving_dependencies_progress;
  let%lwt (_changed_modules, duplicate_providers) =
    commit_modules
      ~transaction
      ~options
      ~profiling
      ~workers
      ~duplicate_providers:SMap.empty
      dirty_modules
  in
  let%lwt _resolved_requires_changed =
    resolve_requires ~transaction ~reader ~options ~profiling ~workers ~parsed ~parsed_set
  in
  let%lwt dependency_info =
    Memory_utils.with_memory_timer_lwt ~options "CalcDepsTypecheck" profiling (fun () ->
        Dep_service.calc_dependency_info ~reader workers ~parsed:parsed_set
    )
  in

  Hh_logger.info "Indexing files";
  let%lwt exports =
    Memory_utils.with_memory_timer_lwt ~options "Indexing" profiling (fun () ->
        Export_service.init ~workers ~reader ~libs:lib_exports parsed_set
    )
  in
  Hh_logger.info "Done";

  let errors =
    let merge_errors = FilenameMap.empty in
    { ServerEnv.local_errors; duplicate_providers; merge_errors; warnings; suppressions }
  in
  let env =
    mk_init_env
      ~files:parsed_set
      ~unparsed:unparsed_set
      ~package_json_files
      ~dependency_info
      ~ordered_libs
      ~libs
      ~errors
      ~exports
  in
  Lwt.return (env, libs_ok)

let exit_if_no_fallback ?msg options =
  if Options.saved_state_no_fallback options then Exit.(exit ?msg Invalid_saved_state)

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
       Profiling_js.merge ~from:load_profiling ~into:profiling;

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
       let updates = CheckedSet.(add ~focused:updates empty) in
       Lwt.return_some (saved_state, updates)
     with
    | Saved_state.Invalid_saved_state invalid_reason ->
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
  let%lwt (env, libs_ok) =
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
  Lwt.return (libs_ok, env, last_estimates)

let full_check ~profiling ~options ~workers ?focus_targets env =
  let { ServerEnv.files = parsed; dependency_info; errors; _ } = env in
  with_transaction (fun transaction reader ->
      let%lwt input = files_to_infer ~options ~focus_targets ~profiling ~parsed ~dependency_info in
      let sig_dependent_files = FilenameSet.empty in
      let all_dependent_files = FilenameSet.empty in
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
      let%lwt (updated_suppressions, _, sig_new_or_changed, _, _) =
        merge
          ~transaction
          ~reader
          ~options
          ~profiling
          ~workers
          ~suppressions:errors.ServerEnv.suppressions
          ~to_merge
          ~components
          ~recheck_set
          ~sig_dependency_graph
          ~deleted:FilenameSet.empty
          ~unparsed_set:FilenameSet.empty
      in

      let%lwt (errors, coverage, _, _, _, _, check_internal_error) =
        Check_files.check_files
          ~reader
          ~options
          ~profiling
          ~workers
          ~errors
          ~updated_suppressions
          ~coverage:env.ServerEnv.coverage
          ~to_check
          ~direct_dependent_files:FilenameSet.empty
          ~sig_new_or_changed
          ~dependency_info
          ~persistent_connections:None
          ~recheck_reasons
          ~cannot_skip_direct_dependents:true
      in
      Base.Option.iter check_internal_error ~f:(Hh_logger.error "%s");

      let checked_files = to_merge_or_check in
      Hh_logger.info "Checked set: %s" (CheckedSet.debug_counts_to_string checked_files);
      Lwt.return ({ env with ServerEnv.checked_files; errors; coverage }, check_internal_error)
  )

let debug_determine_what_to_recheck = Recheck.determine_what_to_recheck

let debug_include_dependencies_and_dependents = include_dependencies_and_dependents
