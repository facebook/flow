(**
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
      match FilenameMap.get file map with
      | Some prev_errset -> Flow_error.ErrorSet.union prev_errset errset
      | None -> errset
    in
    FilenameMap.add file errset map

let merge_error_maps =
  FilenameMap.union ~combine:(fun _ x y -> Some (Flow_error.ErrorSet.union x y))

(* We just want to replace the old coverage with the new one *)
let update_coverage = FilenameMap.union ~combine:(fun _ _ -> Option.return)

(* Filter out duplicate provider error, if any, for the given file. *)
let filter_duplicate_provider map file =
  match FilenameMap.get file map with
  | Some prev_errset ->
    let new_errset =
      Flow_error.ErrorSet.filter
        (fun err -> not (Flow_error.kind_of_error err = Errors.DuplicateProviderError))
        prev_errset
    in
    FilenameMap.add file new_errset map
  | None -> map

let with_memory_info callback =
  let%lwt cgroup_stats = CGroup.get_stats () in
  (* Reading hash_stats while workers are writing can cause assertion errors *)
  let hash_stats = (try Some (SharedMem_js.hash_stats ()) with _ -> None) in
  let heap_size = SharedMem_js.heap_size () in
  callback ~cgroup_stats ~hash_stats ~heap_size;
  Lwt.return_unit

module MemorySamplingLoop = LwtLoop.Make (struct
  type acc =
    cgroup_stats:(CGroup.stats, string) result ->
    hash_stats:SharedMem_js.table_stats option ->
    heap_size:int ->
    unit

  let main callback =
    let%lwt () = with_memory_info callback in
    let%lwt () = Lwt_unix.sleep 1.0 in
    Lwt.return callback

  let catch _ exn =
    let exn = Exception.wrap exn in
    Hh_logger.error "Exception in MemorySamplingLoop: %s" (Exception.to_string exn);
    Lwt.return_unit
end)

let with_timer_lwt =
  let clear_worker_memory () =
    ["worker_rss_start"; "worker_rss_delta"; "worker_rss_hwm_delta"] |> List.iter Measure.delete
  in
  let profile_add_memory profiling getter group metric =
    getter "worker_rss_start"
    |> Option.iter ~f:(fun start ->
           getter "worker_rss_delta"
           |> Option.iter ~f:(fun delta ->
                  getter "worker_rss_hwm_delta"
                  |> Option.iter ~f:(fun hwm_delta ->
                         Profiling_js.add_memory ~group ~metric ~start ~delta ~hwm_delta profiling)))
  in
  let sample_memory timer profiling ~cgroup_stats ~hash_stats ~heap_size =
    Profiling_js.sample_memory profiling ~group:timer ~metric:"heap" ~value:(float heap_size);

    Option.iter hash_stats ~f:(fun { SharedMem_js.nonempty_slots; used_slots; slots } ->
        Profiling_js.sample_memory
          profiling
          ~group:timer
          ~metric:"hash_nonempty_slots"
          ~value:(float nonempty_slots);

        Profiling_js.sample_memory
          profiling
          ~group:timer
          ~metric:"hash_used_slots"
          ~value:(float used_slots);

        Profiling_js.sample_memory profiling ~group:timer ~metric:"hash_slots" ~value:(float slots));

    match cgroup_stats with
    | Error _ -> ()
    | Ok { CGroup.total; total_swap; anon; file; shmem } ->
      Profiling_js.sample_memory profiling ~group:timer ~metric:"cgroup_total" ~value:(float total);

      Profiling_js.sample_memory
        profiling
        ~group:timer
        ~metric:"cgroup_swap"
        ~value:(float total_swap);

      Profiling_js.sample_memory profiling ~group:timer ~metric:"cgroup_anon" ~value:(float anon);

      Profiling_js.sample_memory profiling ~group:timer ~metric:"cgroup_shmem" ~value:(float shmem);

      Profiling_js.sample_memory profiling ~group:timer ~metric:"cgroup_file" ~value:(float file)
  in
  fun ?options timer profiling f ->
    let should_print = Option.value_map options ~default:false ~f:Options.should_profile in
    let sample_memory = sample_memory timer profiling in
    clear_worker_memory ();

    (* Record the cgroup info at the start *)
    let%lwt () = with_memory_info sample_memory in
    (* Asynchronously run a thread that periodically grabs the cgroup stats *)
    let sampling_loop = MemorySamplingLoop.run sample_memory in
    let%lwt ret =
      try%lwt
        let%lwt ret = Profiling_js.with_timer_lwt ~should_print ~timer ~f profiling in
        Lwt.cancel sampling_loop;
        Lwt.return ret
      with exn ->
        let exn = Exception.wrap exn in
        Lwt.cancel sampling_loop;
        Exception.reraise exn
    in
    (* Record the cgroup info at the end *)
    let%lwt () = with_memory_info sample_memory in
    profile_add_memory profiling Measure.get_mean timer "worker_rss_avg";
    profile_add_memory profiling Measure.get_max timer "worker_rss_max";
    clear_worker_memory ();
    Lwt.return ret

let collate_parse_results ~options parse_results =
  let {
    Parsing_service_js.parse_ok;
    parse_skips;
    parse_hash_mismatch_skips;
    parse_fails;
    parse_unchanged;
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
    Inference_utils.fold_whitelisted_well_formed_exports
      ~f:(fun file file_sig_errors errors ->
        let file_sig_errors = File_sig.abstractify_tolerable_errors file_sig_errors in
        let errset =
          Inference_utils.set_of_file_sig_tolerable_errors ~source_file:file file_sig_errors
        in
        update_errset errors file errset)
      options
      parse_ok
      local_errors
  in
  let unparsed =
    List.fold_left
      (fun unparsed (file, info, _) -> (file, info) :: unparsed)
      parse_skips
      parse_fails
  in
  let parse_ok =
    FilenameMap.fold (fun k _ acc -> FilenameSet.add k acc) parse_ok FilenameSet.empty
  in
  (parse_ok, unparsed, parse_unchanged, local_errors)

let parse ~options ~profiling ~workers ~reader parse_next =
  with_timer_lwt ~options "Parsing" profiling (fun () ->
      let%lwt results =
        Parsing_service_js.parse_with_defaults ~reader options workers parse_next
      in
      Lwt.return (collate_parse_results ~options results))

let reparse ~options ~profiling ~transaction ~reader ~workers ~modified ~deleted =
  with_timer_lwt ~options "Parsing" profiling (fun () ->
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
      let (parse_ok, unparsed, unchanged, local_errors) = collate_parse_results ~options results in
      Lwt.return (new_or_changed, parse_ok, unparsed, unchanged, local_errors))

let parse_contents ~options ~profiling ~check_syntax filename contents =
  with_timer_lwt ~options "Parsing" profiling (fun () ->
      (* always enable types when checking an individual file *)
      let types_mode = Parsing_service_js.TypesAllowed in
      let max_tokens = Options.max_header_tokens options in
      let (docblock_errors, info) =
        Parsing_service_js.parse_docblock ~max_tokens filename contents
      in
      let errors = Inference_utils.set_of_docblock_errors ~source_file:filename docblock_errors in
      let parse_options =
        Parsing_service_js.make_parse_options ~fail:check_syntax ~types_mode info options
      in
      let parse_result = Parsing_service_js.do_parse ~info ~parse_options contents filename in
      Lwt.return (errors, parse_result, info))

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
    with_timer_lwt ~options "CommitModules" profiling (fun () ->
        let all_files_set =
          FilenameSet.union (FilenameSet.union parsed_set unparsed_set) deleted
        in
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
                      match err with
                      | Module_js.ModuleDuplicateProviderError { module_name; provider; conflict }
                        ->
                        let error =
                          Error_message.(
                            EDuplicateModuleProvider { module_name; provider; conflict })
                          |> Flow_error.error_of_msg ~trace_reasons:[] ~source_file:file
                        in
                        Flow_error.ErrorSet.add error acc)
                    Flow_error.ErrorSet.empty
                    errors
                in
                update_errset acc file errset)
              errmap
              errors ))
  in
  let commit_modules ~transaction ~reader =
    commit_modules_generic
      ~introduce_files:(Module_js.introduce_files ~reader)
      ~transaction
      ~reader
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
    options:Options.t ->
    root_files:FilenameSet.t ->
    on_miss:(unit -> FilenameSet.t Lwt.t) ->
    FilenameSet.t Lwt.t
end = struct
  type entry = {
    direct_dependents: FilenameSet.t;
    last_hit: float;
  }

  type cache = {
    entries: entry FilenameMap.t;
    size: int;
  }

  let empty_cache = { entries = FilenameMap.empty; size = 0 }

  let max_size = 100

  let cache = ref empty_cache

  let clear () = cache := empty_cache

  let remove_oldest () =
    let { entries; size } = !cache in
    let oldest =
      FilenameMap.fold
        (fun key { last_hit; _ } acc ->
          match acc with
          | Some (_, oldest_hit) when oldest_hit <= last_hit -> acc
          | _ -> Some (key, last_hit))
        entries
        None
    in
    Option.iter oldest ~f:(fun (oldest_key, _) ->
        cache := { entries = FilenameMap.remove oldest_key entries; size = size - 1 })

  let add_after_miss ~root_file ~direct_dependents =
    let entry = { direct_dependents; last_hit = Unix.gettimeofday () } in
    let { entries; size } = !cache in
    cache := { entries = FilenameMap.add root_file entry entries; size = size + 1 };
    if size > max_size then remove_oldest ()

  let get_from_cache ~root_file =
    let { entries; size } = !cache in
    match FilenameMap.get root_file entries with
    | None -> None
    | Some entry ->
      let entry = { entry with last_hit = Unix.gettimeofday () } in
      cache := { entries = FilenameMap.add root_file entry entries; size };
      Some entry

  let with_cache ~options ~root_files ~on_miss =
    match FilenameSet.elements root_files with
    | [root_file] when Options.cache_direct_dependents options ->
      begin
        match get_from_cache ~root_file with
        | None ->
          let%lwt direct_dependents = on_miss () in
          add_after_miss ~root_file ~direct_dependents;
          Lwt.return direct_dependents
        | Some { direct_dependents; last_hit = _ } -> Lwt.return direct_dependents
      end
    | _ ->
      (* Cache is only for when there is a single root file *)
      on_miss ()
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
    with_timer_lwt ~options "ResolveRequires" profiling (fun () ->
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
  Lwt.return errors

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
  let%lwt resolve_errors =
    resolve_requires ~transaction ~reader ~options ~profiling ~workers ~parsed ~parsed_set
  in
  let local_errors = FilenameMap.union resolve_errors local_errors in
  Lwt.return (changed_modules, { ServerEnv.local_errors; merge_errors; warnings; suppressions })

let error_set_of_internal_error file (loc, internal_error) =
  Error_message.EInternal (loc, internal_error)
  |> Flow_error.error_of_msg ~trace_reasons:[] ~source_file:file
  |> Flow_error.ErrorSet.singleton

let calc_deps ~options ~profiling ~dependency_graph ~components to_merge =
  with_timer_lwt ~options "CalcDeps" profiling (fun () ->
      let dependency_graph =
        Pure_dep_graph_operations.filter_dependency_graph dependency_graph to_merge
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
      Lwt.return (dependency_graph, component_map))

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
    ~all_dependency_graph
    ~dependency_graph
    ~all_dependent_files =
  with_timer_lwt ~options "PruneDeps" profiling (fun () ->
      (* Don't just look up the dependencies of the focused or dependent modules. Also look up
       * the dependencies of dependencies, since we need to check transitive dependencies *)
      let preliminary_to_merge =
        Pure_dep_graph_operations.calc_direct_dependencies
          all_dependency_graph
          (CheckedSet.all (CheckedSet.add ~dependents:all_dependent_files input))
      in
      (* So we want to prune our dependencies to only the dependencies which changed. However, two
       dependencies A and B might be in a cycle. If A changed and B did not, we still need to check
       B. Likewise, a dependent A and a dependency B might be in a cycle. If B is not a dependent
       and A and B are unchanged, we still need to check B. So we need to calculate components
       before we can prune. *)
      (* Grab the subgraph containing all our dependencies and sort it into the strongly connected
       cycles *)
      let components = Sort_js.topsort ~roots:preliminary_to_merge dependency_graph in
      let dependencies =
        List.fold_left
          (fun dependencies component ->
            let dependencies =
              if
                Nel.exists (fun fn -> not (CheckedSet.mem fn unchanged_checked)) component
                (* If some member of the component is not unchanged, then keep the component *)
              then
                Nel.fold_left (fun acc fn -> FilenameSet.add fn acc) dependencies component
              (* If every element is unchanged, drop the component *)
              else
                dependencies
            in
            let dependencies =
              let (dependents, non_dependents) =
                List.partition (fun fn -> FilenameSet.mem fn all_dependent_files)
                @@ Nel.to_list component
              in
              if
                dependents <> [] && non_dependents <> []
                (* If some member of the component is a dependent and others are not, then keep the
           others *)
              then
                List.fold_left (fun acc fn -> FilenameSet.add fn acc) dependencies non_dependents
              (* If every element is a dependent or if every element is not, drop the component *)
              else
                dependencies
            in
            dependencies)
          FilenameSet.empty
          components
      in
      (* Definitely recheck input and dependencies. As merging proceeds, dependents may or may not be
       rechecked. *)
      let definitely_to_merge = CheckedSet.add ~dependencies input in
      let to_merge = CheckedSet.add ~dependents:all_dependent_files definitely_to_merge in
      (* NOTE: An important invariant here is that if we recompute Sort_js.topsort with to_merge on
       dependency_graph, we would get exactly the same components. Later, we will filter
       dependency_graph to just to_merge, and correspondingly filter components as well. This will
       work out because every component is either entirely inside to_merge or entirely outside. *)
      Lwt.return (to_merge, components, CheckedSet.all definitely_to_merge))

let remove_old_results (errors, warnings, suppressions, coverage, first_internal_error) file =
  ( FilenameMap.remove file errors,
    FilenameMap.remove file warnings,
    Error_suppressions.remove file suppressions,
    FilenameMap.remove file coverage,
    first_internal_error )

let add_new_results (errors, warnings, suppressions, coverage, first_internal_error) file result =
  match result with
  | Ok (new_errors, new_warnings, new_suppressions, new_coverage) ->
    ( update_errset errors file new_errors,
      update_errset warnings file new_warnings,
      Error_suppressions.update_suppressions suppressions new_suppressions,
      update_coverage coverage new_coverage,
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
    ~dependency_graph
    ~component_map
    ~recheck_set
    acc =
  with_timer_lwt ~options "Merge" profiling (fun () ->
      let%lwt (merged, { Merge_service.skipped_count; sig_new_or_changed }) =
        Merge_service.merge
          ~master_mutator
          ~worker_mutator
          ~reader
          ~intermediate_result_callback
          ~options
          ~workers
          ~dependency_graph
          ~component_map
          ~recheck_set
      in
      let (errs, warnings, suppressions, coverage, first_internal_error) =
        List.fold_left
          (fun acc (file, result) ->
            let component = FilenameMap.find_unsafe file component_map in
            let acc = Nel.fold_left remove_old_results acc component in
            add_new_results acc file result)
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
          Option.map first_internal_error ~f:(spf "First merge internal error:\n%s") ))

let mk_intermediate_result_callback
    ~reader ~options ~profiling ~persistent_connections ~recheck_reasons suppressions =
  let lazy_table_of_aloc =
    Parsing_heaps.Mutator_reader.get_sig_ast_aloc_table_unsafe_lazy ~reader
  in
  let%lwt send_errors_over_connection =
    match persistent_connections with
    | None -> Lwt.return (fun _ -> ())
    | Some clients ->
      with_timer_lwt ~options "MakeSendErrors" profiling (fun () ->
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
                          Option.value
                            (FilenameMap.get file warns_acc)
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
                  let errors_reason =
                    Persistent_connection_prot.Recheck_streaming { recheck_reasons }
                  in
                  Persistent_connection.update_clients
                    ~clients
                    ~errors_reason
                    ~calc_errors_and_warnings:(fun () -> (new_errors, new_warnings)))))
  in
  let intermediate_result_callback results =
    let errors =
      lazy
        (Core_list.map
           ~f:(fun (file, result) ->
             match result with
             | Ok (errors, warnings, suppressions, _) ->
               let errors = Flow_error.make_errors_printable lazy_table_of_aloc errors in
               let warnings = Flow_error.make_errors_printable lazy_table_of_aloc warnings in
               (file, errors, warnings, suppressions)
             | Error msg ->
               let errors = error_set_of_internal_error file msg in
               let errors = Flow_error.make_errors_printable lazy_table_of_aloc errors in
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
    ~dependency_graph
    ~deleted
    ~persistent_connections
    ~recheck_reasons
    ~prep_merge =
  let { ServerEnv.local_errors; merge_errors; warnings; suppressions } = errors in
  let%lwt intermediate_result_callback =
    let persistent_connections =
      match Options.arch options with
      | Options.Classic -> persistent_connections
      | Options.TypesFirst -> None
    in
    mk_intermediate_result_callback
      ~reader
      ~options
      ~profiling
      ~persistent_connections
      ~recheck_reasons
      suppressions
  in
  let%lwt () =
    match prep_merge with
    | None -> Lwt.return_unit
    | Some callback ->
      (* call supplied function to calculate closure of modules to merge *)
      with_timer_lwt ~options "MakeMergeInput" profiling (fun () -> Lwt.return (callback ()))
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
  let%lwt (dependency_graph, component_map) =
    calc_deps ~options ~profiling ~dependency_graph ~components files_to_merge
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
        (FilenameSet.union files_to_merge deleted)
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
        ~dependency_graph
        ~component_map
        ~recheck_set
        (merge_errors, warnings, suppressions, coverage, None)
    in
    let%lwt () =
      if Options.should_profile options then
        with_timer_lwt ~options "PrintGCStats" profiling (fun () ->
            Lwt.return (Gc.print_stat stderr))
      else
        Lwt.return_unit
    in
    let time_to_merge = Unix.gettimeofday () -. merge_start_time in
    Hh_logger.info "Done";
    Lwt.return (result, time_to_merge)
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

let check_files
    ~reader
    ~options
    ~profiling
    ~workers
    ~errors
    ~updated_errors
    ~coverage
    ~merged_files
    ~direct_dependent_files
    ~sig_new_or_changed
    ~dependency_info
    ~persistent_connections
    ~recheck_reasons =
  match Options.arch options with
  | Options.Classic -> Lwt.return (updated_errors, coverage, 0., 0, None)
  | Options.TypesFirst ->
    with_timer_lwt ~options "Check" profiling (fun () ->
        Hh_logger.info "Check prep";
        Hh_logger.info "new or changed signatures: %d" (FilenameSet.cardinal sig_new_or_changed);
        let focused_to_check = CheckedSet.focused merged_files in
        let merged_dependents = CheckedSet.dependents merged_files in
        let skipped_count = ref 0 in
        let all_dependency_graph = Dependency_info.all_dependency_graph dependency_info in
        (* skip dependents whenever none of their dependencies have new or changed signatures *)
        (* NOTE: We don't skip direct dependents because, in particular, they might be direct
         dependents of files that were deleted, became unparsed, or fell out of @flow; those files
         would not be dependencies tracked by the dependency graph; the direct dependents of those
         files would need to be checked nevertheless. Of course, this is a hammer of a solution for
         a nail of a problem.

         TODO: Figure out how to safely skip direct dependents. *)
        let dependents_to_check =
          FilenameSet.filter (fun f ->
              FilenameSet.mem f direct_dependent_files
              || FilenameSet.exists (fun f' -> FilenameSet.mem f' sig_new_or_changed)
                 @@ FilenameMap.find_unsafe f all_dependency_graph
              ||
              ( incr skipped_count;
                false ))
          @@ merged_dependents
        in
        Hh_logger.info "Check will skip %d files" !skipped_count;
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
        let job =
          List.fold_left (fun acc file -> Merge_service.check options ~reader file :: acc)
        in
        let merge new_acc acc =
          intermediate_result_callback (lazy new_acc);
          List.rev_append new_acc acc
        in
        let progress_fn ~total ~start ~length:_ =
          MonitorRPC.status_update
            ServerStatus.(Checking_progress { total = Some total; finished = start })
        in
        let max_size = Options.max_files_checked_per_worker options in
        let%lwt ret =
          MultiWorkerLwt.call
            workers
            ~job
            ~neutral:[]
            ~merge
            ~next:(MultiWorkerLwt.next ~progress_fn ~max_size workers (FilenameSet.elements files))
        in
        let { ServerEnv.merge_errors; warnings; _ } = errors in
        let suppressions = updated_errors.ServerEnv.suppressions in
        let (merge_errors, warnings, suppressions, coverage, first_internal_error) =
          List.fold_left
            (fun acc (file, result) ->
              let acc = remove_old_results acc file in
              add_new_results acc file result)
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
            Option.map first_internal_error ~f:(spf "First check internal error:\n%s") ))

let ensure_parsed ~options ~profiling ~workers ~reader files =
  with_timer_lwt ~options "EnsureParsed" profiling (fun () ->
      let%lwt parse_hash_mismatch_skips =
        Parsing_service_js.ensure_parsed ~reader options workers (CheckedSet.all files)
      in
      if FilenameSet.is_empty parse_hash_mismatch_skips then
        Lwt.return_unit
      else
        let files_to_recheck =
          FilenameSet.fold
            (fun f acc -> SSet.add (File_key.to_string f) acc)
            parse_hash_mismatch_skips
            SSet.empty
        in
        let file_count = SSet.cardinal files_to_recheck in
        let reason =
          Persistent_connection_prot.(
            if file_count = 1 then
              Single_file_changed { filename = SSet.elements files_to_recheck |> List.hd }
            else
              Many_files_changed { file_count })
        in
        ServerMonitorListenerState.push_files_to_recheck ~reason files_to_recheck;
        raise Lwt.Canceled)

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
        let locs = Nel.map ALoc.of_loc locs in
        let resolved_r =
          Module_js.imported_module
            ~options
            ~reader:(Abstract_state_reader.State_reader reader)
            ~node_modules_containers:!Files.node_modules_containers
            file
            locs
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
    let reason =
      Persistent_connection_prot.Unchecked_dependencies { filename = File_key.to_string file }
    in
    ServerMonitorListenerState.push_checked_set_to_force ~reason unchecked_dependencies;
    raise Lwt.Canceled
  )

(* Another special case, similar assumptions as above. *)

(** TODO: handle case when file+contents don't agree with file system state **)
let typecheck_contents_ ~options ~env ~check_syntax ~profiling contents filename =
  let%lwt (errors, parse_result, info) =
    parse_contents ~options ~profiling ~check_syntax filename contents
  in
  let reader = State_reader.create () in
  let lazy_table_of_aloc = Parsing_heaps.Reader.get_sig_ast_aloc_table_unsafe_lazy ~reader in
  match parse_result with
  | Parsing_service_js.Parse_ok parse_ok ->
    (* override docblock info *)
    let (ast, file_sig) = Parsing_service_js.basic parse_ok in
    let info = Docblock.set_flow_mode_for_ide_command info in
    (* merge *)
    let%lwt (cx, typed_ast) =
      with_timer_lwt ~options "MergeContents" profiling (fun () ->
          let%lwt () = ensure_checked_dependencies ~options ~reader ~env filename file_sig in
          Lwt.return
            (Merge_service.merge_contents_context ~reader options filename ast info file_sig))
    in
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
    let errors = Flow_error.make_errors_printable lazy_table_of_aloc errors in
    let warnings = Flow_error.make_errors_printable lazy_table_of_aloc warnings in
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
    Lwt.return (Some (cx, ast, file_sig, typed_ast), errors, warnings, info)
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
    let errors = Flow_error.make_errors_printable lazy_table_of_aloc errors in
    Lwt.return (None, errors, Errors.ConcreteLocPrintableErrorSet.empty, info)
  | Parsing_service_js.Parse_skip
      (Parsing_service_js.Skip_non_flow_file | Parsing_service_js.Skip_resource_file) ->
    (* should never happen *)
    let errors = Flow_error.make_errors_printable lazy_table_of_aloc errors in
    Lwt.return (None, errors, Errors.ConcreteLocPrintableErrorSet.empty, info)

let typecheck_contents ~options ~env ~profiling contents filename =
  let%lwt (cx_opt, errors, warnings, _info) =
    typecheck_contents_ ~options ~env ~check_syntax:true ~profiling contents filename
  in
  Lwt.return (cx_opt, errors, warnings)

let basic_check_contents ~options ~env ~profiling contents filename =
  try%lwt
    let%lwt (cx_opt, _errors, _warnings, info) =
      typecheck_contents_ ~options ~env ~check_syntax:false ~profiling contents filename
    in
    let (cx, file_sig, typed_ast) =
      match cx_opt with
      | Some (cx, _, file_sig, typed_ast) -> (cx, file_sig, typed_ast)
      | None -> failwith "Couldn't parse file"
    in
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
      let errors =
        FilenameSet.fold
          (fun filename errors ->
            match filename with
            | File_key.JsonFile str when Filename.basename str = "package.json" ->
              let ast = Parsing_heaps.Mutator_reader.get_ast_unsafe ~reader filename in
              let package = Package_json.parse ast in
              Module_js.add_package str package;
              begin
                match package with
                | Ok _ -> errors
                | Error parse_err ->
                  let errset =
                    Inference_utils.set_of_package_json_error ~source_file:filename parse_err
                  in
                  update_errset errors filename errset
              end
            | _ -> errors)
          parsed
          FilenameMap.empty
      in
      Lwt.return errors)

let init_libs ~options ~profiling ~local_errors ~warnings ~suppressions ~reader ordered_libs =
  with_timer_lwt ~options "InitLibs" profiling (fun () ->
      let%lwt lib_files =
        let options =
          match Options.verbose options with
          | Some { Verbose.enabled_during_flowlib = false; _ } ->
            (* Normally we disable verbosity while loading the libs. But if we're running with
             * --verbose-flowlib then we want to leave verbosity on *)
            { options with Options.opt_verbose = None }
          | _ -> options
        in
        Init_js.init ~options ~reader ordered_libs
      in
      Lwt.return
      @@ List.fold_left
           (fun acc (lib_file, ok, errs, warnings, suppressions) ->
             let (all_ok, errors_acc, warnings_acc, suppressions_acc) = acc in
             let all_ok =
               if ok then
                 all_ok
               else
                 false
             in
             let errors_acc = update_errset errors_acc lib_file errs in
             let warnings_acc = update_errset warnings_acc lib_file warnings in
             let suppressions_acc =
               Error_suppressions.update_suppressions suppressions_acc suppressions
             in
             (all_ok, errors_acc, warnings_acc, suppressions_acc))
           (true, local_errors, warnings, suppressions)
           lib_files)

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
    ~all_dependency_graph
    ~dependency_graph
    ~input_focused
    ~input_dependencies
    ~all_dependent_files =
  let input =
    CheckedSet.add
      ~focused:input_focused
      ~dependencies:(Option.value ~default:FilenameSet.empty input_dependencies)
      CheckedSet.empty
  in
  (* Filter unchecked files out of the input *)
  let input = CheckedSet.filter input ~f:is_file_checked in
  let focused = CheckedSet.focused input in
  (* Roots is the set of all focused files and all dependent files. *)
  let roots =
    Pure_dep_graph_operations.calc_all_dependents ~dependency_graph ~all_dependency_graph focused
  in
  let dependents = FilenameSet.diff roots focused in
  let dependencies = CheckedSet.dependencies input in
  let checked_files = CheckedSet.add ~focused ~dependents ~dependencies CheckedSet.empty in
  (* It's possible that all_dependent_files contains foo.js, which is a dependent of a
   * dependency. That's fine if foo.js is in the checked set. But if it's just some random
   * other dependent then we need to filter it out.
   *)
  let all_dependent_files = FilenameSet.inter all_dependent_files (CheckedSet.all checked_files) in
  Lwt.return (checked_files, all_dependent_files)

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
    ~options ~input_focused ~input_dependencies ~all_dependent_files =
  let focused = filter_out_node_modules ~options input_focused in
  let dependencies = Option.value ~default:FilenameSet.empty input_dependencies in
  Lwt.return (CheckedSet.add ~focused ~dependencies CheckedSet.empty, all_dependent_files)

(* Called on initialization in non-lazy mode, with optional focus targets.

   When focus targets are not provided, the result is a checked set focusing on parsed files minus
   node modules, plus no dependents (because effectively any dependent is already focused), plus all
   their dependencies (minus those that are already focused). The set of dependencies might contain
   node modules.

   When focus targets are provided, the result is a checked set focusing on those files, plus their
   dependents, plus all their combined dependencies. All these sets might contain node modules.

   In either case, we can consider the result to be "closed" in terms of expected invariants.
*)
let files_to_infer ~options ~profiling ~reader ~dependency_info ?focus_targets ~parsed =
  with_timer_lwt ~options "FilesToInfer" profiling (fun () ->
      match focus_targets with
      | None ->
        unfocused_files_and_dependents_to_infer
          ~options
          ~input_focused:parsed
          ~input_dependencies:None
          ~all_dependent_files:FilenameSet.empty
      | Some input_focused ->
        let all_dependency_graph = Dependency_info.all_dependency_graph dependency_info in
        let dependency_graph = Dependency_info.dependency_graph dependency_info in
        let is_file_checked =
          is_file_tracked_and_checked ~reader:(Abstract_state_reader.Mutator_state_reader reader)
        in
        focused_files_and_dependents_to_infer
          ~is_file_checked
          ~all_dependency_graph
          ~dependency_graph
          ~input_focused
          ~input_dependencies:None
          ~all_dependent_files:FilenameSet.empty)

let restart_if_faster_than_recheck ~options ~env ~to_merge ~file_watcher_metadata =
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
      let files_about_to_recheck = CheckedSet.cardinal to_merge in
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

module Recheck : sig
  type recheck_result = {
    new_or_changed: Utils_js.FilenameSet.t;
    deleted: Utils_js.FilenameSet.t;
    all_dependent_files: Utils_js.FilenameSet.t;
    top_cycle: (File_key.t * int) option;
    merge_skip_count: int;
    check_skip_count: int;
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
    recheck_reasons:Persistent_connection_prot.recheck_reason list ->
    will_be_checked_files:CheckedSet.t ref ->
    env:ServerEnv.env ->
    (ServerEnv.env * recheck_result * string option) Lwt.t

  val parse_and_update_dependency_info :
    profiling:Profiling_js.running ->
    transaction:Transaction.t ->
    reader:Parsing_heaps.Mutator_reader.reader ->
    options:Options.t ->
    workers:MultiWorkerLwt.worker list option ->
    updates:Utils_js.FilenameSet.t ->
    files_to_force:CheckedSet.t ->
    recheck_reasons:Persistent_connection_prot.recheck_reason list ->
    env:ServerEnv.env ->
    ServerEnv.env Lwt.t

  (* Exposed only for testing purposes. Not meant for general consumption. *)
  val determine_what_to_recheck :
    profiling:Profiling_js.running ->
    options:Options.t ->
    is_file_checked:(File_key.t -> bool) ->
    ide_open_files:SSet.t Lazy.t ->
    dependency_graph:FilenameSet.t FilenameMap.t ->
    all_dependency_graph:FilenameSet.t FilenameMap.t ->
    checked_files:CheckedSet.t ->
    freshparsed:FilenameSet.t ->
    unparsed_set:FilenameSet.t ->
    deleted:FilenameSet.t ->
    unchanged_checked:CheckedSet.t ->
    files_to_force:CheckedSet.t ->
    unchanged_files_to_force:CheckedSet.t ->
    direct_dependent_files:FilenameSet.t ->
    (* to_merge, components, recheck_set, all_dependent_files *)
    (CheckedSet.t * File_key.t Nel.t list * FilenameSet.t * FilenameSet.t) Lwt.t
end = struct
  type recheck_result = {
    new_or_changed: Utils_js.FilenameSet.t;
    deleted: Utils_js.FilenameSet.t;
    all_dependent_files: Utils_js.FilenameSet.t;
    top_cycle: (File_key.t * int) option;
    merge_skip_count: int;
    check_skip_count: int;
    estimates: Recheck_stats.estimates option;
  }

  (* This is the first part of the recheck. It parses the files and updates the dependency graph. It
   * does NOT figure out which files to merge or merge them.
   *
   * It returns an updated env and a bunch of intermediate values which `recheck_merge` can use to
   * calculate the to_merge and perform the merge *)
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
    let lazy_table_of_aloc =
      Parsing_heaps.Mutator_reader.get_sig_ast_aloc_table_unsafe_lazy ~reader
    in
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
              Hh_logger.info "%d/%d: %s" i n (File_key.to_string f);
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
        let error_set = Flow_error.make_errors_printable lazy_table_of_aloc error_set in
        if Errors.ConcreteLocPrintableErrorSet.cardinal error_set > 0 then
          Persistent_connection.update_clients
            ~clients:env.ServerEnv.connections
            ~errors_reason:(Persistent_connection_prot.Recheck_streaming { recheck_reasons })
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
      with_timer_lwt ~options "ModuleClearFiles" profiling (fun () ->
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
    let%lwt (changed_modules, errors) =
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
      with_timer_lwt ~options "CalcUnchangedModules" profiling (fun () ->
          Module_js.calc_unchanged_modules ~reader workers unchanged_files_with_dependents)
    in
    let parsed = FilenameSet.union freshparsed unchanged in
    (* direct_dependent_files are unchanged files which directly depend on changed modules,
       or are new / changed files that are phantom dependents. all_dependent_files are
       direct_dependent_files plus their dependents (transitive closure) *)
    let%lwt direct_dependent_files =
      with_timer_lwt ~options "DirectDependentFiles" profiling (fun () ->
          let root_files = FilenameSet.union new_or_changed unchanged_files_with_dependents in
          DirectDependentFilesCache.with_cache ~options ~root_files ~on_miss:(fun () ->
              Dep_service.calc_direct_dependents
                ~reader:(Abstract_state_reader.Mutator_state_reader reader)
                workers
                ~candidates:(FilenameSet.diff unchanged unchanged_files_with_dependents)
                ~root_files
                ~root_modules:(Modulename.Set.union unchanged_modules changed_modules)))
    in
    Hh_logger.info "Re-resolving directly dependent files";

    let node_modules_containers = !Files.node_modules_containers in
    (* requires in direct_dependent_files must be re-resolved before merging. *)
    let mutator =
      Module_heaps.Resolved_requires_mutator.create transaction direct_dependent_files
    in
    let%lwt () =
      with_timer_lwt ~options "ReresolveDirectDependents" profiling (fun () ->
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
          Lwt.return_unit)
    in
    Hh_logger.info "Recalculating dependency graph";
    let%lwt dependency_info =
      with_timer_lwt ~options "CalcDepsTypecheck" profiling (fun () ->
          let files_to_update_dependency_info =
            FilenameSet.union freshparsed direct_dependent_files
          in
          let%lwt updated_dependency_info =
            Dep_service.calc_partial_dependency_info
              ~options
              ~reader
              workers
              files_to_update_dependency_info
              ~parsed
          in
          let old_dependency_info = env.ServerEnv.dependency_info in
          let to_remove = FilenameSet.union unparsed_set deleted in
          match (old_dependency_info, updated_dependency_info) with
          | (Dependency_info.Classic old_map, Dependency_info.Classic updated_map) ->
            Lwt.return
              (Dependency_info.Classic
                 ( old_map
                 |> FilenameSet.fold FilenameMap.remove to_remove
                 |> FilenameMap.union updated_map ))
          | (Dependency_info.TypesFirst old_map, Dependency_info.TypesFirst updated_map) ->
            Lwt.return
              (Dependency_info.TypesFirst
                 ( old_map
                 |> FilenameSet.fold FilenameMap.remove to_remove
                 |> FilenameMap.union updated_map ))
          | _ -> assert false)
    in
    (* Here's how to update unparsed:
     * 1. Remove the parsed files. This removes any file which used to be unparsed but is now parsed
     * 2. Remove the deleted files. This removes any previously unparsed file which was deleted
     * 3. Add the newly unparsed files. This adds new unparsed files or files which became unparsed *)
    let unparsed =
      let to_remove = FilenameSet.union parsed deleted in
      FilenameSet.diff env.ServerEnv.unparsed to_remove |> FilenameSet.union unparsed_set
    in
    let env = { env with ServerEnv.files = parsed; unparsed; dependency_info } in
    let intermediate_values =
      ( deleted,
        direct_dependent_files,
        errors,
        files_to_force,
        freshparsed,
        new_or_changed,
        unchanged_checked,
        unchanged_files_to_force,
        unparsed_set )
    in
    Lwt.return (env, intermediate_values)

  let determine_what_to_recheck
      ~profiling
      ~options
      ~is_file_checked
      ~ide_open_files
      ~dependency_graph
      ~all_dependency_graph
      ~checked_files
      ~freshparsed
      ~unparsed_set
      ~deleted
      ~unchanged_checked
      ~files_to_force
      ~unchanged_files_to_force
      ~direct_dependent_files =
    let%lwt all_dependent_files =
      with_timer_lwt ~options "AllDependentFiles" profiling (fun () ->
          if
            FilenameSet.is_empty direct_dependent_files
            (* as is the case for anything doing `check_contents` *)
          then
            Lwt.return FilenameSet.empty
          (* avoid O(dependency graph) calculations *)
          else
            Lwt.return
              (Pure_dep_graph_operations.calc_all_dependents
                 ~dependency_graph
                 ~all_dependency_graph
                 direct_dependent_files))
    in
    let acceptable_files_to_focus =
      FilenameSet.union freshparsed (CheckedSet.all unchanged_files_to_force)
    in
    let%lwt (updated_checked_files, all_dependent_files) =
      with_timer_lwt ~options "RecalcDepGraph" profiling (fun () ->
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
              ~all_dependency_graph
              ~dependency_graph
              ~input_focused
              ~input_dependencies
              ~all_dependent_files)
    in
    (* Filter updated_checked_files down to the files which we just parsed or unchanged files which
     * will be focused *)
    let input =
      CheckedSet.filter updated_checked_files ~f:(fun fn ->
          FilenameSet.mem fn acceptable_files_to_focus)
    in
    let%lwt (to_merge, components, recheck_set) =
      include_dependencies_and_dependents
        ~options
        ~profiling
        ~unchanged_checked
        ~input
        ~all_dependency_graph
        ~dependency_graph
        ~all_dependent_files
    in
    Lwt.return (to_merge, components, recheck_set, all_dependent_files)

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
          unparsed_set ) =
      intermediate_values
    in
    let dependency_info = env.ServerEnv.dependency_info in
    let all_dependency_graph = Dependency_info.all_dependency_graph dependency_info in
    let dependency_graph = Dependency_info.dependency_graph dependency_info in
    let is_file_checked =
      is_file_tracked_and_checked ~reader:(Abstract_state_reader.Mutator_state_reader reader)
    in
    let%lwt (to_merge, components, recheck_set, all_dependent_files) =
      determine_what_to_recheck
        ~profiling
        ~options
        ~is_file_checked
        ~ide_open_files:(lazy (Persistent_connection.get_opened_files env.ServerEnv.connections))
        ~dependency_graph
        ~all_dependency_graph
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
    will_be_checked_files := CheckedSet.union env.ServerEnv.checked_files to_merge;

    let%lwt estimates =
      restart_if_faster_than_recheck ~options ~env ~to_merge ~file_watcher_metadata
    in
    let%lwt () = ensure_parsed ~options ~profiling ~workers ~reader to_merge in
    (* recheck *)
    let%lwt ( updated_errors,
              coverage,
              merge_skip_count,
              sig_new_or_changed,
              top_cycle,
              time_to_merge,
              merge_internal_error ) =
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
        ~dependency_graph
        ~deleted
        ~persistent_connections:(Some env.ServerEnv.connections)
        ~recheck_reasons
        ~prep_merge:
          (Some
             (fun () ->
               let n = FilenameSet.cardinal all_dependent_files in
               if n > 0 then Hh_logger.info "remerge %d dependent files:" n;

               let _ =
                 FilenameSet.fold
                   (fun f i ->
                     Hh_logger.info "%d/%d: %s" i n (File_key.to_string f);
                     i + 1)
                   all_dependent_files
                   1
               in
               Hh_logger.info "Merge prep"))
    in
    Option.iter merge_internal_error ~f:(Hh_logger.error "%s");

    let merged_files = to_merge in
    let%lwt (errors, coverage, time_to_check_merged, check_skip_count, check_internal_error) =
      check_files
        ~reader
        ~options
        ~profiling
        ~workers
        ~errors
        ~updated_errors
        ~coverage
        ~merged_files
        ~direct_dependent_files
        ~sig_new_or_changed
        ~dependency_info
        ~persistent_connections:(Some env.ServerEnv.connections)
        ~recheck_reasons
    in
    Option.iter check_internal_error ~f:(Hh_logger.error "%s");

    let%lwt () =
      Recheck_stats.record_recheck_time
        ~options
        ~total_time:(time_to_merge +. time_to_check_merged)
        ~rechecked_files:(CheckedSet.cardinal merged_files)
    in
    let checked_files = CheckedSet.union unchanged_checked merged_files in
    Hh_logger.info "Checked set: %s" (CheckedSet.debug_counts_to_string checked_files);

    (* NOTE: unused fields are left in their initial empty state *)
    env.ServerEnv.collated_errors := None;
    Lwt.return
      ( { env with ServerEnv.checked_files; errors; coverage },
        {
          new_or_changed;
          deleted;
          all_dependent_files;
          top_cycle;
          merge_skip_count;
          check_skip_count;
          estimates;
        },
        Option.first_some merge_internal_error check_internal_error )

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
    let%lwt (env, _intermediate_values) =
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
    Lwt.return env
end

let with_transaction f =
  Transaction.with_transaction
  @@ fun transaction ->
  let reader = Mutator_state_reader.create transaction in
  f transaction reader

let recheck
    ~options
    ~workers
    ~updates
    env
    ~files_to_force
    ~file_watcher_metadata
    ~recheck_reasons
    ~will_be_checked_files =
  let should_print_summary = Options.should_profile options in
  let%lwt (profiling, (env, stats, first_internal_error)) =
    Profiling_js.with_profiling_lwt ~label:"Recheck" ~should_print_summary (fun profiling ->
        SharedMem_js.with_memory_profiling_lwt ~profiling ~collect_at_end:true (fun () ->
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
                  ~will_be_checked_files)))
  in
  let {
    Recheck.new_or_changed = modified;
    deleted;
    all_dependent_files = dependent_files;
    top_cycle;
    merge_skip_count;
    check_skip_count;
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
    Option.value_map
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
  FlowEventLogger.recheck
    ~recheck_reasons:
      (List.map Persistent_connection_prot.verbose_string_of_recheck_reason recheck_reasons)
    ~modified
    ~deleted
    ~dependent_files
    ~profiling
    ~merge_skip_count
    ~check_skip_count
    ~estimated_time_to_recheck
    ~estimated_time_to_restart
    ~estimated_time_to_init
    ~estimated_time_per_file
    ~estimated_files_to_recheck
    ~estimated_files_to_init
    ~first_internal_error
    ~scm_update_distance:file_watcher_metadata.MonitorProt.total_update_distance
    ~scm_changed_mergebase:file_watcher_metadata.MonitorProt.changed_mergebase;

  let duration = Profiling_js.get_profiling_duration profiling in
  let dependent_file_count = Utils_js.FilenameSet.cardinal dependent_files in
  let changed_file_count =
    Utils_js.FilenameSet.cardinal modified + Utils_js.FilenameSet.cardinal deleted
  in
  let summary =
    ServerStatus.
      { duration; info = RecheckSummary { dependent_file_count; changed_file_count; top_cycle } }
  in
  Lwt.return (profiling, summary, env)

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

    files |> Core_list.map ~f:(Files.filename_from_string ~options:file_options) |> Bucket.of_list

let mk_init_env ~files ~unparsed ~dependency_info ~ordered_libs ~libs ~errors ~coverage =
  {
    ServerEnv.files;
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
  with_transaction
  @@ fun transaction reader ->
  let file_options = Options.file_options options in
  (* We don't want to walk the file system for the checked in files. But we still need to find the
   * flowlibs *)
  let (ordered_flowlib_libs, _) = Files.init ~flowlibs_only:true file_options in
  let {
    Saved_state.flowconfig_hash = _;
    parsed_heaps;
    unparsed_heaps;
    ordered_non_flowlib_libs;
    local_errors;
    warnings;
    coverage;
    node_modules_containers;
  } =
    saved_state
  in
  Files.node_modules_containers := node_modules_containers;

  Hh_logger.info "Restoring heaps";
  let%lwt () =
    with_timer_lwt ~options "RestoreHeaps" profiling (fun () ->
        let root = Options.root options |> Path.to_string in
        let%lwt () =
          MultiWorkerLwt.call
            workers
            ~job:
              (List.fold_left (fun () (fn, parsed_file_data) ->
                   let { Saved_state.package; file_sig; hash; resolved_requires } =
                     Saved_state.denormalize_parsed_data
                       ~root
                       parsed_file_data.Saved_state.normalized_file_data
                   in
                   (* Every package.json file should have a Package_json.t. Use those to restore the
                    * PackageHeap and the ReversePackageHeap *)
                   begin
                     match fn with
                     | File_key.JsonFile str when Filename.basename str = "package.json" ->
                       begin
                         match package with
                         | None ->
                           failwith
                             (Printf.sprintf "Saved state for `%s` missing Package_json.t data" str)
                         | Some package ->
                           Module_heaps.Package_heap_mutator.add_package_json str package
                       end
                     | _ -> ()
                   end;

                   (* Restore the FileSigHeap *)
                   Parsing_heaps.From_saved_state.add_file_sig fn file_sig;

                   (* Restore the FileHashHeap *)
                   Parsing_heaps.From_saved_state.add_file_hash fn hash;

                   (* Restore the ResolvedRequiresHeap *)
                   Module_heaps.From_saved_state.add_resolved_requires fn resolved_requires))
            ~merge:(fun () () -> ())
            ~neutral:()
            ~next:(MultiWorkerLwt.next workers (FilenameMap.bindings parsed_heaps))
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
          ~next:(MultiWorkerLwt.next workers (FilenameMap.bindings unparsed_heaps)))
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
  let%lwt (libs_ok, local_errors, warnings, suppressions) =
    let suppressions = Error_suppressions.empty in
    init_libs ~options ~profiling ~local_errors ~warnings ~suppressions ~reader ordered_libs
  in
  Hh_logger.info "Resolving dependencies";
  MonitorRPC.status_update ServerStatus.Resolving_dependencies_progress;

  let%lwt (parsed_set, unparsed_set, all_files, parsed, unparsed) =
    with_timer_lwt ~options "PrepareCommitModules" profiling (fun () ->
        let (parsed, parsed_set) =
          FilenameMap.fold
            (fun fn data (parsed, parsed_set) ->
              let parsed = (fn, data.Saved_state.info) :: parsed in
              let parsed_set = FilenameSet.add fn parsed_set in
              (parsed, parsed_set))
            parsed_heaps
            ([], FilenameSet.empty)
        in
        let (unparsed, unparsed_set) =
          FilenameMap.fold
            (fun fn data (unparsed, unparsed_set) ->
              let unparsed = (fn, data.Saved_state.unparsed_info) :: unparsed in
              let unparsed_set = FilenameSet.add fn unparsed_set in
              (unparsed, unparsed_set))
            unparsed_heaps
            ([], FilenameSet.empty)
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
  let%lwt dependency_info =
    with_timer_lwt ~options "CalcDepsTypecheck" profiling (fun () ->
        Dep_service.calc_dependency_info ~options ~reader workers ~parsed:parsed_set)
  in
  let env =
    mk_init_env
      ~files:parsed_set
      ~unparsed:unparsed_set
      ~dependency_info
      ~ordered_libs
      ~libs
      ~errors
      ~coverage
  in
  Lwt.return (env, libs_ok)

let init ~profiling ~workers options =
  let file_options = Options.file_options options in
  with_transaction
  @@ fun transaction reader ->
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
  let%lwt (parsed, unparsed, unchanged, local_errors) =
    parse ~options ~profiling ~workers ~reader next_files
  in
  (* Parsing won't raise warnings *)
  let warnings = FilenameMap.empty in
  (* Libdefs have no coverage *)
  let coverage = FilenameMap.empty in
  assert (FilenameSet.is_empty unchanged);

  Hh_logger.info "Building package heap";
  let%lwt package_errors = init_package_heap ~options ~profiling ~reader parsed in
  let local_errors = merge_error_maps package_errors local_errors in
  Hh_logger.info "Loading libraries";
  let%lwt (libs_ok, local_errors, warnings, suppressions) =
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
  let%lwt (_, errors) =
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
    with_timer_lwt ~options "CalcDepsTypecheck" profiling (fun () ->
        Dep_service.calc_dependency_info ~options ~reader workers ~parsed)
  in
  let env =
    mk_init_env
      ~files:parsed
      ~unparsed:unparsed_set
      ~dependency_info
      ~ordered_libs
      ~libs
      ~errors
      ~coverage
  in
  Lwt.return (FilenameSet.empty, env, libs_ok)

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
         | Core_result.Error { Recheck_updates.msg; _ } ->
           Hh_logger.error "The saved state is no longer valid due to file changes: %s" msg;
           raise Saved_state.(Invalid_saved_state Changed_files)
         | Core_result.Ok updates -> updates
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
       if Options.saved_state_no_fallback options then
         let msg = spf "Failed to load saved state: %s" invalid_reason in
         FlowExitStatus.exit ~msg FlowExitStatus.Invalid_saved_state
       else
         Lwt.return_none)

let query_watchman_for_changed_files ~options =
  match Options.lazy_mode options with
  | Options.NON_LAZY_MODE
  | Options.LAZY_MODE_FILESYSTEM
  | Options.LAZY_MODE_IDE ->
    Lwt.return (fun ~libs:_ -> Lwt.return FilenameSet.(empty, empty))
  | Options.LAZY_MODE_WATCHMAN ->
    let init_settings =
      {
        (* We're not setting up a subscription, we're just sending a single query *)
        Watchman_lwt.subscribe_mode = None;
        (* Hack makes this configurable in their local config. Apparently buck & hgwatchman
         * use 10 seconds. But I've seen 10s timeout, so let's not set a timeout. Instead we'll
         * manually timeout later *)
        init_timeout = Watchman_lwt.No_timeout;
        expression_terms = Watchman_expression_terms.make ~options;
        subscription_prefix = "flow_server_watcher";
        roots = Files.watched_paths (Options.file_options options);
        debug_logging = Options.is_debug_mode options;
      }
    in
    let%lwt watchman_env = Watchman_lwt.init init_settings () in
    let%lwt changed_files =
      match watchman_env with
      | None ->
        failwith "Failed to set up Watchman in order to get the changes since the mergebase"
      | Some watchman_env ->
        (* No timeout. We'll time this out ourselves after init if we need *)
        let%lwt changed_files =
          Watchman_lwt.(get_changes_since_mergebase ~timeout:No_timeout watchman_env)
        in
        let%lwt () = Watchman_lwt.close watchman_env in
        Lwt.return (SSet.of_list changed_files)
    in
    Lwt.return (fun ~libs ->
        let updates =
          Recheck_updates.process_updates ~skip_incompatible:true ~options ~libs changed_files
        in
        match updates with
        | Core_result.Error { Recheck_updates.msg; _ } ->
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
          Lwt.return (updates, files_to_focus))

let init ~profiling ~workers options =
  let start_time = Unix.gettimeofday () in
  (* Don't wait for this thread yet. It will run in the background. Then, after init is done,
   * we'll wait on it. We do this because we want to send the status update that we're waiting for
   * Watchman if init is done but Watchman is not *)
  let get_watchman_updates_thread = query_watchman_for_changed_files ~options in
  let%lwt (updates, env, libs_ok) =
    match%lwt load_saved_state ~profiling ~workers options with
    | None ->
      (* Either there is no saved state or we failed to load it for some reason *)
      init ~profiling ~workers options
    | Some (saved_state, updates) ->
      (* We loaded a saved state successfully! We are awesome! *)
      let%lwt (env, libs_ok) = init_from_saved_state ~profiling ~workers ~saved_state options in
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
        with_transaction
        @@ fun transaction reader ->
        let recheck_reasons = [Persistent_connection_prot.Lazy_init_update_deps] in
        let%lwt env =
          Recheck.parse_and_update_dependency_info
            ~profiling
            ~transaction
            ~reader
            ~options
            ~workers
            ~updates
            ~files_to_force:CheckedSet.empty
            ~recheck_reasons
            ~env
        in
        Lwt.return (FilenameSet.empty, env, libs_ok)
  in
  let%lwt (updates, files_to_focus) =
    let now = Unix.gettimeofday () in
    (* Let's give Watchman another 15 seconds to finish. *)
    let timeout = 15.0 in
    let deadline = now +. timeout in
    MonitorRPC.status_update ~event:(ServerStatus.Watchman_wait_start deadline);
    let%lwt (watchman_updates, files_to_focus) =
      try%lwt
        Lwt_unix.with_timeout timeout
        @@ fun () ->
        let%lwt get_watchman_updates = get_watchman_updates_thread in
        get_watchman_updates ~libs:env.ServerEnv.libs
      with Lwt_unix.Timeout ->
        let msg =
          Printf.sprintf
            "Timed out after %ds waiting for Watchman."
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
  if (FilenameSet.is_empty updates && FilenameSet.is_empty files_to_focus) || not libs_ok then
    Lwt.return (libs_ok, env, last_estimates)
  else
    let files_to_force = CheckedSet.(add ~focused:files_to_focus empty) in
    let recheck_reasons = [Persistent_connection_prot.Lazy_init_typecheck] in
    let%lwt (recheck_profiling, _summary, env) =
      recheck
        ~options
        ~workers
        ~updates
        env
        ~files_to_force
        ~file_watcher_metadata:MonitorProt.empty_file_watcher_metadata
        ~recheck_reasons
        ~will_be_checked_files:(ref files_to_force)
    in
    Profiling_js.merge ~from:recheck_profiling ~into:profiling;
    Lwt.return (true, env, last_estimates)

let full_check ~profiling ~options ~workers ?focus_targets env =
  let { ServerEnv.files = parsed; dependency_info; errors; _ } = env in
  with_transaction (fun transaction reader ->
      let%lwt (input, all_dependent_files) =
        files_to_infer ~options ~reader ?focus_targets ~profiling ~parsed ~dependency_info
      in
      let all_dependency_graph = Dependency_info.all_dependency_graph dependency_info in
      let dependency_graph = Dependency_info.dependency_graph dependency_info in
      let%lwt (to_merge, components, recheck_set) =
        include_dependencies_and_dependents
          ~options
          ~profiling
          ~unchanged_checked:CheckedSet.empty
          ~input
          ~all_dependency_graph
          ~dependency_graph
          ~all_dependent_files
      in
      (* The values to_merge and recheck_set are essentially the same as input, aggregated. This
       is not surprising because files_to_infer returns a closed checked set. Thus, the only purpose
       of calling include_dependencies_and_dependents is to compute components. *)
      let%lwt () = ensure_parsed ~options ~profiling ~workers ~reader to_merge in
      let dependency_graph = Dependency_info.dependency_graph dependency_info in
      let recheck_reasons = [Persistent_connection_prot.Full_init] in
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
          ~dependency_graph
          ~deleted:FilenameSet.empty
          ~persistent_connections:None
          ~recheck_reasons
          ~prep_merge:None
      in
      Option.iter merge_internal_error ~f:(Hh_logger.error "%s");

      let merged_files = to_merge in
      let%lwt (errors, coverage, _, _, check_internal_error) =
        check_files
          ~reader
          ~options
          ~profiling
          ~workers
          ~errors
          ~updated_errors
          ~coverage
          ~merged_files
          ~direct_dependent_files:FilenameSet.empty
          ~sig_new_or_changed
          ~dependency_info
          ~persistent_connections:None
          ~recheck_reasons
      in
      Option.iter check_internal_error ~f:(Hh_logger.error "%s");

      let first_internal_error = Option.first_some merge_internal_error check_internal_error in
      let checked_files = merged_files in
      Hh_logger.info "Checked set: %s" (CheckedSet.debug_counts_to_string checked_files);
      Lwt.return ({ env with ServerEnv.checked_files; errors; coverage }, first_internal_error))

let debug_determine_what_to_recheck = Recheck.determine_what_to_recheck
