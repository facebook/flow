(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module drives the type checker *)

open Utils_js

(****************** typecheck job helpers *********************)

let clear_errors (files: FilenameSet.t) errors =
  FilenameSet.fold
    (fun file { ServerEnv.local_errors; merge_errors; suppressions; severity_cover_set; } ->
      Hh_logger.debug "clear errors %s" (File_key.to_string file);
      { ServerEnv.
        local_errors = FilenameMap.remove file local_errors;
        merge_errors = FilenameMap.remove file merge_errors;
        suppressions = FilenameMap.remove file suppressions;
        severity_cover_set = FilenameMap.remove file severity_cover_set;
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

let update_severity_cover_set map file severity_cover =
  FilenameMap.add file severity_cover map

let with_timer ?options timer profiling f =
  Profiling_js.start_timer ~timer profiling;
  let ret = f () in
  Profiling_js.stop_timer ~timer profiling;

  (* If we're profiling then output timing information to stderr *)
  (match options with
  | Some options when Options.should_profile options ->
      (match Profiling_js.get_finished_timer ~timer profiling with
      | Some (start_wall_age, wall_duration, cpu_usage, flow_cpu_usage) ->
          let stats = Printf.sprintf
            "start_wall_age: %f; wall_duration: %f; cpu_usage: %f; flow_cpu_usage: %f"
            start_wall_age
            wall_duration
            cpu_usage
            flow_cpu_usage in
          Hh_logger.info
            "TimingEvent `%s`: %s"
            timer
            stats
      | _ -> ());
  | _ -> ());

  ret

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
  with_timer ~options "Parsing" profiling (fun () ->
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
          | Module_js.ModuleDuplicateProviderError { module_name; provider; conflict; } ->
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
        let file_sig = Parsing_service_js.get_file_sig_unsafe filename in
        let require_loc = File_sig.(require_loc_map file_sig.module_sig) in
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

  let { ServerEnv.local_errors; merge_errors; suppressions; severity_cover_set } = errors in

  let changed_modules, local_errors = commit_modules
    ~options profiling ~workers parsed unparsed ~old_modules local_errors new_or_changed in

  let resolve_errors = resolve_requires
    ~options profiling ~workers parsed in
  let local_errors = FilenameMap.union resolve_errors local_errors in

  changed_modules, { ServerEnv.local_errors; merge_errors; suppressions; severity_cover_set }

let error_set_of_merge_exception file exc =
  let loc = Loc.({ none with source = Some file }) in
  let msg = Flow_error.(EInternal (loc, MergeJobException exc)) in
  let error = Flow_error.error_of_msg ~trace_reasons:[] ~op:None ~source_file:file msg in
  Errors.ErrorSet.singleton error

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
    dependency_graph
    component_map
    recheck_map
    acc
    =
  with_timer ~options "Merge" profiling (fun () ->
    let merged = Merge_service.merge_strict
      ~intermediate_result_callback ~options ~workers dependency_graph component_map recheck_map
    in
    List.fold_left (fun acc (file, result) ->
      let component = FilenameMap.find_unsafe file component_map in
      (* remove all errors, suppressions for rechecked component *)
      let errors, suppressions, severity_cover_set =
        List.fold_left (fun (errors, suppressions, severity_cover_set) file ->
          FilenameMap.remove file errors,
          FilenameMap.remove file suppressions,
          FilenameMap.remove file severity_cover_set
        ) acc component
      in
      match result with
      | Ok (new_errors, new_suppressions, new_severity_cover) ->
        update_errset errors file new_errors,
        update_suppressions suppressions file new_suppressions,
        update_severity_cover_set severity_cover_set file new_severity_cover
      | Error exc ->
        let new_errors = error_set_of_merge_exception file exc in
        update_errset errors file new_errors, suppressions, severity_cover_set
    ) acc merged
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
  let { ServerEnv.local_errors; merge_errors; suppressions; severity_cover_set } = errors in

  (* The infer_input passed into typecheck basically tells us what the caller wants to typecheck.
   * However, due to laziness, it's possible that certain dependents or dependencies have not been
   * checked yet. So we need to caluclate all the transitive dependents and transitive dependencies
   * and add them to infer_input, unless they're already checked and in unchanged_checked
   *)
  let infer_input =
    let infer_input = CheckedSet.add ~dependents:all_dependent_files infer_input in

    (* Don't just look up the dependencies of the focused or dependent modules. Also look up
     * the dependencies of dependencies, since we need to check transitive dependencies *)
    let roots = CheckedSet.all infer_input in
    let dependency_graph = Dep_service.calc_dependency_graph workers parsed in
    let all_dependencies = Dep_service.calc_all_dependencies dependency_graph roots in
    let infer_input = CheckedSet.add ~dependencies:all_dependencies infer_input in

    CheckedSet.diff infer_input unchanged_checked
  in

  (* Continue profiling noop Infer to compare with pre-refactor metrics *)
  let () = with_timer ~options "Infer" profiling (fun () -> ()) in

  let send_errors_over_connection =
    match persistent_connections with
    | None -> fun _ -> ()
    | Some conns ->
      (* Each merge step uncovers new errors, warnings, suppressions and lint severity covers.

         While more suppressions and severity covers may come in later steps, the suppressions and
         severity covers we've seen so far are sufficient to filter the errors and warnings we've
         seen so far.

         Intuitively, we will not see an error (or warning) before we've seen all the files involved
         in that error, and thus all the suppressions which could possibly suppress the error. *)
      let open Errors in
      let curr_errors = ref ErrorSet.empty in
      let curr_warnings = ref ErrorSet.empty in
      let curr_suppressions = ref (Error_suppressions.union_suppressions suppressions) in
      let curr_severity_cover = ref (ExactCover.union_all severity_cover_set) in
      let filter = Error_suppressions.filter_suppressed_errors in
      function lazy results ->
        let new_errors, new_warnings, suppressions, severity_cover =
          List.fold_left (fun (errs_acc, warns_acc, supps_acc, lints_acc) result ->
            let file, errs_and_warns, supps, lints = result in
            let supps_acc = Error_suppressions.union supps_acc supps in
            let lints_acc = ExactCover.union lints_acc lints in
            (* Filter errors and warnings based on suppressions we've seen so far. *)
            let errs, warns, _, _ = filter supps_acc lints_acc errs_and_warns in
            (* Only add errors we haven't seen before. *)
            let errs_acc = ErrorSet.fold (fun err acc ->
              if ErrorSet.mem err !curr_errors
              then acc
              else ErrorSet.add err acc
            ) errs errs_acc in
            (* Only add warnings we haven't seen before. Note that new warnings are stored by
               filename, because the clients only receive warnings for files they have open. *)
            let warns_acc =
              let acc = Option.value (FilenameMap.get file warns_acc) ~default:ErrorSet.empty in
              let acc = ErrorSet.fold (fun warn acc ->
                if ErrorSet.mem warn !curr_warnings
                then acc
                else ErrorSet.add warn acc
              ) warns acc in
              if ErrorSet.is_empty acc then warns_acc else FilenameMap.add file acc warns_acc
            in
            errs_acc, warns_acc, supps_acc, lints_acc
          ) (ErrorSet.empty, FilenameMap.empty, !curr_suppressions, !curr_severity_cover) results
        in

        curr_errors := ErrorSet.union new_errors !curr_errors;
        curr_warnings := FilenameMap.fold (fun _ -> ErrorSet.union) new_warnings !curr_warnings;
        curr_suppressions := suppressions;
        curr_severity_cover := severity_cover;

        if not (ErrorSet.is_empty new_errors && FilenameMap.is_empty new_warnings) then
          Persistent_connection.update_clients conns ~errors:new_errors ~warnings:new_warnings
  in

  (* call supplied function to calculate closure of modules to merge *)
  let merge_input =
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
    Hh_logger.info "to_merge: %s" (CheckedSet.debug_counts_to_string to_merge);
    Hh_logger.info "Calculating dependencies";
    let dependency_graph, component_map =
      calc_deps ~options ~profiling ~workers (CheckedSet.all to_merge |> FilenameSet.elements) in

    Hh_logger.info "Merging";
    let merge_errors, suppressions, severity_cover_set = try
      let intermediate_result_callback results =
        let errors = lazy (
          List.map (fun (file, result) ->
            match result with
            | Ok (errors, suppressions, severity_cover) ->
              file, errors, suppressions, severity_cover
            | Error exc ->
              let errors = error_set_of_merge_exception file exc in
              let suppressions = Error_suppressions.empty in
              let severity_cover =
                ExactCover.file_cover file
                  (Options.lint_severities options)
              in
              file, errors, suppressions, severity_cover
          ) (Lazy.force results)
        ) in
        send_errors_over_connection errors
      in

      let merge_errors, suppressions, severity_cover_set =
        merge
          ~intermediate_result_callback
          ~options
          ~profiling
          ~workers
          dependency_graph
          component_map
          recheck_map
          (merge_errors, suppressions, severity_cover_set)
      in
      if Options.should_profile options then Gc.print_stat stderr;
      Hh_logger.info "Done";
      merge_errors, suppressions, severity_cover_set
    with
    (* Unrecoverable exceptions *)
    | SharedMem_js.Out_of_shared_memory
    | SharedMem_js.Heap_full
    | SharedMem_js.Hash_table_full
    | SharedMem_js.Dep_table_full as exn -> raise exn
    (* A catch all suppression is probably a bad idea... *)
    | exc when not Build_mode.dev ->
        prerr_endline (Printexc.to_string exc);
        merge_errors, suppressions, severity_cover_set
    in

    let checked = CheckedSet.union unchanged_checked to_merge in
    Hh_logger.info "Checked set: %s" (CheckedSet.debug_counts_to_string checked);

    checked,
    { ServerEnv.local_errors; merge_errors; suppressions; severity_cover_set }

  | None ->
    unchanged_checked,
      { ServerEnv.local_errors; merge_errors; suppressions; severity_cover_set }

(* When checking contents, ensure that dependencies are checked. Might have more
   general utility. *)
let ensure_checked_dependencies ~options ~profiling ~workers ~env resolved_requires =
  let infer_input = Modulename.Set.fold (fun m acc ->
    match Module_js.get_file m ~audit:Expensive.warn with
    | Some f ->
      if FilenameSet.mem f !env.ServerEnv.files
      then CheckedSet.add ~dependencies:(FilenameSet.singleton f) acc
      else acc
    | None -> acc (* complain elsewhere about required module not found *)
  ) resolved_requires CheckedSet.empty in
  let unchanged_checked = !env.ServerEnv.checked_files in

  (* Often, all dependencies have already been checked, so infer_input contains no unchecked files.
   * In that case, let's short-circuit typecheck, since a no-op typecheck still takes time on
   * large repos *)
  if CheckedSet.is_empty (CheckedSet.diff infer_input unchanged_checked)
  then ()
  else begin
    let errors = !env.ServerEnv.errors in
    let parsed = FilenameSet.elements !env.ServerEnv.files in
    let all_dependent_files = FilenameSet.empty in
    let persistent_connections = Some (!env.ServerEnv.connections) in
    let checked, errors = typecheck
      ~options ~profiling ~workers ~errors
      ~unchanged_checked ~infer_input
      ~parsed ~all_dependent_files
      ~persistent_connections
      ~make_merge_input:(fun inferred ->
        Some (
          inferred,
          inferred |> CheckedSet.fold (fun map f ->
            FilenameMap.add f true map
          ) FilenameMap.empty
        )
      )
    in

    (* During a normal initialization or recheck, we update the env with the errors and
     * calculate the collated errors. However, this code is for when the server is in lazy mode,
     * is trying to using typecheck_contents, and is making sure the dependencies are checked. Since
     * we're messing with env.errors, we also need to set collated_errors to None. This will force
     * us to recompute them the next time someone needs them *)
    !env.ServerEnv.collated_errors := None;
    env := { !env with ServerEnv.
      checked_files = checked;
      errors;
    }
  end

(* Another special case, similar assumptions as above. *)
(** TODO: handle case when file+contents don't agree with file system state **)
let typecheck_contents_ ~options ~workers ~env ~check_syntax contents filename =
  let profiling, (cx_opt, errors, warnings, info) = Profiling_js.with_profiling begin fun profiling ->
    let errors, parse_result, info =
      parse_contents ~options ~profiling ~check_syntax filename contents in

    match parse_result with
    | Parsing_service_js.Parse_ok ast ->
        (* override docblock info *)
        let info =
          let open Docblock in
          let flow = match flow info with
          (* If the file does not specify a @flow pragma, we still want to try
            to infer something, but the file might be huge and unannotated,
            which can cause performance issues (including non-termination).
            To avoid this case, we infer the file using "weak mode." *)
          | None -> OptInWeak
          (* Respect @flow pragma *)
          | Some OptIn -> OptIn
          (* Respect @flow strict pragma *)
          | Some OptInStrict -> OptInStrict
          (* Respect @flow weak pragma *)
          | Some OptInWeak -> OptInWeak
          (* Respect @noflow, which `apply_docblock_overrides` does not by
            default. Again, large files can cause non-termination, so
            respecting this pragma gives programmers a way to tell Flow to
            avoid inference on such files. *)
          | Some OptOut -> OptInWeak
          in
          { info with flow = Some flow }
        in

        (* merge *)
        let cx = with_timer ~options "MergeContents" profiling (fun () ->
          let ensure_checked_dependencies =
            ensure_checked_dependencies ~options ~profiling ~workers ~env
          in
          Merge_service.merge_contents_context
            options filename ast info ~ensure_checked_dependencies
        ) in

        (* Filter out suppressed errors *)
        let error_suppressions = Context.error_suppressions cx in
        let severity_cover = Context.severity_cover cx in
        let errors = Context.errors cx in
        let errors, warnings, _, _ =
          Error_suppressions.filter_suppressed_errors error_suppressions severity_cover errors in

        let warnings = if Options.should_include_warnings options
          then warnings
          else Errors.ErrorSet.empty
        in

        Some cx, errors, warnings, info

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
        None, errors, Errors.ErrorSet.empty, info

    | Parsing_service_js.Parse_skip
       (Parsing_service_js.Skip_non_flow_file
      | Parsing_service_js.Skip_resource_file) ->
        (* should never happen *)
        None, errors, Errors.ErrorSet.empty, info
  end in
  profiling, cx_opt, errors, warnings, info

let typecheck_contents ~options ~workers ~env contents filename =
  let _profiling, _cx_opt, errors, warnings, _info =
    typecheck_contents_ ~options ~workers ~env ~check_syntax:true contents filename in
  errors, warnings

let basic_check_contents ~options ~workers ~env contents filename =
  try
    let profiling, cx_opt, _errors, _warnings, info =
      typecheck_contents_ ~options ~workers ~env ~check_syntax:false contents filename in
    let cx = match cx_opt with
      | Some cx -> cx
      | None -> failwith "Couldn't parse file" in
    Ok (profiling, cx, info)
  with exn ->
    let e = spf "%s\n%s"
      (Printexc.to_string exn)
      (Printexc.get_backtrace ()) in
    Error e

let init_package_heap ~options ~profiling parsed =
  with_timer ~options "PackageHeap" profiling (fun () ->
    FilenameSet.iter (fun filename ->
      match filename with
      | File_key.JsonFile str when Filename.basename str = "package.json" ->
        let ast = Parsing_service_js.get_ast_unsafe filename in
        Module_js.add_package str ast
      | _ -> ()
    ) parsed;
  )

let init_libs ~options ~profiling ~local_errors ~suppressions ~severity_cover_set ordered_libs =
  with_timer ~options "InitLibs" profiling (fun () ->
    let lib_files = Init_js.init ~options ordered_libs in
    List.fold_left (fun acc (lib_file, ok, errs, suppressions, severity_cover) ->
      let all_ok, errors_acc, suppressions_acc, severity_cover_set_acc = acc in
      let all_ok = if ok then all_ok else false in
      let errors_acc = update_errset errors_acc lib_file errs in
      let suppressions_acc =
        update_suppressions suppressions_acc lib_file suppressions in
      let severity_cover_set_acc =
        update_severity_cover_set severity_cover_set_acc lib_file severity_cover in
      all_ok, errors_acc, suppressions_acc, severity_cover_set_acc
    ) (true, local_errors, suppressions, severity_cover_set) lib_files
  )

(* Given a set of focused files and a set of parsed files, calculate all the dependents and
 * dependencies and return them as a CheckedSet
 *
 * This is pretty darn expensive for large repos (on the order of a few seconds). What is taking
 * all that time?
 *
 * - Around 75% of the time is dependent_files looking up the dependents
 * - Around 20% of the time is calc_dependency_graph building the dependency graph
 **)
let focused_files_to_infer ~workers ~focused ~parsed =
  let dependency_graph = Dep_service.calc_dependency_graph workers (FilenameSet.elements parsed) in

  let focused = focused |> FilenameSet.filter (fun f ->
    Module_js.is_tracked_file f (* otherwise, f is probably a directory *)
    && Module_js.checked_file ~audit:Expensive.warn f)
  in

  let changed_modules = FilenameSet.fold (fun f changed_modules ->
    let { Module_js.module_name; _ } = Module_js.get_info_unsafe ~audit:Expensive.warn f in
    Modulename.Set.add module_name changed_modules
  ) focused Modulename.Set.empty in

  let unchanged = FilenameSet.diff parsed focused in

  let dependents, _ = Dep_service.dependent_files workers
    ~unchanged
    ~new_or_changed:focused
    (* TODO: (glevi) T21987218 isn't it possible that _module is not provided by f? *)
    ~changed_modules
  in

  let roots = FilenameSet.union focused dependents in
  let dependencies = Dep_service.calc_all_dependencies dependency_graph roots in

  CheckedSet.add ~focused ~dependents ~dependencies CheckedSet.empty

let filter_out_node_modules ~options =
  let root = Options.root options in
  let file_options = Options.file_options options in
  FilenameSet.filter (fun fn ->
    let filename_str = File_key.to_string fn in
    not (Files.is_within_node_modules ~root ~options:file_options filename_str)
  )

(* Without a set of focused files, we just focus on every parsed file. We won't focus on
 * node_modules. If a node module is a dependency, then we'll just treat it as a dependency.
 * Otherwise, we'll ignore it. *)
let unfocused_files_to_infer ~options ~workers ~parsed =
  (* All the non-node_modules files *)
  let focused = filter_out_node_modules ~options parsed in

  (* Calculate dependencies to figure out which node_modules stuff we depend on *)
  let dependency_graph = Dep_service.calc_dependency_graph workers (FilenameSet.elements parsed) in
  let dependencies = Dep_service.calc_all_dependencies dependency_graph focused in
  CheckedSet.add
    ~focused
    ~dependencies
    CheckedSet.empty

let files_to_infer ~options ~workers ~focused ~profiling ~parsed =
  with_timer ~options "FilesToInfer" profiling (fun () ->
    match focused with
    | None ->
      unfocused_files_to_infer ~options ~workers ~parsed
    | Some focused ->
      focused_files_to_infer ~workers ~focused ~parsed

  )

(* We maintain the following invariant across rechecks: The set of
   `files` contains files that parsed successfully in the previous
   phase (which could be the init phase or a previous recheck phase)
*)
let recheck_with_profiling ~profiling ~options ~workers ~updates env ~force_focus =
  let errors = env.ServerEnv.errors in

  (* If foo.js is updated and foo.js.flow exists, then mark foo.js.flow as
   * updated too. This is because sometimes we decide what foo.js.flow
   * provides based on the existence of foo.js *)
  let updates = FilenameSet.fold (fun file updates ->
    if not (File_key.check_suffix file Files.flow_ext) &&
      Parsing_service_js.has_ast (File_key.with_suffix file Files.flow_ext)
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

  (* clear errors, asts for deleted files *)
  Parsing_service_js.remove_batch deleted;
  SharedMem_js.collect options `gentle;

  Hh_logger.info "Parsing";
  (* reparse modified files, updating modified to new_or_changed to reflect
     removal of unchanged files *)
  let new_or_changed, freshparsed, unparsed, new_local_errors =
     reparse ~options ~profiling ~workers modified in
  let new_or_changed_count = FilenameSet.cardinal new_or_changed in

  (* clear errors for new, changed and deleted files *)
  let errors =
    errors
    |> clear_errors new_or_changed
    |> clear_errors deleted
  in

  (* record reparse errors *)
  let errors =
    let () =
      let error_set: Errors.ErrorSet.t =
        FilenameMap.fold (fun _ -> Errors.ErrorSet.union) new_local_errors Errors.ErrorSet.empty
      in
      if Errors.ErrorSet.cardinal error_set > 0
      then Persistent_connection.update_clients env.ServerEnv.connections
        ~errors:error_set ~warnings:FilenameMap.empty
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

  (* remember old modules *)
  let unchanged_checked = CheckedSet.remove new_or_changed_or_deleted env.ServerEnv.checked_files in

  (* clear out records of files, and names of modules provided by those files *)
  let old_modules = Module_js.clear_files workers ~options new_or_changed_or_deleted in

  let dependent_file_count = ref 0 in

  let freshparsed_list = FilenameSet.elements freshparsed in
  let changed_modules, errors =
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
  let all_dependent_files, direct_dependent_files =
    with_timer ~options "DependentFiles" profiling (fun () ->
      Dep_service.dependent_files
        workers
        ~unchanged
        ~new_or_changed
        ~changed_modules
    ) in

  Hh_logger.info "Re-resolving directly dependent files";
  (** TODO [perf] Consider oldifying **)
  Module_js.remove_batch_resolved_requires direct_dependent_files;
  SharedMem_js.collect options `gentle;

  let node_modules_containers = !Files.node_modules_containers in
  (* requires in direct_dependent_files must be re-resolved before merging. *)
  MultiWorker.call workers
    ~job: (fun () files ->
      List.iter (fun f ->
        let file_sig = Parsing_service_js.get_file_sig_unsafe f in
        let require_loc = File_sig.(require_loc_map file_sig.module_sig) in
        let errors = Module_js.add_parsed_resolved_requires ~audit:Expensive.ok ~options
          ~node_modules_containers f require_loc in
        ignore errors (* TODO: why, FFS, why? *)
      ) files
    )
    ~neutral: ()
    ~merge: (fun () () -> ())
    ~next:(MultiWorker.next workers (FilenameSet.elements direct_dependent_files));

  let parsed = FilenameSet.union freshparsed unchanged in
  let parsed_list = FilenameSet.elements parsed in

  Hh_logger.info "Recalculating dependency graph";
  let updated_checked_files, all_dependent_files = match Options.lazy_mode options with
  | None (* Non lazy mode treats every file as focused. *)
  | Some Options.LAZY_MODE_FILESYSTEM -> (* FS mode treats every modified file as focused *)
    let old_focus_targets = CheckedSet.focused env.ServerEnv.checked_files in
    let old_focus_targets = FilenameSet.diff old_focus_targets deleted in
    let old_focus_targets = List.fold_left
      (fun targets (unparsed, _) -> FilenameSet.remove unparsed targets)
      old_focus_targets
      unparsed in
    let parsed = FilenameSet.union old_focus_targets freshparsed in
    let updated_checked_files = unfocused_files_to_infer ~options ~workers ~parsed in
    updated_checked_files, all_dependent_files
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
     * Remember that the IDE might open a new file or keep open a deleted file, so the focused set
     * might be missing that file. If that file reappears, we must remember to refocus on it.
     **)
    let old_focused = CheckedSet.focused env.ServerEnv.checked_files in
    let forced_focus = if force_focus
      then filter_out_node_modules ~options freshparsed
      else FilenameSet.empty in
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
    let focused = old_focused
      |> FilenameSet.union open_in_ide
      |> FilenameSet.union forced_focus in
    let updated_checked_files = focused_files_to_infer ~workers ~focused ~parsed in

    (* It's possible that all_dependent_files contains foo.js, which is a dependent of a dependency.
     * That's fine if foo.js is a focused file or a transitive dependent of a focused file. But if
     * it's just some random other dependent then we need to filter it out. *)
    let focused_or_dependents = FilenameSet.union
      (CheckedSet.focused updated_checked_files) (CheckedSet.dependents updated_checked_files) in
    let all_dependent_files = FilenameSet.inter all_dependent_files focused_or_dependents in
    updated_checked_files, all_dependent_files
  in

  let infer_input =
    CheckedSet.filter ~f:(fun fn -> FilenameSet.mem fn freshparsed) updated_checked_files in

  (* recheck *)
  let checked, errors = typecheck
    ~options
    ~profiling
    ~workers
    ~errors
    ~unchanged_checked
    ~infer_input
    ~parsed:parsed_list
    ~all_dependent_files
    ~persistent_connections:(Some env.ServerEnv.connections)
    ~make_merge_input:(fun inferred ->
      (* need to merge the closure of inferred files and their deps *)

      let n = FilenameSet.cardinal all_dependent_files in
      if n > 0
      then Hh_logger.info "remerge %d dependent files:" n;
      dependent_file_count := n;

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
      let to_merge = CheckedSet.add ~dependents:all_dependent_files inferred in
      Context_cache.oldify_merge_batch (CheckedSet.all to_merge);
      (** TODO [perf]: Consider `aggressive **)
      SharedMem_js.collect options `gentle;

      (* Definitely recheck inferred and direct_dependent_files. As merging proceeds, other
         files in to_merge may or may not be rechecked. *)
      let recheck_map = to_merge |> CheckedSet.fold (
        let roots = CheckedSet.add ~dependents:direct_dependent_files inferred in
        fun recheck_map file ->
          FilenameMap.add file (CheckedSet.mem file roots) recheck_map
      ) FilenameMap.empty in

      Some (to_merge, recheck_map)
    )
  in

  (* NOTE: unused fields are left in their initial empty state *)
  ({ env with ServerEnv.
    files = parsed;
    checked_files = checked;
    errors;
  },
  (new_or_changed_count, deleted_count, !dependent_file_count))

let recheck ~options ~workers ~updates env ~force_focus =
  let profiling, (env, (modified_count, deleted_count, dependent_file_count)) =
    Profiling_js.with_profiling (fun profiling ->
      recheck_with_profiling ~profiling ~options ~workers ~updates env ~force_focus
    )
  in
  FlowEventLogger.recheck
    (** TODO: update log to reflect current terminology **)
    ~modified_count
    ~deleted_count
    ~dependent_file_count
    ~profiling;
  env

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
  let parsed, unparsed, local_errors =
    parse ~options ~profiling ~workers next_files in

  Hh_logger.info "Building package heap";
  let () = init_package_heap ~options ~profiling parsed in

  Hh_logger.info "Loading libraries";
  let libs_ok, local_errors, suppressions, severity_cover_set =
    let suppressions = FilenameMap.empty in
    let severity_cover_set = FilenameMap.empty in
    init_libs ~options ~profiling ~local_errors ~suppressions ~severity_cover_set ordered_libs in

  Hh_logger.info "Resolving dependencies";
  let _, errors =
    let parsed_list = FilenameSet.elements parsed in
    let errors = { ServerEnv.
      local_errors;
      merge_errors = FilenameMap.empty;
      suppressions;
      severity_cover_set;
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

  parsed, libs, libs_ok, errors

let full_check ~profiling ~options ~workers ~focus_targets ~should_merge parsed errors =
  let infer_input = files_to_infer
    ~options ~workers ~focused:focus_targets ~profiling ~parsed:(FilenameSet.of_list parsed) in
  typecheck
    ~options
    ~profiling
    ~workers
    ~errors
    ~unchanged_checked:CheckedSet.empty
    ~infer_input
    ~parsed
    ~all_dependent_files:FilenameSet.empty
    ~persistent_connections:None
    ~make_merge_input:(fun inferred ->
      if not should_merge then None
      else Some (inferred,
        inferred |> CheckedSet.fold (fun map f ->
          FilenameMap.add f true map
        ) FilenameMap.empty)
    )
