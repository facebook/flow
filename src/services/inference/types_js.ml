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

let clear_errors ?(debug=false) (files: filename list) errors =
  List.fold_left (fun { ServerEnv.local_errors; merge_errors; suppressions; } file ->
    if debug then prerr_endlinef "clear errors %s" (string_of_filename file);
    { ServerEnv.
      local_errors = FilenameMap.remove file local_errors;
      merge_errors = FilenameMap.remove file merge_errors;
      suppressions = FilenameMap.remove file suppressions;
    }
  ) errors files

let update_errset map file errset =
  if Errors.ErrorSet.is_empty errset then map
  else
    let errset = match FilenameMap.get file map with
    | Some prev_errset ->
      Errors.ErrorSet.union prev_errset errset
    | None -> errset
    in
    FilenameMap.add file errset map

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

(* Another special case, similar assumptions as above. *)
(** TODO: handle case when file+contents don't agree with file system state **)
let typecheck_contents ~options ?verbose ?(check_syntax=false)
  contents filename =
  let profiling = Profiling_js.empty in

  (* always enable types when checking an individual file *)
  let types_mode = Parsing_service_js.TypesAllowed in
  let use_strict = Options.modules_are_use_strict options in
  let max_tokens = Options.max_header_tokens options in
  let profiling, (docblock_errors, parse_result, info) =
    with_timer "Parsing" profiling (fun () ->
      let docblock_errors, info =
        Parsing_service_js.get_docblock ~max_tokens filename contents in
      let parse_result = Parsing_service_js.do_parse
        ~fail:check_syntax ~types_mode ~use_strict ~info
        contents filename
      in
      docblock_errors, parse_result, info
    )
  in

  let errors = Parsing_service_js.set_of_docblock_errors docblock_errors in

  match parse_result with
  | Parsing_service_js.Parse_ok ast ->
      (* defaults *)
      let metadata = { (Context.metadata_of_options options) with
        Context.checked = true;
        Context.verbose = verbose;
      } in
      (* apply overrides from the docblock *)
      let metadata = Infer_service.apply_docblock_overrides metadata info in
      let require_loc_map = Parsing_service_js.calc_requires ast (info.Docblock.jsx = None) in

      (* infer *)
      let profiling, cx = with_timer "Infer" profiling (fun () ->
        Type_inference_js.infer_ast ~metadata ~filename ast ~require_loc_map
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
      let cache = new Context_cache.context_cache in
      let profiling, () = with_timer "Merge" profiling (fun () ->
        Merge_service.merge_contents_context ~options cache cx
      ) in

      (* Filter out suppressed errors *)
      let error_suppressions = Context.error_suppressions cx in
      let errors = Errors.ErrorSet.fold (fun err errors ->
        let locs = Errors.locs_of_error err in
        let suppressed, _, _ =
          Error_suppressions.check locs error_suppressions in
        if not suppressed
        then Errors.ErrorSet.add err errors
        else errors
      ) (Context.errors cx) errors in

      profiling, Some cx, errors, info

  | Parsing_service_js.Parse_fail fails ->
      let errors = Parsing_service_js.(match fails with
      | Parse_error err ->
          Errors.ErrorSet.add (error_of_parse_error err) errors
      | Docblock_errors errs ->
          List.fold_left (fun errors err ->
            Errors.ErrorSet.add (error_of_docblock_error err) errors
          ) errors errs
      ) in
      profiling, None, errors, info

  | Parsing_service_js.Parse_skip
     (Parsing_service_js.Skip_non_flow_file
    | Parsing_service_js.Skip_resource_file) ->
      (* should never happen *)
      profiling, None, errors, info

(* commit providers for old and new modules, collect errors. *)
let commit_modules workers ~options errors new_or_changed dirty_modules =
  let providers, changed_modules, errmap =
    Module_js.commit_modules workers ~options new_or_changed dirty_modules in
  (* Providers might be new but not changed. This typically happens when old
     providers are deleted, and previously duplicate providers become new
     providers. In such cases, we must clear the old duplicate provider errors
     for the new providers.

     (Note that this is unncessary when the providers are changed, because in
     that case they are rechecked and *all* their errors are cleared. But we
     don't care about optimizing that case for now.) *)
  let errors = List.fold_left filter_duplicate_provider errors providers in
  changed_modules, FilenameMap.fold (fun file errors acc ->
    let errset = List.fold_left (fun acc err ->
      match err with
      | Module_js.ModuleDuplicateProviderError { Module_js.
          module_name; provider; conflict;
        } ->
        let error = Errors.mk_error ~kind:Errors.DuplicateProviderError [
          Loc.({ none with source = Some conflict }), [
            module_name; "Duplicate module provider"];
          Loc.({ none with source = Some provider }), [
            "current provider"]
        ] in
        Errors.ErrorSet.add error acc
    ) Errors.ErrorSet.empty errors in
    update_errset acc file errset
  ) errmap errors

(* helper *)
let typecheck
  ~options
  ~profiling
  ~workers
  ~make_merge_input
  ~old_modules
  ~parsed
  ~unparsed
  ~new_or_changed
  ~errors =
  (* TODO remove after lookup overhaul *)
  Module_js.clear_filename_cache ();
  let parsed_set = parsed in
  let parsed = FilenameSet.elements parsed in

  let new_modules = Module_js.introduce_files workers ~options parsed unparsed in

  let { ServerEnv.local_errors; merge_errors; suppressions } = errors in

  let node_modules_containers = !Files.node_modules_containers in
  let profiling, resolve_errors =
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
  in

  let local_errors = FilenameMap.union resolve_errors local_errors in

  (* conservatively approximate set of modules whose providers will change *)
  (* register providers for modules, warn on dupes etc. *)
  let profiling, (changed_modules, local_errors) =
    with_timer ~options "CommitModules" profiling (fun () ->
      let dirty_modules = List.rev_append old_modules new_modules in
      commit_modules workers ~options local_errors new_or_changed dirty_modules
    )
  in

  (* local inference populates context heap, resolved requires heap *)
  Hh_logger.info "Running local inference";

  let profiling, infer_results =
    match Options.focus_check_target options with
    | Some f ->
      if Module_js.is_tracked_file f (* otherwise, f is probably a directory *)
        && Module_js.checked_file ~audit:Expensive.warn f
      then
        with_timer ~options "Infer" profiling (fun () ->
          let files =
            (* Calculate the set of files to check. This set includes not only
               the files to be "rechecked", which is f and all its dependents,
               but also the dependencies of such files since they may not
               already be checked. *)
            let { Module_js._module; _ } = Module_js.get_info_unsafe ~audit:Expensive.warn f in
            let all_dependent_files, _ = Dep_service.dependent_files workers
              ~unchanged:(FilenameSet.remove f parsed_set)
              ~new_or_changed:(FilenameSet.singleton f)
              ~changed_modules:(Module_js.NameSet.singleton _module) in
            let dependency_graph = Dep_service.calc_dependency_graph workers parsed in
            Dep_service.calc_all_dependencies dependency_graph (FilenameSet.add f all_dependent_files)
          in
          Infer_service.infer ~options ~workers (FilenameSet.elements files)
        )
      else (* terminate *)
        profiling, []
    | _ ->
      with_timer ~options "Infer" profiling (fun () ->
        Infer_service.infer ~options ~workers parsed
      ) in

  let rev_inferred, local_errors, suppressions =
    List.fold_left (fun (inferred, errset, suppressions) (file, errs, supps) ->
      let errset = update_errset errset file errs in
      let suppressions = update_suppressions suppressions file supps in
      file::inferred, errset, suppressions
    ) ([], local_errors, suppressions) infer_results
  in

  let inferred = List.rev rev_inferred in

  (* call supplied function to calculate closure of modules to merge *)
  let profiling, merge_input =
    with_timer ~options "MakeMergeInput" profiling (fun () ->
      make_merge_input changed_modules inferred
    ) in

  match merge_input with
  | Some (to_merge, recheck_map) ->
    (* to_merge is the union of inferred (newly inferred files) and the
       transitive closure of all dependents.

       recheck_map maps each file in to_merge to whether it should be rechecked
       initially.
    *)
    Hh_logger.info "Calculating dependencies";
    let profiling, dependency_graph =
      with_timer ~options "CalcDeps" profiling (fun () ->
        Dep_service.calc_dependency_graph workers to_merge
      ) in
    let partition = Sort_js.topsort dependency_graph in
    if Options.should_profile options then Sort_js.log partition;
    let component_map = Sort_js.component_map partition in
    let profiling, merge_errors = try
      Hh_logger.info "Merging";
      let profiling, merge_errors =
        with_timer ~options "Merge" profiling (fun () ->
          let merged = Merge_service.merge_strict
            ~options ~workers dependency_graph component_map recheck_map
          in
          List.fold_left (fun merge_errors (file, errors) ->
            let merge_errors = List.fold_left (fun merge_errors file ->
              FilenameMap.remove file merge_errors
            ) merge_errors (FilenameMap.find_unsafe file component_map) in
            if Errors.ErrorSet.is_empty errors then merge_errors
            else FilenameMap.add file errors merge_errors
          ) merge_errors merged
        )
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
        profiling, merge_errors in
    profiling, { ServerEnv.local_errors; merge_errors; suppressions; }

  | None ->
    profiling, { ServerEnv.local_errors; merge_errors; suppressions; }


(* We maintain the following invariant across rechecks: The set of
   `files` contains files that parsed successfully in the previous
   phase (which could be the init phase or a previous recheck phase)
*)
let recheck genv env ~updates =
  let workers = genv.ServerEnv.workers in
  let options = genv.ServerEnv.options in
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
  let profiling, (new_or_changed, freshparse_results) =
    with_timer ~options "Parsing" profiling (fun () ->
      Parsing_service_js.reparse_with_defaults options workers modified
    ) in
  let new_or_changed_count = FilenameSet.cardinal new_or_changed in
  let {
    Parsing_service_js.parse_ok = freshparsed;
                       parse_skips = freshparse_skips;
                       parse_fails = freshparse_fail;
  } = freshparse_results in

  (* clear errors for new, changed and deleted files *)
  let errors =
    errors
    |> clear_errors ~debug (FilenameSet.elements new_or_changed)
    |> clear_errors ~debug (FilenameSet.elements deleted)
  in

  (* record reparse errors *)
  let errors =
    let new_local_errors: Errors.ErrorSet.t FilenameMap.t = List.fold_left (fun local_errors (file, _, fail) ->
      let errset = Parsing_service_js.(match fail with
      | Parse_error err -> set_of_parse_error err
      | Docblock_errors errs -> set_of_docblock_errors errs
      ) in
      update_errset local_errors file errset
    ) FilenameMap.empty freshparse_fail in
    let () =
      let error_set: Errors.ErrorSet.t =
        FilenameMap.fold (fun _ -> Errors.ErrorSet.union) new_local_errors Errors.ErrorSet.empty
      in
      Persistent_connection.update_clients env.ServerEnv.connections error_set;
    in
    let local_errors =
      FilenameMap.union
        ~combine:(fun _ x y -> Some (Errors.ErrorSet.union x y))
        new_local_errors
        errors.ServerEnv.local_errors
    in
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
  (* clear out records of files, and names of modules provided by those files *)
  let old_modules = Module_js.clear_files workers ~options new_or_changed_or_deleted in

  let dependent_file_count = ref 0 in

  let unparsed = List.fold_left (fun unparsed (file, info, _) ->
    (file, info) :: unparsed
  ) freshparse_skips freshparse_fail in

  (* recheck *)
  let profiling, errors = typecheck
    ~options
    ~profiling
    ~workers
    ~make_merge_input:(fun changed_modules inferred ->
      (* need to merge the closure of inferred files and their deps *)

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
          let cache = new Context_cache.context_cache in
          List.iter (fun f ->
            let cx = cache#read ~audit:Expensive.ok f in
            Module_js.add_parsed_resolved_requires ~audit:Expensive.ok ~options
              ~node_modules_containers (Context.file cx) (Context.require_loc cx) |> ignore
          ) files
        )
        ~neutral: ()
        ~merge: (fun () () -> ())
        ~next:(MultiWorker.next workers (FilenameSet.elements direct_dependent_files));

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
      let to_merge = FilenameSet.union all_dependent_files (FilenameSet.of_list inferred) in
      Context_cache.oldify_merge_batch to_merge;
      (** TODO [perf]: Consider `aggressive **)
      SharedMem_js.collect genv.ServerEnv.options `gentle;

      let to_merge = FilenameSet.elements to_merge in

      (* Definitely recheck inferred and direct_dependent_files. As merging proceeds, other
         files in to_merge may or may not be rechecked. *)
      let recheck_map = to_merge |> List.fold_left (
        let roots = FilenameSet.union new_or_changed direct_dependent_files in
        fun recheck_map file ->
          FilenameMap.add file (FilenameSet.mem file roots) recheck_map
      ) FilenameMap.empty in

      Some (to_merge, recheck_map)
    )
    ~old_modules
    ~parsed:freshparsed
    ~unparsed
    ~new_or_changed:(FilenameSet.elements new_or_changed)
    ~errors
  in

  FlowEventLogger.recheck
    (** TODO: update log to reflect current terminology **)
    ~modified_count:new_or_changed_count
    ~deleted_count
    ~dependent_file_count:!dependent_file_count
    ~profiling;

  let parsed = FilenameSet.union freshparsed unchanged in

  (* NOTE: unused fields are left in their initial empty state *)
  { env with ServerEnv.
    files = parsed;
    errors;
  }

(* full typecheck *)
let full_check workers ~ordered_libs parse_next options =
  let local_errors = FilenameMap.empty in
  let merge_errors = FilenameMap.empty in
  let suppressions = FilenameMap.empty in

  let profiling = Profiling_js.empty in

  (* force types when --all is set, but otherwise forbid them unless the file
     has @flow in it. *)
  let types_mode = Parsing_service_js.(
    if Options.all options then TypesAllowed else TypesForbiddenByDefault
  ) in

  let use_strict = Options.modules_are_use_strict options in

  let profile = Options.should_profile options in
  let max_header_tokens = Options.max_header_tokens options in

  Hh_logger.info "Parsing";
  let profiling, parse_results =
    with_timer ~options "Parsing" profiling (fun () ->
      Parsing_service_js.parse
        ~types_mode ~use_strict ~profile ~max_header_tokens
        workers parse_next
    ) in
  let {
    Parsing_service_js.parse_ok = parsed;
                       parse_skips = skipped_files;
                       parse_fails = error_files;
  } = parse_results in

  let local_errors = List.fold_left (fun errors (file, _, fail) ->
    let errset = Parsing_service_js.(match fail with
    | Parse_error err -> set_of_parse_error err
    | Docblock_errors errs -> set_of_docblock_errors errs
    ) in
    update_errset errors file errset
  ) local_errors error_files in

  Hh_logger.info "Building package heap";
  let profiling, () = with_timer ~options "PackageHeap" profiling (fun () ->
    FilenameSet.iter (fun filename ->
      match filename with
      | Loc.JsonFile str when Filename.basename str = "package.json" ->
        let ast = Parsing_service_js.get_ast_unsafe filename in
        Module_js.add_package str ast
      | _ -> ()
    ) parsed;
  ) in

  (* load library code *)
  (* if anything errors, we'll infer but not merge client code *)
  Hh_logger.info "Loading libraries";
  let profiling, (libs_ok, local_errors, suppressions) =
    with_timer ~options "InitLibs" profiling (fun () ->
      let lib_files = Init_js.init ~options ordered_libs in
      List.fold_left (fun acc (lib_file, ok, errs, suppressions) ->
        let all_ok, errors_acc, suppressions_acc = acc in
        let all_ok = if ok then all_ok else false in
        let errors_acc = update_errset errors_acc lib_file errs in
        let suppressions_acc =
          update_suppressions suppressions_acc lib_file suppressions in
        all_ok, errors_acc, suppressions_acc
      ) (true, local_errors, suppressions) lib_files
    )
  in

  let unparsed = List.fold_left (fun unparsed (file, info, _) ->
    (file, info) :: unparsed
  ) skipped_files error_files in

  let errors = { ServerEnv.local_errors; merge_errors; suppressions } in

  let all_files = List.fold_left (fun acc (filename, _) ->
    filename::acc
  ) (FilenameSet.elements parsed) unparsed in

  (* typecheck client files *)
  let profiling, errors = typecheck
    ~options
    ~profiling
    ~workers
    ~make_merge_input:(fun _ inferred ->
      if not libs_ok then None
      else Some (inferred,
        inferred |> List.fold_left (fun map f ->
          FilenameMap.add f true map
        ) FilenameMap.empty)
    )
    ~old_modules:[]
    ~parsed
    ~unparsed
    ~new_or_changed:all_files
    ~errors
  in

  (profiling, parsed, errors)

(* initialize flow server state, including full check *)
let server_init genv =
  let options = genv.ServerEnv.options in

  let ordered_libs, libs = Files.init options in

  let get_next_raw =
    Files.make_next_files ~all:false ~subdir:None ~options ~libs in
  let get_next = fun () ->
    get_next_raw ()
    |> List.map (Files.filename_from_string ~options)
    |> Bucket.of_list
  in

  let (profiling, parsed, errors) = full_check
    genv.ServerEnv.workers ~ordered_libs get_next options in

  let profiling = SharedMem.(
    let dep_stats = dep_stats () in
    let hash_stats = hash_stats () in
    let heap_size = heap_size () in
    let memory_metrics = [
      "heap.size", heap_size;
      "dep_table.nonempty_slots", dep_stats.nonempty_slots;
      "dep_table.used_slots", dep_stats.used_slots;
      "dep_table.slots", dep_stats.slots;
      "hash_table.nonempty_slots", hash_stats.nonempty_slots;
      "hash_table.used_slots", hash_stats.used_slots;
      "hash_table.slots", hash_stats.slots;
    ] in
    List.fold_left (fun profiling (metric, value) ->
      Profiling_js.sample_memory
        ~metric:("init_done." ^ metric)
        ~value:(float_of_int value)
         profiling
    ) profiling memory_metrics
  ) in

  SharedMem_js.init_done();

  (* Return an env that initializes invariants required and maintained by
     recheck, namely that `files` contains files that parsed successfully, and
     `errors` contains the current set of errors. *)
  profiling, { ServerEnv.
    files = parsed;
    libs;
    errors;
    connections = Persistent_connection.empty;
  }
