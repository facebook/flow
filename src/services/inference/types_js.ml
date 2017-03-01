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
  if Errors.ErrorSuppressions.is_empty errsup
    then FilenameMap.remove file map
    else FilenameMap.add file errsup map

(* combine error maps into a single error list *)
let collate_errors =
  let open Errors in
  let collate errset acc =
    FilenameMap.fold (fun _key -> ErrorSet.union) errset acc
  in
  let filter_suppressed_errors suppressions errors =
    (* Filter out suppressed errors. also track which suppressions are used. *)
    let errors, suppressions = ErrorSet.fold (fun error (errors, supp_acc) ->
      let locs = Errors.locs_of_error error in
      let (suppressed, supp_acc) = ErrorSuppressions.check locs supp_acc in
      let errors = if not suppressed
        then ErrorSet.add error errors
        else errors in
      errors, supp_acc
    ) errors (ErrorSet.empty, suppressions) in

    (* For each unused suppression, create an error *)
    ErrorSuppressions.unused suppressions
    |> List.fold_left
      (fun errset loc ->
        let err = Errors.mk_error [
          loc, ["Error suppressing comment"; "Unused suppression"]
        ] in
        ErrorSet.add err errset
      )
      errors
  in
  fun { ServerEnv.local_errors; merge_errors; suppressions; } ->
    (* union suppressions from all files together *)
    let suppressions = FilenameMap.fold
      (fun _key -> ErrorSuppressions.union)
      suppressions
      ErrorSuppressions.empty in

    (* union the errors from all files together, filtering suppressed errors *)
    ErrorSet.empty
    |> collate local_errors
    |> collate merge_errors
    |> filter_suppressed_errors suppressions

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

      (* infer *)
      let profiling, cx = with_timer "Infer" profiling (fun () ->
        Type_inference_js.infer_ast
          ~metadata
          ~filename
          ~module_name:(Modulename.String "-")
          ast
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
        Merge_service.merge_strict_context ~options cache [cx]
      ) in

      (* Filter out suppressed errors *)
      let error_suppressions = Context.error_suppressions cx in
      let errors = Errors.ErrorSet.fold (fun err errors ->
        let locs = Errors.locs_of_error err in
        if not (fst (Errors.ErrorSuppressions.check locs error_suppressions))
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

(* commit newly parsed / unparsed files and removed modules, collect errors. *)
let commit_modules workers ~options errors parsed_unparsed removed_modules =
  let providers, errmap =
    Module_js.commit_modules workers ~options parsed_unparsed removed_modules in
  (* Providers might be new but not modified. This typically happens when old
     providers are deleted, and previously duplicate providers become new
     providers. In such cases, we must clear the old duplicate provider errors
     for the new providers.

     (Note that this is unncessary when the providers are modified, because in
     that case they are rechecked and *all* their errors are cleared. But we
     don't care about optimizing that case for now.) *)
  let errors = List.fold_left filter_duplicate_provider errors providers in
  FilenameMap.fold (fun file errors acc ->
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
  ~parsed
  ~removed_modules
  ~unparsed
  ~errors =
  (* TODO remove after lookup overhaul *)
  Module_js.clear_filename_cache ();

  (* add tracking modules for unparsed files *)
  MultiWorker.call workers
    ~job: (fun () ->
      List.iter (fun (filename, docblock) ->
        Module_js.add_unparsed_info ~audit:Expensive.ok
          ~options filename docblock;
        (* TODO: The following step could be moved later (after committing
           modules). Actually, we might be able to get rid of this step
           altogether, since it only involves dummy entries. *)
        Module_js.add_unparsed_resolved_requires ~audit:Expensive.ok filename;
      )
    )
    ~neutral: ()
    ~merge: (fun () () -> ())
    ~next: (MultiWorker.next workers unparsed);

  let parsed = FilenameSet.elements parsed in
  (* create info for parsed files *)
  MultiWorker.call workers
    ~job: (fun () ->
      List.iter (fun filename ->
        let info = Parsing_service_js.get_docblock_unsafe filename in
        Module_js.add_parsed_info ~audit:Expensive.ok
          ~options filename info
      )
    )
    ~neutral: ()
    ~merge: (fun () () -> ())
    ~next: (MultiWorker.next workers parsed);

  let { ServerEnv.local_errors; merge_errors; suppressions } = errors in

  (* create module dependency graph, warn on dupes etc. *)
  let profiling, local_errors =
    with_timer ~options "CommitModules" profiling (fun () ->
      let parsed_unparsed = List.fold_left (fun acc (filename, _) ->
        filename::acc
      ) parsed unparsed in
      commit_modules workers ~options local_errors parsed_unparsed removed_modules
    )
  in

  (* local inference populates context heap, resolved requires heap *)
  Flow_logger.log "Running local inference";
  let profiling, infer_results =
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
      make_merge_input inferred
    ) in

  match merge_input with
  | Some (to_merge, direct_deps, inferred) ->
    (* to_merge is the union of inferred (newly inferred files) and the
       transitive closure of all dependents. direct_deps is the subset of
       to_merge which depend on inferred files directly, or whose import
       resolution paths overlap with them. imports within these files must
       be re-resolved before merging.
       Notes:
       - since the dependencies created by module import statements are only
       one level deep, only imports in direct deps need to be re-resolved
       before merging. in contrast, dependencies on imported types themselves
       are transitive along chains of imports, hence the need to re-merge the
       transitive closure contained in to_merge.
       - residue of module import resolution is held in Module_js.ResolvedRequiresHeap.
       residue of type merging is held in SigContextHeap.
       - since to_merge is the transitive closure of dependencies of inferred,
       and direct_deps are dependencies of inferred, all dependencies of
       direct_deps are included in to_merge
    *)

    (* Definitely recheck inferred and direct_deps. As merging proceeds, other
       files in to_merge may or may not be rechecked. *)
    let recheck_map = to_merge |> List.fold_left (
      let roots = FilenameSet.union inferred direct_deps in
      fun recheck_map file ->
        FilenameMap.add file (FilenameSet.mem file roots) recheck_map
    ) FilenameMap.empty in

    (** TODO [simplification]:
        Move ResolveDirectDeps into MakeMergeInput **)
    Flow_logger.log "Re-resolving directly dependent files";
    let profiling, _ =
      with_timer ~options "ResolveDirectDeps" profiling (fun () ->
        if not (FilenameSet.is_empty direct_deps) then begin
          (** TODO [perf] Consider oldifying **)
          Module_js.remove_batch_resolved_requires direct_deps;
          SharedMem_js.collect options `gentle;

          MultiWorker.call workers
            ~job: (fun () files ->
              let cache = new Context_cache.context_cache in
              List.iter (fun f ->
                (** TODO [perf] Instead of reading the ContextHeap, could read
                    the InfoHeap and ResolvedRequiresHeap. Suspect don't need to
                    update both. **)
                let cx = cache#read ~audit:Expensive.ok f in
                Module_js.add_parsed_resolved_requires ~audit:Expensive.ok ~options cx
              ) files
            )
            ~neutral: ()
            ~merge: (fun () () -> ())
            ~next:(MultiWorker.next workers (FilenameSet.elements direct_deps));
        end
      ) in

    Flow_logger.log "Calculating dependencies";
    let profiling, dependency_graph =
      with_timer ~options "CalcDeps" profiling (fun () ->
        Dep_service.calc_dependencies workers to_merge
      ) in
    let partition = Sort_js.topsort dependency_graph in
    if Options.should_profile options then Sort_js.log partition;
    let component_map = Sort_js.component_map partition in
    let profiling, merge_errors = try
      Flow_logger.log "Merging";
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
      let master_cx = Init_js.get_master_cx options in
      let merge_errors = update_errset merge_errors
        (Context.file master_cx) (Context.errors master_cx) in
      if Options.should_profile options then Gc.print_stat stderr;
      Flow_logger.log "Done";
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
let recheck genv env modified =
  let workers = genv.ServerEnv.workers in
  let options = genv.ServerEnv.options in
  let errors = env.ServerEnv.errors in
  let debug = Options.is_debug_mode options in

  (* If foo.js is modified and foo.js.flow exists, then mark foo.js.flow as
   * modified too. This is because sometimes we decide what foo.js.flow
   * provides based on the existence of foo.js *)
  let modified = FilenameSet.fold (fun file modified ->
    if not (Loc.check_suffix file Files.flow_ext) &&
      Parsing_service_js.has_ast (Loc.with_suffix file Files.flow_ext)
    then FilenameSet.add (Loc.with_suffix file Files.flow_ext) modified
    else modified
  ) modified modified in

  let profiling = Profiling_js.empty in

  (* track deleted files, remove from modified set *)
  let modified, deleted = FilenameSet.partition (fun f ->
    Sys.file_exists (string_of_filename f)
  ) modified in
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

  Flow_logger.log "Parsing";
  (* reparse modified and added files, updating modified to reflect removal of
     unchanged files *)
  let profiling, (modified, freshparse_results) =
    with_timer ~options "Parsing" profiling (fun () ->
      Parsing_service_js.reparse_with_defaults options workers modified
    ) in
  let modified_count = FilenameSet.cardinal modified in
  let {
    Parsing_service_js.parse_ok = freshparsed;
                       parse_skips = freshparse_skips;
                       parse_fails = freshparse_fail;
  } = freshparse_results in

  (* clear errors for modified files, deleted files and master *)
  let master_cx = Init_js.get_master_cx options in
  let errors =
    errors
    |> clear_errors ~debug ([Context.file master_cx])
    |> clear_errors ~debug (FilenameSet.elements modified)
    |> clear_errors ~debug (FilenameSet.elements deleted)
  in

  (* record reparse errors *)
  let errors =
    let local_errors = List.fold_left (fun local_errors (file, _, fail) ->
      let errset = Parsing_service_js.(match fail with
      | Parse_error err -> set_of_parse_error err
      | Docblock_errors errs -> set_of_docblock_errors errs
      ) in
      update_errset local_errors file errset
    ) errors.ServerEnv.local_errors freshparse_fail in
    { errors with ServerEnv.local_errors }
  in

  (* get old (unmodified, undeleted) files that were parsed successfully *)
  let old_parsed = env.ServerEnv.files in
  let undeleted_parsed = FilenameSet.diff old_parsed deleted in
  let unmodified_parsed = FilenameSet.diff undeleted_parsed modified in

  if debug then prerr_endlinef
    "recheck: old = %d, del = %d, undel = %d, fresh = %d, unmod = %d"
    (FilenameSet.cardinal old_parsed)
    (FilenameSet.cardinal deleted)
    (FilenameSet.cardinal undeleted_parsed)
    (FilenameSet.cardinal freshparsed)
    (FilenameSet.cardinal unmodified_parsed);

  (* clear contexts and module registrations for modified and deleted files *)
  (* remember deleted modules *)
  let to_clear = FilenameSet.union modified deleted in
  Context_cache.remove_batch to_clear;
  (* clear out records of files, and names of modules provided by those files *)
  let removed_modules = Module_js.remove_files options workers to_clear in

  (* TODO elsewhere or delete *)
  Context.remove_all_errors master_cx;

  let dependent_file_count = ref 0 in

  let unparsed = List.fold_left (fun unparsed (file, info, _) ->
    (file, info) :: unparsed
  ) freshparse_skips freshparse_fail in

  (* recheck *)
  let profiling, errors = typecheck
    ~options
    ~profiling
    ~workers
    ~make_merge_input:(fun inferred ->
      (* Add non-@flow files to the list of inferred files, so that their
         dependencies are also considered for rechecking. *)
      let modified_files = List.fold_left
        (fun modified_files (f, _) -> FilenameSet.add f modified_files)
        (FilenameSet.of_list inferred) freshparse_skips in

      (* need to merge the closure of inferred files and their deps *)

      (* direct_deps are unmodified files which directly depend on
         inferred files or removed modules. all_deps are direct_deps
         plus their dependents (transitive closure) *)
      let all_deps, direct_deps = Dep_service.dependent_files
        workers
        unmodified_parsed
        modified_files
        removed_modules
      in

      let n = FilenameSet.cardinal all_deps in
      if n > 0
      then Flow_logger.log "remerge %d dependent files:" n;
      dependent_file_count := n;

      let _ = FilenameSet.fold (fun f i ->
        Flow_logger.log "%d/%d: %s" i n (string_of_filename f);
        i + 1
      ) all_deps 1 in
      Flow_logger.log "Merge prep";

      (* merge errors for unmodified dependents will be cleared lazily *)

      (* to_merge is inferred files plus all dependents. prep for re-merge *)
      let to_merge = FilenameSet.union all_deps modified_files in
      Context_cache.oldify_merge_batch to_merge;
      (** TODO [perf]: Consider `aggressive **)
      SharedMem_js.collect genv.ServerEnv.options `gentle;

      Some (FilenameSet.elements to_merge, direct_deps, modified_files)
    )
    ~parsed:freshparsed
    ~removed_modules
    ~unparsed
    ~errors
  in

  (* collate errors by origin *)
  let profiling, errorl = with_timer ~options "CollateErrors" profiling (fun () ->
    collate_errors errors
  ) in

  FlowEventLogger.recheck
    ~modified_count
    ~deleted_count
    ~dependent_file_count:!dependent_file_count
    ~profiling;

  let parsed = FilenameSet.union freshparsed unmodified_parsed in

  (* NOTE: unused fields are left in their initial empty state *)
  { env with ServerEnv.
    files = parsed;
    errorl;
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

  Flow_logger.log "Parsing";
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

  Flow_logger.log "Building package heap";
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
  Flow_logger.log "Loading libraries";
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

  (* typecheck client files *)
  let profiling, errors = typecheck
    ~options
    ~profiling
    ~workers
    ~make_merge_input:(fun inferred ->
      if not libs_ok then None
      (* TODO: the third entry should ideally be inferred to seed linking work,
         but it doesn't matter since the initial stream always contains the
         first entry. *)
      else Some (inferred, FilenameSet.empty, FilenameSet.empty)
    )
    ~parsed
    ~removed_modules:Module_js.NameSet.empty
    ~unparsed
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

  (* collate errors by origin *)
  let profiling, errorl = with_timer ~options "CollateErrors" profiling (fun () ->
    collate_errors errors
  ) in

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
     `errorl` contains the current set of errors. *)
  profiling, { ServerEnv.
    files = parsed;
    libs;
    errorl;
    errors;
    connections = Persistent_connection.empty;
  }
