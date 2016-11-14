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

(* errors are stored in a map from file path to error set, so that the errors
   from checking particular files can be cleared during recheck. *)
let errors_by_file = ref FilenameMap.empty
(* errors encountered during merge have to be stored separately so dependencies
   can be cleared during merge. *)
let merge_errors = ref FilenameMap.empty
(* error suppressions in the code *)
let error_suppressions = ref FilenameMap.empty
(* aggregate error map, built after check or recheck
   by collate_errors
 *)
let all_errors = ref Errors.ErrorSet.empty

(****************** typecheck job helpers *********************)

(* error state handling.
   note: once weve decoupled from hack binary, these will be stored
   in the recurrent env struct, not local state *)
let clear_errors ?(debug=false) (files: filename list) =
  List.iter (fun file ->
    if debug then prerr_endlinef "clear errors %s" (string_of_filename file);
    errors_by_file := FilenameMap.remove file !errors_by_file;
    merge_errors := FilenameMap.remove file !merge_errors;
    error_suppressions := FilenameMap.remove file !error_suppressions;
  ) files;
  all_errors := Errors.ErrorSet.empty

(* helper - save an error set into a global error map.
   clear mapping if errorset is empty *)
let save_errset mapref file errset =
  if Errors.ErrorSet.cardinal errset > 0 then
    mapref :=
      let errset = match FilenameMap.get file !mapref with
      | Some prev_errset ->
        Errors.ErrorSet.union prev_errset errset
      | None -> errset
      in
      FilenameMap.add file errset !mapref

(* Filter out duplicate provider error, if any, for the given file. *)
let filter_duplicate_provider mapref file =
  match FilenameMap.get file !mapref with
  | Some prev_errset ->
    let new_errset = Errors.ErrorSet.filter (fun err ->
      not (Errors.is_duplicate_provider_error err)
    ) prev_errset in
    mapref := FilenameMap.add file new_errset !mapref
  | None -> ()

(* given a reference to a error map (files to errorsets), and
   two parallel lists of such, save the latter into the former. *)
let save_errors mapref files errsets =
  List.iter2 (save_errset mapref) files errsets

let save_errormap mapref errmap =
  FilenameMap.iter (save_errset mapref) errmap

(* given a reference to a error suppression map (files to
   error suppressions), and two parallel lists of such,
   save the latter into the former. *)
let save_suppressions mapref files errsups = Errors.(
  List.iter2 (fun file errsup ->
    mapref := if ErrorSuppressions.cardinal errsup = 0
      then FilenameMap.remove file !mapref
      else FilenameMap.add file errsup !mapref
  ) files errsups
)

(* Given all the errors as a map from file => errorset
 * 1) Filter out the suppressed errors from the error sets
 * 2) Remove files with empty errorsets from the map
 * 3) Add errors for unused suppressions
 * 4) Properly distribute the new errors
 *)
let filter_suppressed_errors errors = Errors.(
  let suppressions = ref ErrorSuppressions.empty in

  let filter_suppressed_error error =
    let (suppressed, sups) = ErrorSuppressions.check error !suppressions in
    suppressions := sups;
    not suppressed
  in

  suppressions := FilenameMap.fold
    (fun _key -> ErrorSuppressions.union)
    !error_suppressions
    ErrorSuppressions.empty;

  let errors = ErrorSet.filter filter_suppressed_error errors in

  (* For each unused suppression, create an error *)
  ErrorSuppressions.unused !suppressions
  |> List.fold_left
    (fun errset loc ->
      let err = Errors.mk_error [
        loc, ["Error suppressing comment"; "Unused suppression"]
      ] in
      ErrorSet.add err errset
    )
    errors
)

(* retrieve a full error list.
   Library errors are forced to the top of the list.
   Note: in-place conversion using an array is to avoid
   memory pressure on pathologically huge error sets, but
   this may no longer be necessary
 *)
let get_errors () =
  !all_errors
  |> filter_suppressed_errors
  |> Errors.ErrorSet.elements

(* relocate errors to their reported positions,
   combine in single error map *)
let collate_errors =
  let open Errors in
  let collate _ errset acc = ErrorSet.union acc errset in
  fun () ->
    all_errors :=
      ErrorSet.empty
      |> FilenameMap.fold collate !errors_by_file
      |> FilenameMap.fold collate !merge_errors

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
  let profiling, (errors, parse_result, info) =
    with_timer "Parsing" profiling (fun () ->
      let docblock_errors, info =
        Parsing_service_js.get_docblock ~max_tokens filename contents in
      let parse_result = Parsing_service_js.do_parse
        ~fail:check_syntax ~types_mode ~use_strict ~info
        contents filename
      in
      let errors = match docblock_errors with
        | None -> Errors.ErrorSet.empty
        | Some errs -> errs
      in
      errors, parse_result, info
    )
  in

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
        if not (fst (Errors.ErrorSuppressions.check err error_suppressions))
        then Errors.ErrorSet.add err errors
        else errors
      ) (Context.errors cx) errors in

      profiling, Some cx, errors, info

  | Parsing_service_js.Parse_err parse_errors ->
      profiling, None, Errors.ErrorSet.union parse_errors errors, info

  | Parsing_service_js.Parse_skip
     (Parsing_service_js.Skip_non_flow_file
    | Parsing_service_js.Skip_resource_file) ->
      (* should never happen *)
      profiling, None, errors, info

(* commit newly inferred and removed modules, collect errors. *)
let commit_modules workers ~options inferred removed =
  let providers, errmap =
    Module_js.commit_modules workers ~options inferred removed in
  (* Providers might be new but not modified. This typically happens when old
     providers are deleted, and previously duplicate providers become new
     providers. In such cases, we must clear the old duplicate provider errors
     for the new providers.

     (Note that this is unncessary when the providers are modified, because in
     that case they are rechecked and *all* their errors are cleared. But we
     don't care about optimizing that case for now.) *)
  List.iter (filter_duplicate_provider errors_by_file) providers;
  save_errormap errors_by_file errmap

(* Sanity checks on InfoHeap and NameHeap. Since this is performance-intensive
   (although it probably doesn't need to be), it is only done under --debug. *)
let heap_check ~audit files = Module_js.(
  let ih = Hashtbl.create 0 in
  let nh = Hashtbl.create 0 in
  files |> List.iter (fun file ->
    let m_file = get_file ~audit (Modulename.Filename file) in
    if not (Loc.check_suffix m_file Files.flow_ext)
    then assert (m_file = file);
    let info = get_module_info ~audit file in
    Hashtbl.add ih file info;
    let m = info.Module_js._module in
    let f = get_file ~audit m in
    Hashtbl.add nh m f;
  );
  nh |> Hashtbl.iter (fun m f ->
    let names = get_module_names ~audit f in
    assert (List.exists (fun name -> name = m) names);
  );
  ih |> Hashtbl.iter (fun _ info ->
    let parsed = info.Module_js.parsed in
    let checked = info.Module_js.checked in
    let required = info.Module_js.required in
    assert (parsed);
    assert (checked || (NameSet.is_empty required));
  );
)

(* helper *)
let typecheck
  ~options
  ~profiling
  ~workers
  ~make_merge_input
  ~files
  ~removed
  ~unparsed
  ~resource_files =
  (* TODO remove after lookup overhaul *)
  Module_js.clear_filename_cache ();
  (* local inference populates context heap, module info heap *)
  Flow_logger.log "Running local inference";
  let profiling, inferred =
    with_timer ~options "Infer" profiling (fun () ->
      Infer_service.infer ~options ~workers
        ~save_errors:(save_errors errors_by_file)
        ~save_suppressions:(save_suppressions error_suppressions)
        files
    ) in

  (* Resource files are treated just like unchecked files, which are already in
     unparsed. TODO: This suggestes that we can remove ~resource_files and merge
     them into ~unparsed upstream. *)
  let unparsed = FilenameSet.fold
    (fun fn acc -> (fn, Docblock.default_info)::acc)
    resource_files unparsed in

  (** TODO [correctness]: check whether these have been cleared **)
  (* add tracking modules for unparsed files *)
  MultiWorker.call workers
    ~job: (fun () ->
      List.iter (fun (filename, docblock) ->
        Module_js.add_unparsed_info ~audit:Expensive.ok
          ~options filename docblock
      )
    )
    ~neutral: ()
    ~merge: (fun () () -> ())
    ~next: (MultiWorker.next workers unparsed);

  (** TODO [correctness]:
      move CommitModules after MakeMergeInput + ResolveDirectDeps **)
  (* create module dependency graph, warn on dupes etc. *)
  let profiling, () = with_timer ~options "CommitModules" profiling (fun () ->
    let filenames = List.fold_left (fun acc (filename, _) ->
      filename::acc
    ) inferred unparsed in
    commit_modules workers ~options filenames removed
  ) in

  (* call supplied function to calculate closure of modules to merge *)
  let profiling, merge_input =
    with_timer ~options "MakeMergeInput" profiling (fun () ->
      make_merge_input inferred
    ) in

  match merge_input with
  | Some (to_merge, direct_deps) ->
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
       - residue of module import resolution is held in Module_js.InfoHeap.
       residue of type merging is held in SigContextHeap.
       - since to_merge is the transitive closure of dependencies of inferred,
       and direct_deps are dependencies of inferred, all dependencies of
       direct_deps are included in to_merge
      *)
    (** TODO [simplification]:
        Move ResolveDirectDeps into MakeMergeInput **)
    Flow_logger.log "Re-resolving directly dependent files";
    let profiling, _ =
      with_timer ~options "ResolveDirectDeps" profiling (fun () ->
        if not (FilenameSet.is_empty direct_deps) then begin
          (** TODO [perf] Consider oldifying **)
          Module_js.clear_infos direct_deps;
          SharedMem_js.collect options `gentle;

          MultiWorker.call workers
            ~job: (fun () files ->
              let cache = new Context_cache.context_cache in
              List.iter (fun f ->
                (** TODO [perf]
                    Instead of reading the ContextHeap, could read the InfoHeap
                  **)
                let cx = cache#read ~audit:Expensive.ok f in
                Module_js.add_module_info ~audit:Expensive.ok ~options cx
              ) files
            )
            ~neutral: ()
            ~merge: (fun () () -> ())
            ~next:(MultiWorker.next workers (FilenameSet.elements direct_deps));
        end
      ) in

    (* TODO [correctness]: This seems to have rotted :( *)
    if Options.is_debug_mode options
    then heap_check ~audit:Expensive.warn to_merge;

    Flow_logger.log "Calculating dependencies";
    let profiling, dependency_graph =
      with_timer ~options "CalcDeps" profiling (fun () ->
        Dep_service.calc_dependencies workers to_merge
      ) in
    let partition = Sort_js.topsort dependency_graph in
    if Options.should_profile options then Sort_js.log partition;
    let profiling = try
      Flow_logger.log "Merging";
      let profiling, () = with_timer ~options "Merge" profiling (fun () ->
        Merge_service.merge_strict
          ~options ~workers ~save_errors:(save_errors merge_errors)
          dependency_graph partition
      ) in
      if Options.should_profile options then Gc.print_stat stderr;
      Flow_logger.log "Done";
      profiling
    with
    (* Unrecoverable exceptions *)
    | SharedMem_js.Out_of_shared_memory
    | SharedMem_js.Heap_full
    | SharedMem_js.Hash_table_full
    | SharedMem_js.Dep_table_full as exn -> raise exn
    (* A catch all suppression is probably a bad idea... *)
    | exc ->
        prerr_endline (Printexc.to_string exc);
        profiling in
    (* collate errors by origin *)
    collate_errors ();
    profiling

  | None ->
    (* collate errors by origin *)
    collate_errors ();
    profiling


(* We maintain the following invariant across rechecks: The set of
   `files` contains files that parsed successfully in the previous
   phase (which could be the init phase or a previous recheck phase)
*)
let recheck genv env modified =
  let workers = genv.ServerEnv.workers in
  let options = genv.ServerEnv.options in
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
  let deleted = FilenameSet.filter (fun f ->
    not (Sys.file_exists (string_of_filename f))
  ) modified in
  let deleted_count = FilenameSet.cardinal deleted in
  let modified = FilenameSet.diff modified deleted in
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
  Parsing_service_js.remove_asts deleted;
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
                       parse_errors = freshparse_errors;
                       parse_resource_files = freshparse_resource_files;
  } = freshparse_results in

  (* clear errors for modified files, deleted files and master *)
  let master_cx = Init_js.get_master_cx options in
  clear_errors ~debug (Context.file master_cx :: FilenameSet.elements modified);
  clear_errors ~debug (FilenameSet.elements deleted);

  (* record reparse errors *)
  let failed_filenames = List.map (fun (file, _) -> file) freshparse_fail in
  save_errors errors_by_file failed_filenames freshparse_errors;

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
  (* clear out infos of files, and names of modules provided by those files *)
  let removed_modules = Module_js.remove_files options workers to_clear in

  (* TODO elsewhere or delete *)
  Context.remove_all_errors master_cx;

  let dependent_file_count = ref 0 in

  (* recheck *)
  let profiling = typecheck
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

      (* clear merge errors for unmodified dependents *)
      FilenameSet.iter (fun file ->
        merge_errors := FilenameMap.remove file !merge_errors;
      ) all_deps;

      (* to_merge is inferred files plus all dependents. prep for re-merge *)
      let to_merge = FilenameSet.union all_deps modified_files in
      Merge_service.remove_batch to_merge;
      (** TODO [perf]: Consider `aggressive **)
      SharedMem_js.collect genv.ServerEnv.options `gentle;

      Some (FilenameSet.elements to_merge, direct_deps)
    )
    ~files:freshparsed
    ~removed:removed_modules
    ~unparsed:(List.rev_append freshparse_fail freshparse_skips)
    ~resource_files:freshparse_resource_files
  in

  FlowEventLogger.recheck
    ~modified_count
    ~deleted_count
    ~dependent_file_count:!dependent_file_count
    ~profiling;

  let parsed = FilenameSet.union freshparsed unmodified_parsed in

  (* NOTE: unused fields are left in their initial empty state *)
  { env with ServerEnv.
    files = parsed;
    errorl = get_errors ();
  }

(* full typecheck *)
let full_check workers ~ordered_libs parse_next options =
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
                       parse_errors = errors;
                       parse_resource_files = resource_files;
  } = parse_results in
  let error_filenames = List.map (fun (file, _) -> file) error_files in
  save_errors errors_by_file error_filenames errors;

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
  let profiling, lib_error = with_timer ~options "InitLibs" profiling (fun () ->
    let lib_files = Init_js.init
      ~options
      ordered_libs
      (fun file errs -> save_errors errors_by_file [file] [errs])
      (fun file sups -> save_suppressions error_suppressions [file] [sups])
    in
    List.exists (fun (_, ok) -> not ok) lib_files
  ) in

  (* typecheck client files *)
  let profiling = typecheck
    ~options
    ~profiling
    ~workers
    ~make_merge_input:(fun inferred ->
      if lib_error then None else Some (inferred, FilenameSet.empty)
    )
    ~files:parsed
    ~removed:Module_js.NameSet.empty
    ~unparsed:(List.rev_append error_files skipped_files)
    ~resource_files
  in

  (profiling, parsed)

(* helper - print errors. used in check-and-die runs *)
let print_errors ~profiling options errors =
  let strip_root = Options.should_strip_root options in
  let root = Options.root options in

  if Options.should_output_json options
  then begin
    let profiling =
      if options.Options.opt_profile
      then Some profiling
      else None in
    Errors.print_error_json ~strip_root ~root ~profiling stdout errors
  end else
    Errors.print_error_summary
      ~flags:(Options.error_flags options)
      ~strip_root
      ~root
      errors

(* initialize flow server state, including full check *)
let server_init genv =
  let options = genv.ServerEnv.options in

  let ordered_libs, libs = Files.init options in

  let get_next_raw = Files.make_next_files ~subdir:None ~options ~libs in
  let get_next = fun () ->
    get_next_raw () |> List.map (Files.filename_from_string ~options)
  in
  let (profiling, parsed) =
    full_check genv.ServerEnv.workers ~ordered_libs get_next options in

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

  let errors = get_errors () in
  if Options.is_check_mode options
  then print_errors ~profiling options errors;

  SharedMem_js.init_done();

  (* Return an env that initializes invariants required and maintained by
     recheck, namely that `files` contains files that parsed successfully, and
     `errorl` contains the current set of errors. *)
  profiling, { ServerEnv.
    files = parsed;
    libs;
    errorl = errors;
  }
