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

module Errors = Errors_js

(* errors are stored by phase,
   in maps from file path to error set
 *)
(* errors encountered during parsing *)
let parse_errors = ref FilenameMap.empty
(* errors encountered during local inference *)
let infer_errors = ref FilenameMap.empty
(* errors encountered during module commit *)
let module_errors = ref FilenameMap.empty
(* errors encountered during merge *)
let merge_errors = ref FilenameMap.empty
(* error suppressions in the code *)
let error_suppressions = ref FilenameMap.empty
(* aggregate error map, built after check or recheck
   by collate_errors
 *)
let all_errors = ref FilenameMap.empty

(****************** typecheck job helpers *********************)

(* error state handling.
   note: once weve decoupled from hack binary, these will be stored
   in the recurrent env struct, not local state *)
let clear_errors ?(debug=false) (files: filename list) =
  List.iter (fun file ->
    if debug then prerr_endlinef "clear errors %s" (string_of_filename file);
    parse_errors := FilenameMap.remove file !parse_errors;
    infer_errors := FilenameMap.remove file !infer_errors;
    module_errors := FilenameMap.remove file !module_errors;
    merge_errors := FilenameMap.remove file !merge_errors;
    error_suppressions := FilenameMap.remove file !error_suppressions;
  ) files;
  all_errors := FilenameMap.empty

(* helper - save an error set into a global error map.
   clear mapping if errorset is empty *)
let save_errset mapref file errset =
  mapref := if Errors.ErrorSet.cardinal errset = 0
    then FilenameMap.remove file !mapref
    else FilenameMap.add file errset !mapref

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

(* distribute errors from a set into a filename-indexed map,
   based on position info contained in error, not incoming key *)
let distrib_errs = Errors.(

  let distrib_error _orig_file error error_map =
    let file = match Loc.source (loc_of_error error) with
    | Some x -> x
    | None ->
        assert_false (spf "distrib_errs: no source for error: %s"
          (Hh_json.json_to_multiline (json_of_errors [error])));
    in
    match FilenameMap.get file error_map with
    | None ->
      FilenameMap.add file (ErrorSet.singleton error) error_map
    | Some error_set ->
      if ErrorSet.mem error error_set
      then error_map
      else FilenameMap.add file (ErrorSet.add error error_set) error_map
  in

  fun _file file_errors error_map ->
    ErrorSet.fold (distrib_error _file) file_errors error_map
)
(* Given all the errors as a map from file => errorset
 * 1) Filter out the suppressed errors from the error sets
 * 2) Remove files with empty errorsets from the map
 * 3) Add errors for unused suppressions
 * 4) Properly distribute the new errors
 *)
let filter_suppressed_errors emap = Errors.(
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

  let emap = emap
    |> FilenameMap.map (ErrorSet.filter filter_suppressed_error)
    |> FilenameMap.filter (fun _ v -> not (ErrorSet.is_empty v)) in

  (* For each unused suppression, create an error *)
  let unused_suppression_errors = List.fold_left
    (fun errset loc ->
      let err = Errors.mk_error [
        loc, ["Error suppressing comment"; "Unused suppression"]
      ] in
      ErrorSet.add err errset
    )
    ErrorSet.empty
    (ErrorSuppressions.unused !suppressions) in

    distrib_errs "" unused_suppression_errors emap
)

(* retrieve a full error list.
   Library errors are forced to the top of the list.
   Note: in-place conversion using an array is to avoid
   memory pressure on pathologically huge error sets, but
   this may no longer be necessary
 *)
let get_errors () =
  let all = !all_errors in
  let all = filter_suppressed_errors all in

  (* flatten to list, with lib errors bumped to top *)
  let append_errset errset errlist =
    List.rev_append (Errors.ErrorSet.elements errset) errlist
  in
  let revlist_libs, revlist_others = FilenameMap.fold (
    fun file errset (libs, others) ->
      if Loc.source_is_lib_file file
      then append_errset errset libs, others
      else libs, append_errset errset others
    ) all ([], [])
  in
  List.rev_append revlist_libs (List.rev revlist_others)

(* relocate errors to their reported positions,
   combine in single error map *)
let collate_errors () =
  all_errors :=
    !parse_errors
    |> FilenameMap.fold distrib_errs !infer_errors
    |> FilenameMap.fold distrib_errs !merge_errors
    |> FilenameMap.fold distrib_errs !module_errors

let with_timer ?options timer timing f =
  let timing = FlowEventLogger.Timing.start_timer ~timer timing in
  let ret = f () in
  let timing = FlowEventLogger.Timing.stop_timer ~timer timing in

  (* If we're profiling then output timing information to stderr *)
  (match options with
  | Some options when Options.should_profile options ->
      (match FlowEventLogger.Timing.get_finished_timer ~timer timing with
      | Some (start_wall_age, wall_duration) ->
          prerr_endlinef
            "TimingEvent `%s`: start_wall_age: %f; wall_duration: %f"
            timer
            start_wall_age
            wall_duration
      | _ -> ());
  | _ -> ());

  (timing, ret)

(* Another special case, similar assumptions as above. *)
(** TODO: handle case when file+contents don't agree with file system state **)
let typecheck_contents ~options ?verbose contents filename =
  let timing = FlowEventLogger.Timing.create () in

  (* always enable types when checking an individual file *)
  let types_mode = Parsing_service_js.TypesAllowed in
  let use_strict = Options.modules_are_use_strict options in
  let timing, (parse_result, info) = with_timer "Parsing" timing (fun () ->
    let info = Docblock.extract (string_of_filename filename) contents in
    let parse_result = Parsing_service_js.do_parse
      ~fail:false ~types_mode ~use_strict ~info
      contents filename
    in
    parse_result, info
  ) in

  match parse_result with
  | Parsing_service_js.Parse_ok ast ->
      (* defaults *)
      let metadata = { (Context.metadata_of_options options) with
        Context.checked = true;
        Context.verbose = verbose;
      } in
      (* apply overrides from the docblock *)
      let metadata = Infer_service.apply_docblock_overrides metadata info in

      let timing, cx = with_timer "Infer" timing (fun () ->
        Type_inference_js.infer_ast
          ~metadata ~filename ~module_name:(Modulename.String "-") ast
      ) in

      let cache = new Context_cache.context_cache in
      let timing, () = with_timer "Merge" timing (fun () ->
        Merge_service.merge_strict_context ~options cache [cx]
      ) in

      (* Filter out suppressed errors *)
      let error_suppressions = Context.error_suppressions cx in
      let errors = (Context.errors cx) |> Errors.ErrorSet.filter (fun err ->
        not (fst (Errors.ErrorSuppressions.check err error_suppressions))
      ) in

      timing, Some cx, errors, info

  | Parsing_service_js.Parse_err errors ->
      timing, None, errors, info

  | Parsing_service_js.Parse_skip ->
      (* should never happen *)
      timing, None, Errors.ErrorSet.empty, info


(* commit newly inferred and removed modules, collect errors. *)
let commit_modules workers ~options inferred removed =
  let errmap = Module_js.commit_modules workers ~options inferred removed in
  save_errormap module_errors errmap

(* Sanity checks on InfoHeap and NameHeap. Since this is performance-intensive
   (although it probably doesn't need to be), it is only done under --debug. *)
let heap_check files = Module_js.(
  let ih = Hashtbl.create 0 in
  let nh = Hashtbl.create 0 in
  files |> List.iter (fun file ->
    let m_file = get_file (Modulename.Filename file) in
    if not (Loc.check_suffix m_file Files_js.flow_ext)
    then assert (m_file = file);
    let info = get_module_info file in
    Hashtbl.add ih file info;
    let m = info.Module_js._module in
    let f = get_file m in
    Hashtbl.add nh m f;
  );
  nh |> Hashtbl.iter (fun m f ->
    assert (get_module_name f = m);
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
let typecheck ~options ~timing ~workers ~make_merge_input files removed unparsed =
  (* TODO remove after lookup overhaul *)
  Module_js.clear_filename_cache ();
  (* local inference populates context heap, module info heap *)
  Flow_logger.log "Running local inference";
  let timing, inferred =
    with_timer ~options "Infer" timing (fun () ->
      Infer_service.infer ~options ~workers
        ~save_errors:(save_errors infer_errors)
        ~save_suppressions:(save_suppressions error_suppressions)
        files
    ) in

  (* add tracking modules for unparsed files *)
  List.iter (fun (filename, docblock) ->
    Module_js.add_unparsed_info ~options filename docblock
  ) unparsed;

  (* create module dependency graph, warn on dupes etc. *)
  let timing, () = with_timer ~options "CommitModules" timing (fun () ->
    let filenames = List.fold_left (fun acc (filename, _) ->
      filename::acc
    ) inferred unparsed in
    commit_modules workers ~options filenames removed
  ) in

  (* call supplied function to calculate closure of modules to merge *)
  let timing, merge_input = with_timer ~options "MakeMergeInput" timing (fun () ->
    make_merge_input inferred
  ) in

  match merge_input with
  | true, to_merge, direct_deps ->
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
    Flow_logger.log "Re-resolving directly dependent files";
    let timing, _ = with_timer ~options "ResolveDirectDeps" timing (fun () ->
      Module_js.clear_infos direct_deps;
      let cache = new Context_cache.context_cache in
      FilenameSet.iter (fun f ->
        let cx = cache#read f in
        Module_js.add_module_info ~options cx
      ) direct_deps;
      if Options.is_debug_mode options then heap_check to_merge;
    ) in
    Flow_logger.log "Calculating dependencies";
    let timing, dependency_graph =
      with_timer ~options "CalcDeps" timing (fun () ->
        Dep_service.calc_dependencies workers to_merge
      ) in
    let partition = Sort_js.topsort dependency_graph in
    if Options.should_profile options then Sort_js.log partition;
    let timing = try
      Flow_logger.log "Merging";
      let timing, () = with_timer ~options "Merge" timing (fun () ->
        Merge_service.merge_strict
          ~options ~workers ~save_errors:(save_errors merge_errors)
          dependency_graph partition
      ) in
      if Options.should_profile options then Gc.print_stat stderr;
      Flow_logger.log "Done";
      timing
    with exc ->
      prerr_endline (Printexc.to_string exc);
      timing
    in
    (* collate errors by origin *)
    collate_errors ();
    timing

  | false, _, _ ->
    (* collate errors by origin *)
    collate_errors ();
    timing


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
    if not (Loc.check_suffix file Files_js.flow_ext) &&
      Parsing_service_js.has_ast (Loc.with_suffix file Files_js.flow_ext)
    then FilenameSet.add (Loc.with_suffix file Files_js.flow_ext) modified
    else modified
  ) modified modified in

  let timing = FlowEventLogger.Timing.create () in

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

  (* force types when --all is set, but otherwise forbid them unless the file
     has @flow in it. *)
  let types_mode = Parsing_service_js.(
    if Options.all options then TypesAllowed else TypesForbiddenByDefault
  ) in

  let use_strict = Options.modules_are_use_strict options in

  Flow_logger.log "Parsing";
  (* reparse modified and added files, updating modified to reflect removal of
     unchanged files *)
  let timing, (modified, (freshparsed, freshparse_skips, freshparse_fail, freshparse_errors)) =
    with_timer ~options "Parsing" timing (fun () ->
      let profile = Options.should_profile options in
      Parsing_service_js.reparse
        ~types_mode ~use_strict ~profile
        workers modified
    ) in
  let modified_count = FilenameSet.cardinal modified in

  (* clear errors for modified files, deleted files and master *)
  let master_cx = Init_js.get_master_cx options in
  clear_errors ~debug (Context.file master_cx :: FilenameSet.elements modified);
  clear_errors ~debug (FilenameSet.elements deleted);

  (* record reparse errors *)
  let failed_filenames = List.map (fun (file, _) -> file) freshparse_fail in
  save_errors parse_errors failed_filenames freshparse_errors;

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
  let removed_modules = Module_js.remove_files to_clear in

  (* TODO elsewhere or delete *)
  Context.remove_all_errors master_cx;

  let dependent_file_count = ref 0 in

  (* recheck *)
  let timing = typecheck
    ~options
    ~timing
    ~workers
    ~make_merge_input:(fun inferred ->
      (* need to merge the closure of inferred files and their deps *)
      let inferred_set = FilenameSet.of_list inferred in

      (* direct_deps are unmodified files which directly depend on
         inferred files or removed modules. all_deps are direct_deps
         plus their dependents (transitive closure) *)
      let all_deps, direct_deps = Dep_service.dependent_files
        workers
        unmodified_parsed
        inferred_set
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
      let to_merge = FilenameSet.union all_deps inferred_set in
      Merge_service.remove_batch to_merge;
      SharedMem.collect `gentle;

      true,
      FilenameSet.elements to_merge,
      direct_deps
    )
    freshparsed
    removed_modules
    (List.rev_append freshparse_fail freshparse_skips)
  in

  FlowEventLogger.recheck
    ~modified_count
    ~deleted_count
    ~dependent_file_count:!dependent_file_count
    ~timing;

  let parsed = FilenameSet.union freshparsed unmodified_parsed in

  (* NOTE: unused fields are left in their initial empty state *)
  { env with ServerEnv.
    files = parsed;
    errorl = get_errors ();
  }

(* full typecheck *)
let full_check workers ~ordered_libs parse_next options =
  let timing = FlowEventLogger.Timing.create () in

  (* force types when --all is set, but otherwise forbid them unless the file
     has @flow in it. *)
  let types_mode = Parsing_service_js.(
    if Options.all options then TypesAllowed else TypesForbiddenByDefault
  ) in

  let use_strict = Options.modules_are_use_strict options in

  let profile = Options.should_profile options in

  Flow_logger.log "Parsing";
  let timing, (parsed, skipped_files, error_files, errors) =
    with_timer ~options "Parsing" timing (fun () ->
      Parsing_service_js.parse
        ~types_mode ~use_strict ~profile
        workers parse_next
    ) in
  let error_filenames = List.map (fun (file, _) -> file) error_files in
  save_errors parse_errors error_filenames errors;

  Flow_logger.log "Building package heap";
  let timing, () = with_timer ~options "PackageHeap" timing (fun () ->
    FilenameSet.iter (fun filename ->
      match filename with
      | Loc.JsonFile str when Filename.basename str = "package.json" ->
        let ast = Parsing_service_js.get_ast_unsafe filename in
        Module_js.add_package str ast
      | _ -> ()
    ) parsed;
  ) in

  let timing = typecheck
    ~options
    ~timing
    ~workers
    ~make_merge_input:(fun inferred ->
      (* after local inference and before merge, bring in libraries *)
      (* if any fail to parse, our return value will suppress merge *)
      let lib_files = Init_js.init
        ~options
        ordered_libs
        (fun file errs -> save_errors parse_errors [file] [errs])
        (fun file errs -> save_errors infer_errors [file] [errs])
        (fun file sups ->
          save_suppressions error_suppressions [file] [sups])
      in
      (* Note: if any libs failed, return false and files to report errors for.
         (other errors will be suppressed.)
         otherwise, return true and files to merge *)
      let err_libs = List.fold_left (
        fun acc (file, ok) -> if ok then acc else file :: acc
      ) [] lib_files in
      if err_libs != []
      then false, err_libs, FilenameSet.empty
      else true, inferred, FilenameSet.empty
    )
    parsed
    Module_js.NameSet.empty
    (List.rev_append error_files skipped_files)
  in

  (timing, parsed)

(* helper - print errors. used in check-and-die runs *)
let print_errors options errors =
  let strip_root = Options.should_strip_root options in
  let root = Options.root options in

  let errors =
    if strip_root then Errors.strip_root_from_errors root errors
    else errors
  in

  if options.Options.opt_json
  then Errors.print_error_json stdout errors
  else
    Errors.print_error_summary
      ~flags:(Options.error_flags options)
      ~strip_root
      ~root
      errors

(* initialize flow server state, including full check *)
let server_init genv =
  let options = genv.ServerEnv.options in

  let ordered_libs, libs = Files_js.init options in

  let get_next_raw = Files_js.make_next_files ~options ~libs in
  let get_next = fun () ->
    get_next_raw () |> List.map (fun file ->
      if Files_js.is_json_file file
      then Loc.JsonFile file
      else Loc.SourceFile file
    )
  in
  let (timing, parsed) =
    full_check genv.ServerEnv.workers ~ordered_libs get_next options in

  SharedMem.init_done();

  let errors = get_errors () in
  if Options.is_check_mode options
  then print_errors options errors;

  (* Return an env that initializes invariants required and maintained by
     recheck, namely that `files` contains files that parsed successfully, and
     `errorl` contains the current set of errors. *)
  timing, { ServerEnv.
    files = parsed;
    libs;
    errorl = errors;
  }
