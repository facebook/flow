(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
module Reqs = Merge_js.Reqs

type 'a merge_job_result = ('a, Error_message.t) result
type 'a merge_job_results = (File_key.t * 'a merge_job_result) list
type 'a merge_job =
  worker_mutator: Context_heaps.Merge_context_mutator.worker_mutator ->
  options:Options.t ->
  reader: Mutator_state_reader.t ->
  File_key.t Nel.t ->
  'a merge_job_result

type 'a merge_results = 'a merge_job_results * int (* skipped count *)

type merge_strict_context_result = {
  cx: Context.t;
  other_cxs: Context.t list;
  master_cx: Context.sig_t;
  file_sigs: File_sig.With_ALoc.t FilenameMap.t;
  typed_asts: (ALoc.t, ALoc.t * Type.t) Flow_ast.program FilenameMap.t;
  coverage_map: Coverage.file_coverage FilenameMap.t;
}

(* To merge the contexts of a component with their dependencies, we call the
   functions `merge_component_strict` and `restore` defined in merge_js.ml
   with appropriate reqs prepared below.

   (a) orig_sig_cxs: the original signature contexts of dependencies outside the
   component.

   (b) sig_cxs: the copied signature contexts of such dependencies.

   (c) impls: edges between files within the component

   (d) dep_impls: edges from files in the component to cxs of direct
   dependencies, when implementations are found.

   (e) unchecked: edges from files in the component to files which are known to
   exist are not checked (no @flow, @noflow, unparsed). Note that these
   dependencies might be provided by a (typed) libdef, but we don't know yet.

   (f) res: edges between files in the component and resource files, labeled
   with the requires they denote.

   (g) decls: edges between files in the component and libraries, classified
   by requires (when implementations of such requires are not found).
*)
let reqs_of_component ~reader component required =
  let dep_cxs, reqs =
    List.fold_left (fun (dep_cxs, reqs) req ->
      let r, locs, resolved_r, file = req in
      let locs = locs |> Nel.to_list |> ALocSet.of_list in
      Module_heaps.(match Reader_dispatcher.get_file ~reader ~audit:Expensive.ok resolved_r with
      | Some (File_key.ResourceFile f) ->
        dep_cxs, Reqs.add_res f file locs reqs
      | Some dep ->
        let info = Reader_dispatcher.get_info_unsafe ~reader ~audit:Expensive.ok dep in
        if info.checked && info.parsed then
          (* checked implementation exists *)
          let m = Files.module_ref dep in
          if Nel.mem dep component then
            (* impl is part of component *)
            dep_cxs, Reqs.add_impl m file locs reqs
          else
            (* look up impl sig_context *)
            let leader = Context_heaps.Reader_dispatcher.find_leader ~reader dep in
            let dep_cx = Context_heaps.Reader_dispatcher.find_sig ~reader leader in
            dep_cx::dep_cxs, Reqs.add_dep_impl m file (dep_cx, locs) reqs
        else
          (* unchecked implementation exists *)
          dep_cxs, Reqs.add_unchecked r file locs reqs
      | None ->
        (* implementation doesn't exist *)
        dep_cxs, Reqs.add_decl r file (locs, resolved_r) reqs
      )
    ) ([], Reqs.empty) required
  in

  let master_cx = Context_heaps.Reader_dispatcher.find_sig ~reader File_key.Builtins in

  master_cx, dep_cxs, reqs

let merge_strict_context_generic ~options ~reader ~get_ast_unsafe ~get_file_sig_unsafe component =
  let required, file_sigs =
    Nel.fold_left (fun (required, file_sigs) file ->
      let file_sig = get_file_sig_unsafe ~reader file in
      let file_sigs = FilenameMap.add file file_sig file_sigs in
      let require_loc_map = File_sig.With_ALoc.(require_loc_map file_sig.module_sig) in
      let required = SMap.fold (fun r locs acc ->
        let resolved_r = Module_js.find_resolved_module ~reader ~audit:Expensive.ok
          file r in
        (r, locs, resolved_r, file) :: acc
      ) require_loc_map required in
      required, file_sigs
    ) ([], FilenameMap.empty) component in

  let master_cx, dep_cxs, file_reqs =
    reqs_of_component ~reader component required
  in

  let metadata = Context.metadata_of_options options in
  let lint_severities = Options.lint_severities options in
  let file_options = Some (Options.file_options options) in
  let strict_mode = Options.strict_mode options in
  let (((cx, _, _), other_cxs) as cx_nel) = Merge_js.merge_component_strict
    ~metadata ~lint_severities ~file_options ~strict_mode ~file_sigs
    ~get_ast_unsafe:(get_ast_unsafe ~reader)
    ~get_docblock_unsafe:(Parsing_heaps.Reader_dispatcher.get_docblock_unsafe ~reader)
    component file_reqs dep_cxs master_cx
  in

  let typed_asts, coverage_map = Nel.fold_left (fun (typed_asts, cov_map) (ctx, typed_ast, cov) ->
    let file = Context.file ctx in
    FilenameMap.add file typed_ast typed_asts, FilenameMap.add file cov cov_map
  ) (FilenameMap.empty, FilenameMap.empty) cx_nel in

  let other_cxs = Core_list.map ~f:(fun (cx, _, _) -> cx) other_cxs in

  { cx; other_cxs; master_cx; file_sigs; typed_asts; coverage_map }

let merge_strict_context ~options ~reader component =
  merge_strict_context_generic ~options ~reader
    ~get_ast_unsafe:(match options.Options.opt_arch with
      | Options.Classic -> fun ~reader file ->
        let (_, _, comments) as ast = Parsing_heaps.Reader_dispatcher.get_ast_unsafe ~reader file in
        let aloc_ast = Ast_loc_utils.abstractify_mapper#program ast in
        (comments, aloc_ast)
      | Options.TypesFirst -> fun ~reader file ->
        let (_, _, comments) as ast = Parsing_heaps.Reader_dispatcher.get_sig_ast_unsafe ~reader file in
        let aloc_ast = Ast_loc_utils.abstractify_mapper#program ast in
        (comments, aloc_ast)
    )
    ~get_file_sig_unsafe:(match options.Options.opt_arch with
      | Options.Classic -> fun ~reader file ->
        let loc_file_sig = Parsing_heaps.Reader_dispatcher.get_file_sig_unsafe ~reader file in
        File_sig.abstractify_locs loc_file_sig
      | Options.TypesFirst -> fun ~reader file ->
        let loc_file_sig = Parsing_heaps.Reader_dispatcher.get_sig_file_sig_unsafe ~reader file in
        File_sig.abstractify_locs loc_file_sig
    )
    component

(* Variation of merge_strict_context where requires may not have already been
   resolved. This is used by commands that make up a context on the fly. *)
let merge_contents_context ~reader options file ast info file_sig =
  let (_, _, comments) = ast in
  let aloc_ast = Ast_loc_utils.abstractify_mapper#program ast in
  let reader = Abstract_state_reader.State_reader reader in
  let file_sig = File_sig.abstractify_locs file_sig in
  let required =
    let require_loc_map = File_sig.With_ALoc.(require_loc_map file_sig.module_sig) in
    SMap.fold (fun r (locs: ALoc.t Nel.t) required ->
      let resolved_r = Module_js.imported_module
        ~options
        ~reader
        ~node_modules_containers:!Files.node_modules_containers
        file locs r in
      (r, locs, resolved_r, file) :: required
    ) require_loc_map []
  in
  let file_sigs = FilenameMap.singleton file file_sig in

  let component = Nel.one file in

  let master_cx, dep_cxs, file_reqs =
    begin try reqs_of_component ~reader component required with
      | Key_not_found _  ->
        failwith "not all dependencies are ready yet, aborting..."
      | e -> raise e
    end
  in

  let metadata = Context.metadata_of_options options in
  let lint_severities = Options.lint_severities options in
  let file_options = Some (Options.file_options options) in
  let strict_mode = Options.strict_mode options in
  let ((cx, tast, _), _) = Merge_js.merge_component_strict
    ~metadata ~lint_severities ~file_options ~strict_mode ~file_sigs
    ~get_ast_unsafe:(fun _ -> (comments, aloc_ast))
    ~get_docblock_unsafe:(fun _ -> info)
    component file_reqs dep_cxs master_cx
  in

  (cx, tast)

(* Entry point for merging a component *)
let merge_strict_component ~worker_mutator ~options ~reader component =
  let file = Nel.hd component in

  (* We choose file as the leader, and other_files are followers. It is always
     OK to choose file as leader, as explained below.

     Note that cycles cannot happen between unchecked files. Why? Because files
     in cycles must have their dependencies recorded, yet dependencies are never
     recorded for unchecked files.

     It follows that when file is unchecked, there are no other_files! We don't
     have to worry that some other_file may be checked when file is unchecked.

     It also follows when file is checked, other_files must be checked too!
  *)
  let info = Module_heaps.Mutator_reader.get_info_unsafe ~reader ~audit:Expensive.ok file in
  if info.Module_heaps.checked then (
    let reader = Abstract_state_reader.Mutator_state_reader reader in
    let { cx; other_cxs = _; master_cx; coverage_map; _ } =
      merge_strict_context ~options ~reader component in

    let module_refs = List.rev_map Files.module_ref (Nel.to_list component) in
    let md5 = Merge_js.ContextOptimizer.sig_context cx module_refs in

    Context.clear_master_shared cx master_cx;

    let errors = Context.errors cx in
    let suppressions = Context.error_suppressions cx in
    let severity_cover = Context.severity_cover cx in
    let include_suppressions = Context.include_suppressions cx in

    let errors, warnings, suppressions =
      Error_suppressions.filter_lints ~include_suppressions suppressions errors severity_cover in

    Context.remove_all_errors cx;
    Context.remove_all_error_suppressions cx;
    Context.remove_all_lint_severities cx;

    Context.clear_intermediates cx;

    Context_heaps.Merge_context_mutator.add_merge_on_diff
      ~audit:Expensive.ok worker_mutator cx component md5;

    Ok (errors, warnings, suppressions, coverage_map)
  )
  else
    let errors = Flow_error.ErrorSet.empty in
    let suppressions = Error_suppressions.empty in
    let warnings = Flow_error.ErrorSet.empty in
    let coverage = FilenameMap.empty in
    Ok (errors, warnings, suppressions, coverage)

let check_file options ~reader file =
  let info = Module_heaps.Mutator_reader.get_info_unsafe ~reader ~audit:Expensive.ok file in
  if info.Module_heaps.checked then (
    let reader = Abstract_state_reader.Mutator_state_reader reader in
    let { cx; coverage_map; _; } = merge_strict_context_generic ~options ~reader
      ~get_ast_unsafe:(fun ~reader file ->
        let (_, _, comments) as ast = Parsing_heaps.Reader_dispatcher.get_ast_unsafe ~reader file in
        let aloc_ast = Ast_loc_utils.abstractify_mapper#program ast in
        (comments, aloc_ast)
      )
      ~get_file_sig_unsafe:(fun ~reader file ->
        Parsing_heaps.Reader_dispatcher.get_file_sig_unsafe ~reader file
        |> File_sig.abstractify_locs
      )
      (Nel.one file) in
    let errors = Context.errors cx in
    let suppressions = Context.error_suppressions cx in
    let severity_cover = Context.severity_cover cx in
    let include_suppressions = Context.include_suppressions cx in
    let errors, warnings, suppressions =
      Error_suppressions.filter_lints ~include_suppressions suppressions errors severity_cover in
    errors, warnings, suppressions, coverage_map
  )
  else
    let errors = Flow_error.ErrorSet.empty in
    let suppressions = Error_suppressions.empty in
    let warnings = Flow_error.ErrorSet.empty in
    let coverage = FilenameMap.empty in
    (errors, warnings, suppressions, coverage)

(* Wrap a potentially slow operation with a timer that fires every interval seconds. When it fires,
 * it calls ~on_timer. When the operation finishes, the timer is cancelled *)
let with_async_logging_timer ~interval ~on_timer ~f =
  let start_time = Unix.gettimeofday () in
  let timer = ref None in
  let cancel_timer () = Option.iter ~f:Timer.cancel_timer !timer in
  let rec run_timer ?(first_run=false) () =
    if not first_run
    then begin
      let run_time = Unix.gettimeofday () -. start_time in
      on_timer run_time
    end;
    timer := Some (Timer.set_timer ~interval ~callback:run_timer)
  in
  (* Timer is unimplemented in Windows. *)
  if not Sys.win32 then run_timer ~first_run:true ();
  let ret = begin try f ()
  with e ->
    cancel_timer ();
    raise e
  end in
  cancel_timer ();
  ret

let merge_strict_job ~worker_mutator ~reader ~job ~options merged elements =
  List.fold_left (fun merged -> function
    | Merge_stream.Component component ->
      (* A component may have several files: there's always at least one, and
         multiple files indicate a cycle. *)
      let files = component
      |> Nel.to_list
      |> Core_list.map ~f:File_key.to_string
      |> String.concat "\n\t"
      in

      let merge_timeout = Options.merge_timeout options in
      let interval = Option.value_map ~f:(min 15.0) ~default:15.0 merge_timeout in

      try with_async_logging_timer
        ~interval
        ~on_timer:(fun run_time ->
          Hh_logger.info "[%d] Slow MERGE (%f seconds so far): %s" (Unix.getpid()) run_time files;
          Option.iter merge_timeout ~f:(fun merge_timeout ->
            if run_time >= merge_timeout then raise (Error_message.EMergeTimeout run_time)
          )
        )
        ~f:(fun () ->
          let start_time = Unix.gettimeofday () in
          (* prerr_endlinef "[%d] MERGE: %s" (Unix.getpid()) files; *)
          let ret = job ~worker_mutator ~options ~reader component in
          let merge_time = Unix.gettimeofday () -. start_time in
          if Options.should_profile options then begin
            let length = Nel.length component in
            let leader = Nel.hd component |> File_key.to_string in
            Flow_server_profile.merge ~length ~merge_time ~leader;
            if merge_time > 1.0
            then Hh_logger.info "[%d] perf: merged %s in %f" (Unix.getpid()) files merge_time
          end;
          (Nel.hd component, ret) :: merged
        )
      with
      | SharedMem_js.Out_of_shared_memory
      | SharedMem_js.Heap_full
      | SharedMem_js.Hash_table_full
      | SharedMem_js.Dep_table_full as exc -> raise exc
      (* A catch all suppression is probably a bad idea... *)
      | unwrapped_exc ->
        let exc = Exception.wrap unwrapped_exc in

        let exn_str = Printf.sprintf "%s: %s" files (Exception.to_string exc) in
        (* Ensure heaps are in a good state before continuing. *)
        Context_heaps.Merge_context_mutator.add_merge_on_exn
          ~audit:Expensive.ok worker_mutator ~options component;
        (* In dev mode, fail hard, but log and continue in prod. *)
        if Build_mode.dev then Exception.reraise exc else
          prerr_endlinef "(%d) merge_strict_job THROWS: [%d] %s\n"
            (Unix.getpid()) (Nel.length component) exn_str;
        (* An errored component is always changed. *)
        let file = Nel.hd component in
        let file_loc = Loc.({ none with source = Some file }) |> ALoc.of_loc in
        (* We can't pattern match on the exception type once it's marshalled
           back to the master process, so we pattern match on it here to create
           an error result. *)
        let result = Error Error_message.(match unwrapped_exc with
        | EDebugThrow loc -> EInternal (loc, DebugThrow)
        | EMergeTimeout s -> EInternal (file_loc, MergeTimeout s)
        | _ -> EInternal (file_loc, MergeJobException exc)
        ) in
        (file, result) :: merged
  ) merged elements

(* make a map from component leaders to components *)
let merge_runner
    ~job ~master_mutator ~worker_mutator ~reader ~intermediate_result_callback ~options ~workers
    dependency_graph component_map recheck_map =
  let num_workers = Options.max_workers options in
  (* make a map from files to their component leaders *)
  let leader_map =
    FilenameMap.fold (fun file component acc ->
      Nel.fold_left (fun acc file_ ->
        FilenameMap.add file_ file acc
      ) acc component
    ) component_map FilenameMap.empty
  in
  (* lift recheck map from files to leaders *)
  let recheck_leader_map = FilenameMap.map (
    Nel.exists (fun f -> FilenameMap.find_unsafe f recheck_map)
  ) component_map in

  let start_time = Unix.gettimeofday () in
  let stream = Merge_stream.create
    ~num_workers
    ~dependency_graph
    ~leader_map
    ~component_map
    ~recheck_leader_map
    ~intermediate_result_callback
  in
  Merge_stream.update_server_status stream;
  (* returns parallel lists of filenames, error sets, and suppression sets *)
  let%lwt ret = MultiWorkerLwt.call
    workers
    ~job: (merge_strict_job ~worker_mutator ~reader ~options ~job)
    ~neutral: []
    ~merge:(Merge_stream.merge ~master_mutator ~reader stream)
    ~next:(Merge_stream.next stream)
  in
  let total_files = Merge_stream.total_files stream in
  let skipped_files = Merge_stream.skipped_files stream in
  Hh_logger.info "Merge skipped %d of %d modules" skipped_files total_files;
  let elapsed = Unix.gettimeofday () -. start_time in
  if Options.should_profile options then Hh_logger.info "merged (strict) in %f" elapsed;
  Lwt.return (ret, skipped_files)

let merge_strict = merge_runner ~job:merge_strict_component
