(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Loc_collections
module Reqs = Merge_js.Reqs

type 'a unit_result = ('a, ALoc.t * Error_message.internal_error) result

type 'a file_keyed_result = File_key.t * 'a unit_result

type error_acc =
  Flow_error.ErrorSet.t
  * Flow_error.ErrorSet.t
  * Error_suppressions.t
  * Coverage_response.file_coverage FilenameMap.t option
  * float

type type_acc =
  ( Context.t
  * File_sig.With_ALoc.t FilenameMap.t
  * (ALoc.t, ALoc.t * Type.t) Flow_ast.program Utils_js.FilenameMap.t )
  option

type acc = type_acc * error_acc

type 'a merge_job_results = 'a file_keyed_result list

type 'a merge_job =
  worker_mutator:Context_heaps.Merge_context_mutator.worker_mutator ->
  options:Options.t ->
  reader:Mutator_state_reader.t ->
  File_key.t Nel.t ->
  'a unit_result

type sig_opts_data = {
  skipped_count: int;
  sig_new_or_changed: FilenameSet.t;
}

type 'a merge_results = 'a merge_job_results * sig_opts_data

type merge_context_result = {
  cx: Context.t;
  other_cxs: Context.t list;
  master_cx: Context.sig_t;
  file_sigs: File_sig.With_ALoc.t FilenameMap.t;
  typed_asts: (ALoc.t, ALoc.t * Type.t) Flow_ast.program FilenameMap.t;
  coverage_map: Coverage_response.file_coverage FilenameMap.t option;
}

(* To merge the contexts of a component with their dependencies, we call the
   functions `merge_component` and `restore` defined in merge_js.ml
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
  let (dep_cxs, reqs) =
    List.fold_left
      (fun (dep_cxs, reqs) req ->
        let (r, locs, resolved_r, file) = req in
        let locs = locs |> Nel.to_list |> ALocSet.of_list in
        Module_heaps.(
          match Reader_dispatcher.get_file ~reader ~audit:Expensive.ok resolved_r with
          | Some (File_key.ResourceFile f) -> (dep_cxs, Reqs.add_res f file locs reqs)
          | Some dep ->
            let info = Reader_dispatcher.get_info_unsafe ~reader ~audit:Expensive.ok dep in
            if info.checked && info.parsed then
              (* checked implementation exists *)
              let m = Files.module_ref dep in
              if Nel.mem ~equal:File_key.equal dep component then
                (* impl is part of component *)
                (dep_cxs, Reqs.add_impl m file locs reqs)
              else
                (* look up impl sig_context *)
                let leader = Context_heaps.Reader_dispatcher.find_leader ~reader dep in
                let dep_cx = Context_heaps.Reader_dispatcher.find_sig ~reader leader in
                (dep_cx :: dep_cxs, Reqs.add_dep_impl m file (dep_cx, locs) reqs)
            else
              (* unchecked implementation exists *)
              (dep_cxs, Reqs.add_unchecked r file locs reqs)
          | None ->
            (* implementation doesn't exist *)
            (dep_cxs, Reqs.add_decl r file (locs, resolved_r) reqs)))
      ([], Reqs.empty)
      required
  in
  let master_cx = Context_heaps.Reader_dispatcher.find_sig ~reader File_key.Builtins in
  (master_cx, dep_cxs, reqs)

let merge_context_generic ~options ~reader ~get_ast_unsafe ~get_file_sig_unsafe ~phase component =
  let (required, file_sigs) =
    Nel.fold_left
      (fun (required, file_sigs) file ->
        let file_sig = get_file_sig_unsafe ~reader file in
        let file_sigs = FilenameMap.add file file_sig file_sigs in
        let require_loc_map = File_sig.With_ALoc.(require_loc_map file_sig.module_sig) in
        let required =
          SMap.fold
            (fun r locs acc ->
              let resolved_r = Module_js.find_resolved_module ~reader ~audit:Expensive.ok file r in
              (r, locs, resolved_r, file) :: acc)
            require_loc_map
            required
        in
        (required, file_sigs))
      ([], FilenameMap.empty)
      component
  in
  let (master_cx, dep_cxs, file_reqs) = reqs_of_component ~reader component required in
  let metadata = Context.metadata_of_options options in
  let lint_severities = Options.lint_severities options in
  let strict_mode = Options.strict_mode options in
  let get_aloc_table_unsafe =
    Parsing_heaps.Reader_dispatcher.get_sig_ast_aloc_table_unsafe ~reader
  in
  let (((full_cx, _), other_cxs) as cx_nel) =
    Merge_js.merge_component
      ~metadata
      ~lint_severities
      ~strict_mode
      ~file_sigs
      ~phase
      ~get_ast_unsafe:(get_ast_unsafe ~reader)
      ~get_aloc_table_unsafe
      ~get_docblock_unsafe:(Parsing_heaps.Reader_dispatcher.get_docblock_unsafe ~reader)
      component
      file_reqs
      dep_cxs
      master_cx
  in
  (* Only compute coverage on Types-First checking, or Classic merging phases *)
  let coverage_map =
    match (Options.arch options, phase) with
    | (_, Context.Normalizing)
    | (Options.TypesFirst, Context.Merging) ->
      None
    | (Options.TypesFirst, Context.Checking)
    | (Options.Classic, _) ->
      Some
        (Nel.fold_left
           (fun acc (ctx, typed_ast) ->
             let file = Context.file ctx in
             let cov = Coverage.file_coverage ~full_cx typed_ast in
             FilenameMap.add file cov acc)
           FilenameMap.empty
           cx_nel)
  in
  let typed_asts =
    Nel.fold_left
      (fun typed_asts (ctx, typed_ast) ->
        let file = Context.file ctx in
        FilenameMap.add file typed_ast typed_asts)
      FilenameMap.empty
      cx_nel
  in
  let other_cxs = Base.List.map ~f:(fun (cx, _) -> cx) other_cxs in
  { cx = full_cx; other_cxs; master_cx; file_sigs; typed_asts; coverage_map }

let merge_context ~options ~reader component =
  merge_context_generic
    ~options
    ~reader
    ~phase:
      (match Options.arch options with
      | Options.Classic -> Context.Checking
      | Options.TypesFirst -> Context.Merging)
    ~get_ast_unsafe:
      (match Options.arch options with
      | Options.Classic ->
        fun ~reader file ->
          let ((_, _, comments) as ast) =
            Parsing_heaps.Reader_dispatcher.get_ast_unsafe ~reader file
          in
          let aloc_ast = Ast_loc_utils.loc_to_aloc_mapper#program ast in
          (comments, aloc_ast)
      | Options.TypesFirst ->
        fun ~reader file ->
          let (_, _, comments) = Parsing_heaps.Reader_dispatcher.get_ast_unsafe ~reader file in
          let aloc_ast = Parsing_heaps.Reader_dispatcher.get_sig_ast_unsafe ~reader file in
          (comments, aloc_ast))
    ~get_file_sig_unsafe:
      (match Options.arch options with
      | Options.Classic ->
        fun ~reader file ->
          let loc_file_sig = Parsing_heaps.Reader_dispatcher.get_file_sig_unsafe ~reader file in
          File_sig.abstractify_locs loc_file_sig
      | Options.TypesFirst -> Parsing_heaps.Reader_dispatcher.get_sig_file_sig_unsafe)
    component

(* Variation of merge_context where requires may not have already been
   resolved. This is used by commands that make up a context on the fly. *)
let merge_contents_context ~reader options file ast info file_sig =
  let (_, _, comments) = ast in
  let aloc_ast = Ast_loc_utils.loc_to_aloc_mapper#program ast in
  let reader = Abstract_state_reader.State_reader reader in
  let file_sig = File_sig.abstractify_locs file_sig in
  let required =
    let require_loc_map = File_sig.With_ALoc.(require_loc_map file_sig.module_sig) in
    SMap.fold
      (fun r (locs : ALoc.t Nel.t) required ->
        let resolved_r =
          Module_js.imported_module
            ~options
            ~reader
            ~node_modules_containers:!Files.node_modules_containers
            file
            locs
            r
        in
        (r, locs, resolved_r, file) :: required)
      require_loc_map
      []
  in
  let file_sigs = FilenameMap.singleton file file_sig in
  let component = Nel.one file in
  let (master_cx, dep_cxs, file_reqs) =
    try reqs_of_component ~reader component required with
    | Key_not_found _ -> failwith "not all dependencies are ready yet, aborting..."
    | e -> raise e
  in
  let metadata = Context.metadata_of_options options in
  let lint_severities = Options.lint_severities options in
  let strict_mode = Options.strict_mode options in
  let get_aloc_table_unsafe =
    Parsing_heaps.Reader_dispatcher.get_sig_ast_aloc_table_unsafe ~reader
  in
  let ((cx, tast), _) =
    Merge_js.merge_component
      ~metadata
      ~lint_severities
      ~strict_mode
      ~file_sigs
      ~get_ast_unsafe:(fun _ -> (comments, aloc_ast))
      ~get_aloc_table_unsafe
      ~get_docblock_unsafe:(fun _ -> info)
      ~phase:Context.Checking
      component
      file_reqs
      dep_cxs
      master_cx
  in
  (cx, tast)

(* Entry point for merging a component *)
let merge_component ~worker_mutator ~options ~reader component =
  let start_merge_time = Unix.gettimeofday () in
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
      merge_context ~options ~reader component
    in
    let module_refs = List.rev_map Files.module_ref (Nel.to_list component) in
    let md5 = Merge_js.ContextOptimizer.sig_context cx module_refs in
    Context.clear_master_shared cx master_cx;

    let errors = Context.errors cx in
    let suppressions = Context.error_suppressions cx in
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
    Context.remove_all_errors cx;
    Context.remove_all_error_suppressions cx;
    Context.remove_all_lint_severities cx;

    Context.clear_intermediates cx;

    Context_heaps.Merge_context_mutator.add_merge_on_diff
      ~audit:Expensive.ok
      worker_mutator
      cx
      component
      md5;
    let merge_time = Unix.gettimeofday () -. start_merge_time in
    Ok (errors, warnings, suppressions, coverage_map, merge_time)
  ) else
    let errors = Flow_error.ErrorSet.empty in
    let suppressions = Error_suppressions.empty in
    let warnings = Flow_error.ErrorSet.empty in
    let coverage = None in
    Ok (errors, warnings, suppressions, coverage, 0.0)

let check_file options ~reader file =
  let start_check_time = Unix.gettimeofday () in
  let info = Module_heaps.Mutator_reader.get_info_unsafe ~reader ~audit:Expensive.ok file in
  if info.Module_heaps.checked then
    let reader = Abstract_state_reader.Mutator_state_reader reader in
    let { cx; coverage_map; typed_asts; file_sigs; _ } =
      merge_context_generic
        ~options
        ~reader
        ~phase:Context.Checking
        ~get_ast_unsafe:(fun ~reader file ->
          let ((_, _, comments) as ast) =
            Parsing_heaps.Reader_dispatcher.get_ast_unsafe ~reader file
          in
          let aloc_ast = Ast_loc_utils.loc_to_aloc_mapper#program ast in
          (comments, aloc_ast))
        ~get_file_sig_unsafe:(fun ~reader file ->
          Parsing_heaps.Reader_dispatcher.get_file_sig_unsafe ~reader file
          |> File_sig.abstractify_locs)
        (Nel.one file)
    in
    let errors = Context.errors cx in
    let suppressions = Context.error_suppressions cx in
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
    let check_time = Unix.gettimeofday () -. start_check_time in
    (Some (cx, file_sigs, typed_asts), (errors, warnings, suppressions, coverage_map, check_time))
  else
    let errors = Flow_error.ErrorSet.empty in
    let suppressions = Error_suppressions.empty in
    let warnings = Flow_error.ErrorSet.empty in
    let coverage = None in
    (None, (errors, warnings, suppressions, coverage, 0.0))

(* Wrap a potentially slow operation with a timer that fires every interval seconds. When it fires,
 * it calls ~on_timer. When the operation finishes, the timer is cancelled *)
let with_async_logging_timer ~interval ~on_timer ~f =
  let start_time = Unix.gettimeofday () in
  let timer = ref None in
  let cancel_timer () = Base.Option.iter ~f:Timer.cancel_timer !timer in
  let rec run_timer ?(first_run = false) () =
    ( if not first_run then
      let run_time = Unix.gettimeofday () -. start_time in
      on_timer run_time );
    timer := Some (Timer.set_timer ~interval ~callback:run_timer)
  in
  (* Timer is unimplemented in Windows. *)
  if not Sys.win32 then run_timer ~first_run:true ();
  let ret =
    try f ()
    with e ->
      cancel_timer ();
      raise e
  in
  cancel_timer ();
  ret

let merge_job ~worker_mutator ~reader ~job ~options merged elements =
  List.fold_left
    (fun merged -> function
      | Merge_stream.Component component ->
        (* A component may have several files: there's always at least one, and
         multiple files indicate a cycle. *)
        let files =
          component |> Nel.to_list |> Base.List.map ~f:File_key.to_string |> String.concat "\n\t"
        in
        let merge_timeout = Options.merge_timeout options in
        let interval = Base.Option.value_map ~f:(min 15.0) ~default:15.0 merge_timeout in
        (try
           with_async_logging_timer
             ~interval
             ~on_timer:(fun run_time ->
               Hh_logger.info
                 "[%d] Slow MERGE (%f seconds so far): %s"
                 (Unix.getpid ())
                 run_time
                 files;
               Base.Option.iter merge_timeout ~f:(fun merge_timeout ->
                   if run_time >= merge_timeout then
                     raise (Error_message.EMergeTimeout (run_time, files))))
             ~f:(fun () ->
               let start_time = Unix.gettimeofday () in
               (* prerr_endlinef "[%d] MERGE: %s" (Unix.getpid()) files; *)
               let ret = job ~worker_mutator ~options ~reader component in
               let merge_time = Unix.gettimeofday () -. start_time in
               if Options.should_profile options then (
                 let length = Nel.length component in
                 let leader = Nel.hd component |> File_key.to_string in
                 Flow_server_profile.merge ~length ~merge_time ~leader;
                 if merge_time > 1.0 then
                   Hh_logger.info "[%d] perf: merged %s in %f" (Unix.getpid ()) files merge_time
               );
               (Nel.hd component, ret) :: merged)
         with
        | (SharedMem_js.Out_of_shared_memory | SharedMem_js.Heap_full | SharedMem_js.Hash_table_full)
          as exc ->
          raise exc
        (* A catch all suppression is probably a bad idea... *)
        | unwrapped_exc ->
          let exc = Exception.wrap unwrapped_exc in
          let exn_str = Printf.sprintf "%s: %s" files (Exception.to_string exc) in
          (* Ensure heaps are in a good state before continuing. *)
          Context_heaps.Merge_context_mutator.add_merge_on_exn
            ~audit:Expensive.ok
            worker_mutator
            ~options
            component;

          (* In dev mode, fail hard, but log and continue in prod. *)
          if Build_mode.dev then
            Exception.reraise exc
          else
            prerr_endlinef
              "(%d) merge_job THROWS: [%d] %s\n"
              (Unix.getpid ())
              (Nel.length component)
              exn_str;

          (* An errored component is always changed. *)
          let file = Nel.hd component in
          let file_loc = Loc.{ none with source = Some file } |> ALoc.of_loc in
          (* We can't pattern match on the exception type once it's marshalled
           back to the master process, so we pattern match on it here to create
           an error result. *)
          let result =
            Error
              Error_message.(
                match unwrapped_exc with
                | EDebugThrow loc -> (loc, DebugThrow)
                | EMergeTimeout (s, _) -> (file_loc, MergeTimeout s)
                | _ -> (file_loc, MergeJobException exc))
          in
          (file, result) :: merged))
    merged
    elements

let merge_runner
    ~job
    ~master_mutator
    ~worker_mutator
    ~reader
    ~intermediate_result_callback
    ~options
    ~workers
    ~sig_dependency_graph
    ~component_map
    ~recheck_set =
  let num_workers = Options.max_workers options in
  (* (1) make a map from files to their component leaders
     (2) lift recheck set from files to their component leaders *)
  let (leader_map, recheck_leader_set) =
    FilenameMap.fold
      (fun leader component (leader_map, recheck_leader_set) ->
        let (leader_map, recheck_leader) =
          Nel.fold_left
            (fun (leader_map, recheck_leader) file ->
              ( FilenameMap.add file leader leader_map,
                recheck_leader || FilenameSet.mem file recheck_set ))
            (leader_map, false)
            component
        in
        let recheck_leader_set =
          if recheck_leader then
            FilenameSet.add leader recheck_leader_set
          else
            recheck_leader_set
        in
        (leader_map, recheck_leader_set))
      component_map
      (FilenameMap.empty, FilenameSet.empty)
  in
  let start_time = Unix.gettimeofday () in
  let stream =
    Merge_stream.create
      ~num_workers
      ~arch:(Options.arch options)
      ~sig_dependency_graph
      ~leader_map
      ~component_map
      ~recheck_leader_set
      ~intermediate_result_callback
  in
  Merge_stream.update_server_status stream;

  (* returns parallel lists of filenames, error sets, and suppression sets *)
  let%lwt ret =
    MultiWorkerLwt.call
      workers
      ~job:(merge_job ~worker_mutator ~reader ~options ~job)
      ~neutral:[]
      ~merge:(Merge_stream.merge ~master_mutator ~reader stream)
      ~next:(Merge_stream.next stream)
  in
  let total_files = Merge_stream.total_files stream in
  let skipped_count = Merge_stream.skipped_count stream in
  let sig_new_or_changed = Merge_stream.sig_new_or_changed master_mutator in
  Hh_logger.info "Merge skipped %d of %d modules" skipped_count total_files;
  let elapsed = Unix.gettimeofday () -. start_time in
  if Options.should_profile options then Hh_logger.info "merged in %f" elapsed;
  Lwt.return (ret, { skipped_count; sig_new_or_changed })

let merge = merge_runner ~job:merge_component

let check options ~reader file =
  let result =
    let check_timeout = Options.merge_timeout options in
    (* TODO: add new option *)
    let interval = Base.Option.value_map ~f:(min 5.0) ~default:5.0 check_timeout in
    let file_str = File_key.to_string file in
    try
      with_async_logging_timer
        ~interval
        ~on_timer:(fun run_time ->
          Hh_logger.info
            "[%d] Slow CHECK (%f seconds so far): %s"
            (Unix.getpid ())
            run_time
            file_str;
          Base.Option.iter check_timeout ~f:(fun check_timeout ->
              if run_time >= check_timeout then
                raise (Error_message.ECheckTimeout (run_time, file_str))))
        ~f:(fun () -> Ok (check_file options ~reader file))
    with
    | (SharedMem_js.Out_of_shared_memory | SharedMem_js.Heap_full | SharedMem_js.Hash_table_full) as
      exc ->
      raise exc
    (* A catch all suppression is probably a bad idea... *)
    | unwrapped_exc ->
      let exc = Exception.wrap unwrapped_exc in
      let exn_str = Printf.sprintf "%s: %s" (File_key.to_string file) (Exception.to_string exc) in
      (* In dev mode, fail hard, but log and continue in prod. *)
      if Build_mode.dev then
        Exception.reraise exc
      else
        prerr_endlinef "(%d) check_job THROWS: %s\n" (Unix.getpid ()) exn_str;
      let file_loc = Loc.{ none with source = Some file } |> ALoc.of_loc in
      (* We can't pattern match on the exception type once it's marshalled
         back to the master process, so we pattern match on it here to create
         an error result. *)
      Error
        Error_message.(
          match unwrapped_exc with
          | EDebugThrow loc -> (loc, DebugThrow)
          | ECheckTimeout (s, _) -> (file_loc, CheckTimeout s)
          | _ -> (file_loc, CheckJobException exc))
  in
  (file, result)
