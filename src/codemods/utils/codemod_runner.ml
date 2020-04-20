(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

let log_input_files fileset =
  Hh_logger.info
    "Running codemod on %d files:\n%s"
    (FilenameSet.cardinal fileset)
    (FilenameSet.elements fileset |> Base.List.map ~f:File_key.to_string |> String.concat "\n")

(* Computes the set of files that will be transformed, based on the current
   options. This excludes:

   (i) files that do not have the proper extension,

   (ii) files that are not tracked under Flow (ie. not @flow), unless the --all
   flag has been passed, and,

   (iii) library files.
 *)
let get_target_filename_set ~options ~libs ~all file_set =
  SSet.fold
    (fun f acc ->
      if
        Files.is_valid_path ~options f
        && (all || not (Files.is_ignored options f))
        && not (SSet.mem f libs)
      then
        FilenameSet.add (Files.filename_from_string ~options f) acc
      else
        acc)
    file_set
    FilenameSet.empty

(* TypedRunner - This runner does a full local typecheck and makes available the
 * typed AST as well as the full CX to each job. This runner is needed for any type
 * based transforms. It runs a full local typecheck so can be slow on large codebases. *)
module TypedRunner = struct
  (* We calculate the files that need to be merged in the same way that recheck
     would, but making the following assumptions:

     (i) No files have already been checked (The codemod command does not use a running
     server).

     (ii) There are no dependent files. We do not care about downstream dependencies
     of the files that are codemoded/reduced over, since we will not be reporting
     errors there.

     (iii) The set of roots (ie. targets of the transformation) becomes the focused
     set.
  *)
  let merge_targets ~env ~options ~profiling roots =
    let { ServerEnv.dependency_info; _ } = env in
    let sig_dependency_graph = Dependency_info.sig_dependency_graph dependency_info in
    let implementation_dependency_graph =
      Dependency_info.implementation_dependency_graph dependency_info
    in
    Hh_logger.info "Calculating dependencies";
    let%lwt (to_merge, _to_check, _to_merge_or_check, _components, _recheck_set) =
      Types_js.include_dependencies_and_dependents
        ~options
        ~profiling
        ~unchanged_checked:CheckedSet.empty
        ~input:(CheckedSet.of_focused_list (FilenameSet.elements roots))
        ~implementation_dependency_graph
        ~sig_dependency_graph
        ~sig_dependent_files:FilenameSet.empty
        ~all_dependent_files:FilenameSet.empty
    in
    let roots = CheckedSet.all to_merge in
    let components = Sort_js.topsort ~roots (Utils_js.FilenameGraph.to_map sig_dependency_graph) in
    let%lwt (sig_dependency_graph, component_map) =
      Types_js.calc_deps ~options ~profiling ~sig_dependency_graph ~components roots
    in
    Lwt.return (sig_dependency_graph, component_map, roots)

  (* As we merge, we will process the target files in Classic mode. These are
     included in roots set. *)
  let merge_job ~f ~roots ~info ~worker_mutator ~options ~reader component =
    let leader = Nel.hd component in
    let reader = Abstract_state_reader.Mutator_state_reader reader in
    let result =
      if Module_js.checked_file ~reader ~audit:Expensive.ok leader then (
        let { Merge_service.cx; master_cx; file_sigs; typed_asts; _ } =
          Merge_service.merge_context ~options ~reader component
        in
        let full_cx = Context.copy_of_context cx in
        let module_refs = List.rev_map Files.module_ref (Nel.to_list component) in
        let md5 = Merge_js.ContextOptimizer.sig_context cx module_refs in
        Context.clear_master_shared cx master_cx;
        Context.remove_all_errors cx;
        Context.remove_all_error_suppressions cx;
        Context.clear_intermediates cx;
        Context_heaps.Merge_context_mutator.add_merge_on_diff
          ~audit:Expensive.ok
          worker_mutator
          cx
          component
          md5;

        match Options.arch options with
        | Options.Classic ->
          Nel.fold_left
            (fun acc file ->
              if info then Hh_logger.print_with_newline "Processing %s" (File_key.to_string file);
              (* Merge will have potentially merged more that the target files (roots).
                 To avoid processing all those files, we pick the ones in the roots set. *)
              if FilenameSet.mem file roots then
                let file_sig = FilenameMap.find file file_sigs in
                let typed_ast = FilenameMap.find file typed_asts in
                let ast = Parsing_heaps.Reader_dispatcher.get_ast_unsafe ~reader file in
                let metadata = Context.metadata full_cx in
                let ccx = { Codemod_context.Typed.file; file_sig; metadata; full_cx; typed_ast } in
                let result = f ast ccx in
                FilenameMap.add file (Ok result) acc
              else
                acc)
            FilenameMap.empty
            component
        | Options.TypesFirst -> FilenameMap.empty
      ) else
        FilenameMap.empty
    in
    Ok result

  (* The processing step in Types-First needs to happen right after the check phase.
     We have already merged any necessary dependencies, so now we only check the
     target files for processing. *)
  let check_job ~f ~info ~reader ~options acc roots =
    List.fold_left
      (fun acc file ->
        if info then Hh_logger.print_with_newline "Processing %s" (File_key.to_string file);
        match Merge_service.check options ~reader file with
        | (file, Ok (Some (full_cx, file_sigs, typed_asts), _)) ->
          let file_sig = FilenameMap.find file file_sigs in
          let typed_ast = FilenameMap.find file typed_asts in
          let reader = Abstract_state_reader.Mutator_state_reader reader in
          let ast = Parsing_heaps.Reader_dispatcher.get_ast_unsafe ~reader file in
          let metadata = Context.metadata full_cx in
          let ccx = { Codemod_context.Typed.file; file_sig; metadata; full_cx; typed_ast } in
          let result = f ast ccx in
          FilenameMap.add file (Ok result) acc
        | (_, Ok (None, _)) -> acc
        | (_, Error e) -> FilenameMap.add file (Error e) acc)
      acc
      roots

  let run ~genv ~should_print_summary ~info ~f options roots =
    PidLog.disable ();
    (* Disable monitor updates as this is a single-use tool *)
    MonitorRPC.disable ();
    let workers = genv.ServerEnv.workers in

    Profiling_js.with_profiling_lwt ~label:"Codemod" ~should_print_summary (fun profiling ->
        let%lwt (_libs_ok, env, _recheck_stats) = Types_js.init ~profiling ~workers options in

        (* Create roots set based on file list *)
        let roots =
          get_target_filename_set
            ~options:(Options.file_options options)
            ~libs:env.ServerEnv.libs
            ~all:(Options.all options)
            roots
        in
        (* Discard uparseable files *)
        let roots = FilenameSet.inter roots env.ServerEnv.files in
        log_input_files roots;

        let%lwt results =
          Transaction.with_transaction (fun transaction ->
              let reader = Mutator_state_reader.create transaction in

              (* Calculate dependencies that need to be merged *)
              let%lwt (sig_dependency_graph, component_map, files_to_merge) =
                merge_targets ~env ~options ~profiling roots
              in
              let (master_mutator, worker_mutator) =
                Context_heaps.Merge_context_mutator.create transaction files_to_merge
              in
              Hh_logger.info "Merging %d files" (FilenameSet.cardinal files_to_merge);
              let%lwt (merge_result, _) =
                Merge_service.merge_runner
                  ~job:(merge_job ~f ~roots ~info)
                  ~master_mutator
                  ~worker_mutator
                  ~reader
                  ~intermediate_result_callback:(fun _ -> ())
                  ~options
                  ~workers
                  ~sig_dependency_graph
                  ~component_map
                  ~recheck_set:files_to_merge
              in
              Hh_logger.info "Merging done.";
              let merge_result =
                List.fold_left
                  (fun acc (file, result) ->
                    match result with
                    | Ok result -> FilenameMap.union result acc
                    | Error e -> FilenameMap.add file (Error e) acc)
                  FilenameMap.empty
                  merge_result
              in
              match Options.arch options with
              | Options.Classic ->
                (* Nothing to do here *)
                Lwt.return merge_result
              | Options.TypesFirst ->
                Hh_logger.info "Checking %d files" (FilenameSet.cardinal roots);
                let%lwt result =
                  MultiWorkerLwt.call
                    workers
                    ~job:(check_job ~f ~info ~reader ~options)
                    ~neutral:FilenameMap.empty
                    ~merge:FilenameMap.union
                    ~next:(MultiWorkerLwt.next workers (FilenameSet.elements roots))
                in
                Hh_logger.info "Done";
                Lwt.return result)
        in
        Lwt.return results)

  let digest ~reporter results =
    FilenameMap.fold
      (fun file_key result acc ->
        match result with
        | Ok ok ->
          let (acc_files, acc_result) = acc in
          (file_key :: acc_files, reporter.Codemod_report.combine acc_result ok)
        | Error (aloc, err) ->
          Utils_js.prerr_endlinef
            "%s: %s"
            (Reason.string_of_aloc aloc)
            (Error_message.string_of_internal_error err);
          acc)
      results
      ([], reporter.Codemod_report.empty)

  let run_and_digest ~genv ~should_print_summary ~info ~f ~reporter options roots =
    let%lwt (_, results) = run ~genv ~should_print_summary ~info ~f options roots in
    Lwt.return (digest ~reporter results)
end

let untyped_runner_job ~mk_ccx ~f ~abstract_reader file_list =
  List.fold_left
    (fun results file ->
      let file_sig =
        Parsing_heaps.Reader_dispatcher.get_file_sig_unsafe ~reader:abstract_reader file
      in
      let ast = Parsing_heaps.Reader_dispatcher.get_ast_unsafe ~reader:abstract_reader file in
      FilenameMap.add file (Some (f ast (mk_ccx file file_sig))) results)
    FilenameMap.empty
    file_list

let untyped_digest ~reporter results =
  FilenameMap.fold
    (fun file_key r acc ->
      match r with
      | None -> acc
      | Some result ->
        let (acc_files, acc_result) = acc in
        (file_key :: acc_files, reporter.Codemod_report.combine result acc_result))
    results
    ([], reporter.Codemod_report.empty)

(* UntypedRunner - This runner just parses the specified roots, this means no addition
 * info other than what is passed to the job is available but means this runner is fast. *)
module UntypedRunner = struct
  let run ~genv ~should_print_summary ~f options roots =
    let workers = genv.ServerEnv.workers in
    Profiling_js.with_profiling_lwt ~label:"Codemod" ~should_print_summary (fun _profiling ->
        let file_options = Options.file_options options in
        let all = Options.all options in
        let (_ordered_libs, libs) = Files.init file_options in

        (* creates a closure that lists all files in the given root, returned in chunks *)
        let filename_set = get_target_filename_set ~options:file_options ~libs ~all roots in
        let next = Parsing_service_js.next_of_filename_set workers filename_set in

        Transaction.with_transaction (fun transaction ->
            let reader = Mutator_state_reader.create transaction in
            let%lwt {
                  Parsing_service_js.parse_ok;
                  parse_skips = _;
                  parse_hash_mismatch_skips = _;
                  parse_fails = _;
                  parse_unchanged = _;
                } =
              Parsing_service_js.parse_with_defaults ~reader options workers next
            in
            let roots = FilenameMap.keys parse_ok |> FilenameSet.of_list in
            log_input_files roots;
            let next = Parsing_service_js.next_of_filename_set workers roots in
            let mk_ccx file file_sig = { Codemod_context.Untyped.file; file_sig } in
            let abstract_reader = Abstract_state_reader.Mutator_state_reader reader in
            MultiWorkerLwt.call
              workers
              ~job:(fun _c file_key -> untyped_runner_job ~mk_ccx ~f ~abstract_reader file_key)
              ~neutral:FilenameMap.empty
              ~merge:FilenameMap.union
              ~next))

  let run_and_digest ~genv ~should_print_summary ~info:_ ~f ~reporter options roots =
    let%lwt (_, results) = run ~genv ~should_print_summary ~f options roots in
    Lwt.return (untyped_digest ~reporter results)
end

(* UntypedFlowInitRunner - This runner calls `Types_js.init` which results in the full
 * codebase being parsed as well as resolving requires and calculating the dependency
 * graph. This info is available inside each job via the different Heap API's. This
 * runner is useful if you need access to do transforms that require context outside
 * of a single file e.g. looking up the exports of a module your file is importing.
 * For large codebases this runner can be slow. *)
module UntypedFlowInitRunner = struct
  type init = reader:State_reader.t -> unit

  let unit_init ~reader:_ = ()

  let run ~genv ~should_print_summary ~init ~f options roots =
    let workers = genv.ServerEnv.workers in
    Profiling_js.with_profiling_lwt ~label:"Codemod" ~should_print_summary (fun profiling ->
        let%lwt (_libs_ok, env, _recheck_stats) = Types_js.init ~profiling ~workers options in

        let file_options = Options.file_options options in
        let all = Options.all options in
        let libs = env.ServerEnv.libs in

        (* creates a closure that lists all files in the given root, returned in chunks *)
        let filename_set = get_target_filename_set ~options:file_options ~libs ~all roots in
        (* Discard uparseable files *)
        let filename_set = FilenameSet.inter filename_set env.ServerEnv.files in
        let next = Parsing_service_js.next_of_filename_set workers filename_set in

        let reader = State_reader.create () in
        init ~reader;
        let mk_ccx file file_sig = { Codemod_context.UntypedFlowInit.file; reader; file_sig } in
        let abstract_reader = Abstract_state_reader.State_reader reader in
        MultiWorkerLwt.call
          workers
          ~job:(fun _c file_key -> untyped_runner_job ~f ~mk_ccx ~abstract_reader file_key)
          ~neutral:FilenameMap.empty
          ~merge:FilenameMap.union
          ~next)

  let run_and_digest ~genv ~should_print_summary ~info:_ ~init ~f ~reporter options roots =
    let%lwt (_, results) = run ~genv ~should_print_summary ~init ~f options roots in
    Lwt.return (untyped_digest ~reporter results)
end

type ('a, 'ctx) abstract_visitor = (Loc.t, Loc.t) Flow_ast.program -> 'ctx -> 'a

type 'a visitor =
  | Typed_visitor of ('a, Codemod_context.Typed.t) abstract_visitor
  | UntypedFlowInitRunner_visitor of
      (* Runner init function which is called after Types_js.init but before any of the
       * jobs. This is useful to setup/populate any shared memory for the jobs. *)
      (UntypedFlowInitRunner.init * ('a, Codemod_context.UntypedFlowInit.t) abstract_visitor)
  | Untyped_visitor of ('a, Codemod_context.Untyped.t) abstract_visitor

let run_and_digest ~genv ~should_print_summary ~info ~visitor ~reporter options roots =
  let run =
    match visitor with
    | Typed_visitor f -> TypedRunner.run_and_digest ~f
    | UntypedFlowInitRunner_visitor (init, f) -> UntypedFlowInitRunner.run_and_digest ~init ~f
    | Untyped_visitor f -> UntypedRunner.run_and_digest ~f
  in
  run ~genv ~should_print_summary ~info ~reporter options roots
