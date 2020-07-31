(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

let log_input_files fileset =
  Hh_logger.debug
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
let get_target_filename_set ~options ~libs ~all filename_set =
  FilenameSet.filter
    (fun f ->
      let s = File_key.to_string f in
      Files.is_valid_path ~options s
      && (all || not (Files.is_ignored options s))
      && not (SSet.mem s libs))
    filename_set

type 'a unit_result = ('a, ALoc.t * Error_message.internal_error) result

type ('a, 'ctx) abstract_visitor = (Loc.t, Loc.t) Flow_ast.Program.t -> 'ctx -> 'a

(* A runner needs to specify two main things, in order to be used in repeat mode:
 *
 * (i) An initializing routine for the first time the codemod is applied.
 *
 * (ii) A "re-checking" routine for every successive time the codemod is run.
 *
 * The difference between the two is that (i) produces an initial "environment"
 * (in the case of typed codemods this is a ServerEnv.env), whereas (ii) takes an
 * env as argument and returns an output env.
 *)
module type StepRunnerT = sig
  type env

  type 'a visitor

  val init_run :
    ServerEnv.genv ->
    visit:'a visitor ->
    FilenameSet.t ->
    (Profiling_js.finished * (env * 'a unit_result Utils_js.FilenameMap.t)) Lwt.t

  val recheck_run :
    ServerEnv.genv ->
    env ->
    iteration:int ->
    visit:'a visitor ->
    FilenameSet.t ->
    (Profiling_js.finished * (env * 'a unit_result Utils_js.FilenameMap.t)) Lwt.t

  val digest :
    reporter:'a Codemod_report.t ->
    'a unit_result Utils_js.FilenameMap.t ->
    Utils_js.FilenameMap.key list * 'a
end

(* TypedRunner - This runner does a full local typecheck and makes available the
 * typed AST as well as the full CX to each job. This runner is needed for any type
 * based transforms. It runs a full local typecheck so can be slow on large codebases. *)
module TypedRunner :
  StepRunnerT with type 'a visitor = ('a, Codemod_context.Typed.t) abstract_visitor = struct
  type env = ServerEnv.env

  type 'a visitor = (Loc.t, Loc.t) Flow_ast.Program.t -> Codemod_context.Typed.t -> 'a

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
  let merge_job ~visit ~roots ~iteration ~worker_mutator ~options ~reader component =
    let leader = Nel.hd component in
    let reader = Abstract_state_reader.Mutator_state_reader reader in
    let result =
      if Module_js.checked_file ~reader ~audit:Expensive.ok leader then (
        let (cx, master_cx, check_opt) =
          let open Merge_service in
          match merge_context ~options ~reader component with
          | MergeResult { cx; master_cx } -> (cx, master_cx, None)
          | CheckResult { cx; master_cx; file_sigs; typed_asts; _ } ->
            (cx, master_cx, Some (file_sigs, typed_asts))
        in
        let full_cx = Context.copy_of_context cx in
        let module_refs = List.rev_map Files.module_ref (Nel.to_list component) in
        let md5 = Merge_js.ContextOptimizer.sig_context cx module_refs in
        Context.clear_master_shared cx master_cx;
        Context_heaps.Merge_context_mutator.add_merge_on_diff
          ~audit:Expensive.ok
          worker_mutator
          cx
          component
          md5;
        let metadata = Context.metadata_of_options options in
        match check_opt with
        | None -> FilenameMap.empty
        | Some (file_sigs, typed_asts) ->
          Nel.fold_left
            (fun acc file ->
              (* Merge will have potentially merged more that the target files (roots).
                 To avoid processing all those files, we pick the ones in the roots set. *)
              if FilenameSet.mem file roots then
                let file_sig = FilenameMap.find file file_sigs in
                let typed_ast = FilenameMap.find file typed_asts in
                let ast = Parsing_heaps.Reader_dispatcher.get_ast_unsafe ~reader file in
                let docblock = Parsing_heaps.Reader_dispatcher.get_docblock_unsafe ~reader file in
                let ccx =
                  {
                    Codemod_context.Typed.file;
                    file_sig;
                    metadata;
                    options;
                    full_cx;
                    typed_ast;
                    docblock;
                    iteration;
                  }
                in
                let result = visit ast ccx in
                FilenameMap.add file (Ok result) acc
              else
                acc)
            FilenameMap.empty
            component
      ) else
        FilenameMap.empty
    in
    Ok result

  (* The processing step in Types-First needs to happen right after the check phase.
     We have already merged any necessary dependencies, so now we only check the
     target files for processing. *)
  let check_job ~visit ~iteration ~reader ~options acc roots =
    let metadata = Context.metadata_of_options options in
    List.fold_left
      (fun acc file ->
        match Merge_service.check options ~reader file with
        | (file, Ok (Some (full_cx, file_sigs, typed_asts), _)) ->
          let file_sig = FilenameMap.find file file_sigs in
          let typed_ast = FilenameMap.find file typed_asts in
          let reader = Abstract_state_reader.Mutator_state_reader reader in
          let ast = Parsing_heaps.Reader_dispatcher.get_ast_unsafe ~reader file in
          let docblock = Parsing_heaps.Reader_dispatcher.get_docblock_unsafe ~reader file in
          let ccx =
            {
              Codemod_context.Typed.file;
              file_sig;
              metadata;
              options;
              full_cx;
              typed_ast;
              docblock;
              iteration;
            }
          in
          let result = visit ast ccx in
          FilenameMap.add file (Ok result) acc
        | (_, Ok (None, _)) -> acc
        | (_, Error e) -> FilenameMap.add file (Error e) acc)
      acc
      roots

  let merge_and_check env workers options profiling roots ~visit ~iteration =
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
            ~job:(merge_job ~visit ~roots ~iteration)
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
              ~job:(check_job ~visit ~iteration ~reader ~options)
              ~neutral:FilenameMap.empty
              ~merge:FilenameMap.union
              ~next:(MultiWorkerLwt.next workers (FilenameSet.elements roots))
          in
          Hh_logger.info "Done";
          Lwt.return result)

  let init_run genv ~visit roots =
    let { ServerEnv.options; workers } = genv in
    let should_print_summary = Options.should_profile options in
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
        let%lwt results = merge_and_check env workers options profiling roots ~visit ~iteration:0 in
        Lwt.return (env, results))

  (* The roots that are passed in here have already been filtered by earlier iterations. *)
  let recheck_run genv env ~iteration ~visit roots =
    let { ServerEnv.workers; options } = genv in
    let should_print_summary = Options.should_profile options in
    Profiling_js.with_profiling_lwt ~label:"Codemod" ~should_print_summary (fun profiling ->
        (* Diff heaps are not cleared like the rest of the heaps during recheck
           so we clear them here, before moving further into recheck. *)
        Diff_heaps.remove_batch roots;
        let%lwt (_, _, env) =
          Types_js.recheck
            ~profiling
            ~options
            ~workers
            ~updates:roots
            env
            ~files_to_force:CheckedSet.empty
            ~file_watcher_metadata:MonitorProt.empty_file_watcher_metadata
            ~recheck_reasons:[]
            ~will_be_checked_files:(ref CheckedSet.empty)
        in
        log_input_files roots;
        let%lwt results = merge_and_check env workers options profiling roots ~visit ~iteration in
        Lwt.return (env, results))

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
end

let untyped_runner_job ~mk_ccx ~visit ~abstract_reader file_list =
  List.fold_left
    (fun results file ->
      let file_sig =
        Parsing_heaps.Reader_dispatcher.get_file_sig_unsafe ~reader:abstract_reader file
      in
      let ast = Parsing_heaps.Reader_dispatcher.get_ast_unsafe ~reader:abstract_reader file in
      FilenameMap.add file (Ok (visit ast (mk_ccx file file_sig))) results)
    FilenameMap.empty
    file_list

let untyped_digest ~reporter results =
  FilenameMap.fold
    (fun file_key r acc ->
      match r with
      | Error _ -> acc
      | Ok result ->
        let (acc_files, acc_result) = acc in
        (file_key :: acc_files, reporter.Codemod_report.combine result acc_result))
    results
    ([], reporter.Codemod_report.empty)

(* UntypedRunner - This runner just parses the specified roots, this means no addition
 * info other than what is passed to the job is available but means this runner is fast. *)
module UntypedRunner :
  StepRunnerT with type 'a visitor = ('a, Codemod_context.Untyped.t) abstract_visitor = struct
  type env = unit

  type 'a visitor = (Loc.t, Loc.t) Flow_ast.Program.t -> Codemod_context.Untyped.t -> 'a

  let run genv ~visit roots =
    let { ServerEnv.workers; options } = genv in
    let should_print_summary = Options.should_profile options in
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
            let%lwt result =
              MultiWorkerLwt.call
                workers
                ~job:(fun _c file_key ->
                  untyped_runner_job ~mk_ccx ~visit ~abstract_reader file_key)
                ~neutral:FilenameMap.empty
                ~merge:FilenameMap.union
                ~next
            in
            Lwt.return ((), result)))

  let digest = untyped_digest

  let init_run genv ~visit roots = run genv ~visit roots

  let recheck_run genv _env ~iteration:_ ~visit roots = run genv ~visit roots
end

(* UntypedFlowInitRunner - This runner calls `Types_js.init` which results in the full
 * codebase being parsed as well as resolving requires and calculating the dependency
 * graph. This info is available inside each job via the different Heap API's. This
 * runner is useful if you need access to do transforms that require context outside
 * of a single file e.g. looking up the exports of a module your file is importing.
 * For large codebases this runner can be slow.
 *)
type 'a untyped_flow_init_runner_visitor = {
  (* Runner init function which is called after Types_js.init but before any of the
   * jobs. This is useful to setup/populate any shared memory for the jobs. *)
  init: reader:State_reader.t -> unit;
  visit: ('a, Codemod_context.UntypedFlowInit.t) abstract_visitor;
}

module UntypedFlowInitRunner :
  StepRunnerT with type 'a visitor = 'a untyped_flow_init_runner_visitor = struct
  type env = unit

  type 'a visitor = 'a untyped_flow_init_runner_visitor

  let run genv ~visit roots =
    let { visit; init } = visit in
    let { ServerEnv.workers; options } = genv in
    let should_print_summary = Options.should_profile options in
    Profiling_js.with_profiling_lwt ~label:"Codemod" ~should_print_summary (fun profiling ->
        let options =
          {
            options with
            Options.opt_saved_state_fetcher = Options.Dummy_fetcher;
            (* Parse all files (even non @flow files) *)
            opt_all = true;
          }
        in
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
        log_input_files filename_set;
        let%lwt result =
          MultiWorkerLwt.call
            workers
            ~job:(fun _c file_key -> untyped_runner_job ~visit ~mk_ccx ~abstract_reader file_key)
            ~neutral:FilenameMap.empty
            ~merge:FilenameMap.union
            ~next
        in
        Lwt.return ((), result))

  let digest = untyped_digest

  let init_run genv ~visit roots = run genv ~visit roots

  let recheck_run genv _env ~iteration:_ ~visit roots = run genv ~visit roots
end

(* When the '--repeat' flag is passed, then the entire codemod will be run until
 * the targeted files stabilize. This function will run the codemod configuration
 * described by StepRunner multiple times until we reach this fixpoint. StepRunner
 * can be a typed or untyped codemod configuration.
 *)
module RepeatRunner (StepRunner : StepRunnerT) : sig
  val run :
    genv:ServerEnv.genv ->
    write:bool ->
    repeat:bool ->
    reporter:'a Codemod_report.t ->
    visit:'a StepRunner.visitor ->
    Utils_js.FilenameSet.t ->
    unit Lwt.t
end = struct
  let max_number_of_iterations = 10

  let header iteration filenames =
    (* Use natural counting starting from 1. *)
    Utils_js.print_endlinef ">>> Iteration: %d" (iteration + 1);
    Utils_js.print_endlinef ">>> Running codemod on %d files..." (FilenameSet.cardinal filenames)

  let post_run options ~write ~reporter results =
    let strip_root =
      if Options.should_strip_root options then
        Some (Options.root options)
      else
        None
    in
    let reporter_options =
      { Codemod_report.strip_root; exact_by_default = Options.exact_by_default options }
    in
    (* post-process *)
    let (files, report) = StepRunner.digest ~reporter results in
    let%lwt changed_files = Codemod_printer.print_asts ~strip_root ~write files in
    Codemod_printer.print_results ~reporter_options ~report:reporter.Codemod_report.report report;
    Lwt.return (Base.Option.map ~f:FilenameSet.of_list changed_files)

  let rec loop_run genv env iteration ~write ~visit ~reporter changed_files =
    if iteration > max_number_of_iterations then begin
      Utils_js.print_endlinef ">>> Reached maximum number of iterations (10). Exiting...";
      Lwt.return ()
    end else
      match changed_files with
      | None -> Lwt.return ()
      | Some set when FilenameSet.is_empty set -> Lwt.return ()
      | Some roots ->
        header iteration roots;
        let%lwt (_, (env, results)) = StepRunner.recheck_run genv env ~iteration ~visit roots in
        let%lwt changed_files = post_run genv.ServerEnv.options ~write ~reporter results in
        loop_run genv env (iteration + 1) ~write ~visit ~reporter changed_files

  let run ~genv ~write ~repeat ~reporter ~visit roots =
    if repeat then header (* iteration *) 0 roots;
    let%lwt (_prof, (env, results)) = StepRunner.init_run genv ~visit roots in
    let%lwt changed_files = post_run genv.ServerEnv.options ~write ~reporter results in
    if repeat then
      loop_run genv env 1 ~write ~visit ~reporter changed_files
    else
      Lwt.return ()
end

type 'a visitor =
  | Typed_visitor of ('a, Codemod_context.Typed.t) abstract_visitor
  | UntypedFlowInitRunner_visitor of 'a untyped_flow_init_runner_visitor
  | Untyped_visitor of ('a, Codemod_context.Untyped.t) abstract_visitor

module RR_TypedRunner = RepeatRunner (TypedRunner)
module RR_UntypedRunner = RepeatRunner (UntypedRunner)
module RR_UntypedFlowInitRunner = RepeatRunner (UntypedFlowInitRunner)

let run ~genv ~write ~repeat ~visitor ~reporter roots =
  let run =
    match visitor with
    | Typed_visitor v -> RR_TypedRunner.run ~visit:v
    | UntypedFlowInitRunner_visitor v -> RR_UntypedFlowInitRunner.run ~visit:v
    | Untyped_visitor v -> RR_UntypedRunner.run ~visit:v
  in
  run ~genv ~write ~repeat ~reporter roots
