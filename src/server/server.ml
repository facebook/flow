(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let sample_init_memory profiling =
  let open SharedMem_js in
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
  List.iter (fun (metric, value) ->
    Profiling_js.legacy_sample_memory
      ~metric:("init_done." ^ metric)
      ~value:(float_of_int value)
       profiling
  ) memory_metrics

let init ~profiling ?focus_targets genv =
  (* write binary path and version to server log *)
  Hh_logger.info "executable=%s" (Sys_utils.executable_path ());
  Hh_logger.info "version=%s" Flow_version.version;

  let workers = genv.ServerEnv.workers in
  let options = genv.ServerEnv.options in

  MultiWorkerLwt.set_report_canceled_callback (fun ~total ~finished ->
    Hh_logger.info "Canceling progress %d/%d" finished total;
    MonitorRPC.status_update
      ~event:ServerStatus.(Canceling_progress { total = Some total; finished; });
  );

  MonitorRPC.status_update ~event:ServerStatus.Init_start;

  let%lwt libs_ok, env, last_estimates = Types_js.init ~profiling ~workers options in

  (* If any libs errored, skip typechecking and just show lib errors. Note
   * that `init` above has done all parsing, not just lib parsing, resolved
   * and committed modules, etc.
   *
   * Furthermore, if we're in lazy mode, we forego typechecking until later,
   * when it proceeds on an as-needed basis. *)
  let%lwt env =
    if not libs_ok || Options.is_lazy_mode options
    then Lwt.return env
    else Types_js.full_check ~profiling ~workers ?focus_targets ~options env
  in

  sample_init_memory profiling;

  SharedMem_js.init_done();

  (* Return an env that initializes invariants required and maintained by
     recheck, namely that `files` contains files that parsed successfully, and
     `errors` contains the current set of errors. *)
  Lwt.return (env, last_estimates)

let rec serve ~genv ~env =
  Hh_logger.debug "Starting aggressive shared mem GC";
  SharedMem_js.collect `aggressive;
  Hh_logger.debug "Finished aggressive shared mem GC";

  MonitorRPC.status_update ~event:ServerStatus.Ready;

  (* Ok, server is settled. Let's go to sleep until we get a message from the monitor *)
  let%lwt () = ServerMonitorListenerState.wait_for_anything
    ~process_updates:(Rechecker.process_updates genv env)
    ~get_forced:(fun () -> env.ServerEnv.checked_files) (* We're not in the middle of a recheck *)
  in

  (* If there's anything to recheck or updates to the env from the monitor, let's consume them *)
  let%lwt _profiling, env = Rechecker.recheck_loop genv env in

  (* Run a workload (if there is one) *)
  let%lwt env = Option.value_map (ServerMonitorListenerState.pop_next_workload ())
    ~default:(Lwt.return env)
    ~f:(fun workload ->
      Hh_logger.info "Running a serial workload";
      workload env
    ) in

  (* Flush the logs asynchronously *)
  Lwt.async EventLoggerLwt.flush;

  serve ~genv ~env

(* The main entry point of the daemon
* the only trick to understand here, is that env.modified is the set
* of files that changed, it is only set back to SSet.empty when the
* type-checker succeeded. So to know if there is some work to be done,
* we look if env.modified changed.
*)
let create_program_init ~shared_mem_config ?focus_targets options =
  let num_workers = Options.max_workers options in
  let handle = SharedMem_js.init ~num_workers shared_mem_config in
  let genv = ServerEnvBuild.make_genv options handle in

  let program_init = fun profiling ->
    let%lwt env = init ~profiling ?focus_targets genv in
    if shared_mem_config.SharedMem_js.log_level > 0 then Measure.print_stats ();
    Lwt.return env
  in
  genv, program_init

let run ~monitor_channels ~shared_mem_config options =
  MonitorRPC.init ~channels:monitor_channels;
  let genv, program_init =
    create_program_init ~shared_mem_config options in

  let initial_lwt_thread () =
    (* Read messages from the server monitor and add them to a stream as they come in *)
    let listening_thread = ServerMonitorListener.listen_for_messages genv in

    (* Initialize *)
    let%lwt env =
      let t = Unix.gettimeofday () in
      Hh_logger.info "Initializing Server (This might take some time)";

      let should_print_summary = Options.should_profile options in
      let%lwt profiling, (env, last_estimates) = Profiling_js.with_profiling_lwt program_init
        ~label:"Init" ~should_print_summary
      in

      let event = ServerStatus.(Finishing_up {
        duration = Profiling_js.get_profiling_duration profiling;
        info = InitSummary}) in
      MonitorRPC.status_update ~event;

      begin match last_estimates with
      | None ->
        FlowEventLogger.init_done profiling
      | Some { Recheck_stats.
          estimated_time_to_recheck;
          estimated_time_to_restart;
          estimated_time_to_init;
          estimated_time_to_merge_a_file;
          estimated_files_to_merge;
          estimated_files_to_init;
        } ->
          FlowEventLogger.init_done
            ~estimated_time_to_recheck
            ~estimated_time_to_restart
            ~estimated_time_to_init
            ~estimated_time_to_merge_a_file
            ~estimated_files_to_merge
            ~estimated_files_to_init
            profiling
      end;

      Hh_logger.info "Server is READY";

      let t' = Unix.gettimeofday () in
      Hh_logger.info "Took %f seconds to initialize." (t' -. t);

      Lwt.return env
    in

    (* Run both these threads. If either of them fail, return immediately *)
    LwtUtils.iter_all [
      listening_thread;
      serve ~genv ~env
    ]
  in
  LwtInit.run_lwt initial_lwt_thread

let run_from_daemonize ~monitor_channels ~shared_mem_config options =
  try run ~monitor_channels ~shared_mem_config options
  with
  | SharedMem_js.Out_of_shared_memory as exn ->
    let exn = Exception.wrap exn in
    let bt = Exception.get_backtrace_string exn in
    let msg = Utils.spf "Out of shared memory%s" (if bt = "" then bt else ":\n"^bt) in
    FlowExitStatus.(exit ~msg Out_of_shared_memory)
  | e ->
    let e = Exception.wrap e in
    let msg = Utils.spf "Unhandled exception: %s" (Exception.to_string e) in
    FlowExitStatus.(exit ~msg Unknown_error)

let check_once ~shared_mem_config ~format_errors ?focus_targets options =
  PidLog.disable ();
  MonitorRPC.disable ();

  LoggingUtils.set_server_options ~server_options:options;

  let initial_lwt_thread () =
    let _, program_init =
      create_program_init ~shared_mem_config ?focus_targets options in

    let should_print_summary = Options.should_profile options in
    let%lwt profiling, (print_errors, errors, warnings) =
      Profiling_js.with_profiling_lwt ~label:"Init" ~should_print_summary (fun profiling ->
        let%lwt env, _ = program_init profiling in
        let%lwt (errors, warnings, suppressed_errors) =
          Profiling_js.with_timer_lwt ~timer:"CollateErrors" profiling
            ~f:(fun () -> Lwt.return (ErrorCollator.get ~options env))
        in
        let collated_errors = (errors, warnings, suppressed_errors) in
        let%lwt print_errors =
          Profiling_js.with_timer_lwt ~timer:"FormatErrors" profiling
            ~f:(fun () -> Lwt.return (format_errors collated_errors))
        in
        Lwt.return (print_errors, errors, warnings)
      )
    in

    print_errors profiling;

    let event = ServerStatus.(Finishing_up {
      duration = Profiling_js.get_profiling_duration profiling;
      info = InitSummary}) in
    MonitorRPC.status_update ~event;

    FlowEventLogger.init_done profiling;

    Lwt.return (errors, warnings)
  in
  LwtInit.run_lwt initial_lwt_thread

let daemonize =
  let entry = Server_daemon.register_entry_point run_from_daemonize in
  fun ~log_file ~shared_mem_config ~argv ~file_watcher_pid options ->
    Server_daemon.daemonize ~log_file ~shared_mem_config ~argv ~options
      ~file_watcher_pid entry
