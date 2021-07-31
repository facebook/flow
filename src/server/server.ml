(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let sample_init_memory profiling =
  SharedMem.(
    let hash_stats = hash_stats () in
    let heap_size = heap_size () in
    let memory_metrics =
      [
        ("heap.size", heap_size);
        ("hash_table.nonempty_slots", hash_stats.nonempty_slots);
        ("hash_table.used_slots", hash_stats.used_slots);
        ("hash_table.slots", hash_stats.slots);
      ]
    in
    List.iter
      (fun (metric, value) ->
        Profiling_js.legacy_sample_memory
          ~metric:("init_done." ^ metric)
          ~value:(float_of_int value)
          profiling)
      memory_metrics)

let extract_flowlibs_or_exit options =
  match Files.default_lib_dir (Options.file_options options) with
  | Some libdir ->
    let libdir =
      match libdir with
      | Files.Prelude path -> Flowlib.Prelude path
      | Files.Flowlib path -> Flowlib.Flowlib path
    in
    (try Flowlib.extract libdir with
    | e ->
      let e = Exception.wrap e in
      let err = Exception.get_ctor_string e in
      let libdir_str = libdir |> Flowlib.path_of_libdir |> Path.to_string in
      let msg = Printf.sprintf "Could not extract flowlib files into %s: %s" libdir_str err in
      Exit.(exit ~msg Could_not_extract_flowlibs))
  | None -> ()

let init ~profiling ?focus_targets genv =
  (* write binary path and version to server log *)
  Hh_logger.info "executable=%s" (Sys_utils.executable_path ());
  Hh_logger.info "version=%s" Flow_version.version;

  let workers = genv.ServerEnv.workers in
  let options = genv.ServerEnv.options in
  MultiWorkerLwt.set_report_canceled_callback (fun ~total ~finished ->
      Hh_logger.info "Canceling progress %d/%d" finished total;
      MonitorRPC.status_update
        ~event:ServerStatus.(Canceling_progress { total = Some total; finished }));

  MonitorRPC.status_update ~event:ServerStatus.Init_start;

  extract_flowlibs_or_exit options;

  let%lwt (libs_ok, env, last_estimates) = Types_js.init ~profiling ~workers options in
  (* If any libs errored, skip typechecking and just show lib errors. Note
   * that `init` above has done all parsing, not just lib parsing, resolved
   * and committed modules, etc.
   *
   * Furthermore, if we're in lazy mode, we forego typechecking until later,
   * when it proceeds on an as-needed basis. *)
  let%lwt (env, first_internal_error) =
    if (not libs_ok) || Options.is_lazy_mode options then
      Lwt.return (env, None)
    else
      Types_js.full_check ~profiling ~workers ?focus_targets ~options env
  in
  sample_init_memory profiling;

  SharedMem.init_done ();

  (* Return an env that initializes invariants required and maintained by
     recheck, namely that `files` contains files that parsed successfully, and
     `errors` contains the current set of errors. *)
  Lwt.return (env, last_estimates, first_internal_error)

(* A thread that samples memory stats every second and then logs an idle heartbeat event even
 * `idle_period_in_seconds` seconds. *)
let rec idle_logging_loop =
  (* The time in seconds to gather data before logging. Shouldn't be too small or we'll flood the
   * logs. *)
  let idle_period_in_seconds = 300. in
  (* Grab memory stats. Since we're idle, we don't really care much about sharedmemory stats. But
   * our cgroup stats may change depending on the memory pressure *)
  let sample profiling =
    let%lwt cgroup_stats = CGroup.get_stats () in
    begin
      match cgroup_stats with
      | Error _ -> ()
      | Ok { CGroup.total; total_swap; anon; file; shmem } ->
        Profiling_js.sample_memory profiling ~metric:"cgroup_total" ~value:(float total);
        Profiling_js.sample_memory profiling ~metric:"cgroup_swap" ~value:(float total_swap);
        Profiling_js.sample_memory profiling ~metric:"cgroup_anon" ~value:(float anon);
        Profiling_js.sample_memory profiling ~metric:"cgroup_shmem" ~value:(float shmem);
        Profiling_js.sample_memory profiling ~metric:"cgroup_file" ~value:(float file)
    end;
    Lwt.return_unit
  in
  (* Sample every second *)
  let rec sample_loop profiling =
    let%lwt () = Lwt.join [sample profiling; Lwt_unix.sleep 1.0] in
    sample_loop profiling
  in
  fun ~options start_time ->
    let should_print_summary = Options.should_profile options in
    let%lwt (profiling, ()) =
      Profiling_js.with_profiling_lwt ~label:"Idle" ~should_print_summary (fun profiling ->
          let sampler_thread = sample_loop profiling in
          let timeout = Lwt_unix.sleep idle_period_in_seconds in
          Lwt.pick [sampler_thread; timeout])
    in
    FlowEventLogger.idle_heartbeat ~idle_time:(Unix.gettimeofday () -. start_time) ~profiling;
    Lwt.async EventLoggerLwt.flush;
    idle_logging_loop ~options start_time

(* A thread which performs tiny incremental GC slices until it is canceled or
 * finishes a full collection cycle. Each call to collect_slice should only
 * block for a few milliseconds. *)
let rec gc_loop () =
  let%lwt () = Lwt.pause () in
  if SharedMem.collect_slice 10000 then
    Lwt.return_unit
  else
    gc_loop ()

let rec serve ~genv ~env =
  MonitorRPC.status_update ~event:ServerStatus.Ready;

  let options = genv.ServerEnv.options in

  (* Kick off the idle thread. This will loop forever, because the idle logging
   * thread loops forever. *)
  let idle_thread =
    let start_time = Unix.gettimeofday () in
    let logging_thread = idle_logging_loop ~options start_time in
    let gc_thread = gc_loop () in
    LwtUtils.iter_all [logging_thread; gc_thread]
  in

  (* Kick off a thread to wait for a message from the monitor. *)
  let wait_thread =
    ServerMonitorListenerState.wait_for_anything
      ~process_updates:(fun ?skip_incompatible ->
        Rechecker.process_updates ?skip_incompatible ~options env)
      ~get_forced:(fun () -> env.ServerEnv.checked_files)
  in

  (* Run the idle and wait threads together until we get a message from the
   * monitor. This will complete the wait thread and cause the idle thread to be
   * canceled. *)
  let%lwt () = Lwt.pick [idle_thread; wait_thread] in

  (* If there's anything to recheck or updates to the env from the monitor, let's consume them *)
  let%lwt (_profiling, env) = Rechecker.recheck_loop genv env in
  (* Run a workload (if there is one) *)
  let%lwt env =
    Base.Option.value_map
      (ServerMonitorListenerState.pop_next_workload ())
      ~default:(Lwt.return env)
      ~f:(fun workload ->
        Hh_logger.info "Running a serial workload";
        workload env)
  in
  (* Flush the logs asynchronously *)
  Lwt.async EventLoggerLwt.flush;

  serve ~genv ~env

let on_compact () =
  MonitorRPC.status_update ~event:ServerStatus.GC_start;
  let old_size = SharedMem.heap_size () in
  let start_t = Unix.gettimeofday () in
  fun () ->
    let new_size = SharedMem.heap_size () in
    let time_taken = Unix.gettimeofday () -. start_t in
    if old_size <> new_size then (
      Hh_logger.log
        "Sharedmem GC: %d bytes before; %d bytes after; in %f seconds"
        old_size
        new_size
        time_taken;
      EventLogger.sharedmem_gc_ran `aggressive old_size new_size time_taken
    )

(* The main entry point of the daemon
 * the only trick to understand here, is that env.modified is the set
 * of files that changed, it is only set back to SSet.empty when the
 * type-checker succeeded. So to know if there is some work to be done,
 * we look if env.modified changed.
 *)
let create_program_init ~shared_mem_config ~init_id ?focus_targets options =
  SharedMem.on_compact := on_compact;
  let num_workers = Options.max_workers options in
  let handle = SharedMem.init ~num_workers shared_mem_config in
  let genv = ServerEnvBuild.make_genv ~options ~init_id handle in
  let program_init profiling =
    let%lwt ret = init ~profiling ?focus_targets genv in
    if shared_mem_config.SharedMem.log_level > 0 then Measure.print_stats ();
    Lwt.return ret
  in
  (genv, program_init)

let run ~monitor_channels ~init_id ~shared_mem_config options =
  MonitorRPC.init ~channels:monitor_channels;
  let (genv, program_init) = create_program_init ~init_id ~shared_mem_config options in
  let initial_lwt_thread () =
    (* Read messages from the server monitor and add them to a stream as they come in *)
    let listening_thread = ServerMonitorListener.listen_for_messages genv in
    (* Initialize *)
    let%lwt env =
      let t = Unix.gettimeofday () in
      Hh_logger.info "Initializing Server (This might take some time)";

      let should_print_summary = Options.should_profile options in
      let%lwt (profiling, (env, last_estimates, first_internal_error)) =
        Profiling_js.with_profiling_lwt program_init ~label:"Init" ~should_print_summary
      in
      let event =
        ServerStatus.(
          Finishing_up
            { duration = Profiling_js.get_profiling_duration profiling; info = InitSummary })
      in
      MonitorRPC.status_update ~event;

      begin
        match last_estimates with
        | None -> FlowEventLogger.init_done ?first_internal_error profiling
        | Some
            {
              Recheck_stats.estimated_time_to_recheck;
              estimated_time_to_restart;
              estimated_time_to_init;
              estimated_time_per_file;
              estimated_files_to_recheck;
              estimated_files_to_init;
            } ->
          FlowEventLogger.init_done
            ~estimated_time_to_recheck
            ~estimated_time_to_restart
            ~estimated_time_to_init
            ~estimated_time_per_file
            ~estimated_files_to_recheck
            ~estimated_files_to_init
            ?first_internal_error
            profiling
      end;

      Hh_logger.info "Server is READY";

      let t' = Unix.gettimeofday () in
      Hh_logger.info "Took %f seconds to initialize." (t' -. t);

      Lwt.return env
    in
    (* Run both these threads. If either of them fail, return immediately *)
    LwtUtils.iter_all [listening_thread; serve ~genv ~env]
  in
  LwtInit.run_lwt initial_lwt_thread

let exit_msg_of_exception exn msg =
  let bt = Exception.get_full_backtrace_string max_int exn in
  Utils.spf
    "%s%s"
    msg
    (if bt = "" then
      bt
    else
      ":\n" ^ bt)

let run_from_daemonize ~init_id ~monitor_channels ~shared_mem_config options =
  try run ~monitor_channels ~shared_mem_config ~init_id options with
  | SharedMem.Out_of_shared_memory as exn ->
    let exn = Exception.wrap exn in
    let msg = exit_msg_of_exception exn "Out of shared memory" in
    Exit.(exit ~msg Out_of_shared_memory)
  | SharedMem.Hash_table_full as exn ->
    let exn = Exception.wrap exn in
    let msg = exit_msg_of_exception exn "Hash table is full" in
    Exit.(exit ~msg Hash_table_full)
  | SharedMem.Heap_full as exn ->
    let exn = Exception.wrap exn in
    let msg = exit_msg_of_exception exn "Heap is full" in
    Exit.(exit ~msg Heap_full)
  | MonitorRPC.Monitor_died -> Exit.(exit ~msg:"Monitor died unexpectedly" Killed_by_monitor)
  | e ->
    let e = Exception.wrap e in
    let msg = Utils.spf "Unhandled exception: %s" (Exception.to_string e) in
    Exit.(exit ~msg Unknown_error)

let check_once ~init_id ~shared_mem_config ~format_errors ?focus_targets options =
  PidLog.disable ();
  MonitorRPC.disable ();

  LoggingUtils.set_server_options ~server_options:options;

  let should_log_server_profiles = Options.should_profile options && not Sys.win32 in
  (* This must happen before we create the workers in create_program_init *)
  if should_log_server_profiles then Flow_server_profile.init ();

  let initial_lwt_thread () =
    let (_, program_init) =
      create_program_init ~shared_mem_config ~init_id ?focus_targets options
    in
    let should_print_summary = Options.should_profile options in
    let%lwt (profiling, (print_errors, errors, warnings, first_internal_error)) =
      Profiling_js.with_profiling_lwt ~label:"Init" ~should_print_summary (fun profiling ->
          (if should_log_server_profiles then
            let rec sample_processor_info () =
              Flow_server_profile.processor_sample ();
              let%lwt () = Lwt_unix.sleep 1.0 in
              sample_processor_info ()
            in
            Lwt.async sample_processor_info);
          let%lwt (env, _, first_internal_error) = program_init profiling in
          let reader = State_reader.create () in
          let (errors, warnings, suppressed_errors) =
            ErrorCollator.get ~profiling ~reader ~options env
          in
          let collated_errors = (errors, warnings, suppressed_errors) in
          let%lwt print_errors =
            Profiling_js.with_timer_lwt ~timer:"FormatErrors" profiling ~f:(fun () ->
                Lwt.return (format_errors collated_errors))
          in
          Lwt.return (print_errors, errors, warnings, first_internal_error))
    in
    print_errors profiling;

    let event =
      ServerStatus.(
        Finishing_up
          { duration = Profiling_js.get_profiling_duration profiling; info = InitSummary })
    in
    MonitorRPC.status_update ~event;

    FlowEventLogger.init_done ?first_internal_error profiling;

    Lwt.return (errors, warnings)
  in
  LwtInit.run_lwt initial_lwt_thread

let daemonize =
  let entry = Server_daemon.register_entry_point run_from_daemonize in
  fun ~init_id ~log_file ~shared_mem_config ~argv ~file_watcher_pid options ->
    Server_daemon.daemonize
      ~init_id
      ~log_file
      ~shared_mem_config
      ~argv
      ~options
      ~file_watcher_pid
      entry
