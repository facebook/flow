(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Logger = FlowServerMonitorLogger

type exit_reason =
  | Watcher_stopped
  | Watcher_died
  | Watcher_missed_changes

(* TODO - Push-based API for dfind. While the FileWatcher API is push based, dfind is faking it
 * by polling every seconds.
 *
 * If we decide we care about this, we could Build DfindServerLwt which can listen for events and
 * write messages at the same time (we don't want to block the file watching if the writes block)
 *)
class type watcher =
  object
    method name : string

    method start_init : unit

    method wait_for_init : timeout:float option -> (unit, string) result Lwt.t

    method get_and_clear_changed_files :
      (SSet.t * MonitorProt.file_watcher_metadata option * bool) Lwt.t

    method wait_for_changed_files : unit Lwt.t

    method stop : unit Lwt.t

    method waitpid : exit_reason Lwt.t

    method getpid : int option
  end

class dummy : watcher =
  object
    method name = "dummy"

    method start_init = ()

    method wait_for_init ~timeout:_ = Lwt.return (Ok ())

    method get_and_clear_changed_files = Lwt.return (SSet.empty, None, false)

    method wait_for_changed_files = Lwt.return_unit

    method stop = Lwt.return_unit

    method waitpid =
      let (wait_forever_thread, _) = Lwt.task () in
      wait_forever_thread

    method getpid = None
  end

let changes_since_mergebase =
  let fold_relative_paths acc root paths =
    Base.List.fold
      ~init:acc
      ~f:(fun acc change ->
        let path = File_path.concat root change |> File_path.to_string in
        SSet.add path acc)
      paths
  in
  let files_changed_since_mergebase_with vcs root mergebase_with =
    let root_str = File_path.to_string root in
    match vcs with
    | Vcs.Git -> Git.files_changed_since_mergebase_with ~cwd:root_str mergebase_with
    | Vcs.Hg -> Hg.files_changed_since_mergebase_with ~cwd:root_str mergebase_with
  in
  let fold_files_changed_since_mergebase_with acc vcs root mergebase_with =
    let vcs_name = Vcs.name vcs in
    match%lwt files_changed_since_mergebase_with vcs root mergebase_with with
    | Error _ ->
      Logger.error
        "Not checking changes since mergebase! %s failed to determine the initial mergebase."
        vcs_name;
      Lwt.return acc
    | Ok (mergebase, changes) ->
      Logger.info
        "%s reports the initial mergebase as %S, and %d changes"
        vcs_name
        mergebase
        (List.length changes);
      Lwt.return (fold_relative_paths acc root changes)
  in
  let rec helper ~mergebase_with (seen_roots, (acc : SSet.t)) paths =
    match paths with
    | [] -> Lwt.return acc
    | path :: paths ->
      let%lwt (seen_roots, acc) =
        match Vcs.find_root path with
        | Some (vcs, root) ->
          let root_str = File_path.to_string root in
          if SSet.mem root_str seen_roots then
            Lwt.return (seen_roots, acc)
          else
            let seen_roots = SSet.add root_str seen_roots in
            let%lwt acc = fold_files_changed_since_mergebase_with acc vcs root mergebase_with in
            Lwt.return (seen_roots, acc)
        | None -> Lwt.return (seen_roots, acc)
      in
      helper ~mergebase_with (seen_roots, acc) paths
  in
  (fun ~mergebase_with watch_paths -> helper ~mergebase_with (SSet.empty, SSet.empty) watch_paths)

class dfind (monitor_options : FlowServerMonitorOptions.t) : watcher =
  object (self)
    val mutable dfind_instance = None

    val mutable is_initial = true

    val mutable watch_paths = []

    val mutable files = SSet.empty

    method name = "dfind"

    method private get_dfind =
      match dfind_instance with
      | None -> failwith "Dfind was not initialized"
      | Some dfind -> dfind

    method start_init =
      let server_options = monitor_options.FlowServerMonitorOptions.server_options in
      let file_options = Options.file_options server_options in
      watch_paths <- Files.watched_paths file_options;
      let in_fd = Daemon.null_fd () in
      let log_file =
        let flowconfig_name = Options.flowconfig_name server_options in
        let tmp_dir = Options.temp_dir server_options in
        let root = Options.root server_options in
        Server_files_js.dfind_log_file ~flowconfig_name ~tmp_dir root
      in
      let log_fd = Daemon.fd_of_path log_file in
      let fds = (in_fd, log_fd, log_fd) in
      let dfind = DfindLibLwt.init fds ("flow_server_events", watch_paths) in
      dfind_instance <- Some dfind

    method wait_for_init ~timeout:_ =
      let%lwt result =
        try%lwt
          let%lwt result = DfindLibLwt.wait_until_ready self#get_dfind in
          Lwt.return (Ok result)
        with
        | Sys_error msg when msg = "Broken pipe" ->
          Lwt.return (Error "Failed to initialize dfind: broken pipe")
        | End_of_file
        | Unix.Unix_error (Unix.EPIPE, _, _) ->
          Lwt.return (Error "Failed to initialize dfind: broken pipe")
      in
      match result with
      | Ok result ->
        let%lwt () =
          let open FlowServerMonitorOptions in
          if Options.lazy_mode monitor_options.server_options then (
            let mergebase_with = monitor_options.file_watcher_mergebase_with in
            let%lwt changes = changes_since_mergebase ~mergebase_with watch_paths in
            files <- SSet.union files changes;
            Lwt.return_unit
          ) else
            Lwt.return_unit
        in
        Lwt.return (Ok result)
      | Error msg -> Lwt.return (Error msg)

    (* We don't want two threads to talk to dfind at the same time. And we don't want those two
     * threads to get the same file change events *)
    val dfind_mutex = Lwt_mutex.create ()

    method private fetch =
      Lwt_mutex.with_lock dfind_mutex (fun () ->
          let dfind = self#get_dfind in
          try%lwt
            let%lwt new_files = DfindLibLwt.get_changes dfind in
            files <- SSet.union files new_files;
            Lwt.return_unit
          with
          (* ignore the dfind server dying. use waitpid to detect this instead *)
          | Sys_error msg when msg = "Broken pipe" ->
            Logger.debug "Connection to dfind broke";
            Lwt.return_unit
          | End_of_file
          | Unix.Unix_error (Unix.EPIPE, _, _) ->
            Logger.debug "Connection to dfind broke";
            Lwt.return_unit
      )

    method get_and_clear_changed_files =
      let%lwt () = self#fetch in
      let ret = (files, None, is_initial) in
      files <- SSet.empty;
      is_initial <- false;
      Lwt.return ret

    method wait_for_changed_files =
      let%lwt () = self#fetch in
      if not (SSet.is_empty files) then
        Lwt.return_unit
      else
        let%lwt () = Lwt_unix.sleep 1.0 in
        self#wait_for_changed_files

    method stop =
      let dfind = self#get_dfind in
      let pid = DfindLibLwt.pid dfind in
      DfindLibLwt.stop dfind;
      dfind_instance <- None;

      (* Reap the killed process *)
      let%lwt _ = LwtSysUtils.blocking_waitpid pid in
      Lwt.return_unit

    method waitpid =
      let dfind = self#get_dfind in
      let pid = DfindLibLwt.pid dfind in
      let%lwt (_, status) = LwtSysUtils.blocking_waitpid pid in
      begin
        match status with
        | Unix.WEXITED exit_status ->
          let exit_type =
            try Some (Exit.error_type exit_status) with
            | Not_found -> None
          in
          let exit_status_string =
            Base.Option.value_map ~default:"Invalid_exit_code" ~f:Exit.to_string exit_type
          in
          Logger.error
            "File watcher (%s) exited with code %s (%d)"
            self#name
            exit_status_string
            exit_status
        | Unix.WSIGNALED signal ->
          Logger.error
            "File watcher (%s) was killed with %s signal"
            self#name
            (PrintSignal.string_of_signal signal)
        | Unix.WSTOPPED signal ->
          Logger.error
            "File watcher (%s) was stopped with %s signal"
            self#name
            (PrintSignal.string_of_signal signal)
      end;
      Lwt.return Watcher_died

    method getpid =
      let dfind = self#get_dfind in
      Some (DfindLibLwt.pid dfind)
  end

module WatchmanFileWatcher : sig
  class watchman :
    mergebase_with:string -> Options.t -> FlowServerMonitorOptions.watchman_options -> watcher
end = struct
  exception Watchman_failure of Watchman.failure

  type env = {
    mutable instance: Watchman.env;
    mutable files: SSet.t;
    mutable metadata: MonitorProt.file_watcher_metadata;
    mutable is_initial: bool;
    listening_thread: exit_reason Lwt.t;
    changes_condition: unit Lwt_condition.t;
    init_settings: Watchman.init_settings;
  }

  module WatchmanListenLoop = LwtLoop.Make (struct
    module J = Hh_json_helpers.AdhocJsonHelpers

    type acc = env

    let should_pause = ref true

    let extract_hg_update_metadata = function
      | None -> ("<UNKNOWN>", "<UNKNOWN REV>")
      | Some metadata ->
        let distance = J.get_number_val "distance" ~default:"<UNKNOWN>" metadata in
        let rev = J.get_string_val "rev" ~default:"<UNKNOWN REV>" metadata in
        (distance, rev)

    let log_state_enter name metadata =
      FlowEventLogger.file_watcher_event_started
        ~name
        ~data:(Base.Option.value_map ~f:Hh_json.json_to_string ~default:"" metadata)

    let log_state_leave name metadata =
      FlowEventLogger.file_watcher_event_finished
        ~name
        ~data:(Base.Option.value_map ~f:Hh_json.json_to_string ~default:"" metadata)

    let broadcast env =
      if (not (SSet.is_empty env.files)) || env.metadata.MonitorProt.missed_changes then
        Lwt_condition.broadcast env.changes_condition ()

    let main env =
      let%lwt (instance, pushed_changes) =
        match%lwt Watchman.get_changes env.instance with
        | Ok (instance, pushed_changes) -> Lwt.return (instance, pushed_changes)
        | Error Watchman.Dead -> raise (Watchman_failure Watchman.Dead)
        | Error Watchman.Restarted ->
          StatusStream.file_watcher_deferred "Watchman restart";
          (match%lwt Watchman.recover_from_restart env.instance with
          | Ok (instance, pushed_changes) ->
            StatusStream.file_watcher_ready ();
            Lwt.return (instance, pushed_changes)
          | Error err -> raise (Watchman_failure err))
      in
      env.instance <- instance;
      match pushed_changes with
      | Watchman.Files_changed { changes; changed_mergebase } ->
        (* this event tells us all the files that changed. if changed_mergebase,
           then some of these changes are upstream files. we could avoid rechecking
           them if we re-init. we signal this by setting changed_mergebase. *)
        env.files <- SSet.union env.files changes;
        let metadata = { MonitorProt.changed_mergebase; missed_changes = false } in
        env.metadata <- MonitorProt.merge_file_watcher_metadata env.metadata metadata;
        broadcast env;
        Lwt.return env
      | Watchman.Missed_changes { prev_mergebase; mergebase; changes_since_mergebase } ->
        (* When watchman restarts, we miss any filesystem changes that might happen while it's
           down. Likewise, if so many files change that Watchman's underlying file watchers
           can't keep up, it acts like it restarted. To re-synchronize, we need to recheck
           all of the files that could have changed while it wasn't watching:

           1) a file that was previously unchanged is now changed
           2) a changed file changed again
           3) a previously changed file was reverted
           4) the mergebase changed, changing some committed files

           Since watchman told us the changes since mergebase, we know about (1) and (2).
           We handle (3) by setting [missed_changes = true], which triggers a recheck of
           all focused (i.e. previously changed) files. But we can't incrementally
           handle (4): watchman can't tell us all the files that changed between the two
           mergebase commits (`hg` can, but it's not worth implementing this). *)
        let changed_mergebase = not (String.equal prev_mergebase mergebase) in
        if changed_mergebase then
          Logger.info
            "Watchman missed changes, and the mergebase changed from %S to %S."
            prev_mergebase
            mergebase
        else
          Logger.info "Watchman missed changes, but the mergebase didn't change.";
        Logger.info
          "Watchman reports %d files have changed since the mergebase"
          (SSet.cardinal changes_since_mergebase);
        env.files <- SSet.union env.files changes_since_mergebase;
        let metadata =
          { MonitorProt.changed_mergebase = Some changed_mergebase; missed_changes = true }
        in
        env.metadata <- MonitorProt.merge_file_watcher_metadata env.metadata metadata;
        broadcast env;
        Lwt.return env
      | Watchman.State_enter (name, metadata) ->
        (match name with
        | "hg.update" ->
          let (distance, rev) = extract_hg_update_metadata metadata in
          log_state_enter name metadata;
          Logger.info
            "Watchman reports an hg.update just started. Moving %s revs from %s"
            distance
            rev
        | _ when List.mem name env.init_settings.Watchman.defer_states ->
          log_state_enter name metadata;
          Logger.info "Watchman reports %s just started. Filesystem notifications are paused." name;
          StatusStream.file_watcher_deferred name;
          ()
        | _ -> ());
        Lwt.return env
      | Watchman.State_leave (name, metadata) ->
        (match name with
        | "hg.update" ->
          let (distance, rev) = extract_hg_update_metadata metadata in
          log_state_leave name metadata;
          Logger.info
            "Watchman reports an hg.update just finished. Moved %s revs to %s"
            distance
            rev;
          Lwt.return env
        | _ when List.mem name env.init_settings.Watchman.defer_states ->
          log_state_leave name metadata;
          Logger.info "Watchman reports %s ended. Filesystem notifications resumed." name;
          StatusStream.file_watcher_ready ();
          Lwt.return env
        | _ -> Lwt.return env)

    let catch _ exn =
      match Exception.to_exn exn with
      | Watchman_failure _ ->
        Logger.error "Watchman unavailable. Exiting...";
        Exception.reraise exn
      | _ ->
        let msg = Exception.to_string exn in
        FlowEventLogger.watchman_uncaught_failure
          ("Uncaught exception in Watchman listening loop: " ^ msg);
        Logger.error ~exn:(Exception.to_exn exn) "Uncaught exception in Watchman listening loop";
        raise (Watchman_failure Watchman.Dead)
  end)

  let listen env =
    match%lwt WatchmanListenLoop.run env with
    | () ->
      (* the loop was canceled (stopped intentionally) *)
      Lwt.return Watcher_stopped
    | exception Watchman_failure failure ->
      let reason =
        match failure with
        | Watchman.Dead -> Watcher_died
        | Watchman.Restarted -> Watcher_missed_changes
      in
      Lwt.return reason

  class watchman
    ~(mergebase_with : string)
    (server_options : Options.t)
    (watchman_options : FlowServerMonitorOptions.watchman_options) : watcher =
    object (self)
      val mutable env = None

      val mutable init_thread = None

      val mutable init_settings = None

      method name = "watchman"

      method private get_env =
        match env with
        | None -> failwith "Watchman was not initialized"
        | Some env -> env

      method start_init =
        let { FlowServerMonitorOptions.debug; defer_states; sync_timeout } = watchman_options in
        let file_options = Options.file_options server_options in
        let watchman_expression_terms = Watchman_expression_terms.make ~options:server_options in
        let should_track_mergebase = Options.lazy_mode server_options in
        let settings =
          {
            Watchman.debug_logging = debug;
            defer_states;
            expression_terms = watchman_expression_terms;
            mergebase_with;
            roots = Files.watched_paths file_options;
            should_track_mergebase;
            (* Defer updates during `hg.update` and defer_states *)
            subscribe_mode = Watchman.Defer_changes;
            subscription_prefix = "flow_watcher";
            sync_timeout;
          }
        in
        init_settings <- Some settings;

        init_thread <- Some (Watchman.init settings)

      method wait_for_init ~timeout =
        let go_exn () =
          let%lwt watchman = Base.Option.value_exn init_thread in
          init_thread <- None;

          match watchman with
          | Ok (watchman, files) ->
            let (waiter, wakener) = Lwt.task () in
            let new_env =
              {
                instance = watchman;
                files;
                listening_thread =
                  (let%lwt env = waiter in
                   listen env
                  );
                is_initial = true;
                changes_condition = Lwt_condition.create ();
                metadata = MonitorProt.empty_file_watcher_metadata;
                init_settings = Base.Option.value_exn init_settings;
              }
            in
            env <- Some new_env;
            Lwt.wakeup wakener new_env;
            Lwt.return (Ok ())
          | Error _ as err -> Lwt.return err
        in
        let go () =
          try%lwt go_exn () with
          | Lwt.Canceled as exn -> Exception.(reraise (wrap exn))
          | exn ->
            let e = Exception.wrap exn in
            let str = Exception.get_ctor_string e in
            let stack = Exception.get_full_backtrace_string 500 e in
            let msg = Printf.sprintf "Failed to initialize watchman: %s\n%s" str stack in
            FlowEventLogger.watchman_uncaught_failure msg;
            Lwt.return (Error msg)
        in
        match timeout with
        | Some timeout ->
          (try%lwt Lwt_unix.with_timeout timeout go with
          | Lwt_unix.Timeout ->
            Lwt.return (Error "Failed to initialize watchman: Watchman timed out"))
        | None -> go ()

      (* Should we throw away metadata even if files is empty? glevi thinks that's fine, since we
       * probably don't care about hg updates or mergebase changing if no files were affected *)
      method get_and_clear_changed_files =
        let env = self#get_env in
        let ret = (env.files, Some env.metadata, env.is_initial) in
        env.files <- SSet.empty;
        env.metadata <- MonitorProt.empty_file_watcher_metadata;
        env.is_initial <- false;
        Lwt.return ret

      method wait_for_changed_files =
        let env = self#get_env in
        Lwt_condition.wait env.changes_condition

      method stop =
        (* Flow doesn't own the watchman process, so it's not Flow's job to stop the watchman
         * process. What we can do, though, is stop listening to the messages *)
        let env = self#get_env in
        Logger.info "Canceling Watchman listening thread & closing connection";
        Lwt.cancel env.listening_thread;
        Watchman.close env.instance

      method waitpid =
        (* If watchman dies, we can start it back up again and use clockspec to make sure we didn't
         * miss anything. So from the point of view of the FileWatcher abstraction, watchman never
         * dies and this method can just wait forever.
         *
         * However it's possible that something Really Really Bad might happen to watchman. If
         * the watchman listening thread itself dies, then we need to tell the monitor that this
         * file watcher is dead. *)
        let env = self#get_env in
        (* waitpid should return a thread that resolves when the listening_thread resolves. So why
         * don't we just return the listening_thread?
         *
         * It's because we need to return a cancelable thread. The listening_thread will be fulfilled
         * with [Watcher_stopped] when it is canceled, rather than rejected with [Lwt.Canceled] like
         * normal. That is the wrong behavior.
         *
         * So how do we wrap the listening_thread in a cancelable thread? By running it
         * asynchronously, having it signal when it resolves, and waiting for the signal *)
        let signal = Lwt_condition.create () in
        Lwt.async (fun () ->
            let%lwt result = env.listening_thread in
            Lwt_condition.signal signal result;
            Lwt.return_unit
        );

        Lwt_condition.wait signal

      method getpid = None
    end
end

class watchman = WatchmanFileWatcher.watchman

module EdenFSFileWatcher : sig
  class edenfs :
    mergebase_with:string -> Options.t -> FlowServerMonitorOptions.edenfs_options -> watcher
end = struct
  exception EdenFS_failure of Edenfs_watcher.edenfs_watcher_error

  type env = {
    instance: Edenfs_watcher.instance;
    mutable files: SSet.t;
    mutable metadata: MonitorProt.file_watcher_metadata;
    mutable is_initial: bool;
    listening_thread: exit_reason Lwt.t;
    changes_condition: unit Lwt_condition.t;
  }

  (** Convert EdenFS watcher changes to a set of file paths and metadata updates.
      Note: The paths from EdenFS watcher are already absolute (the Rust code joins
      them with root_absolute), so we just add them directly without concatenating
      with root again. *)
  let convert_changes (changes_list : Edenfs_watcher.changes list) :
      SSet.t * MonitorProt.file_watcher_metadata =
    let add_files files paths =
      Base.List.fold paths ~init:files ~f:(fun acc path ->
          (* Paths from EdenFS watcher are already absolute *)
          SSet.add path acc
      )
    in
    Base.List.fold
      changes_list
      ~init:(SSet.empty, MonitorProt.empty_file_watcher_metadata)
      ~f:(fun (files, metadata) change ->
        match change with
        | Edenfs_watcher_types.FileChanges paths -> (add_files files paths, metadata)
        | Edenfs_watcher_types.CommitTransition { from_commit; to_commit; file_changes } ->
          Logger.info
            "EdenFS watcher reports commit transition from %s to %s with %d changed files"
            from_commit
            to_commit
            (List.length file_changes);
          let files = add_files files file_changes in
          let metadata =
            MonitorProt.merge_file_watcher_metadata
              metadata
              { MonitorProt.changed_mergebase = Some true; missed_changes = false }
          in
          (files, metadata)
        | Edenfs_watcher_types.StateEnter _
        | Edenfs_watcher_types.StateLeave _ ->
          (files, metadata)
    )

  module EdenFSListenLoop = LwtLoop.Make (struct
    type acc = env

    let should_pause = ref true

    let log_state_enter name = FlowEventLogger.file_watcher_event_started ~name ~data:""

    let log_state_leave name = FlowEventLogger.file_watcher_event_finished ~name ~data:""

    let broadcast env =
      if (not (SSet.is_empty env.files)) || env.metadata.MonitorProt.missed_changes then
        Lwt_condition.broadcast env.changes_condition ()

    let handle_state_changes (changes_list : Edenfs_watcher.changes list) =
      Base.List.iter changes_list ~f:(function
          | Edenfs_watcher_types.StateEnter name ->
            log_state_enter name;
            Logger.info "EdenFS reports %s just started. Filesystem notifications are paused." name;
            StatusStream.file_watcher_deferred name
          | Edenfs_watcher_types.StateLeave name ->
            log_state_leave name;
            Logger.info "EdenFS reports %s ended. Filesystem notifications resumed." name;
            StatusStream.file_watcher_ready ()
          | Edenfs_watcher_types.FileChanges _
          | Edenfs_watcher_types.CommitTransition _ ->
            ()
          )

    (** Wait for the notification fd to become readable, then get changes.
        This uses the raw fd with Lwt_unix.wait_read to integrate with the LWT scheduler
        without wrapping the fd in a cached Lwt_unix.file_descr (which caused shutdown issues). *)
    let get_changes_async_lwt instance =
      match Edenfs_watcher.get_notification_fd instance with
      | Error err -> Lwt.return (Error err)
      | Ok fd ->
        let lwt_fd = Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:true fd in
        (match%lwt
           try%lwt
             let%lwt () = Lwt_unix.wait_read lwt_fd in
             Lwt.return (Ok ())
           with
           | Unix.Unix_error (Unix.EBADF, _, _) ->
             Lwt.return (Error (Edenfs_watcher_types.EdenfsWatcherError "Notification fd closed"))
           | exn ->
             let msg = Exception.wrap exn |> Exception.to_string in
             Lwt.return (Error (Edenfs_watcher_types.EdenfsWatcherError msg))
         with
        | Error err -> Lwt.return (Error err)
        | Ok () -> Lwt.return (Edenfs_watcher.get_changes_async instance))

    let main env =
      match%lwt get_changes_async_lwt env.instance with
      | Error (Edenfs_watcher_types.LostChanges msg) ->
        Logger.error "EdenFS watcher lost changes: %s" msg;
        env.metadata <-
          MonitorProt.merge_file_watcher_metadata
            env.metadata
            { MonitorProt.changed_mergebase = None; missed_changes = true };
        broadcast env;
        Lwt.return env
      | Error err -> raise (EdenFS_failure err)
      | Ok (changes_list, _clock, _telemetry) ->
        handle_state_changes changes_list;
        let (new_files, new_metadata) = convert_changes changes_list in
        env.files <- SSet.union env.files new_files;
        env.metadata <- MonitorProt.merge_file_watcher_metadata env.metadata new_metadata;
        broadcast env;
        Lwt.return env

    let catch _ exn =
      match Exception.to_exn exn with
      | EdenFS_failure _ ->
        Logger.error "EdenFS watcher unavailable. Exiting...";
        Exception.reraise exn
      | _ ->
        let msg = Exception.to_string exn in
        FlowEventLogger.file_watcher_uncaught_failure
          ("Uncaught exception in EdenFS listening loop: " ^ msg);
        Logger.error ~exn:(Exception.to_exn exn) "Uncaught exception in EdenFS listening loop";
        raise (EdenFS_failure (Edenfs_watcher_types.EdenfsWatcherError msg))
  end)

  let listen env =
    match%lwt EdenFSListenLoop.run env with
    | () ->
      (* the loop was canceled (stopped intentionally) *)
      Lwt.return Watcher_stopped
    | exception EdenFS_failure (Edenfs_watcher_types.LostChanges _) ->
      Lwt.return Watcher_missed_changes
    | exception EdenFS_failure _ -> Lwt.return Watcher_died

  class edenfs
    ~(mergebase_with : string)
    (server_options : Options.t)
    (edenfs_options : FlowServerMonitorOptions.edenfs_options) : watcher =
    let file_options = Options.file_options server_options in
    let root_path = Options.root server_options in
    let watch_paths = Files.watched_paths file_options in
    object (self)
      val mutable env = None

      val mutable init_thread = None

      method name = "edenfs"

      method private get_env =
        match env with
        | None -> failwith "EdenFS watcher was not initialized"
        | Some env -> env

      method start_init =
        let { FlowServerMonitorOptions.edenfs_debug; edenfs_timeout_secs; edenfs_throttle_time_ms }
            =
          edenfs_options
        in
        let settings =
          {
            Edenfs_watcher_types.root = root_path;
            watch_spec = Edenfs_watcher.watch_spec server_options;
            debug_logging = edenfs_debug;
            timeout_secs = edenfs_timeout_secs;
            throttle_time_ms = edenfs_throttle_time_ms;
            report_telemetry = true;
            state_tracking = true;
            sync_queries_obey_deferral = false;
          }
        in
        init_thread <- Some (Lwt.return (Edenfs_watcher.init settings))

      method wait_for_init ~timeout =
        let go_exn () =
          let%lwt result = Base.Option.value_exn init_thread in
          init_thread <- None;

          match result with
          | Ok (instance, _clock) ->
            let (waiter, wakener) = Lwt.task () in
            let new_env =
              {
                instance;
                files = SSet.empty;
                listening_thread =
                  (let%lwt env = waiter in
                   listen env
                  );
                is_initial = true;
                changes_condition = Lwt_condition.create ();
                metadata = MonitorProt.empty_file_watcher_metadata;
              }
            in
            env <- Some new_env;
            Lwt.wakeup wakener new_env;
            (* For lazy mode, get initial files changed since mergebase.
               Unlike Watchman which returns initial files during init, we use the
               VCS-based approach like dfind to get files changed since mergebase. *)
            let%lwt () =
              if Options.lazy_mode server_options then (
                let%lwt changes = changes_since_mergebase ~mergebase_with watch_paths in
                new_env.files <- SSet.union new_env.files changes;
                Lwt.return_unit
              ) else
                Lwt.return_unit
            in
            Lwt.return (Ok ())
          | Error err ->
            let msg = Edenfs_watcher.show_edenfs_watcher_error err in
            Lwt.return (Error msg)
        in
        let go () =
          try%lwt go_exn () with
          | Lwt.Canceled as exn -> Exception.(reraise (wrap exn))
          | exn ->
            let e = Exception.wrap exn in
            let str = Exception.get_ctor_string e in
            let stack = Exception.get_full_backtrace_string 500 e in
            let msg = Printf.sprintf "Failed to initialize EdenFS watcher: %s\n%s" str stack in
            FlowEventLogger.file_watcher_uncaught_failure msg;
            Lwt.return (Error msg)
        in
        match timeout with
        | Some timeout ->
          (try%lwt Lwt_unix.with_timeout timeout go with
          | Lwt_unix.Timeout -> Lwt.return (Error "Failed to initialize EdenFS watcher: timed out"))
        | None -> go ()

      method get_and_clear_changed_files =
        let env = self#get_env in
        let ret = (env.files, Some env.metadata, env.is_initial) in
        env.files <- SSet.empty;
        env.metadata <- MonitorProt.empty_file_watcher_metadata;
        env.is_initial <- false;
        Lwt.return ret

      method wait_for_changed_files =
        let env = self#get_env in
        Lwt_condition.wait env.changes_condition

      method stop =
        let env = self#get_env in
        Logger.info "Canceling EdenFS listening thread";
        Lwt.cancel env.listening_thread;
        Lwt.return_unit

      method waitpid =
        let env = self#get_env in
        let signal = Lwt_condition.create () in
        Lwt.async (fun () ->
            let%lwt result = env.listening_thread in
            Lwt_condition.signal signal result;
            Lwt.return_unit
        );
        Lwt_condition.wait signal

      method getpid = None
    end
end

class edenfs = EdenFSFileWatcher.edenfs
