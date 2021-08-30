(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Logger = FlowServerMonitorLogger

exception FileWatcherDied of Exception.t

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

class dfind (monitor_options : FlowServerMonitorOptions.t) : watcher =
  object (self)
    val mutable dfind_instance = None

    val mutable files = SSet.empty

    method name = "dfind"

    method private get_dfind =
      match dfind_instance with
      | None -> failwith "Dfind was not initialized"
      | Some dfind -> dfind

    method start_init =
      let file_options =
        Options.file_options monitor_options.FlowServerMonitorOptions.server_options
      in
      let watch_paths = Files.watched_paths file_options in
      let null_fd = Daemon.null_fd () in
      let fds = (null_fd, null_fd, null_fd) in
      let dfind = DfindLibLwt.init fds ("flow_server_events", watch_paths) in
      dfind_instance <- Some dfind

    method wait_for_init ~timeout:_ =
      let%lwt result = DfindLibLwt.wait_until_ready self#get_dfind in
      Lwt.return (Ok result)

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
          | Sys_error msg as e when msg = "Broken pipe" ->
            let exn = Exception.wrap e in
            raise (FileWatcherDied exn)
          | (End_of_file | Unix.Unix_error (Unix.EPIPE, _, _)) as e ->
            let exn = Exception.wrap e in
            raise (FileWatcherDied exn))

    method get_and_clear_changed_files =
      let%lwt () = self#fetch in
      let ret = (files, None, false) in
      files <- SSet.empty;
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
  class watchman : Options.t -> FlowServerMonitorOptions.watchman_options -> watcher
end = struct
  exception Watchman_failure of Watchman.failure

  type env = {
    mutable instance: Watchman.env;
    mutable files: SSet.t;
    mutable metadata: MonitorProt.file_watcher_metadata;
    mutable mergebase: string option;
    mutable finished_an_hg_update: bool;
    mutable is_initial: bool;
    listening_thread: exit_reason Lwt.t;
    changes_condition: unit Lwt_condition.t;
    init_settings: Watchman.init_settings;
    should_track_mergebase: bool;
    survive_restarts: bool;
  }

  let get_mergebase_and_changes env =
    if env.should_track_mergebase then
      match%lwt Watchman.get_mergebase_and_changes env.instance with
      | Ok _ as ok -> Lwt.return ok
      | Error Watchman.Dead
      | Error Watchman.Restarted ->
        Lwt.return (Error "Failed to query mergebase from Watchman")
    else
      Lwt.return (Ok None)

  module WatchmanListenLoop = LwtLoop.Make (struct
    module J = Hh_json_helpers.AdhocJsonHelpers

    type acc = env

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
      if not (SSet.is_empty env.files) then Lwt_condition.broadcast env.changes_condition ()

    (** When watchman restarts, we miss any filesystem changes that might happen while it's
        down. To re-synchronize, we need to recheck all of the files that could have changed
        while it wasn't watching:

        1) a file that was previously unchanged is now changed
        2) a changed file changed again
        3) a previously changed file was reverted
        4) the mergebase changed, changing some committed files

        Since we can ask watchman for the changes since mergebase, it can tell us about
        (1) and (2). We handle (3) separately, by setting [missed_changes = true] which
        triggers a recheck of all focused (i.e. previously changed) files. But we can't
        handle (4): watchman can't tell us all the files that changed between the two
        mergebase commits (`hg` can, but it's not worth implementing this). so if the
        mergebase changes, we restart. *)
    let handle_restart env =
      StatusStream.file_watcher_deferred "Watchman restart";
      match%lwt get_mergebase_and_changes env with
      | Ok mergebase_and_changes ->
        (match (mergebase_and_changes, env.mergebase) with
        | (Some { Watchman.clock; mergebase; changes }, Some old_mergebase)
          when mergebase = old_mergebase ->
          Logger.info "Watchman restarted, but the mergebase didn't change.";
          Watchman.force_update_clockspec clock env.instance;
          env.metadata <- { env.metadata with MonitorProt.missed_changes = true };
          StatusStream.file_watcher_ready ();
          Lwt.return (Some (env.instance, Watchman.Files_changed changes))
        | _ -> Lwt.return None)
      | Error _ -> Lwt.return None

    let main env =
      let%lwt (instance, pushed_changes) =
        match%lwt Watchman.get_changes env.instance with
        | Ok (instance, pushed_changes) -> Lwt.return (instance, pushed_changes)
        | Error Watchman.Dead -> raise (Watchman_failure Watchman.Dead)
        | Error Watchman.Restarted ->
          if env.survive_restarts then
            match%lwt handle_restart env with
            | Some (instance, pushed_changes) -> Lwt.return (instance, pushed_changes)
            | None -> raise (Watchman_failure Watchman.Restarted)
          else
            raise (Watchman_failure Watchman.Restarted)
      in
      env.instance <- instance;
      match pushed_changes with
      | Watchman.Files_changed new_files ->
        env.files <- SSet.union env.files new_files;
        let%lwt () =
          (*
           ******* GOAL *******
           *
           * We want to know when an N-files-changed notification is due to the user changing
           * their mergebase with master. This could be due to pulling master & rebasing their
           * work onto the new master, or just moving from one commit to another.
           *
           ******* PREVIOUS SOLUTION *******
           *
           * Unfortunately, Watchman's mercurial integration is racy and not to be trusted.
           * Previously we tried this:
           *
           * 1. Keep a count of how many transactions are currently in progress
           * 2. When hg.update ends, wait for the transaction count to drop to 0
           * 3. When there are 0 in-progress transactions, then query for the mergebase
           *
           * This worked pretty well, but looking at some logs I would see step 3 would not
           * always fire. Maybe we were missing some state_leave notifications. Also, the
           * source control people aren't confident that waiting for the transactions to
           * is a strong guarantee.
           *
           ******* CURRENT SOLUTION *******
           *
           * So this is a more simple solution. It's based around the assumption that once
           * Watchman tells us that N files have changed, things have settled down enough
           * that it's safe to query for the mergebase. And since querying for the mergebase
           * is expensive, we only do so when we see an hg.update. After an hg.update it's
           * more acceptable to delay a file-changed notification than after a user saves a
           * file in the IDE
           *)
          if env.finished_an_hg_update then
            let%lwt new_mergebase =
              match%lwt get_mergebase_and_changes env with
              | Ok mergebase_and_changes -> Lwt.return mergebase_and_changes
              | Error msg ->
                (* TODO: handle this more gracefully than `failwith` *)
                failwith msg
            in
            match (new_mergebase, env.mergebase) with
            | (Some { Watchman.mergebase; changes = _; clock = _ }, Some old_mergebase)
              when mergebase <> old_mergebase ->
              Logger.info "Watchman reports mergebase changed from %S to %S" old_mergebase mergebase;
              env.mergebase <- Some mergebase;
              env.metadata <- { env.metadata with MonitorProt.changed_mergebase = true };
              env.finished_an_hg_update <- false;
              Lwt.return_unit
            | _ -> Lwt.return_unit
          else
            Lwt.return_unit
        in
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
          env.finished_an_hg_update <- true;
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
      | Watchman.Changed_merge_base _ ->
        failwith "We're not using an scm aware subscription, so we should never get these"

    let catch _ exn =
      match Exception.to_exn exn with
      | Watchman_failure _ ->
        Logger.error "Watchman unavailable. Exiting...";
        Exception.reraise exn
      | _ ->
        let msg = Exception.to_string exn in
        EventLogger.watchman_uncaught_failure
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
    (server_options : Options.t) (watchman_options : FlowServerMonitorOptions.watchman_options) :
    watcher =
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
        let {
          FlowServerMonitorOptions.debug;
          defer_states;
          mergebase_with;
          sync_timeout;
          survive_restarts = _;
        } =
          watchman_options
        in
        let file_options = Options.file_options server_options in
        let watchman_expression_terms = Watchman_expression_terms.make ~options:server_options in
        let settings =
          {
            Watchman.debug_logging = debug;
            defer_states;
            expression_terms = watchman_expression_terms;
            mergebase_with;
            roots = Files.watched_paths file_options;
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

          let should_track_mergebase =
            Options.lazy_mode server_options = Options.LAZY_MODE_WATCHMAN
          in
          let survive_restarts = watchman_options.FlowServerMonitorOptions.survive_restarts in
          match watchman with
          | Some watchman ->
            let (waiter, wakener) = Lwt.task () in
            let new_env =
              {
                instance = watchman;
                files = SSet.empty;
                listening_thread =
                  (let%lwt env = waiter in
                   listen env);
                mergebase = None;
                is_initial = true;
                finished_an_hg_update = false;
                changes_condition = Lwt_condition.create ();
                metadata = MonitorProt.empty_file_watcher_metadata;
                init_settings = Base.Option.value_exn init_settings;
                should_track_mergebase;
                survive_restarts;
              }
            in
            (match%lwt get_mergebase_and_changes new_env with
            | Ok mergebase_and_changes ->
              let (mergebase, files) =
                match mergebase_and_changes with
                | Some { Watchman.mergebase; changes; clock = _ } ->
                  Logger.info
                    "Watchman reports the initial mergebase as %S, and %d changes"
                    mergebase
                    (SSet.cardinal changes);
                  (Some mergebase, changes)
                | None ->
                  if should_track_mergebase then
                    Logger.warn
                      "Not checking changes since mergebase! SCM-aware queries are not supported for your VCS by your version of Watchman.";
                  (None, SSet.empty)
              in
              let new_env = { new_env with mergebase; files } in
              env <- Some new_env;
              Lwt.wakeup wakener new_env;
              Lwt.return (Ok ())
            | Error msg ->
              Lwt.return (Error (Printf.sprintf "Failed to initialize watchman: %s" msg)))
          | None -> Lwt.return (Error "Failed to initialize watchman")
        in
        let go () =
          try%lwt go_exn () with
          | Lwt.Canceled as exn -> Exception.(reraise (wrap exn))
          | exn ->
            let e = Exception.wrap exn in
            let str = Exception.get_ctor_string e in
            let stack = Exception.get_full_backtrace_string 500 e in
            let msg = Printf.sprintf "Failed to initialize watchman: %s\n%s" str stack in
            EventLogger.watchman_uncaught_failure msg;
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
            Lwt.return_unit);

        Lwt_condition.wait signal

      method getpid = None
    end
end

class watchman = WatchmanFileWatcher.watchman
