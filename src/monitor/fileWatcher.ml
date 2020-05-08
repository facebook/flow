(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Logger = FlowServerMonitorLogger

exception FileWatcherDied of exn

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

    method get_and_clear_changed_files : (SSet.t * MonitorProt.file_watcher_metadata option) Lwt.t

    method wait_for_changed_files : unit Lwt.t

    method stop : unit Lwt.t

    method waitpid : unit Lwt.t

    method getpid : int option
  end

class dummy : watcher =
  object
    method name = "dummy"

    method start_init = ()

    method wait_for_init ~timeout:_ = Lwt.return (Ok ())

    method get_and_clear_changed_files = Lwt.return (SSet.empty, None)

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
          | Sys_error msg as exn when msg = "Broken pipe" -> raise (FileWatcherDied exn)
          | (End_of_file | Unix.Unix_error (Unix.EPIPE, _, _)) as exn -> raise (FileWatcherDied exn))

    method get_and_clear_changed_files =
      let%lwt () = self#fetch in
      let ret = (files, None) in
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
            (try Some (FlowExitStatus.error_type exit_status) with Not_found -> None)
          in
          let exit_status_string =
            Base.Option.value_map ~default:"Invalid_exit_code" ~f:FlowExitStatus.to_string exit_type
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
      Lwt.return_unit

    method getpid =
      let dfind = self#get_dfind in
      Some (DfindLibLwt.pid dfind)
  end

module WatchmanFileWatcher : sig
  class watchman : FlowServerMonitorOptions.t -> watcher
end = struct
  type env = {
    mutable instance: Watchman.watchman_instance;
    mutable files: SSet.t;
    mutable metadata: MonitorProt.file_watcher_metadata;
    mutable mergebase: string option;
    mutable finished_an_hg_update: bool;
    listening_thread: unit Lwt.t;
    changes_condition: unit Lwt_condition.t;
    init_settings: Watchman.init_settings;
    should_track_mergebase: bool;
  }

  let get_mergebase env =
    if env.should_track_mergebase then (
      let%lwt (instance, mergebase) =
        (* callers should provide their own timeout *)
        Watchman.(get_mergebase ~timeout:None env.instance)
      in
      env.instance <- instance;
      match mergebase with
      | Ok mergebase -> Lwt.return (Ok (Some mergebase))
      | Error msg -> Lwt.return (Error msg)
    ) else
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

    let broadcast env =
      if not (SSet.is_empty env.files) then Lwt_condition.broadcast env.changes_condition ()

    let main env =
      let deadline = Unix.time () +. 604800. in
      let%lwt (instance, result) = Watchman.get_changes ~deadline env.instance in
      env.instance <- instance;
      match result with
      | Watchman.Watchman_pushed pushed_changes ->
        begin
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
                  match%lwt get_mergebase env with
                  | Ok mergebase -> Lwt.return mergebase
                  | Error msg ->
                    (* TODO: handle this more gracefully than `failwith` *)
                    failwith msg
                in
                match (new_mergebase, env.mergebase) with
                | (Some new_mergebase, Some old_mergebase) when new_mergebase <> old_mergebase ->
                  Logger.info
                    "Watchman reports mergebase changed from %S to %S"
                    old_mergebase
                    new_mergebase;
                  env.mergebase <- Some new_mergebase;
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
              Logger.info
                "Watchman reports an hg.update just started. Moving %s revs from %s"
                distance
                rev
            | _ -> ());
            Lwt.return env
          | Watchman.State_leave (name, metadata) ->
            (match name with
            | "hg.update" ->
              let (distance, rev) = extract_hg_update_metadata metadata in
              env.metadata <-
                MonitorProt.
                  {
                    env.metadata with
                    total_update_distance =
                      env.metadata.total_update_distance + int_of_string distance;
                  };
              env.finished_an_hg_update <- true;
              Logger.info
                "Watchman reports an hg.update just finished. Moved %s revs to %s"
                distance
                rev;
              Lwt.return env
            | _ -> Lwt.return env)
          | Watchman.Changed_merge_base _ ->
            failwith "We're not using an scm aware subscription, so we should never get these"
        end
      | Watchman.Watchman_synchronous _ ->
        failwith "Flow should never use the synchronous watchman API"
      | Watchman.Watchman_unavailable ->
        (* TODO (glevi) - Should we die if we get this for too long? *)
        Logger.error "Watchman unavailable. Retrying...";

        (* Watchman.get_changes will restart the connection. However it has some backoff
         * built in and will do nothing if called too early. That turns this LwtLoop module into a
         * busy wait. So let's add a sleep here to yield and prevent spamming the logs too much. *)
        let%lwt () = Lwt_unix.sleep 1.0 in
        Lwt.return env

    let catch _ exn =
      let exn = Exception.to_exn exn in
      Logger.error ~exn "Uncaught exception in Watchman listening loop";

      (* By exiting this loop we'll let the server know that something went wrong with Watchman *)
      Lwt.return_unit
  end)

  class watchman (monitor_options : FlowServerMonitorOptions.t) : watcher =
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
        let { FlowServerMonitorOptions.server_options; file_watcher_debug; _ } = monitor_options in
        let file_options = Options.file_options server_options in
        let watchman_expression_terms = Watchman_expression_terms.make ~options:server_options in
        let settings =
          {
            (* Defer updates during `hg.update` *)
            Watchman.subscribe_mode = Some Watchman.Defer_changes;
            expression_terms = watchman_expression_terms;
            subscription_prefix = "flow_watcher";
            roots = Files.watched_paths file_options;
            debug_logging = file_watcher_debug;
          }
        in
        init_settings <- Some settings;

        init_thread <- Some (Watchman.init settings ())

      method wait_for_init ~timeout =
        let go () =
          let%lwt watchman = Base.Option.value_exn init_thread in
          init_thread <- None;

          let should_track_mergebase =
            let server_options = monitor_options.FlowServerMonitorOptions.server_options in
            Options.lazy_mode server_options = Options.LAZY_MODE_WATCHMAN
          in
          match watchman with
          | Some watchman ->
            let (waiter, wakener) = Lwt.task () in
            let new_env =
              {
                instance = Watchman.Watchman_alive watchman;
                files = SSet.empty;
                listening_thread =
                  (let%lwt env = waiter in
                   WatchmanListenLoop.run env);
                mergebase = None;
                finished_an_hg_update = false;
                changes_condition = Lwt_condition.create ();
                metadata = MonitorProt.empty_file_watcher_metadata;
                init_settings = Base.Option.value_exn init_settings;
                should_track_mergebase;
              }
            in
            (match%lwt get_mergebase new_env with
            | Ok mergebase ->
              Base.Option.iter
                mergebase
                ~f:(Logger.info "Watchman reports the initial mergebase as %S");
              let new_env = { new_env with mergebase } in
              env <- Some new_env;
              Lwt.wakeup wakener new_env;
              Lwt.return (Ok ())
            | Error msg ->
              Lwt.return (Error (Printf.sprintf "Failed to initialize watchman: %s" msg)))
          | None -> Lwt.return (Error "Failed to initialize watchman")
        in
        match timeout with
        | Some timeout ->
          (try%lwt Lwt_unix.with_timeout timeout go
           with Lwt_unix.Timeout ->
             Lwt.return (Error "Failed to initialize watchman: Watchman timed out"))
        | None -> go ()

      (* Should we throw away metadata even if files is empty? glevi thinks that's fine, since we
       * probably don't care about hg updates or mergebase changing if no files were affected *)
      method get_and_clear_changed_files =
        let env = self#get_env in
        let ret = (env.files, Some env.metadata) in
        env.files <- SSet.empty;
        env.metadata <- MonitorProt.empty_file_watcher_metadata;
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
        Watchman.with_instance
          env.instance
          ~try_to_restart:false
          ~on_alive:Watchman.close
          ~on_dead:(fun _ -> Lwt.return_unit)

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
         * It's because we need to return a cancelable thread. The listening_thread will resolve to
         * unit when it is canceled. That is the wrong behavior.
         *
         * So how do we wrap the listening_thread in a cancelable thread? By running it
         * asynchronously, having it signal when it resolves, and waiting for the signal *)
        let signal = Lwt_condition.create () in
        Lwt.async (fun () ->
            let%lwt () = env.listening_thread in
            Lwt_condition.signal signal ();
            Lwt.return_unit);

        Lwt_condition.wait signal

      method getpid = None
    end
end

class watchman = WatchmanFileWatcher.watchman
