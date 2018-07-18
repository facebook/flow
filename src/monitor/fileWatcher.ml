(**
 * Copyright (c) 2018-present, Facebook, Inc.
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
    method name: string
    method start_init: unit
    method wait_for_init: unit Lwt.t
    method get_and_clear_changed_files: SSet.t Lwt.t
    method wait_for_changed_files: unit Lwt.t
    method stop: unit Lwt.t
    method waitpid: Unix.process_status Lwt.t
    method getpid: int option
  end

class dummy : watcher = object
  method name = "dummy"
  method start_init = ()
  method wait_for_init = Lwt.return_unit
  method get_and_clear_changed_files = Lwt.return SSet.empty
  method wait_for_changed_files = Lwt.return_unit
  method stop = Lwt.return_unit
  method waitpid = let wait_forever_thread, _ = Lwt.task () in wait_forever_thread
  method getpid = None
end

class dfind (monitor_options: FlowServerMonitorOptions.t) : watcher =
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

    method wait_for_init =
      DfindLibLwt.wait_until_ready (self#get_dfind)

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
        | End_of_file
        | Unix.Unix_error (Unix.EPIPE, _, _) as exn -> raise (FileWatcherDied exn)
      )

    method get_and_clear_changed_files =
      let%lwt () = self#fetch in
      let ret = files in
      files <- SSet.empty;
      Lwt.return ret

    method wait_for_changed_files =
      let%lwt () = self#fetch in
      if not (SSet.is_empty files)
      then Lwt.return_unit
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
      Lwt.return status

    method getpid =
      let dfind = self#get_dfind in
      Some (DfindLibLwt.pid dfind)
  end

module WatchmanFileWatcher : sig
  class watchman : FlowServerMonitorOptions.t -> watcher
end = struct
  type env = {
    mutable instance: Watchman_lwt.watchman_instance;
    mutable files: SSet.t;
    listening_thread: unit Lwt.t;
    changes_condition: unit Lwt_condition.t;
  }

  module WatchmanListenLoop = LwtLoop.Make(struct
    module J = Hh_json_helpers.AdhocJsonHelpers

    type acc = env

    (* Each subscription will send hg.update state_enter/state_leaves. So let's only react for the
     * first and last *)
    let hg_update_semaphore = ref 0
    let should_broadcast = ref false

    let extract_hg_update_metadata = function
    | None -> "<UNKNOWN>", "<UNKNOWN REV>"
    | Some metadata ->
      let distance = J.get_number_val "distance" ~default:"<UNKNOWN>" metadata in
      let rev = J.get_string_val "rev" ~default:"<UNKNOWN REV>" metadata in
      distance, rev

    let broadcast env =
      if !should_broadcast && not (SSet.is_empty env.files)
      then Lwt_condition.broadcast env.changes_condition ()

    let main env =
      let deadline = Unix.time () +. 604800. in
      let%lwt instance, result = Watchman_lwt.get_changes ~deadline env.instance in
      env.instance <- instance;
      begin match result with
      | Watchman_lwt.Watchman_pushed pushed_changes ->
        begin match pushed_changes with
        | Watchman_lwt.Files_changed new_files ->
          env.files <- SSet.union env.files new_files;
          broadcast env
        | Watchman_lwt.State_enter (name, metadata) ->
          if name = "hg.update" then begin
            if !hg_update_semaphore = 0 then begin
              should_broadcast := false;
              let distance, rev = extract_hg_update_metadata metadata in
              Logger.info
                "Watchman reports an hg.update just started. Moving %s revs from %s" distance rev
            end;
            incr hg_update_semaphore
          end
        | Watchman_lwt.State_leave (name, metadata) ->
          if name = "hg.update" then begin
            decr hg_update_semaphore;
            if !hg_update_semaphore = 0
            then begin
              let distance, rev = extract_hg_update_metadata metadata in
              Logger.info
                "Watchman reports an hg.update just finished. Moved %s revs to %s" distance rev;

              let debounce_time = 5.0 in
              Logger.info
                "Debouncing by deferring file watcher notifications for %f seconds" debounce_time;

              Lwt.async (fun () ->
                let%lwt () = Lwt_unix.sleep debounce_time in
                if !hg_update_semaphore = 0 then begin
                  should_broadcast := true;
                  broadcast env
                end;
                Lwt.return_unit
              )
            end
          end
        | Watchman_lwt.Changed_merge_base _ ->
          failwith "We're not using an scm aware subscription, so we should never get these"
        end
      | Watchman_lwt.Watchman_synchronous _ ->
        failwith "Flow should never use the synchronous watchman API"
      | Watchman_lwt.Watchman_unavailable ->
        (* TODO (glevi) - Should we die if we get this for too long? *)
        Logger.error "Watchman unavailable. Retrying..."
      end;
      Lwt.return env

    external reraise : exn -> 'a = "%reraise"

    let catch _ exn =
      match exn with
      | Lwt.Canceled -> Lwt.return_unit
      | _ ->
        Logger.error ~exn "Uncaught exception in Watchman listening loop";
        reraise exn
  end)

  class watchman (monitor_options: FlowServerMonitorOptions.t) : watcher =
    object (self)
      val mutable env = None
      val mutable init_thread = None

      method name = "watchman"

      method private get_env =
        match env with
        | None -> failwith "Watchman was not initialized"
        | Some env -> env

      method start_init =
        let { FlowServerMonitorOptions.server_options; file_watcher_debug; _; } = monitor_options in
        let file_options = Options.file_options server_options in

        let watchman_expression_terms =
          let module J = Hh_json_helpers.AdhocJsonHelpers in

          let suffixes =
            let exts = SSet.elements @@ Files.get_all_watched_extensions file_options in
            let exts = Files.flow_ext :: exts in

            exts
            (* Turn .foo.bar into .bar, since suffix can't deal with multi-part extensions *)
            |> List.map (fun ext -> Filename.extension ("foo" ^ ext))
            (* Strip off the leading '.' *)
            |> List.map (fun ext ->
              if ext <> "" && ext.[0] = '.'
              then String.sub ext 1 (String.length ext - 1)
              else ext
            )
          in

          (* Unfortunately watchman can't deal with absolute paths. Its "wholename" feature only
           * works for relative paths to the watch root, and we don't know the watch root until we
           * init.
           *
           * Luckily, all we really need is to specify a superset of the files we care about. So
           * watching all .flowconfigs instead of just our .flowconfig is fine *)
          let absolute_paths =
            (* Config file *)
            let paths = [Server_files_js.config_file @@ Options.root server_options] in
            (* Module resolver *)
            Option.value_map (Options.module_resolver server_options)
              ~default:paths ~f:(fun module_resolver -> Path.to_string module_resolver :: paths)
          in

          (* Include any file with this basename *)
          let basenames = "package.json" :: (List.map Filename.basename absolute_paths) in

          [
            J.strlist ["type"; "f"]; (* Watch for files *)
            J.pred "anyof" @@ [
              J.assoc_strlist "suffix" suffixes;
              J.assoc_strlist "name" basenames;
            ];
            J.pred "not" @@ [ (* Ignore changes in source control dirs *)
              J.pred "anyof" @@ [
                J.strlist ["dirname"; ".hg"];
                J.strlist ["dirname"; ".git"];
                J.strlist ["dirname"; ".svn"];
              ]
            ]
          ]
        in

        init_thread <- Some (Watchman_lwt.init {
          (* Defer updates during `hg.update` *)
          Watchman_lwt.subscribe_mode = Some Watchman_lwt.Defer_changes;
          (* Hack makes this configurable in their local config. Apparently buck & hgwatchman also
           * use 10 seconds *)
          init_timeout = 10;
          expression_terms = watchman_expression_terms;
          subscription_prefix = "flow_watcher";
          roots = Files.watched_paths file_options;
          debug_logging = file_watcher_debug;
        })

      method wait_for_init =
        let%lwt watchman = Option.value_exn init_thread in
        init_thread <- None;

        begin match watchman with
        | Some watchman ->
          let waiter, wakener = Lwt.task () in
          let new_env = {
            instance = Watchman_lwt.Watchman_alive watchman;
            files = SSet.empty;
            listening_thread = (let%lwt env = waiter in WatchmanListenLoop.run env);
            changes_condition = Lwt_condition.create ();
          } in
          env <- Some new_env;
          Lwt.wakeup wakener new_env
        | None ->
          failwith "Failed to initialize watchman"
        end;
        Lwt.return_unit

      method get_and_clear_changed_files =
        let env = self#get_env in
        let ret = env.files in
        env.files <- SSet.empty;
        Lwt.return ret

      method wait_for_changed_files =
        let env = self#get_env in
        Lwt_condition.wait env.changes_condition

      method stop =
        (* Flow doesn't own the watchman process, so it's not Flow's job to stop the watchman
         * process. What we can do, though, is stop listening to the messages *)
        let env = self#get_env in
        Lwt.cancel env.listening_thread;
        Lwt.return_unit

      method waitpid =
        (* If watchman dies, we can start it back up again and use clockspec to make sure we didn't
         * miss anything. So from the point of view of the FileWatcher abstraction, watchman never
         * dies and this method can just wait forever *)
        let waiter, _ = Lwt.task () in
        waiter

      method getpid = None
    end

end

class watchman = WatchmanFileWatcher.watchman
