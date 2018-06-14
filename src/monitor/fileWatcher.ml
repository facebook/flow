(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception FileWatcherDied of exn

(* TODO - Push-based API. The interfaces for both dfind and Watchman both are a pull-based APIs.
 * This means we need to periodically poll for changes. Ideally they'd be push-based APIs, which
 * would remove the latency of polling. But this would require some more work:
 *
 * 1. Build DfindServerLwt which can listen for events and write messages at the same time
 *    (we don't want to block the file watching if the writes block)
 * 2. Build WatchmanLwt. Watchman already can push messages to us, we just need to be able to
 *    notice and call a callback when that happens
 *)
class type watcher =
  object
    method name: string
    method init: unit Lwt.t
    method get_and_clear_changed_files: SSet.t Lwt.t
    method wait_for_changed_files: unit Lwt.t
    method stop: unit Lwt.t
    method waitpid: Unix.process_status Lwt.t
  end

class dummy : watcher = object
  method name = "dummy"
  method init = Lwt.return_unit
  method get_and_clear_changed_files = Lwt.return SSet.empty
  method wait_for_changed_files = Lwt.return_unit
  method stop = Lwt.return_unit
  method waitpid = let wait_forever_thread, _ = Lwt.task () in wait_forever_thread
end

class dfind (watch_paths: Path.t list) : watcher =
  object (self)
    val mutable dfind_instance = None
    val mutable files = SSet.empty

    method name = "dfind"

    method private get_dfind =
      match dfind_instance with
      | None -> failwith "Dfind was not initialized"
      | Some dfind -> dfind

    method init =
      let null_fd = Daemon.null_fd () in
      let fds = (null_fd, null_fd, null_fd) in
      let dfind = DfindLibLwt.init fds ("flow_server_events", watch_paths) in
      let%lwt () = DfindLibLwt.wait_until_ready dfind in
      dfind_instance <- Some dfind;
      Lwt.return_unit

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
  end
