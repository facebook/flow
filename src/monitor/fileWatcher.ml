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
    method init: unit -> unit Lwt.t
    method get_changed_files: unit -> SSet.t Lwt.t
    method stop: unit -> unit Lwt.t
    method waitpid: unit -> Unix.process_status Lwt.t
  end

class dummy : watcher = object
  method name = "dummy"
  method init () = Lwt.return_unit
  method get_changed_files () = Lwt.return SSet.empty
  method stop () = Lwt.return_unit
  method waitpid () = let wait_forever_thread, _ = Lwt.task () in wait_forever_thread
end

class dfind (watch_paths: Path.t list) : watcher =
  object (self)
    val mutable dfind_instance = None

    method name = "dfind"

    method private get_dfind () =
      match dfind_instance with
      | None -> failwith "Dfind was not initialized"
      | Some dfind -> dfind

    method init () =
      let null_fd = Daemon.null_fd () in
      let fds = (null_fd, null_fd, null_fd) in
      let dfind = DfindLibLwt.init fds ("flow_server_events", watch_paths) in
      let%lwt () = DfindLibLwt.wait_until_ready dfind in
      dfind_instance <- Some dfind;
      Lwt.return_unit

    (* We don't want two threads to talk to dfind at the same time. And we don't want those two
     * threads to get the same file change events *)
    val dfind_mutex = Lwt_mutex.create ()
    method private get_changed_files () =
      Lwt_mutex.with_lock dfind_mutex (fun () ->
        let dfind = self#get_dfind () in
        try%lwt DfindLibLwt.get_changes dfind
        with
        | Sys_error msg as exn when msg = "Broken pipe" -> raise (FileWatcherDied exn)
        | End_of_file
        | Unix.Unix_error (Unix.EPIPE, _, _) as exn -> raise (FileWatcherDied exn)
      )

    method stop () =
      let dfind = self#get_dfind () in
      let pid = DfindLibLwt.pid dfind in
      DfindLibLwt.stop dfind;
      dfind_instance <- None;
      (* Reap the killed process *)
      let%lwt _ = LwtSysUtils.blocking_waitpid pid in
      Lwt.return_unit

    method waitpid () =
      let dfind = self#get_dfind () in
      let pid = DfindLibLwt.pid dfind in
      let%lwt (_, status) = LwtSysUtils.blocking_waitpid pid in
      Lwt.return status
  end
