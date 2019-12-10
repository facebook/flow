(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open MonitorProt

type channels = (monitor_to_server_message, server_to_monitor_message) Daemon.channel_pair

type state =
  | Uninitialized
  | Initialized of {
      infd: Lwt_unix.file_descr;
      outfd: Unix.file_descr;
    }
  | Disabled

let state = ref Uninitialized

let with_channel select_channel ~on_disabled ~f =
  match !state with
  | Uninitialized ->
    (* Probably means someone is calling this module from a worker thread *)
    failwith "MonitorRPC can only be used by the master thread"
  | Disabled ->
    (* Probably means that this is a `flow check` and there is no server monitor *)
    on_disabled ()
  | Initialized { infd; outfd } -> f (select_channel (infd, outfd))

let with_infd ~on_disabled ~f = with_channel fst ~on_disabled ~f

let with_outfd ~on_disabled ~f = with_channel snd ~on_disabled ~f

(* The main server process will initialize this with the channels to the monitor process *)
let init ~channels:(ic, oc) =
  let infd =
    Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:true (Daemon.descr_of_in_channel ic)
  in
  let outfd = Daemon.descr_of_out_channel oc in
  state := Initialized { infd; outfd }

(* If there is no monitor process (like in `flow check`), we can disable MonitorRPC *)
let disable () = state := Disabled

(* Read a single message from the monitor. *)
let read () =
  with_infd
    ~on_disabled:(fun () -> failwith "MonitorRPC is disabled")
    ~f:Marshal_tools_lwt.from_fd_with_preamble

(* Sends a message to the monitor.
 *
 * This is a no-op if the MonitorRPC is disabled. This allows the server to stream things like
 * status updates without worrying whether or not there is a monitor
 *
 * Unliked read, this is synchronous. We don't currently have a use case for async sends, and it's a
 * little painful to thread lwt through to everywhere we send data
 *)
let send ~msg =
  with_outfd
    ~on_disabled:(fun () -> ())
    ~f:(fun outfd -> Marshal_tools.to_fd_with_preamble outfd msg |> ignore)

(* Respond to a request from an ephemeral client *)
let respond_to_request ~request_id ~response = send ~msg:(Response (request_id, response))

(* Exception while handling the request *)
let request_failed ~request_id ~exn_str = send ~msg:(RequestFailed (request_id, exn_str))

(* Send a message to a persistent client *)
let respond_to_persistent_connection ~client_id ~response =
  send ~msg:(PersistentConnectionResponse (client_id, response))

(* Send a status update to the monitor *)
let status_update =
  (* Remember the last status so that we only send updates when something changes *)
  let last_status = ref ServerStatus.initial_status in
  fun ~event ->
    if !state = Disabled then
      ()
    else
      let new_status = ServerStatus.update ~event ~status:!last_status in
      if new_status <> !last_status then (
        last_status := new_status;
        send ~msg:(StatusUpdate new_status)
      )
