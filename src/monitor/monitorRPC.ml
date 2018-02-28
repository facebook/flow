(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open MonitorProt

type out_channel = server_to_monitor_message Daemon.out_channel
type channels = (monitor_to_server_message, server_to_monitor_message) Daemon.channel_pair

type state =
| Uninitialized
| Initialized of channels
| Disabled

let state = ref Uninitialized

let with_ic, with_oc =
  let with_channel select_channel ~on_disabled ~f = match !state with
  | Uninitialized ->
    (* Probably means someone is calling this module from a worker thread *)
    failwith "MonitorRPC can only be used by the master thread"
  | Disabled ->
    (* Probably means that this is a `flow check` and there is no server monitor *)
    on_disabled ()
  | Initialized channels ->
    f (select_channel channels)
  in

  (with_channel fst),
  (with_channel snd)

(* The main server process will initialize this with the channels to the monitor process *)
let init ~channels =
  state := Initialized channels

(* If there is no monitor process (like in `flow check`), we can disable MonitorRPC *)
let disable () =
  state := Disabled

(* Wait with a timeout for a message from the monitor. If the timeout is hit, will return None.
 * Otherwise will return Some single_message.
 *
 * At some point we probably could read multiple messages, but since we have to process them
 * serially, there isn't a major advantage. If the pipe's buffer fills up, the monitor will
 * wait for it to drain before writing anything else
 *
 * This will throw if the MonitorRPC is disabled, since `flow check` should never try to read
 * from the monitor *)
let read ~timeout =
  with_ic
    ~on_disabled:(fun () -> failwith "MonitorRPC is disabled")
    ~f:(fun ic ->
      let fd = Daemon.descr_of_in_channel ic in
      match Sys_utils.select_non_intr [ fd ] [] [] timeout with
      | [], _, _ -> None
      | _ ->
        let msg: MonitorProt.monitor_to_server_message = Marshal_tools.from_fd_with_preamble fd in
        Some msg
    )

(* Sends a message to the monitor.
 *
 * This is a no-op if the MonitorRPC is disabled. This allows the server to stream things like
 * status updates without worrying whether or not there is a monitor
 *)
let send ~msg =
  with_oc
    ~on_disabled:(fun () -> ())
    ~f:(fun oc ->
      let fd = Daemon.descr_of_out_channel oc in
      Marshal_tools.to_fd_with_preamble fd msg |> ignore
    )

(* Respond to a request from an ephemeral client *)
let respond_to_request ~request_id ~response =
  send ~msg:(Response (request_id, response))

(* Exception while handling the request *)
let request_failed ~request_id ~exn_str =
  send ~msg:(RequestFailed (request_id, exn_str))

(* Send a message to a persistent client *)
let respond_to_persistent_connection ~client_id ~response =
  send ~msg:(PersistentConnectionResponse (client_id, response))

(* Send a status update to the monitor *)
let status_update =
  (* Remember the last status so that we only send updates when something changes *)
  let last_status = ref ServerStatus.initial_status in

  fun ~event ->
    if !state = Disabled then () else
    let new_status = ServerStatus.update ~event ~status:!last_status in
    if new_status <> !last_status
    then begin
      last_status := new_status;
      send ~msg:(StatusUpdate new_status)
    end
