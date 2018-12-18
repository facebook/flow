(**
 * Copyright (c) 2018, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE fn in the "hack" directory of this source tree.
 *
 *)

type pipe_from_server = Unix.file_descr

let make_pipe_from_server fd = fd

let read_from_server fd =
  try
    let readable, _, _ = Unix.select [fd] [] [] (0.0) in
    if readable = [] then None else
    Some (Marshal_tools.from_fd_with_preamble fd)
  with e ->
    (* If something went wrong here, the system is likely in broken state
     * (the server died). We'll keep going so that monitor
     * can resolve this (by restarting the server / exiting itself *)
    let stack = Printexc.get_backtrace () in
    Hh_logger.exc stack e;
    None

let pipe_to_monitor_ref = ref None
let previous_message = ref (MonitorRpc.PROGRESS None)

let make_pipe_to_monitor fd = pipe_to_monitor_ref := Some fd

let send_to_monitor msg =
  match !pipe_to_monitor_ref with
  | None -> () (* This function can be invoked in non-server code paths,
                * when there is no monitor. *)
  | Some fd ->
    (* Avoid sending the same message repeatedly. *)
    if msg = !previous_message then () else
    begin
      previous_message := msg;
      let _ : int = Marshal_tools.to_fd_with_preamble fd msg in
      ()
    end

let send_progress_to_monitor ?(include_in_logs=true) fmt =
  let f s =
    if include_in_logs then Hh_logger.log "%s" s;
    send_to_monitor (MonitorRpc.PROGRESS (Some s))
  in
  Printf.ksprintf f fmt
