(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let ( >>= ) = Lwt.( >>= )

type t = {
  infd: Lwt_unix.file_descr;
  outfd: Lwt_unix.file_descr;
  daemon_handle: (DfindServer.msg, unit) Daemon.handle;
}

let descr_of_in_channel ic =
  Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:true (Daemon.descr_of_in_channel ic)

let descr_of_out_channel oc =
  Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:true (Daemon.descr_of_out_channel oc)

let init log_fds (scuba_table, roots) =
  let name = Printf.sprintf "file watching process for server %d" (Unix.getpid ()) in
  let ({ Daemon.channels = (ic, oc); _ } as daemon_handle) =
    Daemon.spawn ~name log_fds DfindServer.entry_point (scuba_table, roots)
  in
  { infd = descr_of_in_channel ic; outfd = descr_of_out_channel oc; daemon_handle }

let pid handle = handle.daemon_handle.Daemon.pid

let wait_until_ready handle =
  Marshal_tools_lwt.from_fd_with_preamble handle.infd >>= fun msg ->
  assert (msg = DfindServer.Ready);
  Lwt.return ()

let request_changes handle =
  Marshal_tools_lwt.to_fd_with_preamble handle.outfd () >>= fun _ ->
  Marshal_tools_lwt.from_fd_with_preamble handle.infd

let get_changes handle =
  let rec loop acc =
    (request_changes handle >>= function
     | DfindServer.Updates s -> Lwt.return s
     | DfindServer.Ready -> assert false
    )
    >>= fun diff ->
    if SSet.is_empty diff then
      Lwt.return acc
    else
      let acc = SSet.union diff acc in
      loop acc
  in
  loop SSet.empty

let stop handle = Daemon.kill handle.daemon_handle
