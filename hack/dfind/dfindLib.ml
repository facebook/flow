(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

module type MARSHAL_TOOLS = sig
  type 'a result

  type fd

  val return : 'a -> 'a result

  val ( >>= ) : 'a result -> ('a -> 'b result) -> 'b result

  val descr_of_in_channel : 'a Daemon.in_channel -> fd

  val descr_of_out_channel : 'a Daemon.out_channel -> fd

  val to_fd_with_preamble :
    ?timeout:Timeout.t ->
    ?flags:Marshal.extern_flags list ->
    fd ->
    'a ->
    int result

  val from_fd_with_preamble : ?timeout:Timeout.t -> fd -> 'a result
end

module DFindLibFunctor (Marshal_tools : MARSHAL_TOOLS) : sig
  type t

  val init :
    Unix.file_descr * Unix.file_descr * Unix.file_descr ->
    string * Path.t list ->
    t

  val wait_until_ready : t -> unit Marshal_tools.result

  val pid : t -> int

  val get_changes : ?timeout:Timeout.t -> t -> SSet.t Marshal_tools.result

  val stop : t -> unit
end = struct
  let ( >>= ) = Marshal_tools.( >>= )

  type t = {
    infd: Marshal_tools.fd;
    outfd: Marshal_tools.fd;
    daemon_handle: (DfindServer.msg, unit) Daemon.handle;
  }

  let init log_fds (scuba_table, roots) =
    let name =
      Printf.sprintf "file watching process for server %d" (Unix.getpid ())
    in
    let ({ Daemon.channels = (ic, oc); _ } as daemon_handle) =
      Daemon.spawn ~name log_fds DfindServer.entry_point (scuba_table, roots)
    in
    {
      infd = Marshal_tools.descr_of_in_channel ic;
      outfd = Marshal_tools.descr_of_out_channel oc;
      daemon_handle;
    }

  let pid handle = handle.daemon_handle.Daemon.pid

  let wait_until_ready handle =
    Marshal_tools.from_fd_with_preamble handle.infd
    >>= fun msg ->
    assert (msg = DfindServer.Ready);
    Marshal_tools.return ()

  let request_changes ?timeout handle =
    Marshal_tools.to_fd_with_preamble handle.outfd ()
    >>= (fun _ -> Marshal_tools.from_fd_with_preamble ?timeout handle.infd)

  let get_changes ?timeout daemon =
    let rec loop acc =
      request_changes ?timeout daemon
      >>= (function
            | DfindServer.Updates s -> Marshal_tools.return s
            | DfindServer.Ready -> assert false)
      >>= fun diff ->
      if SSet.is_empty diff then
        Marshal_tools.return acc
      else
        let acc = SSet.union diff acc in
        loop acc
    in
    loop SSet.empty

  let stop handle = Daemon.kill handle.daemon_handle
end

module RegularMarshalTools :
  MARSHAL_TOOLS with type 'a result = 'a and type fd = Unix.file_descr = struct
  include Marshal_tools

  type 'a result = 'a

  type fd = Unix.file_descr

  let return x = x

  let ( >>= ) x f = f x

  let descr_of_in_channel = Daemon.descr_of_in_channel

  let descr_of_out_channel = Daemon.descr_of_out_channel
end

include DFindLibFunctor (RegularMarshalTools)
