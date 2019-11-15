(*
 * Copyright (c) 2017, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

module Marshal_tools_lwt = Marshal_tools.MarshalToolsFunctor (struct
  type 'a result = 'a Lwt.t

  type fd = Lwt_unix.file_descr

  let return = Lwt.return

  let fail = Lwt.fail

  let ( >>= ) = Lwt.( >>= )

  let write ?timeout fd ~buffer ~offset ~size =
    if timeout <> None then raise (Invalid_argument "Use Lwt timeouts directly");
    Lwt_unix.wait_write fd >>= fun () -> Lwt_unix.write fd buffer offset size

  let read ?timeout fd ~buffer ~offset ~size =
    if timeout <> None then raise (Invalid_argument "Use lwt timeouts directly");
    Lwt_unix.wait_read fd >>= fun () -> Lwt_unix.read fd buffer offset size

  let log str = Lwt_log_core.ign_error str
end)

include Marshal_tools_lwt

(* The Timeout module probably doesn't work terribly well with Lwt. Luckily, timeouts are super easy
 * to write in Lwt, so we don't **really** need them *)
let to_fd_with_preamble ?flags fd obj = to_fd_with_preamble ?flags fd obj

let from_fd_with_preamble fd = from_fd_with_preamble fd
