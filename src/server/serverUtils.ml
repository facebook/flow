(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type client = {
  ic : in_channel;
  oc : out_channel;
  close : unit -> unit;
}

let shutdown_client (_ic, oc) =
  try
    (* descr_of_out_channel fails with "Bad file descriptor" for a closed socket *)
    let cli = Unix.descr_of_out_channel oc in
    Unix.shutdown cli Unix.SHUTDOWN_ALL;
    close_out oc
  with _ -> ()


type connection_state =
  | Connection_ok
  | Build_id_mismatch

let msg_to_channel oc msg =
  Marshal.to_channel oc msg [];
  flush oc
