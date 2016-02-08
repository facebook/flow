(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type client = {
  ic : in_channel;
  oc : out_channel;
  close : unit -> unit;
}

let shutdown_client (_ic, oc) =
  let cli = Unix.descr_of_out_channel oc in
  try
    Unix.shutdown cli Unix.SHUTDOWN_ALL;
    close_out oc
  with _ -> ()


type connection_state =
  | Connection_ok
  | Build_id_mismatch

let msg_to_channel oc msg =
  Marshal.to_channel oc msg [];
  flush oc
