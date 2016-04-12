(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type env = {
  root : Path.t;
}

let main env =
  let ic, oc = ClientConnect.connect { ClientConnect.
    root = env.root;
    autostart = true;
    ai_mode = None;
    retries = Some 800;
    retry_if_init = true;
    expiry = None;
    no_load = false;
    to_ide = false;
  } in
  ServerCommand.connect_debug oc;
  (* Exit this via ctrl-C *)
  while true do
    print_endline (Timeout.input_line ic);
  done;
  Exit_status.Ok
