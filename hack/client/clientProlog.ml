(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let num_build_retries = 60

type env = {
  root: Path.t;
}

let main env =
  let ic, oc = ClientConnect.connect { ClientConnect.
    root = env.root;
    autostart = true;
    retries = Some num_build_retries;
    retry_if_init = true;
    expiry = None;
    no_load = false;
  } in
  let path = ServerCommand.(rpc (ic, oc) ServerRpc.PROLOG) in
  close_out oc;
  Unix.execv path [||]
