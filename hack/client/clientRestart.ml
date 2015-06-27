(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let main env =
  if ClientConnectSimple.server_exists env.ClientStart.root
  then HackClientStop.kill_server env.ClientStart.root
  else Printf.eprintf "Warning: no server to restart for %s\n%!"
    (Path.to_string env.ClientStart.root);
  ClientStart.start_server env;
  Exit_status.Ok
