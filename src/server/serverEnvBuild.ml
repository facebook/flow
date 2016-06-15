(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* Building the environment *)
(*****************************************************************************)
let make_genv options watch_paths handle =
  let check_mode = Options.is_check_mode options in
  let multicore = Options.max_workers options > 0 in
  let workers =
    if multicore then
      Some (ServerWorker.make options handle)
    else
      None
  in
  let dfind =
    if check_mode then None
    else
      let tmp_dir = Options.temp_dir options in
      let root = Options.root options in
      let log_file = Server_files_js.dfind_log_file ~tmp_dir root in
      let log_fd = Daemon.fd_of_path log_file in
      Some (DfindLib.init (log_fd, log_fd) ("flow_server_events", watch_paths))
  in
  { ServerEnv.options; workers; dfind; }
