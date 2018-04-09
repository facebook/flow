(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type logging_context = {
  argv: string;
  command: string option;
  from: string option;
  root: string option;
  start_time: float;
}

let get_context _ = {
  argv = "";
  command = None;
  from = None;
  root = None;
  start_time = 0.0;
}
let restore_context _ = ()
let set_command _ = ()
let set_from _ = ()
let set_root _ = ()

let status_response ~num_errors:_ = ()
let init_done ~profiling:_ = ()
let init_flow_command ~version:_ = ()
let killed _ = ()
let lock_lost _ = ()
let lock_stolen _ = ()
let out_of_date _ = ()
let exit _ _ = ()
let report_from_monitor_server_exit_due_to_signal _ = ()
let recheck
    ~modified:_
    ~deleted:_
    ~dependent_files:_
    ~profiling:_ = ()
let murdered_by_oom_killer _ = ()
let ephemeral_command_success ?json_data:_ ~client_context:_ ~profiling:_ = ()
let ephemeral_command_failure ?json_data:_ ~client_context:_ = ()
let persistent_command_success ?json_data:_ ~request:_ ~client_context:_ ~profiling:_ = ()
let persistent_command_failure ?json_data:_ ~request:_ ~client_context:_ = ()
