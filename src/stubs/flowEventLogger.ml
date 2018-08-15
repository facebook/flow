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
  root_name: string option;
  start_time: float;
}

type persistent_context = {
  start_lsp_state: string option;
  start_lsp_state_reason: string option;
  start_server_status: string option;
  start_watcher_status: string option;
}

type persistent_delay = {
  init_duration: float;
  command_count: int;
  command_duration: float;
  command_worst: string option;
  command_worst_duration: float option;
  recheck_count: int;
  recheck_dependent_files: int;
  recheck_changed_files: int;
  recheck_duration: float;
  recheck_worst_duration: float option;
  recheck_worst_dependent_file_count: int option;
  recheck_worst_changed_file_count: int option;
  recheck_worst_cycle_leader: string option;
  recheck_worst_cycle_size: int option;
}

let get_context _ = {
  argv = "";
  command = None;
  from = None;
  root = None;
  root_name = None;
  start_time = 0.0;
}
let restore_context _ = ()
let set_command _ = ()
let set_from _ = ()
let set_root _ = ()
let set_root_name _ = ()
let set_monitor_options ~file_watcher:_  = ()
let set_server_options ~lazy_mode:_ ~cancelable_rechecks:_ = ()

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
let persistent_command_success ~server_logging_context:_ ~request:_ ~extra_data:_
  ~client_context:_ ~persistent_context:_ ~persistent_delay:_
  ~server_profiling:_ ~client_duration:_ ~wall_start:_ ~error:_ = ()
let persistent_command_failure ~server_logging_context:_ ~request:_ ~extra_data:_
  ~client_context:_ ~persistent_context:_ ~persistent_delay:_
  ~server_profiling:_ ~client_duration:_ ~wall_start:_ ~error:_ = ()
let persistent_expected_error ~client_context:_ ~error:_ = ()
let persistent_unexpected_error ~client_context:_ ~error:_ = ()
