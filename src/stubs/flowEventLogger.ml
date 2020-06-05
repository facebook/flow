(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type logging_context = { from: string option }

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

let context = ref { from = None }

let get_context () = !context

let get_from_I_AM_A_CLOWN () = !context.from

let restore_context _ = ()

let set_command _ = ()

let set_from from = context := { from }

let set_root _ = ()

let set_root_name _ = ()

let set_saved_state_filename _ = ()

let set_monitor_options ~file_watcher:_ = ()

let set_server_options
    ~lazy_mode:_ ~arch:_ ~abstract_locations:_ ~max_workers:_ ~enabled_rollouts:_ ~debug:_ =
  ()

let status_response ~num_errors:_ = ()

let init_done
    ?estimated_time_to_recheck:_
    ?estimated_time_to_restart:_
    ?estimated_time_to_init:_
    ?estimated_time_per_file:_
    ?estimated_files_to_recheck:_
    ?estimated_files_to_init:_
    ?first_internal_error:_
    _profiling =
  ()

let init_flow_command ~version:_ = ()

let killed _ = ()

let lock_lost _ = ()

let lock_stolen _ = ()

let out_of_date _ = ()

let exit _ _ = ()

let report_from_monitor_server_exit_due_to_signal _ = ()

let recheck
    ~recheck_reasons:_
    ~modified:_
    ~deleted:_
    ~to_merge:_
    ~to_check:_
    ~sig_dependent_files:_
    ~all_dependent_files:_
    ~merge_skip_count:_
    ~check_skip_count:_
    ~profiling:_
    ~estimated_time_to_recheck:_
    ~estimated_time_to_restart:_
    ~estimated_time_to_init:_
    ~estimated_time_per_file:_
    ~estimated_files_to_recheck:_
    ~estimated_files_to_init:_
    ~first_internal_error:_
    ~slowest_file:_
    ~num_slow_files:_
    ~scm_update_distance:_
    ~scm_changed_mergebase:_ =
  ()

let murdered_by_oom_killer _ = ()

let ephemeral_command_success ?json_data:_ ~client_context:_ ~profiling:_ = ()

let ephemeral_command_failure ?json_data:_ ~client_context:_ = ()

let persistent_command_success
    ~server_logging_context:_
    ~request:_
    ~extra_data:_
    ~client_context:_
    ~persistent_context:_
    ~persistent_delay:_
    ~server_profiling:_
    ~client_duration:_
    ~wall_start:_
    ~error:_ =
  ()

let persistent_command_failure
    ~server_logging_context:_
    ~request:_
    ~extra_data:_
    ~client_context:_
    ~persistent_context:_
    ~persistent_delay:_
    ~server_profiling:_
    ~client_duration:_
    ~wall_start:_
    ~error:_ =
  ()

let persistent_expected_error ~request:_ ~client_context:_ ~error:_ = ()

let persistent_unexpected_error ~request:_ ~client_context:_ ~error:_ = ()

let saved_state_fb_fetcher_success
    ~repo_root:_
    ~merge_base_hash:_
    ~merge_base_timestamp:_
    ~saved_state_hash:_
    ~changed_files_count:_
    ~saved_state_filename:_
    ~profiling:_ =
  ()

let saved_state_fb_fetcher_error ~step:_ ~trace:_ ~profiling:_ = ()

let load_saved_state_success ~changed_files_count:_ = ()

let load_saved_state_error ~saved_state_filename:_ ~changed_files_count:_ ~invalid_reason:_ = ()

let idle_heartbeat ~idle_time:_ ~profiling:_ = ()

let live_parse_errors ~request:_ ~data:_ ~wall_start:_ = ()

let live_non_parse_errors ~request:_ ~data:_ ~wall_start:_ = ()

let live_non_parse_errors_failed ~request:_ ~data:_ ~wall_start:_ = ()
