(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

let disable_logging () = ()

let get_context () = !context

let get_from_I_AM_A_CLOWN () = !context.from

let restore_context _ = ()

let set_command _ = ()

let set_eden _ = ()

let set_from from = context := { from }

let set_root _ = ()

let set_root_name _ = ()

let set_saved_state_filename _ = ()

let set_monitor_options ~file_watcher:_ ~vcs:_ = ()

let set_server_options
    ~lazy_mode:_
    ~max_workers:_
    ~long_lived_workers:_
    ~enabled_rollouts:_
    ~debug:_
    ~log_saving:_
    ~log_file:_ =
  ()

let status_response ~num_errors:_ = ()

let init_done ?first_internal_error:_ ~saved_state_fetcher:_ _profiling = ()

let init_flow_command ~init_id:_ = ()

let init_worker ~init_id:_ _ = ()

let should_log () = false

let lock_lost _ = ()

let lock_stolen _ = ()

let out_of_date _ = ()

let exit ?error:_ _ _ = ()

let report_from_monitor_server_exit_due_to_signal _ = ()

let recheck
    ~modified_count:_
    ~deleted_count:_
    ~merged_dependency_count:_
    ~dependent_file_count:_
    ~merge_skip_count:_
    ~check_skip_count:_
    ~profiling:_
    ~time_to_resolve_all_type_errors:_
    ~time_to_resolve_all_type_errors_in_one_file:_
    ~time_to_resolve_all_subtyping_errors:_
    ~time_to_resolve_all_subtyping_errors_in_one_file:_
    ~first_internal_error:_
    ~slowest_file:_
    ~num_slow_files:_
    ~scm_changed_mergebase:_ =
  ()

let recheck_canceled
    ~priority:_ ~num_files_to_prioritize:_ ~num_files_to_recheck:_ ~num_files_to_force:_ =
  ()

let recheck_series ~recheck_count:_ ~profiling:_ = ()

let reinit ~profiling:_ = ()

let reinit_full_check ~profiling:_ = ()

let murdered_by_oom_killer _ = ()

let ephemeral_command_success ~json_data:_ ~client_context:_ ~profiling:_ = ()

let ephemeral_command_failure ~json_data:_ ~client_context:_ = ()

let persistent_command_success
    ~server_logging_context:_
    ~request_id:_
    ~method_name:_
    ~request:_
    ~extra_data:_
    ~client_context:_
    ~persistent_context:_
    ~persistent_delay:_
    ~server_profiling:_
    ~client_duration:_
    ~wall_start:_
    ~activity_key:_
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
    ~activity_key:_
    ~error:_ =
  ()

let persistent_expected_error ~request:_ ~client_context:_ ~activity_key:_ ~error:_ = ()

let persistent_unexpected_error ~request:_ ~client_context:_ ~activity_key:_ ~error:_ = ()

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

let load_saved_state_error ~saved_state_filename:_ ~changed_files_count:_ ~invalid_reason:_ ~trace:_
    =
  ()

let idle_heartbeat ~idle_time:_ ~profiling:_ = ()

let live_parse_errors ~request:_ ~data:_ ~wall_start:_ = ()

let live_non_parse_errors ~request:_ ~data:_ ~wall_start:_ = ()

let live_non_parse_errors_failed ~request:_ ~data:_ ~wall_start:_ = ()

let file_watcher_event_started ~name:_ ~data:_ = ()

let file_watcher_event_finished ~name:_ ~data:_ = ()

let watchman_error ?request:_ ?response:_ _ = ()

let watchman_warning _ = ()

let watchman_uncaught_failure _ = ()

let watchman_connection_reestablished _ = ()

let watchman_connection_reestablishment_failed _ = ()

let sharedmem_gc_ran _ _ _ _ = ()

let sharedmem_init_done _ = ()

let sharedmem_failed_memfd_init _ = ()

let worker_exception _ = ()

let dfind_ready _ _ = ()

let parsing_exception _ = ()
