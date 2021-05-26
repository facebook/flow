(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | Interrupted
  | No_error
  | Windows_killed_by_task_manager
  | Type_error
  | Out_of_time
  | Kill_error
  | Unused_server
  | No_server_running
  | Out_of_retries
  | Invalid_flowconfig
  | Path_is_not_a_file
  | Build_id_mismatch
  | Input_error
  | Lock_stolen
  | Could_not_find_flowconfig
  | Could_not_extract_flowlibs
  | Server_out_of_date
  | Out_of_shared_memory
  | Flowconfig_changed
  | Server_client_directory_mismatch
  | Commandline_usage_error
  | No_input
  | Server_start_failed of Unix.process_status
  | Missing_flowlib
  | Autostop
  | Killed_by_monitor
  | Invalid_saved_state
  | Restart
  | Socket_error
  | Dfind_died
  | Dfind_unresponsive
  | Watchman_error
  | Watchman_failed
  | File_watcher_missed_changes
  | Hash_table_full
  | Heap_full
  | EventLogger_restart_out_of_retries
  | Unknown_error

exception Exit_with of t

val error_code : t -> int

val error_type : int -> t

val error_type_opt : int -> t option

val unpack_process_status : Unix.process_status -> string * int

val to_string : t -> string

val json_props_of_t : ?msg:string -> t -> (string * Hh_json.json) list
