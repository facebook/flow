(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t =
  | No_error
  | Build_error
  | Build_terminated
  | Checkpoint_error
  | Input_error
  | Kill_error
  | No_server_running
  | Out_of_time
  | Out_of_retries
  | Server_already_exists
  | Server_initializing
  | Type_error
  | Build_id_mismatch
  | Monitor_connection_failure
  | Unused_server
  | Lock_stolen
  | Lost_parent_monitor
  | Interrupted
  | Worker_oomed
  | Worker_busy
  (** An uncaught Not_found exception in the worker. *)
  | Worker_not_found_exception
  | Worker_failed_to_send_job
  | Socket_error
  | Missing_hhi
  | Dfind_died
  | Dfind_unresponsive
  | EventLogger_Timeout
  | EventLogger_restart_out_of_retries
  | EventLogger_broken_pipe
  | CantRunAI
  | Watchman_failed
  (** It is faster to exit the server (and have the Monitor restart the server)
   * on a Watchman fresh instance than to compute the files that have been
   * deleted and do an incremental check.
   *)
  | Watchman_fresh_instance
  | File_heap_stale
  | Hhconfig_deleted
  | Hhconfig_changed
  | Server_shutting_down
  | Server_name_not_found
  | IDE_malformed_request
  | IDE_no_server
  | IDE_out_of_retries
  | Nfs_root
  | IDE_init_failure
  | IDE_typechecker_died
  | Redecl_heap_overflow
  | Out_of_shared_memory
  | Shared_mem_assertion_failure
  | Hash_table_full
  | IDE_persistent_client_already_exists
  | Lazy_decl_bug
  | Decl_heap_elems_bug
  | Parser_heap_build_error
  | Heap_full

exception Exit_with of t

let exit_code = function
  | No_error -> 0
  | Build_error -> 2
  | Build_terminated -> 1
  | Checkpoint_error -> 8
  | Input_error -> 10
  | Kill_error -> 1
  | No_server_running -> 6
  | Out_of_time -> 7
  | Out_of_retries -> 7
  | Server_already_exists -> 77
  | Server_initializing -> 1
  | Server_shutting_down -> 1
  | Type_error -> 2
  | Build_id_mismatch -> 9
  | Monitor_connection_failure -> 9
  | Unused_server -> 5
  | Lock_stolen -> 11
  | Lost_parent_monitor -> 12
  | Shared_mem_assertion_failure -> 14
  | Out_of_shared_memory -> 15
  | Hash_table_full -> 16
  | Heap_full -> 17
  | Interrupted -> -6
  | Worker_oomed -> 30
  | Worker_busy -> 31
  | Worker_not_found_exception -> 32
  | Worker_failed_to_send_job -> 33
  | Missing_hhi -> 97
  | Socket_error -> 98
  | Dfind_died -> 99
  | Dfind_unresponsive -> 100
  | EventLogger_Timeout -> 101
  | EventLogger_restart_out_of_retries -> 108
  | CantRunAI -> 102
  | Watchman_failed -> 103
  | Watchman_fresh_instance -> 109
  | Hhconfig_deleted -> 104
  | Hhconfig_changed -> 4
  | Server_name_not_found -> 105
  | EventLogger_broken_pipe -> 106
  | Redecl_heap_overflow -> 107
  | IDE_malformed_request -> 201
  | IDE_no_server -> 202
  | IDE_out_of_retries -> 203
  | Nfs_root -> 204
  | IDE_init_failure -> 205
  | IDE_typechecker_died -> 206
  | IDE_persistent_client_already_exists -> 207
  | Lazy_decl_bug -> 208
  | Decl_heap_elems_bug -> 209
  | Parser_heap_build_error -> 210
  | File_heap_stale -> 211


let exit t =
  let ec = exit_code t in
  Pervasives.exit ec

let to_string = function
  | No_error -> "Ok"
  | Build_error -> "Build_error"
  | Build_terminated -> "Build_terminated"
  | Checkpoint_error -> "Checkpoint_error"
  | Input_error -> "Input_error"
  | Kill_error -> "Kill_error"
  | No_server_running -> "No_server_running"
  | Out_of_time -> "Out_of_time"
  | Out_of_retries -> "Out_of_retries"
  | Server_already_exists -> "Server_already_exists"
  | Server_initializing -> "Server_initializing"
  | Server_shutting_down -> "Server_shutting_down"
  | Type_error -> "Type_error"
  | Build_id_mismatch -> "Build_id_mismatch"
  | Monitor_connection_failure -> "Monitor_connection_failure"
  | Unused_server -> "Unused_server"
  | Lock_stolen -> "Lock_stolen"
  | Lost_parent_monitor -> "Lost_parent_monitor"
  | Interrupted -> "Interrupted"
  | Worker_oomed -> "Worker_oomed"
  | Worker_busy -> "Worker_busy"
  | Worker_not_found_exception -> "Worker_not_found_exception"
  | Worker_failed_to_send_job -> "Worker_failed_to_send_job"
  | Socket_error -> "Socket_error"
  | Missing_hhi -> "Missing_hhi"
  | Dfind_died -> "Dfind_died"
  | Dfind_unresponsive -> "Dfind_unresponsive"
  | EventLogger_Timeout -> "EventLogger_Timeout"
  | EventLogger_restart_out_of_retries -> "EventLogger_restart_out_of_retries"
  | EventLogger_broken_pipe -> "EventLogger_broken_pipe"
  | CantRunAI -> "CantRunAI"
  | Watchman_failed -> "Watchman_failed"
  | Watchman_fresh_instance -> "Watchman_fresh_instance"
  | Hhconfig_deleted -> "Hhconfig_deleted"
  | Hhconfig_changed -> "Hhconfig_changed"
  | Server_name_not_found -> "Server_name_not_found"
  | IDE_malformed_request -> "IDE_malformed_request"
  | IDE_no_server -> "IDE_no_server"
  | IDE_out_of_retries -> "IDE_out_of_retries"
  | Nfs_root -> "Nfs_root"
  | IDE_init_failure -> "IDE_init_failure"
  | IDE_typechecker_died -> "IDE_typechecker_died"
  | Redecl_heap_overflow -> "Redecl_heap_overflow"
  | Shared_mem_assertion_failure -> "Shared_mem_assertion_failure"
  | Out_of_shared_memory -> "Out_of_shared_memory"
  | Hash_table_full -> "Hash_table_full"
  | IDE_persistent_client_already_exists ->
    "IDE_persistent_client_already_exists"
  | Lazy_decl_bug -> "Lazy_decl_bug"
  | Decl_heap_elems_bug -> "Decl_heap_elems_bug"
  | Parser_heap_build_error -> "Parser_heap_build_error"
  | Heap_full -> "Heap_full"
  | File_heap_stale -> "File_heap_stale"


let unpack = function
  | Unix.WEXITED n -> "exit", n
  | Unix.WSIGNALED n -> "signaled", n
  | Unix.WSTOPPED n -> "stopped", n
