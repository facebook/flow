(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This `.mli` file was generated automatically. It may include extra
definitions that should not actually be exposed to the caller. If you notice
that this interface file is a poor interface, please take a few minutes to
clean it up manually, and then delete this comment once the interface is in
shape. *)

type t =
  | No_error
  | Build_error
  | Build_terminated
  | Checkpoint_error
  | Input_error
  | Kill_error
  | No_server_running_should_retry
  | Server_hung_up_should_retry
  | Server_hung_up_should_abort
  | Out_of_time
  | Out_of_retries
  | Server_already_exists
  | Type_error
  | Build_id_mismatch
  | Monitor_connection_failure
  | Unused_server
  | Lock_stolen
  | Lost_parent_monitor
  | Interrupted
  | Worker_oomed
  | Worker_busy
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
  | Watchman_fresh_instance
  | Watchman_invalid_result
  | File_provider_stale
  | Hhconfig_deleted
  | Hhconfig_changed
  | Server_shutting_down
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
  | IDE_new_client_connected
  | Lazy_decl_bug
  | Decl_heap_elems_bug
  | Parser_heap_build_error
  | Heap_full
  | Sql_assertion_failure
  | Local_type_env_stale
  | Sql_cantopen
  | Sql_corrupt
  | Sql_misuse
  | Uncaught_exception
  | Decl_not_found
  | Big_rebase_detected
  | Failed_to_load_should_retry
  | Failed_to_load_should_abort

exception Exit_with of t

val exit_code : t -> int

val exit : t -> 'a

val to_string : t -> string

val unpack : Unix.process_status -> string * int
