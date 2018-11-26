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
  | Unknown_error

exception Exit_with of t

val exit: ?msg:string -> t -> 'a
val error_code: t -> int
val error_type: int -> t
val to_string: t -> string
val set_json_mode: pretty:bool -> unit
