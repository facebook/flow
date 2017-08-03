type t =
  | No_error
  | Server_initializing
  | Type_error
  | Out_of_time
  | Kill_error
  | No_server_running
  | Out_of_retries
  | Invalid_flowconfig
  | Build_id_mismatch
  | Input_error
  | Lock_stolen
  | Could_not_find_flowconfig
  | Server_out_of_date
  | Out_of_shared_memory
  | Server_client_directory_mismatch
  | Commandline_usage_error
  | No_input
  | Server_start_failed of Unix.process_status
  | Missing_flowlib
  | Socket_error
  | Dfind_died
  | Dfind_unresponsive
  | Unknown_error

exception Exit_with of t

val exit: ?msg:string -> t -> 'a
val error_code: t -> int
val error_type: int -> t
val to_string: t -> string
