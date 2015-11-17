type t =
  (* The generic 0 exit code *)
  | Ok
  (* Tried and failed to connect to a server due to the server still
   * initializing *)
  | Server_initializing
  (* There are flow errors *)
  | Type_error
  (* A command with a timeout timed out *)
  | Out_of_time
  (* Failed to kill a server *)
  | Kill_error
  (* There is no server running and we were told not to start one *)
  | No_server_running
  (* Ran out of retries *)
  | Out_of_retries
  (* Invalid .flowconfig *)
  | Invalid_flowconfig
  (* Different binaries being used together *)
  | Build_id_mismatch
  (* Generic "Bad Input" kind of error *)
  | Input_error
  (* Failed to aquire lock or lost lock *)
  | Lock_stolen
  (* Specific error for not being able to find a .flowconfig *)
  | Could_not_find_flowconfig
  (* Generic out-of-date error. This could be a version thing or maybe
   * something changed and Flow can't handle it incrementally yet *)
  | Server_out_of_date
  (* A weird error where a client talks to the wrong server. Really should
   * never happen *)
  | Server_client_directory_mismatch
  (* Failed to parse the command line or misuse of command line arguments *)
  | Commandline_usage_error
  (* Failed to start a server *)
  | Server_start_failed of Unix.process_status
  (* Connected to a dying server *)
  | Server_dying
  (* Something went wrong with extracting the flowlib *)
  | Missing_flowlib

  (* The hack code might throw this *)
  | Socket_error
  (* The hack code might throw this *)
  | Dfind_died
  (* The hack code might throw this *)
  | Dfind_unresponsive

  (* When the shared memory is missing space (e.g. full /dev/shm) *)
  | Out_of_shared_memory

  (* A generic something-else-went-wrong *)
  | Unknown_error

 (* Exit codes are part of Flow's API and thus changing exit codes is a
  * breaking change to Flow's API. Tools that call Flow may be watching for
  * certain exit codes.
  *
  * In reality, probably no one cares about many of these exit codes. The ones
  * I know are definitely being watched for are:
  *
  * Ok
  * Type_error
  * Out_of_time
  *)
let error_code = function
  | Ok -> 0
  | Server_initializing -> 1
  | Type_error -> 2
  | Out_of_time -> 3
  | Kill_error -> 4
  | No_server_running -> 6
  | Out_of_retries -> 7
  | Invalid_flowconfig -> 8
  | Build_id_mismatch -> 9
  | Input_error -> 10
  | Lock_stolen -> 11
  | Could_not_find_flowconfig -> 12
  | Server_out_of_date -> 13
  | Server_client_directory_mismatch -> 14
  | Out_of_shared_memory -> 15
  (* EX_USAGE -- command line usage error -- from glibc's sysexits.h *)
  | Commandline_usage_error -> 64
  | Server_start_failed _ -> 78
  | Server_dying -> 79
  | Missing_flowlib -> 97
  | Socket_error -> 98
  | Dfind_died -> 99
  | Dfind_unresponsive -> 100
  | Unknown_error -> 110

let unpack_process_status = function
  | Unix.WEXITED n -> "exit", n
  | Unix.WSIGNALED n -> "signaled", n
  | Unix.WSTOPPED n -> "stopped", n

let to_string = function
  | Ok -> "Ok"
  | Input_error -> "Input_error"
  | Could_not_find_flowconfig -> "Could_not_find_flowconfig"
  | Server_out_of_date -> "Server_out_of_date"
  | Server_client_directory_mismatch -> "Server_client_directory_mismatch"
  | Kill_error -> "Kill_error"
  | No_server_running -> "No_server_running"
  | Out_of_time -> "Out_of_time"
  | Out_of_retries -> "Out_of_retries"
  | Invalid_flowconfig -> "Invalid_flowconfig"
  | Server_initializing -> "Server_initializing"
  | Server_dying -> "Server_dying"
  | Server_start_failed status ->
      let reason, code = unpack_process_status status in
      Utils.spf "Server_start_failed (%s, %d)" reason code
  | Type_error -> "Type_error"
  | Build_id_mismatch -> "Build_id_mismatch"
  | Lock_stolen -> "Lock_stolen"
  | Socket_error -> "Socket_error"
  | Missing_flowlib -> "Missing_flowlib"
  | Dfind_died -> "Dfind_died"
  | Dfind_unresponsive -> "Dfind_unresponsive"
  | Out_of_shared_memory -> "Out_of_shared_memory"
  | Unknown_error -> "Unknown_error"
  | Commandline_usage_error -> "Commandline_usage_error"

exception Exit_with of t

let exit ?msg t =
  (match msg with
  | Some msg -> prerr_endline msg
  | None -> ());
  FlowEventLogger.exit msg (to_string t);
  Pervasives.exit (error_code t)
