type t =
  (* The generic 0 exit code *)
  | No_error
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
  (* When the shared memory is missing space (e.g. full /dev/shm) *)
  | Out_of_shared_memory
  (* A weird error where a client talks to the wrong server. Really should
   * never happen *)
  | Server_client_directory_mismatch
  (* Failed to parse the command line or misuse of command line arguments *)
  | Commandline_usage_error
  (* No input *)
  | No_input
  (* Failed to start a server *)
  | Server_start_failed of Unix.process_status
  (* Something went wrong with extracting the flowlib *)
  | Missing_flowlib

  (* The hack code might throw this *)
  | Socket_error
  (* The hack code might throw this *)
  | Dfind_died
  (* The hack code might throw this *)
  | Dfind_unresponsive

  (* A generic something-else-went-wrong *)
  | Unknown_error

 (* Exit codes are part of Flow's API and thus changing exit codes is a
  * breaking change to Flow's API. Tools that call Flow may be watching for
  * certain exit codes.
  *
  * In reality, probably no one cares about many of these exit codes. The ones
  * I know are definitely being watched for are:
  *
  * No_error
  * Type_error
  * Out_of_time
  *)
let error_code = function
  | No_error -> 0
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
  | No_input -> 66
  | Server_start_failed _ -> 78
  | Missing_flowlib -> 97
  | Socket_error -> 98
  | Dfind_died -> 99
  | Dfind_unresponsive -> 100
  | Unknown_error -> 110

(* Return an error type given an error code *)
let error_type = function
  | 1 -> Server_initializing
  | 2 -> Type_error
  | 3 -> Out_of_time
  | 4 -> Kill_error
  | 6 -> No_server_running
  | 7 -> Out_of_retries
  | 8 -> Invalid_flowconfig
  | 9 -> Build_id_mismatch
  | 10 -> Input_error
  | 11 -> Lock_stolen
  | 12 -> Could_not_find_flowconfig
  | 13 -> Server_out_of_date
  | 14 -> Server_client_directory_mismatch
  | 15 -> Out_of_shared_memory
  | 64 -> Commandline_usage_error
  | 66 -> No_input
  (* The process status is made up *)
  | 78 -> Server_start_failed (Unix.WEXITED (-1))
  | 97 -> Missing_flowlib
  | 98 -> Socket_error
  | 99 -> Dfind_died
  | 100 -> Dfind_unresponsive
  | 110 -> Unknown_error
  | _ -> raise Not_found


let unpack_process_status = function
  | Unix.WEXITED n -> "exit", n
  | Unix.WSIGNALED n -> "signaled", n
  | Unix.WSTOPPED n -> "stopped", n

let to_string = function
  | No_error -> "Ok"
  | Input_error -> "Input_error"
  | Could_not_find_flowconfig -> "Could_not_find_flowconfig"
  | Server_out_of_date -> "Server_out_of_date"
  | Server_client_directory_mismatch -> "Server_client_directory_mismatch"
  | Out_of_shared_memory -> "Out_of_shared_memory"
  | Kill_error -> "Kill_error"
  | No_server_running -> "No_server_running"
  | Out_of_time -> "Out_of_time"
  | Out_of_retries -> "Out_of_retries"
  | Invalid_flowconfig -> "Invalid_flowconfig"
  | Server_initializing -> "Server_initializing"
  | Server_start_failed status ->
      let reason, code = unpack_process_status status in
      Utils_js.spf "Server_start_failed (%s, %d)" reason code
  | Type_error -> "Type_error"
  | Build_id_mismatch -> "Build_id_mismatch"
  | Lock_stolen -> "Lock_stolen"
  | Socket_error -> "Socket_error"
  | Missing_flowlib -> "Missing_flowlib"
  | Dfind_died -> "Dfind_died"
  | Dfind_unresponsive -> "Dfind_unresponsive"
  | Unknown_error -> "Unknown_error"
  | Commandline_usage_error -> "Commandline_usage_error"
  | No_input -> "No_input"

exception Exit_with of t

type json_mode = { pretty: bool }
let json_mode = ref None

let set_json_mode ~pretty =
  json_mode := Some { pretty }

let format_json ~msg t =
  let open Hh_json in

  let exit_props = [
    "code", JSON_Number (error_code t |> string_of_int);
    "reason", JSON_String (to_string t);
  ] @ Option.value_map msg ~default:[] ~f:(fun msg -> [ "msg", JSON_String msg ]) in

  let props = [
    "flowVersion", JSON_String Flow_version.version;
    "exit", JSON_Object exit_props;
  ] in

  JSON_Object props

let print_json ~msg t =
  match t with
  (* Commands that exit with these exit codes handle json output themselves *)
  | No_error | Type_error -> ()
  | _ -> begin
    match !json_mode with
    | None -> ()
    | Some { pretty } ->
      format_json ~msg t |> Hh_json.json_to_string ~pretty |> print_endline
  end

let exit ?msg t =
  (match msg with
  | Some msg -> prerr_endline msg
  | None -> ());
  print_json ~msg t;
  FlowEventLogger.exit msg (to_string t);
  Pervasives.exit (error_code t)
