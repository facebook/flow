(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let print_version () =
  Utils.print_endlinef
    "Flow, a static type checker for JavaScript, version %s"
    FlowConfig.version

let expand_path file =
  let path = Path.make file in
  if Path.file_exists path
  then Path.to_string path
  else
    let file = Filename.concat (Sys.getcwd()) file in
    let path = Path.make file in
    if Path.file_exists path
    then Path.to_string path
    else begin
      Printf.printf "File not found\n";
      exit 2
    end

(* line split/transform utils *)
module Line : sig
  (* split string at nth line. if it exists, returns pre, line, post *)
  val split_nth : string -> int -> (string * string * string) option

  (* transform nth line, if it exists. returns reconstructed string *)
  val transform_nth : string -> int -> (string -> string) -> string

end = struct
  let breaks = "\r\n"

  let rec eol s x i =
    if i >= x then x else
    if String.contains breaks s.[i] then i else
    eol s x (i + 1)

  let rec line s x n i =
    if n <= 0 then i, eol s x (i + 1) else
    let i = eol s x i in
    if i >= x then x, x else
    line s x (n - 1) (i + 1)

  let split_nth s n =
    let x = String.length s in
    let i, j = line s x n 0 in
    if i = x then None else
    Some String.(sub s 0 i, sub s i (j - i), sub s j (x - j))

  let transform_nth s n f =
    match split_nth s n with
    | Some (pre, s, post) -> pre ^ (f s) ^ post
    | None -> s
end

let global_kill_time = ref None

let set_timeout max_wait_time_seconds =
  global_kill_time := Some (Unix.time() +. (float_of_int max_wait_time_seconds))

let timeout_go_boom () =
  print_endline "Timeout exceeded, exiting";
  exit 3

let check_timeout () =
  match !global_kill_time with
    | None -> ()
    | Some kill_time ->
        if Unix.time () > kill_time then timeout_go_boom ()

let sleep seconds =
  match !global_kill_time with
    | None -> Unix.sleep seconds
    | Some kill_time ->
        if int_of_float (ceil (kill_time -. Unix.time ())) <= seconds
        then timeout_go_boom ()
        else Unix.sleep seconds

let collect_error_flags main color one_line show_all_errors =
  let color = match color with
  | Some "never" -> Tty.Color_Never
  | Some "always" -> Tty.Color_Always
  | Some "auto"
  | None -> Tty.Color_Auto
  | _ -> assert false (* the enum type enforces this *)
  in
  main { Errors_js.color; one_line; show_all_errors; }

let error_flags prev = CommandSpec.ArgSpec.(
  prev
  |> collect collect_error_flags
  |> flag "--color" (enum ["auto"; "never"; "always"])
      ~doc:"Display terminal output in color. never, always, auto (default: auto)"
  |> flag "--one-line" no_arg
      ~doc:"Escapes newlines so that each error prints on one line"
  |> flag "--show-all-errors" no_arg
      ~doc:"Print all errors (the default is to truncate after 50 errors)"
)

let json_flags prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--json" no_arg ~doc:"Output results in JSON format"
)

let temp_dir_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--temp-dir" string
      ~doc:"Directory in which to store temp files (default: /tmp/flow/)"
)

(* relativize a loc's source path to a given root whenever strip_root is set *)
let relativize strip_root root loc =
  if not strip_root then loc else Loc.({
    loc with source = match loc.source with
    | Some source -> Some (Files_js.relative_path (Path.to_string root) source)
    | None -> None
  })

type command_params = {
  from : string;
  retries : int;
  retry_if_init : bool;
  timeout : int;
  no_auto_start : bool;
  temp_dir : string;
}

let collect_server_flags main timeout from retries retry_if_init no_auto_start temp_dir =
  let default def = function
  | Some x -> x
  | None -> def in
  main {
    from = (default "" from);
    retries = (default 3 retries);
    retry_if_init = (default true retry_if_init);
    timeout = (default 0 timeout);
    no_auto_start = no_auto_start;
    temp_dir = (default FlowConfig.default_temp_dir temp_dir);
  }


let server_flags prev = CommandSpec.ArgSpec.(
  prev
  |> collect collect_server_flags
  |> flag "--timeout" (optional int)
      ~doc:"Maximum time to wait, in seconds"
  |> flag "--from" (optional string)
      ~doc:"Specify client (for use by editor plugins)"
  |> flag "--retries" (optional int)
      ~doc:"Set the number of retries. (default: 3)"
  |> flag "--retry-if-init" (optional bool)
      ~doc:"retry if the server is initializing (default: true)"
  |> flag "--no-auto-start" no_arg
      ~doc:"If the server if it is not running, do not start it; just exit"
  |> temp_dir_flag
)

let start_flow_server ?temp_dir root =
  Printf.fprintf stderr "Flow server launched for %s\n%!"
    (Path.to_string root);
  let temp_dir_arg = match temp_dir with
  | Some dir -> Printf.sprintf "--temp-dir=%s " (Filename.quote dir)
  | None -> ""
  in
  let flow_server = Printf.sprintf "%s start %s%s 1>&2"
    (Filename.quote (Sys.argv.(0)))
    temp_dir_arg
    (Filename.quote (Path.to_string root)) in
  match Unix.system flow_server with
    | Unix.WEXITED 0 -> ()
    | _ -> (Printf.fprintf stderr "Could not start flow server!\n"; exit 77)


let server_exists ~tmp_dir root =
  not (Lock.check (FlowConfig.lock_file ~tmp_dir root))

let wait_on_server_restart ic =
  try
    while true do
      let _ = input_char ic in
      ()
    done
  with
  | End_of_file
  | Sys_error _ ->
     (* Server has exited and hung up on us *)
     Printf.printf "Old server has exited\n%!";
     ()

(* Function connecting to hh_server *)
let connect ~tmp_dir root =
  if not (server_exists ~tmp_dir root)
  then raise CommandExceptions.Server_missing;
  let ic, oc, cstate =
    try
      let sock_name = Socket.get_path (FlowConfig.socket_file ~tmp_dir root) in
      let sockaddr = Unix.ADDR_UNIX sock_name in
      let ic, oc = Unix.open_connection sockaddr in
      try
        Printf.fprintf oc "%s\n%!" Build_id.build_id_ohai;
        let cstate : ServerUtils.connection_state = Marshal.from_channel ic in
        ic, oc, cstate
      with e ->
        Unix.shutdown_connection ic;
        close_in_noerr ic;
        raise e
    with _ ->
      if not (Lock.check (FlowConfig.init_file ~tmp_dir root))
      then raise CommandExceptions.Server_initializing
      else raise CommandExceptions.Server_cant_connect
  in
  let () = match cstate with
    | ServerUtils.Connection_ok -> ()
    | ServerUtils.Build_id_mismatch ->
        (* The server is out of date and is going to exit. Subsequent calls
         * to connect on the Unix Domain Socket might succeed, connecting to
         * the server that is about to die, and eventually we will be hung
         * up on while trying to read from our end.
         *
         * To avoid that fate, when we know the server is about to exit, we
         * wait for the connection to be closed, signaling that the server
         * has exited and the OS has cleaned up after it, then we try again.
         *)
        wait_on_server_restart ic;
        close_in_noerr ic;
        raise CommandExceptions.Server_out_of_date in
  ic, oc

let rec connect_with_autostart server_flags root =
  check_timeout ();
  try
    connect ~tmp_dir:server_flags.temp_dir root
  with
  | CommandExceptions.Server_initializing ->
      let init_msg = "flow server still initializing. If it was " ^
                     "just started this can take some time." in
      if server_flags.retry_if_init
      then (
        Printf.fprintf stderr "%s Retrying... %s\r%!" init_msg (Tty.spinner());
        sleep 1;
        connect_with_autostart server_flags root
      ) else (
        Printf.fprintf stderr "%s Try again...\n%!" init_msg;
        exit 2
      )
  | CommandExceptions.Server_cant_connect ->
      retry server_flags root
        1 "Error: could not connect to flow server, retrying..."
  | CommandExceptions.Server_busy ->
      retry server_flags root
        1 "Error: flow server is busy, retrying..."
  | CommandExceptions.Server_out_of_date
  | CommandExceptions.Server_missing ->
    if not server_flags.no_auto_start
    then (
      start_flow_server ~temp_dir:server_flags.temp_dir root;
      retry server_flags root
        3 "The flow server will be ready in a moment."
    ) else (
      prerr_endline (Utils.spf
          "Error: There is no flow server running in '%s'."
          (Path.to_string root));
      exit 2
    )
  | _ -> Printf.fprintf stderr "Something went wrong :(\n%!"; exit 2

and retry server_flags root delay message =
  check_timeout ();
  let retries = server_flags.retries in
  if retries > 0
  then (
    prerr_endline message;
    sleep delay;
    let server_flags = { server_flags with retries = retries - 1 } in
    connect_with_autostart server_flags root
  ) else (
    prerr_endline "Out of retries, exiting!";
    exit 2
  )

let rec search_for_root config start recursion_limit : Path.t option =
  let fs_root = Path.make "/" in
  if start = fs_root then None
  else if Wwwroot.is_www_directory ~config start then Some start
  else if recursion_limit <= 0 then None
  else search_for_root config (Path.parent start) (recursion_limit - 1)

(* Given a valid file or directory, find a valid flow root directory *)
(* NOTE: exists on invalid file or .flowconfig not found! *)
let guess_root dir_or_file =
  let dir_or_file = match dir_or_file with
  | Some dir_or_file -> dir_or_file
  | None -> "." in
  if not (Sys.file_exists dir_or_file) then (
    Printf.fprintf stderr "Could not find file or directory %s; canceling \
    search for .flowconfig.\nSee \"flow init --help\" for more info\n%!" dir_or_file;
    exit 2
  ) else (
    let dir = if Sys.is_directory dir_or_file
      then dir_or_file
      else Filename.dirname dir_or_file in
    match search_for_root ".flowconfig" (Path.make dir) 50 with
    | Some root ->
        root
    | None ->
        Printf.fprintf stderr "Could not find a .flowconfig in %s or any \
        of its parent directories.\nSee \"flow init --help\" for more info\n%!" dir;
        exit 2
  )

(* convert 1,1 based line/column to 1,0 for internal use *)
let convert_input_pos (line, column) =
  let column =
    if column > 1
    then column - 1
    else 0 in
  (line, column)

(* copied (and adapted) from Hack's ClientCheck module *)
let get_path_of_file file =
  let path = Path.make file in
  if Path.file_exists path
  then Path.to_string path
  else
    (* Filename.concat does not return a normalized path when the file does
       not exist. Thus, we do it on our own... *)
    let file = Files_js.normalize_path (Sys.getcwd()) file in
    let path = Path.make file in
    Path.to_string path

let range_string_of_loc loc = Loc.(
  let file = match loc.source with Some file -> file | None -> "" in
  let l0, c0 = loc.start.line, loc.start.column + 1 in
  let l1, c1 = loc._end.line, loc._end.column in
  Utils.spf "%s:%d:%d,%d:%d" file l0 c0 l1 c1
)

let exe_name = Filename.basename Sys.executable_name
