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
      FlowExitStatus.(exit ~msg:"File not found" Input_error)
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
  FlowExitStatus.(exit ~msg:"Timeout exceeded, exiting" Out_of_time)

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

let collect_error_flags main color one_line show_all_errors old_output_format =
  let color = match color with
  | Some "never" -> Tty.Color_Never
  | Some "always" -> Tty.Color_Always
  | Some "auto"
  | None -> Tty.Color_Auto
  | _ -> assert false (* the enum type enforces this *)
  in
  main { Errors_js.color; one_line; show_all_errors; old_output_format; }

let error_flags prev = CommandSpec.ArgSpec.(
  prev
  |> collect collect_error_flags
  |> flag "--color" (enum ["auto"; "never"; "always"])
      ~doc:"Display terminal output in color. never, always, auto (default: auto)"
  |> flag "--one-line" no_arg
      ~doc:"Escapes newlines so that each error prints on one line"
  |> flag "--show-all-errors" no_arg
      ~doc:"Print all errors (the default is to truncate after 50 errors)"
  |> flag "--old-output-format" no_arg
      ~doc:"Use old output format (absolute file names, line and column numbers)"
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

let shm_dir_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--shm-dir" string
      ~doc:"Directory in which to store shared memory heap (default: /dev/shm/)"
)

let from_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--from" (optional string)
      ~doc:"Specify client (for use by editor plugins)"
)

let strip_root_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--strip-root" no_arg
      ~doc:"Print paths without the root"
)

let verbose_flags =
  let collector main verbose indent =
    let opt_verbose =
      if verbose || indent
      then Some (if indent then 2 else 0)
      else None
    in
    main opt_verbose
  in
  fun prev -> CommandSpec.ArgSpec.(
    prev
    |> collect collector
    |> flag "--verbose" no_arg
        ~doc:"Print verbose info during typecheck"
    |> flag "--verbose-indent" no_arg
        ~doc:"Indent verbose info during typecheck (implies --verbose)"
  )

let root_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--root" string
      ~doc:"Project root directory containing the .flowconfig"
)

(* relativize a loc's source path to a given root whenever strip_root is set *)
let relativize strip_root root loc =
  if not strip_root then loc
  else Reason_js.strip_root_from_loc root loc

type command_params = {
  from : string;
  retries : int;
  retry_if_init : bool;
  timeout : int;
  no_auto_start : bool;
  temp_dir : string;
  shm_dir : string;
}

let collect_server_flags
  main timeout retries retry_if_init no_auto_start temp_dir shm_dir from =
  let default def = function
  | Some x -> x
  | None -> def in
  FlowEventLogger.set_from from;
  main {
    from = (default "" from);
    retries = (default 3 retries);
    retry_if_init = (default true retry_if_init);
    timeout = (default 0 timeout);
    no_auto_start = no_auto_start;
    temp_dir = (default FlowConfig.default_temp_dir temp_dir);
    shm_dir = (default FlowConfig.default_shm_dir shm_dir);
  }


let server_flags prev = CommandSpec.ArgSpec.(
  prev
  |> collect collect_server_flags
  |> flag "--timeout" (optional int)
      ~doc:"Maximum time to wait, in seconds"
  |> flag "--retries" (optional int)
      ~doc:"Set the number of retries. (default: 3)"
  |> flag "--retry-if-init" (optional bool)
      ~doc:"retry if the server is initializing (default: true)"
  |> flag "--no-auto-start" no_arg
      ~doc:"If the server is not running, do not start it; just exit"
  |> temp_dir_flag
  |> shm_dir_flag
  |> from_flag
)

let connect server_flags root =
  let tmp_dir = server_flags.temp_dir in
  let config_options = FlowConfig.((get root).options) in
  let log_file =
    Path.to_string (FlowConfig.log_file ~tmp_dir root config_options) in
  let retries = server_flags.retries in
  let retry_if_init = server_flags.retry_if_init in
  let expiry = match server_flags.timeout with
  | 0 -> None
  | n -> Some (Unix.time () +. float n) in
  CommandConnect.(
    let env = {
      root;
      autostart = not server_flags.no_auto_start;
      retries;
      retry_if_init;
      expiry;
      tmp_dir;
      log_file;
    } in
    connect env
  )

let rec search_for_root config start recursion_limit : Path.t option =
  if start = Path.parent start then None (* Reach fs root, nothing to do. *)
  else if Wwwroot.is_www_directory ~config start then Some start
  else if recursion_limit <= 0 then None
  else search_for_root config (Path.parent start) (recursion_limit - 1)

(* Given a valid file or directory, find a valid Flow root directory *)
(* NOTE: exists on invalid file or .flowconfig not found! *)
let guess_root dir_or_file =
  let dir_or_file = match dir_or_file with
  | Some dir_or_file -> dir_or_file
  | None -> "." in
  if not (Sys.file_exists dir_or_file) then (
    let msg = Utils.spf
      "Could not find file or directory %s; canceling \
      search for .flowconfig.\nSee \"flow init --help\" for more info"
      dir_or_file in
    FlowExitStatus.(exit ~msg Could_not_find_flowconfig)
  ) else (
    let dir = if Sys.is_directory dir_or_file
      then dir_or_file
      else Filename.dirname dir_or_file in
    match search_for_root ".flowconfig" (Path.make dir) 50 with
    | Some root ->
        FlowEventLogger.set_root (Some (Path.to_string root));
        root
    | None ->
        let msg = Utils.spf
          "Could not find a .flowconfig in %s or any \
          of its parent directories.\nSee \"flow init --help\" for more info\n%!"
          dir in
        FlowExitStatus.(exit ~msg Could_not_find_flowconfig)
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

let get_file_from_filename_or_stdin path = function
  | Some filename ->
      ServerProt.FileName (expand_path filename)
  | None ->
      let contents = Sys_utils.read_stdin_to_string () in
      let filename = (match path with
        | Some ""
        | None -> None
        | Some str -> Some (get_path_of_file str)
      ) in
      ServerProt.FileContent (filename, contents)

let range_string_of_loc loc = Loc.(
  let file = match loc.source with
  | Some file -> string_of_filename file
  | None -> ""
  in
  let l0, c0 = loc.start.line, loc.start.column + 1 in
  let l1, c1 = loc._end.line, loc._end.column in
  Utils.spf "%s:%d:%d,%d:%d" file l0 c0 l1 c1
)

let exe_name = Filename.basename Sys.executable_name
