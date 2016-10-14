(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

let print_version () =
  print_endlinef
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
      let msg = Printf.sprintf "File not found: %s" (Path.to_string path) in
      FlowExitStatus.(exit ~msg Input_error)
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
  main { Options.color; one_line; show_all_errors; old_output_format; }

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
  |> flag "--pretty" no_arg ~doc:"Pretty-print JSON output (implies --json)"
)

let temp_dir_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--temp-dir" string
      ~doc:"Directory in which to store temp files (default: /tmp/flow/)"
)

let shm_dirs_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--sharedmemory-dirs" string
      ~doc:"Directory in which to store shared memory heap (default: /dev/shm/)"
)

let shm_min_avail_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--sharedmemory-minimum-available" int
      ~doc:"Flow will only use a filesystem for shared memory if it has at \
        least these many bytes available (default: 536870912 - which is 512MB)"
)

let shm_dep_table_pow_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--sharedmemory-dep-table-pow" int
      ~doc:"The exponent for the size of the shared memory dependency table. \
        The default is 17, implying a size of 2^17 bytes"
)

let shm_hash_table_pow_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--sharedmemory-hash-table-pow" int
      ~doc:"The exponent for the size of the shared memory hash table. \
        The default is 19, implying a size of 2^19 bytes"
)

let shm_log_level_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--sharedmemory-log-level" int
      ~doc:"The logging level for shared memory statistics. \
        0=none, 1=some"
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
  let collector main verbose indent depth =
    let opt_verbose =
      if verbose || indent || depth != None
      then Some { Verbose.
        indent = if indent then 2 else 0;
        depth = match depth with
          | Some n when n >= 0 -> n
          | _ -> 1
      }
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
    |> flag "--verbose-depth" int
        ~doc:"Recursively print types up to specified depth (default 1, implies --verbose)"
  )

let quiet_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--quiet" no_arg
      ~doc:"Suppress output about server startup"
)

let root_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--root" string
      ~doc:"Project root directory containing the .flowconfig"
)

let ignore_version_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--ignore-version" no_arg
      ~doc:"Ignore the version constraint in .flowconfig"
)

type flowconfig_params = {
  ignores: string list;
  includes: string list;
  libs: string list;
}

let list_of_string_arg = function
| None -> []
| Some arg_str -> Str.split (Str.regexp ",") arg_str

let collect_flowconfig_flags main ignores_str includes_str lib_str =
    let ignores = list_of_string_arg ignores_str in
    let includes = list_of_string_arg includes_str in
    let libs = list_of_string_arg lib_str in
    main { ignores; includes; libs; }

let ignore_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--ignore" (optional string)
    ~doc:"Specify one or more ignore patterns, comma separated"
)

let include_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--include" (optional string)
    ~doc:"Specify one or more include patterns, comma separated"
)

let libs_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--libs" (optional string)
    ~doc:"Specify one or more lib files/directories, comma separated"
)

let flowconfig_flags prev = CommandSpec.ArgSpec.(
  prev
  |> collect collect_flowconfig_flags
  |> ignore_flag
  |> include_flag
  |> libs_flag
)

(* relativize a loc's source path to a given root whenever strip_root is set *)
let relativize strip_root root loc =
  if not strip_root then loc
  else Reason.strip_root_from_loc root loc

type command_params = {
  from               : string;
  retries            : int;
  retry_if_init      : bool;
  timeout            : int;
  no_auto_start      : bool;
  temp_dir           : string option;
  shm_dirs           : string option;
  shm_min_avail      : int option;
  shm_dep_table_pow  : int option;
  shm_hash_table_pow : int option;
  shm_log_level      : int option;
  ignore_version     : bool;
  quiet              : bool;
}

let collect_server_flags
    main
    timeout
    retries
    retry_if_init
    no_auto_start
    temp_dir
    shm_dirs
    shm_min_avail
    shm_dep_table_pow
    shm_hash_table_pow
    shm_log_level
    from
    ignore_version
    quiet =
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
    temp_dir;
    shm_dirs;
    shm_min_avail;
    shm_dep_table_pow;
    shm_hash_table_pow;
    shm_log_level;
    ignore_version;
    quiet;
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
  |> shm_dirs_flag
  |> shm_min_avail_flag
  |> shm_dep_table_pow_flag
  |> shm_hash_table_pow_flag
  |> shm_log_level_flag
  |> from_flag
  |> ignore_version_flag
  |> quiet_flag
)

let ignores_of_arg root patterns extras =
  let patterns = List.rev_append extras patterns in
  List.map (fun s ->
   let root = Path.to_string root
     |> Sys_utils.normalize_filename_dir_sep in
   let reg = s
     |> Str.split_delim FlowConfig.project_root_token
     |> String.concat root
     |> Str.regexp in
    (s, reg)
  ) patterns

let includes_of_arg root paths =
  List.fold_left (fun acc path ->
    let path = Files.make_path_absolute root path in
    Path_matcher.add acc path
  ) Path_matcher.empty paths

let connect server_flags root =
  let flowconfig = Server_files_js.config_file root in
  let config_options = FlowConfig.((get flowconfig).options) in
  let normalize dir = Path.(dir |> make |> to_string) in
  let tmp_dir = Option.value_map
    ~f:normalize
    ~default:config_options.FlowConfig.Opts.temp_dir
    server_flags.temp_dir in
  let shm_dirs = Option.map
    ~f:(fun dirs -> dirs |> Str.split (Str.regexp ",") |> List.map normalize)
    server_flags.shm_dirs in
  let log_file =
    Path.to_string (Server_files_js.log_file ~tmp_dir root config_options) in
  let retries = server_flags.retries in
  let retry_if_init = server_flags.retry_if_init in
  let expiry = match server_flags.timeout with
  | 0 -> None
  | n -> Some (Unix.time () +. float n) in
  let env = { CommandConnect.
    root;
    autostart = not server_flags.no_auto_start;
    retries;
    retry_if_init;
    expiry;
    tmp_dir;
    shm_dirs;
    shm_min_avail = server_flags.shm_min_avail;
    shm_dep_table_pow = server_flags.shm_dep_table_pow;
    shm_hash_table_pow = server_flags.shm_hash_table_pow;
    shm_log_level = server_flags.shm_log_level;
    log_file;
    ignore_version = server_flags.ignore_version;
    quiet = server_flags.quiet;
  } in
  CommandConnect.connect env

let rec search_for_root config start recursion_limit : Path.t option =
  if start = Path.parent start then None (* Reach fs root, nothing to do. *)
  else if Path.file_exists (Path.concat start config) then Some start
  else if recursion_limit <= 0 then None
  else search_for_root config (Path.parent start) (recursion_limit - 1)

(* Given a valid file or directory, find a valid Flow root directory *)
(* NOTE: exists on invalid file or .flowconfig not found! *)
let guess_root dir_or_file =
  let dir_or_file = match dir_or_file with
  | Some dir_or_file -> dir_or_file
  | None -> "." in
  if not (Sys.file_exists dir_or_file) then (
    let msg = spf
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
        let msg = spf
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
    let file = Files.normalize_path (Sys.getcwd()) file in
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
  spf "%s:%d:%d,%d:%d" file l0 c0 l1 c1
)

let exe_name = Filename.basename Sys.executable_name
