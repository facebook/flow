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
    Flow_version.version

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

let init_loggers ~from ~options ?(default=Hh_logger.Level.default_filter) () =
  FlowEventLogger.set_from from;
  Hh_logger.Level.set_filter (
    let verbose = Options.verbose options in
    let debug = Options.is_debug_mode options in
    let quiet = Options.is_quiet options in
    if quiet then (function _ -> false)
    else if verbose != None || debug then (function _ -> true)
    else default
  )

let collect_error_flags main color one_line show_all_errors =
  let color = match color with
  | Some "never" -> Tty.Color_Never
  | Some "always" -> Tty.Color_Always
  | Some "auto"
  | None -> Tty.Color_Auto
  | _ -> assert false (* the enum type enforces this *)
  in
  main { Errors.Cli_output.color; one_line; show_all_errors; }

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
  |> flag "--pretty" no_arg ~doc:"Pretty-print JSON output (implies --json)"
)

let temp_dir_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--temp-dir" string
      ~doc:"Directory in which to store temp files (default: /tmp/flow/)"
)

type shared_mem_params = {
  shm_dirs: string option;
  shm_min_avail: int option;
  shm_dep_table_pow: int option;
  shm_hash_table_pow: int option;
  shm_log_level: int option;
}

let collect_shm_flags main
    shm_dirs shm_min_avail shm_dep_table_pow shm_hash_table_pow shm_log_level =
  main { shm_dirs; shm_min_avail; shm_dep_table_pow; shm_hash_table_pow; shm_log_level; }

let shm_flags prev = CommandSpec.ArgSpec.(
  prev
  |> collect collect_shm_flags
  |> flag "--sharedmemory-dirs" string
      ~doc:"Directory in which to store shared memory heap (default: /dev/shm/)"
  |> flag "--sharedmemory-minimum-available" int
      ~doc:"Flow will only use a filesystem for shared memory if it has at \
        least these many bytes available (default: 536870912 - which is 512MB)"
  |> flag "--sharedmemory-dep-table-pow" int
      ~doc:"The exponent for the size of the shared memory dependency table. \
        The default is 17, implying a size of 2^17 bytes"
  |> flag "--sharedmemory-hash-table-pow" int
      ~doc:"The exponent for the size of the shared memory hash table. \
        The default is 19, implying a size of 2^19 bytes"
  |> flag "--sharedmemory-log-level" int
      ~doc:"The logging level for shared memory statistics. \
        0=none, 1=some"
)

let shm_config shm_flags flowconfig =
  let dep_table_pow = Option.value shm_flags.shm_dep_table_pow
    ~default:(FlowConfig.shm_dep_table_pow flowconfig) in
  let hash_table_pow = Option.value shm_flags.shm_hash_table_pow
    ~default:(FlowConfig.shm_hash_table_pow flowconfig) in
  let shm_dirs = Option.value_map shm_flags.shm_dirs
    ~default:(FlowConfig.shm_dirs flowconfig)
    ~f:(Str.split (Str.regexp ","))
    |> List.map Path.(fun dir -> dir |> make |> to_string) in
  let shm_min_avail = Option.value shm_flags.shm_min_avail
    ~default:(FlowConfig.shm_min_avail flowconfig) in
  let log_level = Option.value shm_flags.shm_log_level
    ~default:(FlowConfig.shm_log_level flowconfig) in
  { SharedMem_js.
    global_size = FlowConfig.shm_global_size flowconfig;
    heap_size = FlowConfig.shm_heap_size flowconfig;
    dep_table_pow;
    hash_table_pow;
    shm_dirs;
    shm_min_avail;
    log_level;
  }

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

let assert_version flowconfig =
  match FlowConfig.required_version flowconfig with
  | None -> ()
  | Some version_constraint ->
    if not (Semver.satisfies version_constraint Flow_version.version)
    then
      let msg = Utils_js.spf
        "Wrong version of Flow. The config specifies version %s but this is version %s"
        version_constraint
        Flow_version.version
      in
      FlowExitStatus.(exit ~msg Invalid_flowconfig)

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

let file_options =
  let default_lib_dir tmp_dir =
    let root = Path.make (Tmp.temp_dir tmp_dir "flowlib") in
    try
      Flowlib.extract_flowlib root;
      root
    with _ ->
      let msg = "Could not locate flowlib files" in
      FlowExitStatus.(exit ~msg Could_not_find_flowconfig)
  in
  let ignores_of_arg root patterns extras =
    let patterns = List.rev_append extras patterns in
    List.map (fun s ->
     let root = Path.to_string root
       |> Sys_utils.normalize_filename_dir_sep in
     let reg = s
       |> Str.split_delim Files.project_root_token
       |> String.concat root
       |> Str.regexp in
      (s, reg)
    ) patterns
  in
  let includes_of_arg root paths =
    List.fold_left (fun acc path ->
      let path = Files.make_path_absolute root path in
      Path_matcher.add acc path
    ) Path_matcher.empty paths
  in
  let lib_paths ~root flowconfig extras =
    let flowtyped_path = Files.get_flowtyped_path root in
    let has_explicit_flowtyped_lib = ref false in
    let config_libs =
      List.fold_right (fun lib abs_libs ->
        let abs_lib = Files.make_path_absolute root lib in
        (**
         * "flow-typed" is always included in the libs list for convenience,
         * but there's no guarantee that it exists on the filesystem.
         *)
        if abs_lib = flowtyped_path then has_explicit_flowtyped_lib := true;
        abs_lib::abs_libs
      ) (FlowConfig.libs flowconfig) []
    in
    let config_libs =
      if !has_explicit_flowtyped_lib = false
         && (Sys.file_exists (Path.to_string flowtyped_path))
      then flowtyped_path::config_libs
      else config_libs
    in
    match extras with
    | [] -> config_libs
    | _ -> config_libs @ (List.map Path.make extras)
  in
  fun ~root ~no_flowlib ~temp_dir ~includes ~ignores ~libs flowconfig ->
    let default_lib_dir =
      if no_flowlib || FlowConfig.no_flowlib flowconfig
      then None
      else Some (default_lib_dir temp_dir) in
    let ignores = ignores_of_arg
      root
      (FlowConfig.ignores flowconfig)
      ignores in
    let includes =
      includes
      |> List.rev_append (FlowConfig.includes flowconfig)
      |> includes_of_arg root in
    { Files.
      default_lib_dir;
      ignores;
      includes;
      lib_paths = lib_paths ~root flowconfig libs;
      module_file_exts = FlowConfig.module_file_exts flowconfig;
      module_resource_exts = FlowConfig.module_resource_exts flowconfig;
      node_resolver_dirnames = FlowConfig.node_resolver_dirnames flowconfig;
    }

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

let lib_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--lib" (optional string)
    ~doc:"Specify one or more lib files/directories, comma separated"
)

let flowconfig_flags prev = CommandSpec.ArgSpec.(
  prev
  |> collect collect_flowconfig_flags
  |> ignore_flag
  |> include_flag
  |> lib_flag
)

type command_params = {
  from               : string;
  retries            : int;
  retry_if_init      : bool;
  timeout            : int;
  no_auto_start      : bool;
  temp_dir           : string option;
  shm_flags          : shared_mem_params;
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
    shm_flags
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
    shm_flags;
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
  |> shm_flags
  |> from_flag
  |> ignore_version_flag
  |> quiet_flag
)

let lints_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--lints" (optional string)
    ~doc:"Specify one or more lint rules, comma separated"
)

let log_file ~tmp_dir root flowconfig =
  match FlowConfig.log_file flowconfig with
  | Some x -> x
  | None -> Path.make (Server_files_js.file_of_root "log" ~tmp_dir root)

module Options_flags = struct
  type t = {
    all: bool;
    debug: bool;
    flowconfig_flags: flowconfig_params;
    (* Defer parsing of the lints flag until after the fowconfig lint settings is known,
     * to properly detect redundant settings (and avoid false positives) *)
    lints_flag: string option;
    max_workers: int option;
    munge_underscore_members: bool;
    no_flowlib: bool;
    profile: bool;
    quiet: bool;
    strip_root: bool;
    temp_dir: string option;
    traces: int option;
    verbose: Verbose.t option;
    weak: bool;
  }
end

let parse_lints_flag base_settings flag =
    let flag = Option.value flag ~default:"" in
    let lines = Str.split_delim (Str.regexp ",") flag
      |> List.map (fun s -> (1, s)) in
    match LintSettings.of_lines base_settings lines with
    | Ok settings -> settings
    | Error (_, msg) ->
      let msg = spf "Error parsing --lints: %s" msg in
      FlowExitStatus.(exit ~msg Commandline_usage_error)

let options_flags =
  let collect_options_flags main
    debug profile all weak traces no_flowlib munge_underscore_members max_workers
    flowconfig_flags lints_flag verbose strip_root temp_dir quiet =
    main { Options_flags.
      debug;
      profile;
      all;
      weak;
      traces;
      no_flowlib;
      munge_underscore_members;
      max_workers;
      flowconfig_flags;
      lints_flag;
      verbose;
      strip_root;
      temp_dir;
      quiet;
   }
  in
  fun prev ->
    let open CommandSpec.ArgSpec in
    prev
    |> collect collect_options_flags
    |> flag "--debug" no_arg
        ~doc:"Print debug info during typecheck"
    |> flag "--profile" no_arg
        ~doc:"Output profiling information"
    |> flag "--all" no_arg
        ~doc:"Typecheck all files, not just @flow"
    |> flag "--weak" no_arg
        ~doc:"Typecheck with weak inference, assuming dynamic types by default"
    |> flag "--traces" (optional int)
        ~doc:"Outline an error path up to a specified level"
    |> flag "--no-flowlib" no_arg
        ~doc:"Do not include embedded declarations"
    |> flag "--munge-underscore-members" no_arg
        ~doc:"Treat any class member name with a leading underscore as private"
    |> flag "--max-workers" (optional int)
        ~doc:"Maximum number of workers to create (capped by number of cores)"
    |> flowconfig_flags
    |> lints_flag
    |> verbose_flags
    |> strip_root_flag
    |> temp_dir_flag
    |> quiet_flag

(* For commands that take both --quiet and --json or --pretty, make the latter two imply --quiet *)
let options_and_json_flags =
  let collect_options_and_json main options_flags json pretty =
    main { options_flags with
      Options_flags.quiet = options_flags.Options_flags.quiet || json || pretty
    } json pretty
  in
  fun prev ->
    prev
    |> CommandSpec.ArgSpec.collect collect_options_and_json
    |> options_flags
    |> json_flags

let make_options ~flowconfig ~lazy_ ~root (options_flags: Options_flags.t) =
  let temp_dir =
    options_flags.Options_flags.temp_dir
    |> Option.value ~default:(FlowConfig.temp_dir flowconfig)
    |> Path.make
    |> Path.to_string
  in
  let open Options_flags in
  let file_options =
    let no_flowlib = options_flags.no_flowlib in
    let { includes; ignores; libs } = options_flags.flowconfig_flags in
    file_options ~root ~no_flowlib ~temp_dir ~includes ~ignores ~libs flowconfig
  in
  let lint_settings =
    parse_lints_flag (FlowConfig.lint_settings flowconfig) options_flags.lints_flag
  in
  { Options.
    opt_lazy = lazy_;
    opt_root = root;
    opt_debug = options_flags.debug;
    opt_verbose = options_flags.verbose;
    opt_all = options_flags.all || FlowConfig.all flowconfig;
    opt_weak = options_flags.weak || FlowConfig.weak flowconfig;
    opt_traces = Option.value options_flags.traces ~default:(FlowConfig.traces flowconfig);
    opt_quiet = options_flags.Options_flags.quiet;
    opt_module_name_mappers = FlowConfig.module_name_mappers flowconfig;
    opt_modules_are_use_strict = FlowConfig.modules_are_use_strict flowconfig;
    opt_output_graphml = false;
    opt_profile = options_flags.profile;
    opt_strip_root = options_flags.strip_root;
    opt_module = FlowConfig.module_system flowconfig;
    opt_munge_underscores =
      options_flags.munge_underscore_members || FlowConfig.munge_underscores flowconfig;
    opt_temp_dir = temp_dir;
    opt_max_workers =
      Option.value options_flags.max_workers ~default:(FlowConfig.max_workers flowconfig)
      |> min Sys_utils.nbr_procs;
    opt_suppress_comments = FlowConfig.suppress_comments flowconfig;
    opt_suppress_types = FlowConfig.suppress_types flowconfig;
    opt_enable_const_params = FlowConfig.enable_const_params flowconfig;
    opt_enforce_strict_type_args = FlowConfig.enforce_strict_type_args flowconfig;
    opt_enforce_strict_call_arity = FlowConfig.enforce_strict_call_arity flowconfig;
    opt_enable_unsafe_getters_and_setters =
      FlowConfig.enable_unsafe_getters_and_setters flowconfig;
    opt_esproposal_decorators = FlowConfig.esproposal_decorators flowconfig;
    opt_esproposal_export_star_as = FlowConfig.esproposal_export_star_as flowconfig;
    opt_facebook_fbt = FlowConfig.facebook_fbt flowconfig;
    opt_ignore_non_literal_requires = FlowConfig.ignore_non_literal_requires flowconfig;
    opt_esproposal_class_static_fields = FlowConfig.esproposal_class_static_fields flowconfig;
    opt_esproposal_class_instance_fields =
      FlowConfig.esproposal_class_instance_fields flowconfig;
    opt_max_header_tokens = FlowConfig.max_header_tokens flowconfig;
    opt_haste_name_reducers = FlowConfig.haste_name_reducers flowconfig;
    opt_haste_paths_blacklist = FlowConfig.haste_paths_blacklist flowconfig;
    opt_haste_paths_whitelist = FlowConfig.haste_paths_whitelist flowconfig;
    opt_haste_use_name_reducers = FlowConfig.haste_use_name_reducers flowconfig;
    opt_file_options = file_options;
    opt_lint_settings = lint_settings;
  }

let connect server_flags root =
  let flowconfig_path = Server_files_js.config_file root in
  let flowconfig = FlowConfig.get flowconfig_path in
  let normalize dir = Path.(dir |> make |> to_string) in
  let tmp_dir = Option.value_map
    ~f:normalize
    ~default:(FlowConfig.temp_dir flowconfig)
    server_flags.temp_dir in
  let shm_dirs = Option.map
    ~f:(fun dirs -> dirs |> Str.split (Str.regexp ",") |> List.map normalize)
    server_flags.shm_flags.shm_dirs in
  let log_file =
    Path.to_string (log_file ~tmp_dir root flowconfig) in
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
    shm_min_avail = server_flags.shm_flags.shm_min_avail;
    shm_dep_table_pow = server_flags.shm_flags.shm_dep_table_pow;
    shm_hash_table_pow = server_flags.shm_flags.shm_hash_table_pow;
    shm_log_level = server_flags.shm_flags.shm_log_level;
    log_file;
    ignore_version = server_flags.ignore_version;
    emoji = FlowConfig.emoji flowconfig;
    quiet = server_flags.quiet;
  } in
  CommandConnect.connect env

let rec search_for_root config start recursion_limit : Path.t option =
  if start = Path.parent start then None (* Reach fs root, nothing to do. *)
  else if Path.file_exists (Path.concat start config) then Some start
  else if recursion_limit <= 0 then None
  else search_for_root config (Path.parent start) (recursion_limit - 1)

(* Given a valid file or directory, find a valid Flow root directory *)
(* NOTE: exits on invalid file or .flowconfig not found! *)
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
      File_input.FileName (expand_path filename)
  | None ->
      let contents = Sys_utils.read_stdin_to_string () in
      let filename = (match path with
        | Some ""
        | None -> None
        | Some str -> Some (get_path_of_file str)
      ) in
      File_input.FileContent (filename, contents)

let range_string_of_loc ~strip_root loc = Loc.(
  let file = match loc.source with
  | Some file -> Reason.string_of_source ~strip_root file
  | None -> ""
  in
  let l0, c0 = loc.start.line, loc.start.column + 1 in
  let l1, c1 = loc._end.line, loc._end.column in
  spf "%s:%d:%d,%d:%d" file l0 c0 l1 c1
)

let exe_name = Utils_js.exe_name

let send_command (oc:out_channel) (cmd:ServerProt.command): unit =
  let command = { ServerProt.
    client_logging_context = FlowEventLogger.get_context ();
    command = cmd;
  } in
  Marshal.to_channel oc command [];
  flush oc

let wait_for_response (ic:Timeout.in_channel): ServerProt.response =
  Timeout.input_value ic
