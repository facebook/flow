(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

let canonicalize_filenames ?(allow_imaginary=false) ~cwd filenames =
  List.map (fun filename ->
    let filename = Sys_utils.expanduser filename in (* normalize ~ *)
    let filename = Files.normalize_path cwd filename in (* normalize ./ and ../ *)
    match Sys_utils.realpath filename with (* normalize symlinks *)
    | Some abs -> abs
    | None ->
      if allow_imaginary
      then filename
      else
        let msg = Printf.sprintf "File not found: %S" filename in
        FlowExitStatus.(exit ~msg No_input)
  ) filenames

let expand_file_list ?options filenames =
  let paths = List.map Path.make filenames in
  let next_files = match paths with
  | [] -> fun () -> []
  | _ ->
    let filter =
      begin match options with
      | Some options -> Files.is_valid_path ~options
      | _ -> fun filename -> Filename.check_suffix filename ".js"
      end in
    Find.make_next_files
      ~filter
      ~others:(List.tl paths)
      (List.hd paths) in
    Files.get_all next_files

let get_filenames_from_input ?(allow_imaginary=false) input_file filenames =
  let cwd = Sys.getcwd () in
  let input_file_filenames = match input_file with
  | Some "-" ->
    Sys_utils.lines_of_in_channel stdin
    |> canonicalize_filenames ~allow_imaginary ~cwd
  | Some input_file ->
    Sys_utils.lines_of_file input_file
    |> canonicalize_filenames ~allow_imaginary ~cwd:(Filename.dirname input_file)
  | None -> []
  in
  let cli_filenames = match filenames with
  | Some filenames -> canonicalize_filenames ~allow_imaginary ~cwd filenames
  | None -> []
  in
  cli_filenames @ input_file_filenames

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

let collect_error_flags main color include_warnings one_line show_all_errors =
  let color = match color with
  | Some "never" -> Tty.Color_Never
  | Some "always" -> Tty.Color_Always
  | Some "auto"
  | None -> Tty.Color_Auto
  | _ -> assert false (* the enum type enforces this *)
  in
  main { Errors.Cli_output.color; include_warnings; one_line; show_all_errors; }

let include_warnings_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--include-warnings" no_arg
    ~doc:"Include warnings in the error output (warnings are excluded by default)"
)

let error_flags prev = CommandSpec.ArgSpec.(
  prev
  |> collect collect_error_flags
  |> flag "--color" (enum ["auto"; "never"; "always"])
      ~doc:"Display terminal output in color. never, always, auto (default: auto)"
  |> include_warnings_flag
  |> flag "--one-line" no_arg
      ~doc:"Escapes newlines so that each error prints on one line"
  |> flag "--show-all-errors" no_arg
      ~doc:"Print all errors (the default is to truncate after 50 errors)"
)

let collect_json_flags main json pretty =
  if json || pretty then FlowExitStatus.set_json_mode ~pretty;
  main json pretty

let json_flags prev = CommandSpec.ArgSpec.(
  prev
  |> collect collect_json_flags
  |> flag "--json" no_arg ~doc:"Output results in JSON format"
  |> flag "--pretty" no_arg ~doc:"Pretty-print JSON output (implies --json)"
)

let temp_dir_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--temp-dir" string
      ~doc:"Directory in which to store temp files (default: FLOW_TEMP_DIR, or /tmp/flow/)"
      ~env:"FLOW_TEMP_DIR"
)

let collect_lazy_flags main lazy_ lazy_mode =
  main (match lazy_mode with
  | None when lazy_ -> Some Options.LAZY_MODE_FILESYSTEM
  | Some "fs" -> Some Options.LAZY_MODE_FILESYSTEM
  | Some "ide" -> Some Options.LAZY_MODE_IDE
  | _ -> None)

let lazy_flags prev = CommandSpec.ArgSpec.(
  prev
  |> collect collect_lazy_flags
  |> flag "--lazy" no_arg
      ~doc:"EXPERIMENTAL: Don't run a full check"
  |> flag "--lazy-mode" (enum ["fs"; "ide"])
      ~doc:"EXPERIMENTAL: Which type of lazy mode to use: fs or ide (default: fs, implies --lazy)"
)

let input_file_flag verb prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--input-file" string
    ~doc:("File containing list of files to " ^ verb ^ ", one per line. If -, list of files is " ^
          "read from the standard input.")
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

let log_file_flags =
  let normalize log_file =
    let dirname = Path.make (Filename.dirname log_file) in
    let basename = Filename.basename log_file in
    Path.concat dirname basename
    |> Path.to_string
  in

  let collector main server_log_file monitor_log_file =
    main (Option.map ~f:normalize server_log_file) (Option.map ~f:normalize monitor_log_file)
  in

  fun prev -> CommandSpec.ArgSpec.(
    prev
    |> collect collector
    |> flag "--log-file" string
        ~doc:"Path to log file (default: /tmp/flow/<escaped root path>.log)"
    |> flag "--monitor-log-file" string
        ~doc:"Path to log file (default: /tmp/flow/<escaped root path>.monitor_log)"
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
  untyped: string list;
  includes: string list;
  libs: string list;
  (* We store raw_lint_severities as a string list instead of as a LintSettings.t so we
   * can defer parsing of the lint settings until after the flowconfig lint settings
   * are known, to properly detect redundant settings (and avoid false positives) *)
  raw_lint_severities: string list;
}

let list_of_string_arg = function
| None -> []
| Some arg_str -> Str.split (Str.regexp ",") arg_str

let collect_flowconfig_flags main ignores_str untyped_str includes_str lib_str lints_str =
  let ignores = list_of_string_arg ignores_str in
  let untyped = list_of_string_arg untyped_str in
  let includes = list_of_string_arg includes_str in
  let libs = list_of_string_arg lib_str in
  let raw_lint_severities = list_of_string_arg lints_str in
  main { ignores; includes; libs; raw_lint_severities; untyped; }

let file_options =
  let default_lib_dir ~no_flowlib tmp_dir =
    let root = Path.make (Tmp.temp_dir tmp_dir "flowlib") in
    try
      Flowlib.extract_flowlib ~no_flowlib root;
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
  let includes_of_arg ~root ~lib_paths paths =
    (* Explicitly included paths are always added to the path_matcher *)
    let path_matcher = List.fold_left (fun acc path ->
      Path_matcher.add acc (Files.make_path_absolute root path)
    ) Path_matcher.empty paths in
    (* Implicitly included paths are added only if they're not already being watched *)
    let path_len path = path |> Path.to_string |> String.length in
    let implicitly_included_paths_sorted =
      List.sort (fun a b -> (path_len a) - (path_len b)) (root::lib_paths) (* Shortest path first *)
    in
    List.fold_left (fun acc path ->
      (* If this include is already covered by an explicit include or a shorter implicit include,
       * then skip it *)
      if Path_matcher.matches acc (Path.to_string path)
      then acc
      else Path_matcher.add acc path
    ) path_matcher implicitly_included_paths_sorted
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
    | _ -> config_libs @ (List.map (Files.make_path_absolute root) extras)
  in
  fun ~root ~no_flowlib ~temp_dir ~includes ~ignores ~libs ~untyped flowconfig ->
    let default_lib_dir =
      let no_flowlib = no_flowlib || FlowConfig.no_flowlib flowconfig in
      Some (default_lib_dir ~no_flowlib temp_dir)
    in
    let ignores = ignores_of_arg
      root
      (FlowConfig.ignores flowconfig)
      ignores in
    let untyped = ignores_of_arg
      root
      (FlowConfig.untyped flowconfig)
      untyped in
    let lib_paths = lib_paths ~root flowconfig libs in
    let includes =
      includes
      |> List.rev_append (FlowConfig.includes flowconfig)
      |> includes_of_arg ~root ~lib_paths in
    { Files.
      default_lib_dir;
      ignores;
      untyped;
      includes;
      lib_paths;
      module_file_exts = FlowConfig.module_file_exts flowconfig;
      module_resource_exts = FlowConfig.module_resource_exts flowconfig;
      node_resolver_dirnames = FlowConfig.node_resolver_dirnames flowconfig;
    }

let ignore_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--ignore" (optional string)
    ~doc:"Specify one or more ignore patterns, comma separated"
)

let untyped_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--untyped" (optional string)
    ~doc:"Specify one or more patterns, comma separated, for files to treat as untyped"
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

let lints_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--lints" (optional string)
    ~doc:"Specify one or more lint rules, comma separated"
)

let no_restart_flag prev = CommandSpec.ArgSpec.(
  prev
  |> flag "--no-auto-restart" no_arg
    ~doc:"If the server dies, do not try and restart it; just exit"
)

let flowconfig_flags prev = CommandSpec.ArgSpec.(
  prev
  |> collect collect_flowconfig_flags
  |> ignore_flag
  |> untyped_flag
  |> include_flag
  |> lib_flag
  |> lints_flag
)

type command_params = {
  from               : string;
  retries            : int;
  retry_if_init      : bool;
  timeout            : int option;
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
  (match timeout with
  | Some n when n <= 0 ->
    let msg = spf "Timeout must be a positive integer. Got %d" n in
    FlowExitStatus.(exit ~msg Commandline_usage_error)
  | _ -> ());
  main {
    from = (default "" from);
    retries = (default 3 retries);
    retry_if_init = (default true retry_if_init);
    timeout = timeout;
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

(* For commands that take both --quiet and --json or --pretty, make the latter two imply --quiet *)
let server_and_json_flags =
  let collect_server_and_json main server_flags json pretty =
    main { server_flags with
      quiet = server_flags.quiet || json || pretty
    } json pretty
  in
  fun prev ->
    prev
    |> CommandSpec.ArgSpec.collect collect_server_and_json
    |> server_flags
    |> json_flags

let server_log_file ~tmp_dir root flowconfig =
  match FlowConfig.log_file flowconfig with
  | Some x -> x
  | None -> Path.make (Server_files_js.file_of_root "log" ~tmp_dir root)

let monitor_log_file ~tmp_dir root =
  Path.make (Server_files_js.file_of_root "monitor_log" ~tmp_dir root)

module Options_flags = struct
  type t = {
    all: bool;
    debug: bool;
    flowconfig_flags: flowconfig_params;
    include_warnings: bool;
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
    merge_timeout: int;
  }
end

let parse_lints_flag =
  let number =
    let rec number' index acc = function
      | [] -> List.rev acc
      | head::tail -> number' (index + 1) ((index, head)::acc) tail
    in number' 1 []
  in

  fun base_settings flag_settings ->
    let lines = number flag_settings in
    match LintSettings.of_lines base_settings lines with
    | Ok settings -> settings
    | Error (line, msg) ->
      let msg = spf "Error parsing --lints (rule %d): %s" line msg in
      FlowExitStatus.(exit ~msg Commandline_usage_error)

let options_flags =
  let collect_options_flags main
    debug profile all weak traces no_flowlib munge_underscore_members max_workers
    include_warnings flowconfig_flags verbose strip_root temp_dir quiet merge_timeout =
    if merge_timeout < 0
    then FlowExitStatus.(exit ~msg:"--merge-timeout must be non-negative" Commandline_usage_error);
    main { Options_flags.
      debug;
      profile;
      all;
      weak;
      traces;
      no_flowlib;
      munge_underscore_members;
      max_workers;
      include_warnings;
      flowconfig_flags;
      verbose;
      strip_root;
      temp_dir;
      quiet;
      merge_timeout;
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
    |> include_warnings_flag
    |> flowconfig_flags
    |> verbose_flags
    |> strip_root_flag
    |> temp_dir_flag
    |> quiet_flag
    |> flag "--merge-timeout" (required ~default:100 int)
      ~doc:("The maximum time in seconds to attempt to typecheck a file or cycle of files. " ^
        "0 means no timeout (default: 100)")
      ~env:"FLOW_MERGE_TIMEOUT"

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

let make_options ~flowconfig ~lazy_mode ~root (options_flags: Options_flags.t) =
  let temp_dir =
    options_flags.Options_flags.temp_dir
    |> Option.value ~default:(FlowConfig.temp_dir flowconfig)
    |> Path.make
    |> Path.to_string
  in
  let open Options_flags in
  let file_options =
    let no_flowlib = options_flags.no_flowlib in
    let {
      includes;
      ignores;
      libs;
      raw_lint_severities=_;
      untyped;
    } = options_flags.flowconfig_flags in
    file_options ~root ~no_flowlib ~temp_dir ~includes ~ignores ~libs ~untyped flowconfig
  in
  let lint_severities = parse_lints_flag
    (FlowConfig.lint_severities flowconfig) options_flags.flowconfig_flags.raw_lint_severities
  in
  let strict_mode = FlowConfig.strict_mode flowconfig in
  { Options.
    opt_lazy_mode = lazy_mode;
    opt_root = root;
    opt_debug = options_flags.debug;
    opt_verbose = options_flags.verbose;
    opt_all = options_flags.all || FlowConfig.all flowconfig;
    opt_weak = options_flags.weak || FlowConfig.weak flowconfig;
    opt_traces = Option.value options_flags.traces ~default:(FlowConfig.traces flowconfig);
    opt_quiet = options_flags.Options_flags.quiet;
    opt_module_name_mappers = FlowConfig.module_name_mappers flowconfig;
    opt_modules_are_use_strict = FlowConfig.modules_are_use_strict flowconfig;
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
    opt_enforce_strict_call_arity = FlowConfig.enforce_strict_call_arity flowconfig;
    opt_esproposal_decorators = FlowConfig.esproposal_decorators flowconfig;
    opt_esproposal_export_star_as = FlowConfig.esproposal_export_star_as flowconfig;
    opt_facebook_fbt = FlowConfig.facebook_fbt flowconfig;
    opt_ignore_non_literal_requires = FlowConfig.ignore_non_literal_requires flowconfig;
    opt_include_warnings = options_flags.include_warnings || FlowConfig.include_warnings flowconfig;
    opt_esproposal_class_static_fields = FlowConfig.esproposal_class_static_fields flowconfig;
    opt_esproposal_class_instance_fields =
      FlowConfig.esproposal_class_instance_fields flowconfig;
    opt_max_header_tokens = FlowConfig.max_header_tokens flowconfig;
    opt_haste_name_reducers = FlowConfig.haste_name_reducers flowconfig;
    opt_haste_paths_blacklist = FlowConfig.haste_paths_blacklist flowconfig;
    opt_haste_paths_whitelist = FlowConfig.haste_paths_whitelist flowconfig;
    opt_haste_use_name_reducers = FlowConfig.haste_use_name_reducers flowconfig;
    opt_file_options = file_options;
    opt_lint_severities = lint_severities;
    opt_strict_mode = strict_mode;
    opt_merge_timeout =
      if options_flags.merge_timeout = 0
      then None
      else Some (float_of_int options_flags.merge_timeout);
  }

let connect ~client_type server_flags root =
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
    Path.to_string (server_log_file ~tmp_dir root flowconfig) in
  let retries = server_flags.retries in
  let retry_if_init = server_flags.retry_if_init in
  let expiry = match server_flags.timeout with
  | None -> None
  | Some n -> Some (Unix.time () +. float n) in
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
  CommandConnect.connect ~client_type env

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

let get_file_from_filename_or_stdin ~cmd path = function
  | Some filename ->
      if Sys.is_directory filename then
        let msg = spf
          "Provided argument %s is not a file; canceling.\
          \nSee \"flow %s --help\" for more info"
          filename cmd in
        FlowExitStatus.(exit ~msg Path_is_not_a_file)
      else
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

(* What should we do when we connect to the flow server monitor, but it dies before responding to
 * us? Well, we should consume a retry and try to connect again, potentially even starting a new
 * server *)
let rec connect_and_make_request =
  (* Sends the command over the socket *)
  let send_command ?timeout (oc:out_channel) (cmd:ServerProt.Request.command): unit =
    let command = { ServerProt.Request.
      client_logging_context = FlowEventLogger.get_context ();
      command = cmd;
    } in
    Marshal_tools.to_fd_with_preamble ?timeout (Unix.descr_of_out_channel oc) command;
    flush oc
  in

  (* Waits for a response over the socket. If the connection dies, this will throw an exception *)
  let rec wait_for_response ?timeout ~quiet ~root (ic: Timeout.in_channel) =
    let use_emoji = Tty.supports_emoji () &&
      Server_files_js.config_file root
      |> FlowConfig.get
      |> FlowConfig.emoji in

    let response: MonitorProt.monitor_to_client_message = try
      Marshal_tools.from_fd_with_preamble ?timeout (Timeout.descr_of_in_channel ic)
    with
    | Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET), _, _) ->
      if not quiet && Tty.spinner_used () then Tty.print_clear_line stderr;
      raise End_of_file
    | exn ->
      if not quiet && Tty.spinner_used () then Tty.print_clear_line stderr;
      raise exn
    in

    match response with
    (* Let's ignore messages from the server that it is free. It's a confusing message for the
     * user *)
    | MonitorProt.Please_hold status when ServerStatus.is_free status ->
      wait_for_response ?timeout ~quiet ~root ic
    (* The server is busy, so we print the server's status and keep reading from the socket *)
    | MonitorProt.Please_hold status ->
      if not quiet then begin
        if Tty.spinner_used () then Tty.print_clear_line stderr;
        Printf.eprintf
          "Please wait. %s: %s%!"
          (ServerStatus.string_of_status ~use_emoji status)
          (Tty.spinner())
      end;
      wait_for_response ?timeout ~quiet ~root ic
    | MonitorProt.Data response ->
      if not quiet && Tty.spinner_used () then Tty.print_clear_line stderr;
      response
    | MonitorProt.ServerException exn_str ->
      if Tty.spinner_used () then Tty.print_clear_line stderr;
      let msg = Utils_js.spf "Server threw an exception: %s" exn_str in
      FlowExitStatus.(exit ~msg Unknown_error)
  in

  fun ?timeout ?retries server_flags root request ->
    let retries = match retries with
    | None -> server_flags.retries
    | Some retries -> retries in

    if retries < 0
    then FlowExitStatus.(exit ~msg:"Out of retries, exiting!" Out_of_retries);

    let quiet = server_flags.quiet in
    (* connect handles timeouts itself *)
    let ic, oc = connect ~client_type:SocketHandshake.Ephemeral server_flags root in
    send_command ?timeout oc request;
    try wait_for_response ?timeout ~quiet ~root ic
    with End_of_file ->
      if not quiet then begin
        Printf.eprintf
          "Lost connection to the flow server (%d %s remaining)\n%!"
          retries
          (if retries = 1 then "retry" else "retries")
      end;
      connect_and_make_request ?timeout ~retries:(retries - 1) server_flags root request

(* If --timeout is set, wrap connect_and_make_request in a timeout *)
let connect_and_make_request ?retries server_flags root request =
  match server_flags.timeout with
  | None ->
    connect_and_make_request ?retries server_flags root request
  | Some timeout ->
    Timeout.with_timeout
      ~timeout
      ~on_timeout:(fun () -> FlowExitStatus.(exit ~msg:"Timeout exceeded, exiting" Out_of_time))
      ~do_:(fun timeout -> connect_and_make_request ~timeout ?retries server_flags root request)

let failwith_bad_response ~request ~response =
  let msg = Printf.sprintf
    "Bad response to %S: received %S"
    (ServerProt.Request.to_string request)
    (ServerProt.Response.to_string response) in
  failwith msg
