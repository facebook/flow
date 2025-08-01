(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

let run_command command argv =
  try
    let args = CommandSpec.args_of_argv command argv in
    CommandSpec.run command args
  with
  | CommandSpec.Show_help ->
    print_endline (CommandSpec.string_of_usage command);
    Exit.(exit No_error)
  | CommandSpec.Failed_to_parse { arg; msg; details } ->
    let is_pretty_or_json_arg s =
      String.starts_with ~prefix:"--pretty" s || String.starts_with ~prefix:"--json" s
    in
    begin
      match Base.List.find ~f:is_pretty_or_json_arg argv with
      | Some json_arg ->
        let pretty = String.starts_with ~prefix:"--pretty" json_arg in
        Exit.set_json_mode ~pretty
      | None -> ()
    end;
    let details =
      match details with
      | Some x -> Printf.sprintf " (%s)" x
      | None -> ""
    in
    let msg = Printf.sprintf "%s: %s%s" msg arg details in
    Exit.(exit ~msg Commandline_usage_error)

let expand_file_list ?options filenames =
  let paths = Base.List.map ~f:File_path.make filenames in
  let next_files =
    match paths with
    | [] -> (fun () -> [])
    | _ ->
      let filter =
        match options with
        | Some options -> Files.is_valid_path ~options
        | _ -> (fun filename -> Filename.check_suffix filename ".js")
      in
      Find.make_next_files ~filter ~others:(Base.List.tl_exn paths) (Base.List.hd_exn paths)
  in
  Files.get_all next_files

let get_filenames_from_input ?(allow_imaginary = false) input_file filenames =
  let cwd = Sys.getcwd () in
  let handle_imaginary =
    if allow_imaginary then
      Files.imaginary_realpath
    else
      fun fn ->
    Exit.(exit ~msg:(Printf.sprintf "File not found: %S" fn) No_input)
  in
  let input_file_filenames =
    match input_file with
    | Some "-" ->
      Sys_utils.lines_of_in_channel stdin |> Files.canonicalize_filenames ~handle_imaginary ~cwd
    | Some input_file ->
      Sys_utils.lines_of_file input_file
      |> Files.canonicalize_filenames ~handle_imaginary ~cwd:(Filename.dirname input_file)
    | None -> []
  in
  let cli_filenames =
    match filenames with
    | Some filenames -> Files.canonicalize_filenames ~handle_imaginary ~cwd filenames
    | None -> []
  in
  cli_filenames @ input_file_filenames

let print_version () =
  print_endlinef "Flow, a static type checker for JavaScript, version %s" Flow_version.version

let expand_path file =
  let path = File_path.make file in
  if File_path.file_exists path then
    File_path.to_string path
  else
    let file = Filename.concat (Sys.getcwd ()) file in
    let path = File_path.make file in
    if File_path.file_exists path then
      File_path.to_string path
    else
      let msg = Printf.sprintf "File not found: %s" (File_path.to_string path) in
      Exit.(exit ~msg Input_error)

let collect_error_flags
    main
    rendering_mode
    include_warnings
    max_warnings
    one_line
    list_files
    show_all_errors
    show_all_branches
    unicode
    message_width =
  let include_warnings =
    match max_warnings with
    | Some _ -> true
    | None -> include_warnings
  in
  let unicode =
    match unicode with
    | `Never -> false
    | `Always -> true
    | `Auto -> Tty.supports_emoji ()
  in
  let message_width =
    match message_width with
    | Some message_width -> message_width
    | None -> Base.Option.value_map (Tty.get_term_cols ()) ~default:120 ~f:(min 120)
  in
  main
    {
      Flow_errors_utils.Cli_output.rendering_mode;
      include_warnings;
      max_warnings;
      one_line;
      list_files;
      show_all_errors;
      show_all_branches;
      unicode;
      message_width;
    }

let warning_flags prev =
  CommandSpec.ArgSpec.(
    prev
    |> flag
         "--include-warnings"
         truthy
         ~doc:"Include warnings in the error output (warnings are excluded by default)"
    |> flag
         "--max-warnings"
         int
         ~doc:
           "Warnings above this number will cause a nonzero exit code (implies --include-warnings)"
  )

let profile_flag prev =
  let open CommandSpec.ArgSpec in
  prev |> flag "--profile" truthy ~doc:"Output profiling information" ~env:"FLOW_PROFILE"

let error_flags prev =
  CommandSpec.ArgSpec.(
    prev
    |> collect collect_error_flags
    |> flag
         "--color"
         (required
            ~default:Flow_errors_utils.Cli_output.CLI_Color_Auto
            (enum
               [
                 ("never", Flow_errors_utils.Cli_output.CLI_Color_Never);
                 ("always", Flow_errors_utils.Cli_output.CLI_Color_Always);
                 ("auto", Flow_errors_utils.Cli_output.CLI_Color_Auto);
                 ( "unstable_ide_mode_EXPOSED_FOR_TESTING",
                   Flow_errors_utils.Cli_output.IDE_Detailed_Error
                 );
               ]
            )
         )
         ~doc:"Display terminal output in color. never, always, auto (default: auto)"
    |> warning_flags
    |> flag "--one-line" truthy ~doc:"Escapes newlines so that each error prints on one line"
    |> flag "--list-files" truthy ~doc:"List files with errors"
    |> flag
         "--show-all-errors"
         truthy
         ~doc:"Print all errors (the default is to truncate after 50 errors)"
    |> flag
         "--show-all-branches"
         truthy
         ~doc:"Print all branch errors (the default is to print the most relevant branches)"
    |> flag
         "--unicode"
         (required ~default:`Auto (enum [("never", `Never); ("always", `Always); ("auto", `Auto)]))
         ~doc:"Display terminal output with unicode decoration. never, always, auto (default: auto)"
    |> flag
         "--message-width"
         int
         ~doc:
           "Sets the width of messages but not code snippets (defaults to the smaller of 120 or the terminal width)"
  )

let collect_json_flags main json pretty =
  if json || pretty then Exit.set_json_mode ~pretty;
  main json pretty

let json_flags prev =
  CommandSpec.ArgSpec.(
    prev
    |> collect collect_json_flags
    |> flag "--json" truthy ~doc:"Output results in JSON format"
    |> flag "--pretty" truthy ~doc:"Pretty-print JSON output (implies --json)"
  )

let temp_dir_flag prev =
  CommandSpec.ArgSpec.(
    prev
    |> flag
         "--temp-dir"
         string
         ~doc:"Directory in which to store temp files (default: FLOW_TEMP_DIR, or /tmp/flow/)"
         ~env:"FLOW_TEMP_DIR"
  )

let collect_lazy_flags main lazy_ lazy_mode =
  let lazy_mode =
    if lazy_ && Base.Option.is_none lazy_mode then
      (* --lazy === --lazy-mode true *)
      Some FlowConfig.Lazy
    else
      lazy_mode
  in
  main lazy_mode

let lazy_flags prev =
  CommandSpec.ArgSpec.(
    prev
    |> collect collect_lazy_flags
    |> flag "--lazy" truthy ~doc:"Only check changed files. Shorthand for `--lazy-mode true`"
    |> flag
         "--lazy-mode"
         (enum
            [
              ("true", FlowConfig.Lazy);
              ("false", FlowConfig.Non_lazy);
              (* legacy, deprecated options *)
              ("fs", FlowConfig.Lazy);
              ("watchman", FlowConfig.Watchman_DEPRECATED);
              ("none", FlowConfig.Non_lazy);
            ]
         )
         ~doc:"If true, only check changed files"
         ~env:"FLOW_LAZY_MODE"
  )

let input_file_flag verb prev =
  CommandSpec.ArgSpec.(
    prev
    |> flag
         "--input-file"
         string
         ~doc:
           ("File containing list of files to "
           ^ verb
           ^ ", one per line. If -, list of files is "
           ^ "read from the standard input."
           )
  )

let verbose_focus_flag prev =
  CommandSpec.ArgSpec.(
    prev
    |> flag
         "--verbose-focus"
         truthy
         ~doc:"Print verbose output about target file only (implies --verbose)"
  )

type shared_mem_params = {
  shm_heap_size: int option;
  shm_hash_table_pow: int option;
}

let collect_shm_flags main shm_heap_size shm_hash_table_pow =
  main { shm_heap_size; shm_hash_table_pow }

let shm_flags prev =
  CommandSpec.ArgSpec.(
    prev
    |> collect collect_shm_flags
    |> flag
         "--sharedmemory-heap-size"
         int
         ~doc:
           "The maximum size of the shared memory heap. The default is 26843545600 (25 * 2^30 bytes = 25 GiB)"
         ~env:"FLOW_SHAREDMEM_HEAP_SIZE"
    |> flag
         "--sharedmemory-hash-table-pow"
         int
         ~doc:
           "The exponent for the size of the shared memory hash table. The default is 19, implying a size of 2^19 bytes"
  )

let shm_config shm_flags flowconfig =
  let heap_size =
    Base.Option.value shm_flags.shm_heap_size ~default:(FlowConfig.shm_heap_size flowconfig)
  in
  let hash_table_pow =
    Base.Option.value
      shm_flags.shm_hash_table_pow
      ~default:(FlowConfig.shm_hash_table_pow flowconfig)
  in
  { SharedMem.heap_size; hash_table_pow }

let from_flag =
  let collector main from =
    let from =
      match from with
      | Some from -> Some from
      | None ->
        Base.Result.(
          let parent_cmdline =
            Proc_utils.get_proc_stat (Unix.getpid ()) >>= fun proc_stat ->
            let ppid = proc_stat.Proc_utils.ppid in
            Proc_utils.get_proc_stat ppid >>| fun parent_proc_stat ->
            String.trim parent_proc_stat.Proc_utils.cmdline
          in
          (match parent_cmdline with
          | Ok cmdline -> Some ("parent cmdline: " ^ cmdline)
          | Error _ -> None)
        )
    in
    FlowEventLogger.set_from from;
    main
  in
  fun prev ->
    CommandSpec.ArgSpec.(
      prev
      |> collect collector
      |> flag
           "--from"
           (optional string)
           ~doc:"Specify who is calling this CLI command (used by logging)"
    )

let strip_root_flag prev =
  CommandSpec.ArgSpec.(prev |> flag "--strip-root" truthy ~doc:"Print paths without the root")

let wait_for_recheck_flag prev =
  CommandSpec.ArgSpec.(
    prev
    |> flag
         "--wait-for-recheck"
         (optional bool)
         ~doc:
           ("If the server is rechecking, wait for it to complete rather than run sooner using "
           ^ "outdated data"
           )
  )

let vpn_less_flag prev =
  CommandSpec.ArgSpec.(
    prev
    |> flag
         "--vpn-less"
         (optional bool)
         ~doc:"True if enable the VPN-Less mode to query and download saved state"
  )

let path_flag prev =
  CommandSpec.ArgSpec.(
    prev
    |> flag
         "--path"
         (optional string)
         ~doc:"Specify (fake) path to file when reading data from stdin"
  )

let autostop_flag prev =
  CommandSpec.ArgSpec.(prev |> flag "--autostop" truthy ~doc:"" (* empty to omit it from --help *))

let verbose_flags =
  let collector main verbose indent depth enabled_during_flowlib =
    let opt_verbose =
      if verbose || indent || depth != None then
        Some
          {
            Verbose.indent =
              ( if indent then
                2
              else
                0
              );
            depth =
              (match depth with
              | Some n when n >= 0 -> n
              | _ -> 1);
            enabled_during_flowlib;
            focused_files = None;
          }
      else
        None
    in
    main opt_verbose
  in
  fun prev ->
    CommandSpec.ArgSpec.(
      prev
      |> collect collector
      |> flag "--verbose" truthy ~doc:"Print verbose info during typecheck"
      |> flag
           "--verbose-indent"
           truthy
           ~doc:"Indent verbose info during typecheck (implies --verbose)"
      |> flag
           "--verbose-depth"
           int
           ~doc:"Recursively print types up to specified depth (default 1, implies --verbose)"
      |> flag "--verbose-flowlib" truthy ~doc:"Print verbose info while initializing the flowlib"
    )

let slow_to_check_logging_flags =
  let collector
      main
      slow_files_logging_interval
      slow_components_logging_threshold
      slow_expressions_logging_threshold =
    main
      {
        Slow_to_check_logging.slow_files_logging_internal =
          Option.map float_of_int slow_files_logging_interval;
        slow_components_logging_threshold =
          Option.map float_of_int slow_components_logging_threshold;
        slow_expressions_logging_threshold =
          Option.map float_of_int slow_expressions_logging_threshold;
      }
  in
  fun prev ->
    CommandSpec.ArgSpec.(
      prev
      |> collect collector
      |> flag
           "--log-slow-files-interval"
           int
           ~doc:"Specify the interval in seconds to log slow to check files. (default: 5 seconds)"
      |> flag
           "--log-slow-components-threshold"
           int
           ~doc:
             "Specify the threshold in seconds to log slow to check component. (default to not logging anything)"
      |> flag
           "--log-slow-expressions-threshold"
           int
           ~doc:
             "Specify the threshold in seconds to log slow to check expressions. (default to not logging anything)"
    )

let quiet_flag prev =
  CommandSpec.ArgSpec.(prev |> flag "--quiet" truthy ~doc:"Suppress output about server startup")

type on_mismatch_behavior =
  | Choose_newest
  | Stop_server
  | Restart_client
  | Error_client

let on_mismatch_flag prev =
  CommandSpec.ArgSpec.(
    prev
    |> flag
         "--on-mismatch"
         (required
            ~default:Choose_newest
            (enum
               [
                 ("choose-newest", Choose_newest);
                 ("stop-server", Stop_server);
                 ("restart-client", Restart_client);
                 ("error-client", Error_client);
               ]
            )
         )
         ~doc:
           "What to do when the client and server are different versions (choose-newest, stop-server, restart-client, error-client) (default: choose-newest)"
  )

let root_flag prev =
  CommandSpec.ArgSpec.(
    prev |> flag "--root" string ~doc:"Project root directory containing the .flowconfig"
  )

let ignore_version_flag prev =
  CommandSpec.ArgSpec.(
    prev
    |> flag
         "--ignore-version"
         truthy
         ~doc:"Ignore the version constraint in .flowconfig"
         ~env:"FLOW_IGNORE_VERSION"
  )

let log_file_flags =
  let normalize log_file =
    let dirname = File_path.make (Filename.dirname log_file) in
    let basename = Filename.basename log_file in
    File_path.concat dirname basename |> File_path.to_string
  in
  let collector main server_log_file monitor_log_file =
    main
      (Base.Option.map ~f:normalize server_log_file)
      (Base.Option.map ~f:normalize monitor_log_file)
  in
  fun prev ->
    CommandSpec.ArgSpec.(
      prev
      |> collect collector
      |> flag
           "--log-file"
           string
           ~doc:"Path to log file (default: /tmp/flow/<escaped root path>.log)"
           ~env:"FLOW_LOG_FILE"
      |> flag
           "--monitor-log-file"
           string
           ~doc:"Path to log file (default: /tmp/flow/<escaped root path>.monitor_log)"
           ~env:"FLOW_MONITOR_LOG_FILE"
    )

type offset_style =
  | Utf8_offsets
  | JavaScript_offsets

let offset_style_flag prev =
  CommandSpec.ArgSpec.(
    prev
    |> flag
         "--offset-style"
         (enum [("utf8-bytes", Utf8_offsets); ("js-indices", JavaScript_offsets)])
         ~doc:"How to compute offsets in JSON output (utf8-bytes, js-indices) (default: utf8-bytes)"
  )

let offset_kind_of_offset_style = function
  | None
  | Some Utf8_offsets ->
    Offset_utils.Utf8
  | Some JavaScript_offsets -> Offset_utils.JavaScript

let flowconfig_multi_error rev_errs =
  let msg =
    rev_errs
    |> Base.List.map ~f:(fun (ln, msg) -> spf ".flowconfig:%d %s" ln msg)
    |> String.concat "\n"
  in
  Exit.(exit ~msg Invalid_flowconfig)

let flowconfig_multi_warn rev_errs =
  let msg =
    rev_errs
    |> Base.List.map ~f:(fun (ln, msg) -> spf ".flowconfig:%d %s" ln msg)
    |> String.concat "\n"
  in
  prerr_endline msg

let read_config_or_exit ~enforce_warnings ?allow_cache flowconfig_path =
  match FlowConfig.get ?allow_cache flowconfig_path with
  | Ok (config, []) -> config
  | Ok (config, warnings) ->
    if enforce_warnings then
      flowconfig_multi_error warnings
    else
      flowconfig_multi_warn warnings;
    config
  | Error err -> flowconfig_multi_error [err]

let read_config_and_hash_or_exit ~enforce_warnings ?allow_cache flowconfig_path =
  let config = read_config_or_exit ~enforce_warnings ?allow_cache flowconfig_path in
  (* allow cache here because we just read the config, don't need to do it again *)
  let hash = FlowConfig.get_hash ~allow_cache:true flowconfig_path |> Xx.to_string in
  (config, hash)

let check_version required_version =
  match required_version with
  | None -> Ok ()
  | Some version_constraint ->
    (* For the purposes of checking whether the *currently-running* version of Flow is compatible
       with the given project, we'll include pre-releases. For example, this means that 0.61.0-beta
       is compatible with >0.60.0, because it presumably implements the minimum necessary features
       of 0.60.0.

       This is subtly different than determining which version of Flow to run in the first place,
       like when npm/yarn is solving the `flow-bin` constraint. In that case, we do not want
       >0.60.0 to opt into pre-releases automatically. Those sorts of checks should not pass
       `~includes_prereleases`.

       So, if you've explicitly run v0.61.0-beta, and the flowconfig says `>0.60.0`, we'll allow it;
       but if we were looking at the flowconfig to decide which version to run, you should not
       choose the beta. *)
    if Semver.satisfies ~include_prereleases:true version_constraint Flow_version.version then
      Ok ()
    else
      let msg =
        Utils_js.spf
          "Wrong version of Flow. The config specifies version %s but this is version %s"
          version_constraint
          Flow_version.version
      in
      Error msg

let assert_version flowconfig =
  let required_version = FlowConfig.required_version flowconfig in
  match check_version required_version with
  | Ok () -> ()
  | Error msg -> Exit.(exit ~msg Invalid_flowconfig)

type flowconfig_params = {
  ignores: (string * string option) list;
  untyped: string list;
  declarations: string list;
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

let collect_flowconfig_flags
    main ignores_str untyped_str declarations_str includes_str lib_str lints_str =
  let ignores = List.map (fun ignore -> (ignore, None)) (list_of_string_arg ignores_str) in
  let untyped = list_of_string_arg untyped_str in
  let declarations = list_of_string_arg declarations_str in
  let includes = list_of_string_arg includes_str in
  let libs = list_of_string_arg lib_str in
  let raw_lint_severities = list_of_string_arg lints_str in
  main { ignores; includes; libs; raw_lint_severities; untyped; declarations }

let remove_exclusion pattern =
  if String.starts_with ~prefix:"!" pattern then
    String.sub pattern 1 (String.length pattern - 1)
  else
    pattern

let file_options =
  let path_pattern_of_arg root pattern =
    let expand_project_root_token = Files.expand_project_root_token ~root in
    pattern |> remove_exclusion |> expand_project_root_token |> Str.regexp
  in
  let path_patterns_of_args root patterns extras =
    let patterns = Base.List.rev_append extras patterns in
    Base.List.map
      ~f:(fun s ->
        let reg = path_pattern_of_arg root s in
        (s, reg))
      patterns
  in
  let ignores_of_args root patterns extras =
    let patterns = Base.List.rev_append extras patterns in
    Base.List.map
      ~f:(fun (path, backup) ->
        let reg = path_pattern_of_arg root path in
        ((path, backup), reg))
      patterns
  in
  let includes_of_arg ~implicitly_include_root ~root ~lib_paths paths =
    (* Explicitly included paths are always added to the path_matcher *)
    let path_matcher =
      Base.List.fold_left
        ~f:(fun acc path -> Path_matcher.add acc (Files.make_path_absolute root path))
        ~init:Path_matcher.empty
        paths
    in
    (* Implicitly included paths are added only if they're not already being watched *)
    let path_len path = path |> File_path.to_string |> String.length in
    let implicitly_included_paths_sorted =
      let implicitly_included =
        if implicitly_include_root then
          root :: lib_paths
        else
          lib_paths
      in
      Base.List.sort ~compare:(fun a b -> path_len a - path_len b) implicitly_included
      (* Shortest path first *)
    in
    Base.List.fold_left
      ~f:(fun acc path ->
        (* If this include is already covered by an explicit include or a shorter implicit include,
         * then skip it *)
        if Path_matcher.matches acc (File_path.to_string path) then
          acc
        else
          Path_matcher.add acc path)
      ~init:path_matcher
      implicitly_included_paths_sorted
  in
  let lib_paths ~root flowconfig extras =
    let flowtyped_path = Files.get_flowtyped_path root in
    let has_explicit_flowtyped_lib = ref false in
    let config_libs =
      Base.List.fold_right
        ~f:(fun (scoped_project_opt, lib) abs_libs ->
          let abs_lib = Files.make_path_absolute root lib in
          (*
           * "flow-typed" is always included in the libs list for convenience,
           * but there's no guarantee that it exists on the filesystem.
           *)
          if abs_lib = flowtyped_path then has_explicit_flowtyped_lib := true;
          (scoped_project_opt, abs_lib) :: abs_libs)
        (FlowConfig.libs flowconfig)
        ~init:[]
    in
    let config_libs =
      if !has_explicit_flowtyped_lib = false && Sys.file_exists (File_path.to_string flowtyped_path)
      then
        (None, flowtyped_path) :: config_libs
      else
        config_libs
    in
    match extras with
    | [] -> config_libs
    | _ ->
      config_libs @ Base.List.map ~f:(fun lib -> (None, Files.make_path_absolute root lib)) extras
  in
  fun ~root ~no_flowlib ~temp_dir ~includes ~ignores ~libs ~untyped ~declarations flowconfig ->
    let default_lib_dir =
      let no_flowlib = no_flowlib || FlowConfig.no_flowlib flowconfig in
      let libdir =
        match Flowlib.libdir ~no_flowlib temp_dir with
        | Flowlib.Prelude path -> Files.Prelude path
        | Flowlib.Flowlib path -> Files.Flowlib path
      in
      Some libdir
    in
    let ignores = ignores_of_args root (FlowConfig.ignores flowconfig) ignores in
    let untyped = path_patterns_of_args root (FlowConfig.untyped flowconfig) untyped in
    let declarations =
      path_patterns_of_args root (FlowConfig.declarations flowconfig) declarations
    in
    let lib_paths = lib_paths ~root flowconfig libs in
    let includes =
      includes
      |> Base.List.rev_append (FlowConfig.includes flowconfig)
      |> includes_of_arg
           ~implicitly_include_root:(FlowConfig.files_implicitly_include_root flowconfig)
           ~root
           ~lib_paths:(Base.List.map ~f:snd lib_paths)
    in
    let module_declaration_dirnames =
      Base.List.map (FlowConfig.module_declaration_dirnames flowconfig) ~f:(fun dir ->
          dir |> Files.expand_project_root_token ~root |> File_path.make |> File_path.to_string
      )
    in
    let module_file_exts = FlowConfig.module_file_exts flowconfig in
    let module_resource_exts = FlowConfig.module_resource_exts flowconfig in
    let multi_platform = FlowConfig.multi_platform flowconfig |> Base.Option.value ~default:false in
    let multi_platform_extensions = FlowConfig.multi_platform_extensions flowconfig in
    let multi_platform_extension_group_mapping =
      FlowConfig.multi_platform_extension_group_mapping flowconfig
    in
    let node_resolver_dirnames = FlowConfig.node_resolver_dirnames flowconfig in
    Files.mk_options
      ~default_lib_dir
      ~ignores
      ~untyped
      ~declarations
      ~includes
      ~lib_paths
      ~module_declaration_dirnames
      ~module_file_exts
      ~module_resource_exts
      ~multi_platform
      ~multi_platform_extensions
      ~multi_platform_extension_group_mapping
      ~node_resolver_dirnames

let ignore_flag prev =
  CommandSpec.ArgSpec.(
    prev
    |> flag "--ignore" (optional string) ~doc:"Specify one or more ignore patterns, comma separated"
  )

let untyped_flag prev =
  CommandSpec.ArgSpec.(
    prev
    |> flag
         "--untyped"
         (optional string)
         ~doc:"Specify one or more patterns, comma separated, for files to treat as untyped"
  )

let declaration_flag prev =
  CommandSpec.ArgSpec.(
    prev
    |> flag
         "--declaration"
         (optional string)
         ~doc:"Specify one or more patterns, comma separated, for files to treat as declarations"
  )

let include_flag prev =
  CommandSpec.ArgSpec.(
    prev
    |> flag
         "--include"
         (optional string)
         ~doc:"Specify one or more include patterns, comma separated"
  )

let lib_flag prev =
  CommandSpec.ArgSpec.(
    prev
    |> flag
         "--lib"
         (optional string)
         ~doc:"Specify one or more lib files/directories, comma separated"
  )

let lints_flag prev =
  CommandSpec.ArgSpec.(
    prev |> flag "--lints" (optional string) ~doc:"Specify one or more lint rules, comma separated"
  )

let no_restart_flag prev =
  CommandSpec.ArgSpec.(
    prev
    |> flag
         "--no-auto-restart"
         truthy
         ~doc:"If the server dies, do not try and restart it; just exit"
  )

let flowconfig_flags prev =
  CommandSpec.ArgSpec.(
    prev
    |> collect collect_flowconfig_flags
    |> ignore_flag
    |> untyped_flag
    |> declaration_flag
    |> include_flag
    |> lib_flag
    |> lints_flag
  )

type connect_params = {
  retries: int;
  timeout: int option;
  no_auto_start: bool;
  autostop: bool;
  lazy_mode: FlowConfig.lazy_mode option;
  temp_dir: string option;
  shm_flags: shared_mem_params;
  ignore_version: bool;
  quiet: bool;
  on_mismatch: on_mismatch_behavior;
}

let collect_connect_flags
    main lazy_mode timeout retries no_auto_start temp_dir shm_flags ignore_version quiet on_mismatch
    =
  let default def = function
    | Some x -> x
    | None -> def
  in
  (match timeout with
  | Some n when n <= 0 ->
    let msg = spf "Timeout must be a positive integer. Got %d" n in
    Exit.(exit ~msg Commandline_usage_error)
  | _ -> ());
  main
    {
      retries = default 3 retries;
      timeout;
      no_auto_start;
      temp_dir;
      autostop = false;
      lazy_mode;
      shm_flags;
      ignore_version;
      quiet;
      on_mismatch;
    }

let collect_connect_flags_without_lazy main = collect_connect_flags main None

let connect_flags_with_lazy_collector collector =
  CommandSpec.ArgSpec.(
    collector
    |> flag "--timeout" (optional int) ~doc:"Maximum time to wait, in seconds"
    |> flag "--retries" (optional int) ~doc:"Set the number of retries. (default: 3)"
    |> flag "--no-auto-start" truthy ~doc:"If the server is not running, do not start it; just exit"
    |> temp_dir_flag
    |> shm_flags
    |> from_flag
    |> ignore_version_flag
    |> quiet_flag
    |> on_mismatch_flag
  )

let connect_flags_no_lazy prev =
  CommandSpec.ArgSpec.(
    prev |> collect collect_connect_flags_without_lazy |> connect_flags_with_lazy_collector
  )

let connect_flags prev =
  CommandSpec.ArgSpec.(
    prev |> collect collect_connect_flags |> lazy_flags |> connect_flags_with_lazy_collector
  )

(* For commands that take both --quiet and --json or --pretty, make the latter two imply --quiet *)
let connect_and_json_flags =
  let collect_connect_and_json main connect_flags json pretty =
    main { connect_flags with quiet = connect_flags.quiet || json || pretty } json pretty
  in
  fun prev ->
    prev |> CommandSpec.ArgSpec.collect collect_connect_and_json |> connect_flags |> json_flags

let server_log_file ~flowconfig_name ~tmp_dir root =
  File_path.make (Server_files_js.log_file ~flowconfig_name ~tmp_dir root)

let monitor_log_file ~flowconfig_name ~tmp_dir root =
  File_path.make (Server_files_js.monitor_log_file ~flowconfig_name ~tmp_dir root)

module Options_flags = struct
  type t = {
    all: bool;
    debug: bool;
    flowconfig_flags: flowconfig_params;
    include_warnings: bool;
    max_warnings: int option;
    max_workers: int option;
    merge_timeout: int option;
    munge_underscore_members: bool;
    no_flowlib: bool;
    profile: bool;
    quiet: bool;
    slow_to_check_logging: Slow_to_check_logging.t;
    strip_root: bool;
    temp_dir: string option;
    verbose: Verbose.t option;
    wait_for_recheck: bool option;
    vpn_less: bool option;
    include_suppressions: bool;
    estimate_recheck_time: bool option;
    long_lived_workers: bool option;
    distributed: bool;
    no_autoimports: bool;
  }
end

module Saved_state_flags = struct
  type t = {
    saved_state_fetcher: Options.saved_state_fetcher option;
    saved_state_force_recheck: bool;
    saved_state_no_fallback: bool;
    saved_state_skip_version_check: bool;
    saved_state_verify: bool;
  }
end

module Base_flags = struct
  type t = { flowconfig_name: string }
end

let parse_lints_flag =
  let number =
    let rec number' index acc = function
      | [] -> Base.List.rev acc
      | head :: tail -> number' (index + 1) ((index, head) :: acc) tail
    in
    number' 1 []
  in
  fun base_settings flag_settings ->
    let lines = number flag_settings in
    let settings =
      match LintSettings.of_lines base_settings lines with
      | Ok (settings, []) -> Ok settings
      | Ok (_, (line, msg) :: _) ->
        (* upgrade CLI warnings to errors *)
        Error (line, msg)
      | Error _ as err -> err
    in
    match settings with
    | Ok settings -> settings
    | Error (line, msg) ->
      let msg = spf "Error parsing --lints (rule %d): %s" line msg in
      Exit.(exit ~msg Commandline_usage_error)

let options_flags =
  let collect_options_flags
      main
      debug
      profile
      all
      wait_for_recheck
      vpn_less
      no_flowlib
      munge_underscore_members
      max_workers
      include_warnings
      max_warnings
      flowconfig_flags
      verbose
      slow_to_check_logging
      strip_root
      temp_dir
      quiet
      merge_timeout
      include_suppressions
      estimate_recheck_time
      long_lived_workers
      distributed
      no_autoimports =
    (match merge_timeout with
    | Some timeout when timeout < 0 ->
      Exit.(exit ~msg:"--merge-timeout must be non-negative" Commandline_usage_error)
    | _ -> ());

    main
      {
        Options_flags.debug;
        profile;
        all;
        wait_for_recheck;
        vpn_less;
        no_flowlib;
        munge_underscore_members;
        max_workers;
        include_warnings;
        max_warnings;
        flowconfig_flags;
        verbose;
        slow_to_check_logging;
        strip_root;
        temp_dir;
        quiet;
        merge_timeout;
        include_suppressions;
        estimate_recheck_time;
        long_lived_workers;
        distributed;
        no_autoimports;
      }
  in
  fun prev ->
    CommandSpec.ArgSpec.(
      prev
      |> collect collect_options_flags
      |> flag "--debug" truthy ~doc:"Print debug info during typecheck" ~env:"FLOW_DEBUG"
      |> profile_flag
      |> flag "--all" truthy ~doc:"Typecheck all files, not just @flow"
      |> flag
           "--wait-for-recheck"
           (optional bool)
           ~doc:
             "If true, always wait for rechecks to finish before serving commands (default: false)"
      |> flag
           "--vpn-less"
           (optional bool)
           ~doc:
             "If true, always enable vpn-less mode to query and download saved state (default: false)"
      |> flag "--no-flowlib" truthy ~doc:"Do not include embedded declarations" ~env:"NO_FLOWLIB"
      |> flag
           "--munge-underscore-members"
           truthy
           ~doc:"Treat any class member name with a leading underscore as private"
      |> flag
           "--max-workers"
           (optional int)
           ~doc:"Maximum number of workers to create (capped by number of cores)"
           ~env:"FLOW_MAX_WORKERS"
      |> warning_flags
      |> flowconfig_flags
      |> verbose_flags
      |> slow_to_check_logging_flags
      |> strip_root_flag
      |> temp_dir_flag
      |> quiet_flag
      |> flag
           "--merge-timeout"
           int
           ~doc:
             ("The maximum time in seconds to attempt to typecheck a file or cycle of files. "
             ^ "0 means no timeout (default: 100)"
             )
           ~env:"FLOW_MERGE_TIMEOUT"
      |> flag
           "--include-suppressed"
           truthy
           ~doc:"Ignore any `suppress_comment` lines in .flowconfig"
      (* restarting to save time is a hack and should be removed. this should
         not be part of our public API, so not included in the docs. *)
      |> flag "--estimate-recheck-time" (optional bool) ~doc:"" ~env:"FLOW_ESTIMATE_RECHECK_TIME"
      |> flag "--long-lived-workers" (optional bool) ~doc:"" ~env:"FLOW_LONG_LIVED_WORKERS"
      |> flag "--distributed" truthy ~doc:""
      |> flag "--no-autoimports" truthy ~doc:"Disable auto-imports"
    )

let saved_state_flags =
  let collect_saved_state_flags
      main
      saved_state_fetcher
      saved_state_force_recheck
      saved_state_no_fallback
      saved_state_skip_version_check
      saved_state_verify =
    main
      {
        Saved_state_flags.saved_state_fetcher;
        saved_state_force_recheck;
        saved_state_no_fallback;
        saved_state_skip_version_check;
        saved_state_verify;
      }
  in
  fun prev ->
    CommandSpec.ArgSpec.(
      prev
      |> collect collect_saved_state_flags
      |> flag
           "--saved-state-fetcher"
           (enum
              [
                ("none", Options.Dummy_fetcher);
                ("local", Options.Local_fetcher);
                ("scm", Options.Scm_fetcher);
                ("fb", Options.Fb_fetcher);
              ]
           )
           ~doc:"Which saved state fetcher Flow should use (none, local) (default: none)"
      |> flag
           "--saved-state-force-recheck"
           truthy
           ~doc:"Force a lazy server to recheck the changes since the saved state was generated"
      |> flag
           "--saved-state-no-fallback"
           truthy
           ~doc:
             "If saved state fails to load, exit (normally fallback is to initialize from scratch)"
      (* This is really unsafe! Saved state is marshal'd OCaml data and it's
         easy to introduce serialization differences that would lead to
         segfaults. This is only for debugging. *)
      |> flag
           "--saved-state-skip-version-check-DO_NOT_USE_OR_YOU_WILL_BE_FIRED"
           truthy
           ~doc:""
           ~env:"FLOW_SAVED_STATE_SKIP_VERSION_CHECK_DO_NOT_USE_OR_YOU_WILL_BE_FIRED"
      |> flag
           "--saved-state-verify"
           truthy
           ~doc:"Verifies that the saved state matches what is on disk (for debugging only)"
    )

let flowconfig_name_flag prev =
  CommandSpec.ArgSpec.(
    prev
    |> flag
         "--flowconfig-name"
         (required ~default:Server_files_js.default_flowconfig_name string)
         ~doc:
           (Printf.sprintf
              "Set the name of the flow configuration file. (default: %s)"
              Server_files_js.default_flowconfig_name
           )
         ~env:"FLOW_CONFIG_NAME"
  )

let base_flags =
  let collect_base_flags main flowconfig_name = main { Base_flags.flowconfig_name } in
  (fun prev -> CommandSpec.ArgSpec.(prev |> collect collect_base_flags |> flowconfig_name_flag))

let default_file_watcher_timeout = 120

let default_file_watcher_mergebase_with = "master"

let file_watcher_flag prev =
  let open CommandSpec.ArgSpec in
  prev
  |> flag
       "--file-watcher"
       (enum
          [
            ("none", FlowConfig.NoFileWatcher);
            ("dfind", FlowConfig.DFind);
            ("watchman", FlowConfig.Watchman);
          ]
       )
       ~doc:
         ("Which file watcher Flow should use (none, dfind, watchman). "
         ^ "Flow will ignore file system events if this is set to none. (default: dfind)"
         )
  |> flag
       "--file-watcher-debug"
       truthy
       ~doc:"Enable debug logging for the file watcher. This is very noisy"
       ~env:"FLOW_FILE_WATCHER_DEBUG"
  |> flag
       "--file-watcher-timeout"
       uint
       ~doc:
         (Printf.sprintf
            "Maximum time to wait for the file watcher to initialize, in seconds. 0 means no timeout (default: %d)"
            default_file_watcher_timeout
         )
  |> flag
       "--file-watcher-mergebase-with"
       string
       ~doc:
         (Printf.sprintf
            "Symbolic commit against which to compute the mergebase used to find changed files. (default: %s)"
            default_file_watcher_mergebase_with
         )
  |> flag
       "--file-watcher-sync-timeout"
       uint
       ~doc:
         "Maximum time to wait for the file watcher to synchronize, in milliseconds. 0 means no timeout. Currently only used by Watchman."

(* For commands that take both --quiet and --json or --pretty, make the latter two imply --quiet *)
let options_and_json_flags =
  let collect_options_and_json main options_flags json pretty =
    main
      {
        options_flags with
        Options_flags.quiet = options_flags.Options_flags.quiet || json || pretty;
      }
      json
      pretty
  in
  fun prev ->
    prev |> CommandSpec.ArgSpec.collect collect_options_and_json |> options_flags |> json_flags

let json_version_flag prev =
  CommandSpec.ArgSpec.(
    prev
    |> flag
         "--json-version"
         (enum
            [
              ("1", Flow_errors_utils.Json_output.JsonV1);
              ("2", Flow_errors_utils.Json_output.JsonV2);
            ]
         )
         ~doc:"The version of the JSON format (defaults to 1)"
  )

(* If a command uses this flag, then it will automatically exec systemd-run (if systemd-run is
 * available). This can add a couple hundred ms to the start up time of the command. Only commands
 * that are resource-intensive (or spawn resource intensive processes) and probably should run in
 * cgroups should use this flag.
 *)
let no_cgroup_flag =
  (* We only trigger this behavior if we're on Unix and systemd-run is in the path *)
  let get_systemd_binary () =
    if Sys.unix then
      let ic = Unix.open_process_in "which systemd-run 2> /dev/null" in
      let systemd_exe =
        try Some (input_line ic) with
        | _ -> None
      in
      if Unix.close_process_in ic = Unix.WEXITED 0 then
        systemd_exe
      else
        None
    else
      None
  in
  (* Sometimes systemd-run is available but we can't use it. For example, the systemd might not have
     a proper working user session, so we might not be able to run commands via systemd-run as a
     user process. Notably, `--user --scope` is broken under cgroupv2 in systemd < 238, and exits
     code 1 (https://github.com/facebook/flow/issues/8012). *)
  let can_run_systemd () =
    (* Use `timeout` in case it hangs mysteriously. `--quiet` only suppresses stdout. *)
    let ic =
      Unix.open_process_in "timeout 1 systemd-run --quiet --user --scope -- true 2> /dev/null"
    in
    (* If all goes right, `systemd-run` will return immediately with exit code 0 and run `true`
     * asynchronously as a service. If it goes wrong, it will exit with a non-zero exit code *)
    Unix.close_process_in ic = Unix.WEXITED 0
  in
  (* Basically re-exec ourselves with the --no-cgroup flag using systemd-run *)
  let exec_in_cgroup_if_systemd_available () =
    match get_systemd_binary () with
    | None -> ()
    | Some systemd_exe ->
      if can_run_systemd () then
        let flow_args =
          match Array.to_list Sys.argv with
          | [] -> failwith "The argv should never be empty. Element 0 should be the executable."
          | [_] -> failwith "`flow` doesn't use `--no-cgroup` so we shouldn't hit this"
          | flow_exe :: command :: args -> flow_exe :: command :: "--no-cgroup" :: args
        in
        systemd_exe
        :: "--quiet"
        :: "--user"
        :: "--scope"
        :: "--slice"
        :: "flow.slice"
        :: "--"
        :: flow_args
        |> Array.of_list
        |> Unix.execv systemd_exe
  in
  let collect_no_cgroup_flag main no_cgroup =
    if not no_cgroup then exec_in_cgroup_if_systemd_available ();
    main
  in
  fun prev ->
    CommandSpec.ArgSpec.(
      prev
      |> CommandSpec.ArgSpec.collect collect_no_cgroup_flag
      |> flag
           "--no-cgroup"
           truthy
           ~doc:"Don't automatically run this command in a cgroup (if cgroups are available)"
    )

let get_temp_dir cli_value = Base.Option.value cli_value ~default:Server_files_js.default_temp_dir

let make_options
    ~flowconfig_name
    ~flowconfig_hash
    ~flowconfig
    ~lazy_mode
    ~root
    ~options_flags
    ~saved_state_options_flags =
  let open Options_flags in
  let open Saved_state_flags in
  let temp_dir = File_path.make (get_temp_dir options_flags.Options_flags.temp_dir) in
  let file_options =
    let no_flowlib = options_flags.no_flowlib in
    let { includes; ignores; libs; raw_lint_severities = _; untyped; declarations } =
      options_flags.flowconfig_flags
    in
    file_options
      ~root
      ~no_flowlib
      ~temp_dir
      ~implicitly_include_root:(FlowConfig.files_implicitly_include_root flowconfig)
      ~includes
      ~haste_paths_excludes:
        (Base.List.map
           ~f:(fun f -> f |> Files.expand_project_root_token ~root |> Str.regexp)
           (FlowConfig.haste_paths_excludes flowconfig)
        )
      ~haste_paths_includes:
        (Base.List.map
           ~f:(fun f -> f |> Files.expand_project_root_token ~root |> Str.regexp)
           (FlowConfig.haste_paths_includes flowconfig)
        )
      ~ignores
      ~libs
      ~untyped
      ~declarations
      flowconfig
  in
  let lint_severities =
    parse_lints_flag
      (FlowConfig.lint_severities flowconfig)
      options_flags.flowconfig_flags.raw_lint_severities
  in
  let opt_merge_timeout =
    (match options_flags.merge_timeout with
    | None -> FlowConfig.merge_timeout flowconfig
    | Some 0 -> None
    | timeout -> timeout)
    |> Base.Option.map ~f:float_of_int
  in
  (* The CLI flag overrides the .flowconfig *)
  let opt_saved_state_fetcher =
    Base.Option.value
      saved_state_options_flags.saved_state_fetcher
      ~default:(FlowConfig.saved_state_fetcher flowconfig)
  in
  let opt_lazy_mode =
    match Base.Option.first_some lazy_mode (FlowConfig.lazy_mode flowconfig) with
    | Some FlowConfig.Lazy -> true
    | Some FlowConfig.Watchman_DEPRECATED -> true
    | Some FlowConfig.Non_lazy -> false
    | None -> false
  in
  let opt_wait_for_recheck =
    Base.Option.value
      options_flags.wait_for_recheck
      ~default:(FlowConfig.wait_for_recheck flowconfig)
  in
  let opt_vpn_less =
    Base.Option.value options_flags.vpn_less ~default:(FlowConfig.vpn_less flowconfig)
  in
  let opt_format =
    {
      Options.opt_bracket_spacing =
        Base.Option.value (FlowConfig.format_bracket_spacing flowconfig) ~default:true;
      opt_single_quotes =
        Base.Option.value (FlowConfig.format_single_quotes flowconfig) ~default:false;
    }
  in
  let strict_mode = FlowConfig.strict_mode flowconfig in
  let opt_temp_dir = File_path.to_string temp_dir in
  let opt_log_file = server_log_file ~flowconfig_name ~tmp_dir:opt_temp_dir root in
  {
    Options.opt_flowconfig_name = flowconfig_name;
    opt_lazy_mode;
    opt_root = root;
    opt_root_name = FlowConfig.root_name flowconfig;
    opt_debug = options_flags.debug;
    opt_verbose = options_flags.verbose;
    opt_all = options_flags.all || Base.Option.value (FlowConfig.all flowconfig) ~default:false;
    opt_babel_loose_array_spread =
      Base.Option.value (FlowConfig.babel_loose_array_spread flowconfig) ~default:false;
    opt_casting_syntax =
      Base.Option.value (FlowConfig.casting_syntax flowconfig) ~default:Options.CastingSyntax.Both;
    opt_wait_for_recheck;
    opt_vpn_less;
    opt_quiet = options_flags.Options_flags.quiet;
    opt_module_name_mappers = FlowConfig.module_name_mappers flowconfig;
    opt_modules_are_use_strict = FlowConfig.modules_are_use_strict flowconfig;
    opt_profile = options_flags.profile;
    opt_strip_root = options_flags.strip_root;
    opt_module = FlowConfig.module_system flowconfig;
    opt_munge_underscores =
      options_flags.munge_underscore_members || FlowConfig.munge_underscores flowconfig;
    opt_node_main_fields = FlowConfig.node_main_fields flowconfig;
    opt_node_package_export_conditions = FlowConfig.node_package_export_conditions flowconfig;
    opt_temp_dir;
    opt_max_workers =
      Base.Option.first_some options_flags.max_workers (FlowConfig.max_workers flowconfig)
      |> Base.Option.value
           ~default:(Sys_utils.nbr_procs / FlowConfig.max_workers_down_scaling_factor flowconfig)
      |> min (Sys_utils.nbr_procs / FlowConfig.max_workers_down_scaling_factor flowconfig);
    opt_suppress_types = FlowConfig.suppress_types flowconfig;
    opt_max_literal_length = FlowConfig.max_literal_length flowconfig;
    opt_component_syntax = FlowConfig.component_syntax flowconfig;
    opt_react_rules = FlowConfig.react_rules flowconfig;
    opt_hook_compatibility = FlowConfig.hook_compatibility flowconfig;
    opt_hook_compatibility_includes =
      Base.List.map
        ~f:(fun pattern -> pattern |> Files.expand_project_root_token ~root |> Str.regexp)
        (FlowConfig.hook_compatibility_includes flowconfig);
    opt_hook_compatibility_excludes =
      Base.List.map
        ~f:(fun pattern -> pattern |> Files.expand_project_root_token ~root |> Str.regexp)
        (FlowConfig.hook_compatibility_excludes flowconfig);
    opt_dev_only_refinement_info_as_errors =
      FlowConfig.dev_only_refinement_info_as_errors flowconfig;
    opt_enable_const_params =
      Base.Option.value (FlowConfig.enable_const_params flowconfig) ~default:false;
    opt_enable_jest_integration = FlowConfig.jest_integration flowconfig;
    opt_enable_pattern_matching =
      Base.Option.value ~default:false (FlowConfig.pattern_matching flowconfig);
    opt_pattern_matching_includes =
      Base.List.map
        ~f:(Files.expand_project_root_token ~root)
        (FlowConfig.pattern_matching_includes flowconfig);
    opt_constant_condition =
      Base.Option.value ~default:false (FlowConfig.constant_condition flowconfig);
    opt_constant_condition_boolean_literal_includes =
      Base.List.map
        ~f:(Files.expand_project_root_token ~root)
        (FlowConfig.constant_condition_boolean_literal_includes flowconfig);
    opt_constant_condition_null_void_includes =
      Base.List.map
        ~f:(Files.expand_project_root_token ~root)
        (FlowConfig.constant_condition_null_void_includes flowconfig);
    opt_constant_condition_function_includes =
      Base.List.map
        ~f:(Files.expand_project_root_token ~root)
        (FlowConfig.constant_condition_function_includes flowconfig);
    opt_invalid_comparison_general_includes =
      Base.List.map
        ~f:(Files.expand_project_root_token ~root)
        (FlowConfig.invalid_comparison_general_includes flowconfig);
    opt_invalid_comparison_null_check_includes =
      Base.List.map
        ~f:(Files.expand_project_root_token ~root)
        (FlowConfig.invalid_comparison_null_check_includes flowconfig);
    opt_enable_relay_integration = FlowConfig.relay_integration flowconfig;
    opt_enabled_rollouts = FlowConfig.enabled_rollouts flowconfig;
    opt_channel_mode = Base.Option.value ~default:`pipe (FlowConfig.channel_mode flowconfig);
    opt_enums = FlowConfig.enums flowconfig;
    opt_estimate_recheck_time =
      Base.Option.first_some
        options_flags.estimate_recheck_time
        (FlowConfig.estimate_recheck_time flowconfig)
      |> Base.Option.value ~default:true;
    opt_exact_by_default = Base.Option.value ~default:true (FlowConfig.exact_by_default flowconfig);
    opt_facebook_fbs = FlowConfig.facebook_fbs flowconfig;
    opt_facebook_fbt = FlowConfig.facebook_fbt flowconfig;
    opt_facebook_module_interop = FlowConfig.facebook_module_interop flowconfig;
    opt_ignore_non_literal_requires = FlowConfig.ignore_non_literal_requires flowconfig;
    opt_include_warnings =
      options_flags.include_warnings
      || options_flags.max_warnings <> None
      || FlowConfig.include_warnings flowconfig;
    opt_max_header_tokens = FlowConfig.max_header_tokens flowconfig;
    opt_haste_module_ref_prefix = FlowConfig.haste_module_ref_prefix flowconfig;
    opt_file_options = file_options;
    opt_lint_severities = lint_severities;
    opt_strict_mode = strict_mode;
    opt_merge_timeout;
    opt_missing_module_generators = FlowConfig.missing_module_generators flowconfig;
    opt_no_unchecked_indexed_access = FlowConfig.no_unchecked_indexed_access flowconfig;
    opt_saved_state_fetcher;
    opt_saved_state_force_recheck = saved_state_options_flags.saved_state_force_recheck;
    opt_saved_state_no_fallback = saved_state_options_flags.saved_state_no_fallback;
    opt_saved_state_skip_version_check = saved_state_options_flags.saved_state_skip_version_check;
    opt_saved_state_verify = saved_state_options_flags.saved_state_verify;
    opt_node_resolver_allow_root_relative = FlowConfig.node_resolver_allow_root_relative flowconfig;
    opt_node_resolver_root_relative_dirnames =
      Base.List.map
        (FlowConfig.node_resolver_root_relative_dirnames flowconfig)
        ~f:(fun (applicable_dir_opt, dirname) ->
          (Base.Option.map ~f:(Files.expand_project_root_token ~root) applicable_dir_opt, dirname)
      );
    opt_opaque_type_new_bound_syntax = FlowConfig.opaque_type_new_bound_syntax flowconfig;
    opt_projects_options =
      Flow_projects.mk_options
        ~projects:(FlowConfig.projects flowconfig)
        ~projects_overlap_mapping:(FlowConfig.projects_overlap_mapping flowconfig)
        ~map_path:(fun path -> Files.expand_project_root_token ~root path |> Str.regexp)
        ~projects_path_mapping:(FlowConfig.projects_path_mapping flowconfig)
        ~projects_strict_boundary:(FlowConfig.projects_strict_boundary flowconfig)
        ~projects_strict_boundary_validate_import_pattern_opt_outs:
          (FlowConfig.projects_strict_boundary_validate_import_pattern_opt_outs flowconfig)
        ~projects_strict_boundary_import_pattern_opt_outs:
          (FlowConfig.projects_strict_boundary_import_pattern_opt_outs flowconfig)
        ~multi_platform_ambient_supports_platform_project_overrides:
          (FlowConfig.multi_platform_ambient_supports_platform_project_overrides flowconfig);
    opt_include_suppressions = options_flags.include_suppressions;
    opt_distributed = options_flags.distributed;
    opt_unsuppressable_error_codes = FlowConfig.unsuppressable_error_codes flowconfig;
    opt_use_mixed_in_catch_variables =
      Base.Option.value (FlowConfig.use_mixed_in_catch_variables flowconfig) ~default:false;
    opt_ban_spread_key_props =
      Base.Option.value (FlowConfig.ban_spread_key_props flowconfig) ~default:false;
    opt_react_custom_jsx_typing = FlowConfig.react_custom_jsx_typing flowconfig;
    opt_react_ref_as_prop = FlowConfig.react_ref_as_prop flowconfig;
    opt_react_runtime = FlowConfig.react_runtime flowconfig;
    opt_recursion_limit = FlowConfig.recursion_limit flowconfig;
    opt_relay_integration_esmodules = FlowConfig.relay_integration_esmodules flowconfig;
    opt_relay_integration_excludes =
      Base.List.map
        ~f:(fun pattern -> pattern |> Files.expand_project_root_token ~root |> Str.regexp)
        (FlowConfig.relay_integration_excludes flowconfig);
    opt_relay_integration_module_prefix = FlowConfig.relay_integration_module_prefix flowconfig;
    opt_relay_integration_module_prefix_includes =
      Base.List.map
        ~f:(fun pattern -> pattern |> Files.expand_project_root_token ~root |> Str.regexp)
        (FlowConfig.relay_integration_module_prefix_includes flowconfig);
    opt_max_files_checked_per_worker = FlowConfig.max_files_checked_per_worker flowconfig;
    opt_max_seconds_for_check_per_worker = FlowConfig.max_seconds_for_check_per_worker flowconfig;
    opt_slow_to_check_logging = options_flags.slow_to_check_logging;
    opt_strict_es6_import_export = FlowConfig.strict_es6_import_export flowconfig;
    opt_ts_syntax = FlowConfig.ts_syntax flowconfig;
    opt_assert_operator = FlowConfig.assert_operator flowconfig;
    opt_type_expansion_recursion_limit = FlowConfig.type_expansion_recursion_limit flowconfig;
    opt_automatic_require_default =
      Base.Option.value (FlowConfig.automatic_require_default flowconfig) ~default:false;
    opt_format;
    opt_autoimports =
      (not options_flags.no_autoimports)
      && Base.Option.value (FlowConfig.autoimports flowconfig) ~default:true;
    opt_autoimports_min_characters =
      Base.Option.value (FlowConfig.autoimports_min_characters flowconfig) ~default:0;
    opt_autoimports_ranked_by_usage = FlowConfig.autoimports_ranked_by_usage flowconfig;
    opt_autoimports_ranked_by_usage_boost_exact_match_min_length =
      FlowConfig.autoimports_ranked_by_usage_boost_exact_match_min_length flowconfig;
    opt_flowconfig_hash = flowconfig_hash;
    opt_gc_worker =
      {
        Options.gc_minor_heap_size =
          Base.Option.first_some
            (FlowConfig.gc_worker_minor_heap_size flowconfig)
            (Some (1024 * 1024 * 2));
        gc_major_heap_increment = FlowConfig.gc_worker_major_heap_increment flowconfig;
        gc_space_overhead = FlowConfig.gc_worker_space_overhead flowconfig;
        gc_window_size = FlowConfig.gc_worker_window_size flowconfig;
        gc_custom_major_ratio = FlowConfig.gc_worker_custom_major_ratio flowconfig;
        gc_custom_minor_ratio = FlowConfig.gc_worker_custom_minor_ratio flowconfig;
        gc_custom_minor_max_size = FlowConfig.gc_worker_custom_minor_max_size flowconfig;
      };
    opt_log_saving = FlowConfig.log_saving flowconfig;
    opt_log_file;
    opt_long_lived_workers =
      Option.value
        options_flags.long_lived_workers
        ~default:(FlowConfig.long_lived_workers flowconfig);
  }

let make_env flowconfig flowconfig_name connect_flags root =
  let normalize dir = File_path.(dir |> make |> to_string) in
  let tmp_dir = get_temp_dir connect_flags.temp_dir |> normalize in
  let retries = connect_flags.retries in
  let expiry =
    match connect_flags.timeout with
    | None -> None
    | Some n -> Some (Unix.gettimeofday () +. float n)
  in
  let rerun_on_mismatch =
    match connect_flags.on_mismatch with
    | Choose_newest
    | Restart_client ->
      true
    | Stop_server
    | Error_client ->
      false
  in
  let lazy_mode =
    Base.Option.map connect_flags.lazy_mode ~f:(function
        | FlowConfig.Lazy -> "true"
        | FlowConfig.Non_lazy -> "false"
        | FlowConfig.Watchman_DEPRECATED -> "watchman"
        )
  in
  {
    CommandConnect.root;
    autostart = not connect_flags.no_auto_start;
    lazy_mode;
    retries;
    expiry;
    autostop = connect_flags.autostop;
    tmp_dir;
    shm_hash_table_pow = connect_flags.shm_flags.shm_hash_table_pow;
    ignore_version = connect_flags.ignore_version;
    emoji = Base.Option.value (FlowConfig.emoji flowconfig) ~default:false;
    quiet = connect_flags.quiet;
    flowconfig_name;
    rerun_on_mismatch;
  }

let rec search_for_root config start recursion_limit : File_path.t option =
  if start = File_path.parent start then
    None
  (* Reach fs root, nothing to do. *)
  else if File_path.file_exists (File_path.concat start config) then
    Some start
  else if recursion_limit <= 0 then
    None
  else
    search_for_root config (File_path.parent start) (recursion_limit - 1)

(* Given a valid file or directory, find a valid Flow root directory *)
(* NOTE: exits on invalid file or .flowconfig not found! *)
let guess_root flowconfig_name dir_or_file =
  let dir_or_file =
    match dir_or_file with
    | Some dir_or_file -> dir_or_file
    | None -> "."
  in
  if not (Sys.file_exists dir_or_file) then
    let msg =
      spf
        "Could not find file or directory %s; canceling search for %s.\nSee \"flow init --help\" for more info"
        dir_or_file
        flowconfig_name
    in
    Exit.(exit ~msg Could_not_find_flowconfig)
  else
    let dir =
      if Sys.is_directory dir_or_file then
        dir_or_file
      else
        Filename.dirname dir_or_file
    in
    match search_for_root flowconfig_name (File_path.make dir) 50 with
    | Some root ->
      FlowEventLogger.set_root (Some (File_path.to_string root));
      root
    | None ->
      let msg =
        spf
          "Could not find a %s in %s or any of its parent directories.\nSee \"flow init --help\" for more info\n%!"
          flowconfig_name
          dir
      in
      Exit.(exit ~msg Could_not_find_flowconfig)

(* Favor the root argument, over the input file, over the current directory
   as the place to begin searching for the root. *)
let find_a_root ?input ~base_flags root_arg =
  let flowconfig_name = Base_flags.(base_flags.flowconfig_name) in
  guess_root
    flowconfig_name
    (match (root_arg, input) with
    | (Some provided_root, _) -> Some provided_root
    | (None, Some provided_input) -> File_input.path_of_file_input provided_input
    | (None, None) -> None)

(* If a root is given then validate it and use it. Otherwise, favor the input file
   over the current directory as the place to begin searching for the root. *)
let get_the_root ?input ~base_flags root_arg =
  match root_arg with
  | Some provided_root ->
    let root_dir = File_path.make provided_root in
    if File_path.file_exists root_dir && File_path.is_directory root_dir then
      let flowconfig_name = Base_flags.(base_flags.flowconfig_name) in
      let root_config = File_path.concat root_dir flowconfig_name in
      if File_path.file_exists root_config then
        root_dir
      else
        let msg = spf "Failed to open %s" @@ File_path.to_string root_config in
        Exit.(exit ~msg Could_not_find_flowconfig)
    else
      let msg = spf "Invalid root directory %s" provided_root in
      Exit.(exit ~msg Could_not_find_flowconfig)
  | None -> find_a_root ?input ~base_flags None

(* convert 1,1 based line/column to 1,0 for internal use *)
let convert_input_pos (line, column) =
  let column =
    if column > 1 then
      column - 1
    else
      0
  in
  (line, column)

(* copied (and adapted) from Hack's ClientCheck module *)
let get_path_of_file file =
  let path = File_path.make file in
  if File_path.file_exists path then
    File_path.to_string path
  else
    (* Filename.concat does not return a normalized path when the file does
       not exist. Thus, we do it on our own... *)
    let file = Files.normalize_path (Sys.getcwd ()) file in
    let path = File_path.make file in
    File_path.to_string path

let get_file_from_filename_or_stdin ~cmd path = function
  | Some filename ->
    if not (Sys.file_exists filename) then
      let msg =
        spf "Could not find file %s; canceling.\nSee \"flow %s --help\" for more info" filename cmd
      in
      Exit.(exit ~msg No_input)
    else if Sys.is_directory filename then
      let msg =
        spf
          "Provided argument %s is not a file; canceling.\nSee \"flow %s --help\" for more info"
          filename
          cmd
      in
      Exit.(exit ~msg Path_is_not_a_file)
    else
      File_input.FileName (expand_path filename)
  | None ->
    let contents = Sys_utils.read_all stdin in
    let filename =
      match path with
      | Some ""
      | None ->
        None
      | Some str -> Some (get_path_of_file str)
    in
    File_input.FileContent (filename, contents)

(* Takes a list of strings. If there are 2 then they are both parsed as intengers
   and stdin is read from. If there are 3 then the first is treated as a input file
   and the following 2 are parsed as integers. *)
let parse_location_with_optional_filename spec path args =
  let exit () =
    CommandSpec.usage spec;
    Exit.(exit Commandline_usage_error)
  in
  let (file, line, column) =
    match args with
    | [file; line; column] ->
      let file = expand_path file in
      (File_input.FileName file, line, column)
    | [line; column] ->
      (get_file_from_filename_or_stdin ~cmd:CommandSpec.(spec.name) path None, line, column)
    | _ -> exit ()
  in
  let (line, column) =
    try (int_of_string line, int_of_string column) with
    | Failure _ -> exit ()
  in
  let (line, column) = convert_input_pos (line, column) in
  (file, line, column)

let exe_name = Utils_js.exe_name

(* What should we do when we connect to the flow server monitor, but it dies before responding to
 * us? Well, we should consume a retry and try to connect again, potentially even starting a new
 * server *)
let rec connect_and_make_request flowconfig_name =
  (* Sends the command over the socket *)
  let send_command ?timeout (oc : out_channel) (cmd : ServerProt.Request.command) : unit =
    let command =
      {
        ServerCommandWithContext.client_logging_context = FlowEventLogger.get_context ();
        command = cmd;
      }
    in
    Marshal_tools.to_fd_with_preamble ?timeout (Unix.descr_of_out_channel oc) command |> ignore;
    flush oc
  in
  let eprintf_with_spinner msg =
    if Unix.isatty Unix.stderr then (
      if Tty.spinner_used () then Tty.print_clear_line stderr;
      Printf.eprintf "%s: %s%!" msg (Tty.spinner ())
    ) else
      Printf.eprintf "%s\n%!" msg
  in
  let eprintf_with_spinner fmt = Printf.ksprintf eprintf_with_spinner fmt in
  (* Waits for a response over the socket. If the connection dies, this will throw an exception *)
  let rec wait_for_response ?timeout ~quiet ~emoji ~root (ic : Timeout.in_channel) =
    let use_emoji = Tty.supports_emoji () && emoji in
    let response : MonitorProt.monitor_to_client_message =
      try Marshal_tools.from_fd_with_preamble ?timeout (Timeout.descr_of_in_channel ic) with
      | Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET), _, _) ->
        if (not quiet) && Tty.spinner_used () then Tty.print_clear_line stderr;
        raise End_of_file
      | exn ->
        let exn = Exception.wrap exn in
        if (not quiet) && Tty.spinner_used () then Tty.print_clear_line stderr;
        Exception.reraise exn
    in
    match response with
    | MonitorProt.Please_hold status ->
      let status_string =
        match status with
        | (server_status, watcher_status) when ServerStatus.is_free server_status ->
          (* Let's ignore messages from the server that it is free. It's a confusing message for the
           * user *)
          if snd watcher_status = FileWatcherStatus.Ready then
            None
          else
            Some (FileWatcherStatus.string_of_status watcher_status)
        | (server_status, _) -> Some (ServerStatus.string_of_status ~use_emoji server_status)
      in
      Base.Option.iter status_string ~f:(fun status_string ->
          if not quiet then eprintf_with_spinner "Please wait. %s" status_string
      );

      wait_for_response ?timeout ~quiet ~emoji ~root ic
    | MonitorProt.Data response ->
      if (not quiet) && Tty.spinner_used () then Tty.print_clear_line stderr;
      response
    | MonitorProt.ServerException exn_str ->
      if Tty.spinner_used () then Tty.print_clear_line stderr;
      let msg = Utils_js.spf "Server threw an exception: %s" exn_str in
      Exit.(exit ~msg Unknown_error)
  in
  fun ?timeout ?retries connect_flags root request ->
    let retries =
      match retries with
      | None -> connect_flags.retries
      | Some retries -> retries
    in
    (if retries < 0 then Exit.(exit ~msg:"Out of retries, exiting!" Out_of_retries));

    let version_mismatch_strategy =
      match connect_flags.on_mismatch with
      | Choose_newest -> SocketHandshake.Stop_server_if_older
      | Stop_server -> SocketHandshake.Always_stop_server
      | Restart_client -> SocketHandshake.Error_client
      | Error_client -> SocketHandshake.Error_client
    in
    let quiet = connect_flags.quiet in
    let client_handshake =
      ( {
          SocketHandshake.client_build_id = SocketHandshake.build_revision;
          client_version = Flow_version.version;
          is_stop_request = false;
          server_should_hangup_if_still_initializing = false;
          version_mismatch_strategy;
        },
        { SocketHandshake.client_type = SocketHandshake.Ephemeral }
      )
    in

    let flowconfig =
      let path = Server_files_js.config_file flowconfig_name root in
      (* let the server enforce flowconfig warnings; we only care about the flowconfig
         as far as it pertains to connecting to the server. *)
      let enforce_warnings = false in
      read_config_or_exit ~enforce_warnings path
    in

    (* connect handles timeouts itself *)
    let env = make_env flowconfig flowconfig_name connect_flags root in
    let (ic, oc) = CommandConnect.connect ~flowconfig_name ~client_handshake env in
    send_command ?timeout oc request;
    try wait_for_response ?timeout ~quiet ~emoji:env.CommandConnect.emoji ~root ic with
    | End_of_file ->
      if not quiet then
        eprintf_with_spinner
          "Lost connection to the flow server (%d %s remaining)%!"
          retries
          ( if retries = 1 then
            "retry"
          else
            "retries"
          );
      connect_and_make_request
        flowconfig_name
        ?timeout
        ~retries:(retries - 1)
        connect_flags
        root
        request

(* If --timeout is set, wrap connect_and_make_request in a timeout *)
let connect_and_make_request ?retries flowconfig_name connect_flags root request =
  match connect_flags.timeout with
  | None -> connect_and_make_request ?retries flowconfig_name connect_flags root request
  | Some timeout ->
    Timeout.with_timeout
      ~timeout
      ~on_timeout:(fun () -> Exit.(exit ~msg:"Timeout exceeded, exiting" Out_of_time))
      ~do_:(fun timeout ->
        connect_and_make_request ~timeout ?retries flowconfig_name connect_flags root request)

let failwith_bad_response ~request ~response =
  let msg =
    Printf.sprintf
      "Bad response to %S: received %S"
      (ServerProt.Request.to_string request)
      (ServerProt.Response.to_string response)
  in
  failwith msg

let get_check_or_status_exit_code errors warnings max_warnings =
  Exit.(
    Flow_errors_utils.(
      if ConcreteLocPrintableErrorSet.is_empty errors then
        match max_warnings with
        | Some x when ConcreteLocPrintableErrorSet.cardinal warnings > x -> Type_error
        | None
        | Some _ ->
          No_error
      else
        Type_error
    )
  )

let choose_file_watcher ~flowconfig ~lazy_mode ~file_watcher ~file_watcher_debug ~sync_timeout =
  let file_watcher =
    match (file_watcher, lazy_mode) with
    | (None, Some FlowConfig.Watchman_DEPRECATED) -> FlowConfig.Watchman
    | (Some file_watcher, _) -> file_watcher
    | (None, _) -> Base.Option.value ~default:FlowConfig.DFind (FlowConfig.file_watcher flowconfig)
  in
  match file_watcher with
  | FlowConfig.NoFileWatcher -> FlowServerMonitorOptions.NoFileWatcher
  | FlowConfig.DFind -> FlowServerMonitorOptions.DFind
  | FlowConfig.Watchman ->
    let sync_timeout =
      match sync_timeout with
      | Some x -> Some x
      | None -> FlowConfig.watchman_sync_timeout flowconfig
    in
    let defer_states = FlowConfig.watchman_defer_states flowconfig in
    FlowServerMonitorOptions.Watchman
      { FlowServerMonitorOptions.debug = file_watcher_debug; defer_states; sync_timeout }

let choose_file_watcher_mergebase_with ~flowconfig vcs mergebase_with =
  match mergebase_with with
  | Some x -> x
  | None ->
    let mergebase_with =
      match vcs with
      | Some Vcs.Git -> FlowConfig.file_watcher_mergebase_with_git flowconfig
      | Some Vcs.Hg -> FlowConfig.file_watcher_mergebase_with_hg flowconfig
      | None -> None
    in
    let mergebase_with =
      match mergebase_with with
      | Some x -> Some x
      | None -> FlowConfig.file_watcher_mergebase_with flowconfig
    in
    (match mergebase_with with
    | Some x -> x
    | None -> default_file_watcher_mergebase_with)

let choose_file_watcher_timeout ~flowconfig cli_timeout =
  match Base.Option.first_some cli_timeout (FlowConfig.file_watcher_timeout flowconfig) with
  | Some 0 -> None
  | Some x -> Some (float x)
  | None -> Some (float default_file_watcher_timeout)

(* Reads the file from disk to compute the offset. This can lead to strange results -- if the file
 * has changed since the location was constructed, the offset could be incorrect. If the file has
 * changed such that the contents no longer have text at the given line/column, the offset is not
 * included in the JSON output. *)
let json_of_loc_with_offset ?stdin_file ~strip_root loc =
  Base.Option.(
    let file_content =
      let path = Loc.source loc >>= File_key.to_path %> Base.Result.ok in
      match stdin_file with
      | Some fileinput when path = File_input.path_of_file_input fileinput ->
        Some (File_input.content_of_file_input_unsafe fileinput)
      | _ -> path >>= Sys_utils.cat_or_failed
    in
    let offset_table =
      Base.Option.map file_content ~f:(Offset_utils.make ~kind:Offset_utils.Utf8)
    in
    Reason.json_of_loc ~strip_root ~offset_table ~catch_offset_errors:true loc
  )

let subcommand_spec ~name ~doc cmd_list =
  let command_info =
    cmd_list
    |> Base.List.map ~f:(fun (name, command) -> (name, CommandSpec.doc command))
    |> List.filter (fun (cmd, doc) -> cmd <> "" && doc <> "")
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
    |> CommandSpec.format_two_columns ~col_pad:1
  in
  {
    CommandSpec.name;
    doc;
    usage =
      Printf.sprintf
        "Usage: %s %s SUBCOMMAND [OPTIONS]...\n\nValid values for SUBCOMMAND:\n%s\n"
        exe_name
        name
        command_info;
    args = CommandSpec.ArgSpec.(empty |> anon "subcommand" (required (command cmd_list)));
  }

type codemod_params =
  | Codemod_params of {
      options_flags: Options_flags.t;
      saved_state_options_flags: Saved_state_flags.t;
      shm_flags: shared_mem_params;
      ignore_version: bool;
      write: bool;
      repeat: bool;
      log_level: Hh_logger.Level.t option;
      root: string option;
      input_file: string option;
      base_flag: Base_flags.t;
      anon: string list option;
    }

let collect_codemod_flags
    main
    options_flags
    saved_state_options_flags
    shm_flags
    ignore_version
    write
    repeat
    log_level
    root
    input_file
    base_flag
    anon =
  ( if (not write) && repeat then
    let msg = "Error: cannot run codemod with --repeat flag unless --write is also passed" in
    Exit.(exit ~msg Commandline_usage_error)
  );
  let codemod_flags =
    Codemod_params
      {
        options_flags;
        saved_state_options_flags;
        shm_flags;
        ignore_version;
        write;
        repeat;
        log_level;
        root;
        input_file;
        base_flag;
        anon;
      }
  in
  main codemod_flags

let codemod_flags prev =
  let log_level_enum =
    [
      ("off", Hh_logger.Level.Off);
      ("fatal", Hh_logger.Level.Fatal);
      ("error", Hh_logger.Level.Error);
      ("warn", Hh_logger.Level.Warn);
      ("info", Hh_logger.Level.Info);
      ("debug", Hh_logger.Level.Debug);
    ]
  in
  let log_levels = List.map fst log_level_enum in
  CommandSpec.ArgSpec.(
    prev
    |> collect collect_codemod_flags
    |> options_flags
    |> saved_state_flags
    |> shm_flags
    |> from_flag
    |> ignore_version_flag
    |> flag "--write" truthy ~doc:"Edit files in place"
    |> flag "--repeat" truthy ~doc:"Run this codemod repeatedly until no more files change"
    |> flag
         "--log-level"
         (enum log_level_enum)
         ~env:"FLOW_LOG_LEVEL"
         ~doc:(Utils_js.spf "Verbosity of logging (%s)" (String.concat "|" log_levels))
    |> root_flag
    |> input_file_flag
         "File containing a list of paths to transform. Incompatible with stdin and FILE..."
    |> base_flags
    |> anon "FILE..." (list_of string)
  )
