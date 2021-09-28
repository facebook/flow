(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

let ( >>= ) = Base.Result.( >>= )

type line = int * string

type section = line * line list

type warning = int * string

type error = int * string

type file_watcher =
  | NoFileWatcher
  | DFind
  | Watchman

type lazy_mode =
  | Lazy
  | Non_lazy
  | Watchman_DEPRECATED  (** lazy_mode=watchman is deprecated, but implies file_watcher=Watchman *)

let default_temp_dir = Filename.concat Sys_utils.temp_dir_name "flow"

let map_add map (key, value) = SMap.add key value map

module Opts = struct
  type raw_value = int * string

  type raw_values = raw_value list

  type raw_options = raw_values SMap.t

  type error_kind =
    | Failed_to_parse_value of string
    | Failed_to_set of string
    | Duplicate_option

  type opt_error = int * error_kind

  type t = {
    abstract_locations: bool option;
    all: bool option;
    autoimports: bool option;
    automatic_require_default: bool option;
    babel_loose_array_spread: bool option;
    disable_live_non_parse_errors: bool option;
    emoji: bool option;
    enable_const_params: bool;
    enforce_local_inference_annotations: bool;
    local_inference_annotation_dirs: string list;
    enforce_strict_call_arity: bool;
    enums: bool;
    exact_by_default: bool;
    facebook_fbs: string option;
    facebook_fbt: string option;
    facebook_module_interop: bool;
    file_watcher_mergebase_with: string option;
    file_watcher_timeout: int option;
    file_watcher: file_watcher option;
    format_bracket_spacing: bool option;  (** print spaces between brackets in object literals *)
    format_single_quotes: bool option;  (** prefer single-quoted strings *)
    gc_worker_custom_major_ratio: int option;  (** Gc.control's custom_major_ratio *)
    gc_worker_custom_minor_max_size: int option;  (** Gc.control's custom_minor_max_size *)
    gc_worker_custom_minor_ratio: int option;  (** Gc.control's custom_minor_ratio *)
    gc_worker_major_heap_increment: int option;  (** Gc.control's major_heap_increment *)
    gc_worker_minor_heap_size: int option;  (** Gc.control's minor_heap_size *)
    gc_worker_space_overhead: int option;  (** Gc.control's space_overhead *)
    gc_worker_window_size: int option;  (** Gc.control's window_size *)
    generate_tests: bool;
    haste_module_ref_prefix: string option;
    haste_name_reducers: (Str.regexp * string) list;
    haste_paths_excludes: string list;
    haste_paths_includes: string list;
    haste_use_name_reducers: bool;
    ignore_non_literal_requires: bool;
    include_warnings: bool;
    indexed_access: bool;
    lazy_mode: lazy_mode option;
    log_file: Path.t option;
    log_saving: Options.log_saving SMap.t;
    max_files_checked_per_worker: int;
    max_header_tokens: int;
    max_literal_length: int;
    max_rss_bytes_for_check_per_worker: int;
    max_seconds_for_check_per_worker: float;
    max_workers: int;
    merge_timeout: int option;
    module_file_exts: SSet.t;
    module_name_mappers: (Str.regexp * string) list;
    module_resource_exts: SSet.t;
    module_system: Options.module_system;
    modules_are_use_strict: bool;
    munge_underscores: bool;
    new_merge: bool;
    no_flowlib: bool;
    node_main_fields: string list;
    node_resolver_allow_root_relative: bool;
    node_resolver_dirnames: string list;
    node_resolver_root_relative_dirnames: string list;
    prioritize_dependency_checks: bool option;
    react_runtime: Options.react_runtime;
    react_server_component_exts: SSet.t;
    recursion_limit: int;
    refactor: bool option;
    reorder_checking: Options.order_mode;
    root_name: string option;
    run_post_inference_implicit_instantiation: bool;
    saved_state_fetcher: Options.saved_state_fetcher;
    saved_state_load_sighashes: bool;
    shm_hash_table_pow: int;
    shm_heap_size: int;
    shm_log_level: int;
    strict_es6_import_export_excludes: string list;
    strict_es6_import_export: bool;
    suppress_types: SSet.t;
    temp_dir: string;
    env_mode: Options.env_mode;
    traces: int;
    trust_mode: Options.trust_mode;
    type_asserts: bool;
    wait_for_recheck: bool;
    watchman_defer_states: string list;
    watchman_survive_restarts: bool option;
    watchman_sync_timeout: int option;
    weak: bool;
  }

  let warn_on_unknown_opts (raw_opts, config) : (t * warning list, error) result =
    (* If the user specified any options that aren't defined, issue a warning *)
    let warnings =
      SMap.elements raw_opts
      |> Base.List.fold_left
           ~f:(fun acc (k, v) ->
             let msg = spf "Unsupported option specified! (%s)" k in
             Base.List.fold_left ~f:(fun acc (line_num, _) -> (line_num, msg) :: acc) ~init:acc v)
           ~init:[]
    in
    Ok (config, warnings)

  let module_file_exts =
    SSet.empty
    |> SSet.add ".js"
    |> SSet.add ".jsx"
    |> SSet.add ".json"
    |> SSet.add ".mjs"
    |> SSet.add ".cjs"

  let module_resource_exts =
    SSet.empty
    |> SSet.add ".css"
    |> SSet.add ".jpg"
    |> SSet.add ".png"
    |> SSet.add ".gif"
    |> SSet.add ".eot"
    |> SSet.add ".svg"
    |> SSet.add ".ttf"
    |> SSet.add ".woff"
    |> SSet.add ".woff2"
    |> SSet.add ".mp4"
    |> SSet.add ".webm"
    |> SSet.add ".webp"

  let react_server_component_exts = SSet.empty

  let default_options =
    {
      abstract_locations = None;
      all = None;
      autoimports = None;
      automatic_require_default = None;
      babel_loose_array_spread = None;
      disable_live_non_parse_errors = None;
      emoji = None;
      enable_const_params = false;
      enforce_local_inference_annotations = false;
      local_inference_annotation_dirs = [];
      enforce_strict_call_arity = true;
      enums = false;
      exact_by_default = false;
      facebook_fbs = None;
      facebook_fbt = None;
      facebook_module_interop = false;
      file_watcher = None;
      file_watcher_mergebase_with = None;
      file_watcher_timeout = None;
      format_bracket_spacing = None;
      format_single_quotes = None;
      gc_worker_custom_major_ratio = None;
      gc_worker_custom_minor_max_size = None;
      gc_worker_custom_minor_ratio = None;
      gc_worker_major_heap_increment = None;
      gc_worker_minor_heap_size = None;
      gc_worker_space_overhead = None;
      gc_worker_window_size = None;
      generate_tests = false;
      haste_module_ref_prefix = None;
      haste_name_reducers =
        [(Str.regexp "^\\(.*/\\)?\\([a-zA-Z0-9$_.-]+\\)\\.js\\(\\.flow\\)?$", "\\2")];
      haste_paths_excludes = ["\\(.*\\)?/node_modules/.*"];
      haste_paths_includes = ["<PROJECT_ROOT>/.*"];
      haste_use_name_reducers = false;
      ignore_non_literal_requires = false;
      include_warnings = false;
      indexed_access = true;
      lazy_mode = None;
      log_file = None;
      log_saving = SMap.empty;
      max_files_checked_per_worker = 100;
      max_header_tokens = 10;
      max_literal_length = 100;
      max_rss_bytes_for_check_per_worker = (* 200MB *) 200 * 1024 * 1024;
      max_seconds_for_check_per_worker = 5.0;
      max_workers = Sys_utils.nbr_procs;
      merge_timeout = Some 100;
      module_file_exts;
      module_name_mappers = [];
      module_resource_exts;
      module_system = Options.Node;
      modules_are_use_strict = false;
      munge_underscores = false;
      new_merge = false;
      no_flowlib = false;
      node_main_fields = ["main"];
      node_resolver_allow_root_relative = false;
      node_resolver_dirnames = ["node_modules"];
      node_resolver_root_relative_dirnames = [""];
      prioritize_dependency_checks = None;
      react_runtime = Options.ReactRuntimeClassic;
      react_server_component_exts;
      recursion_limit = 10000;
      refactor = None;
      reorder_checking = Options.Lexical;
      root_name = None;
      run_post_inference_implicit_instantiation = false;
      saved_state_fetcher = Options.Dummy_fetcher;
      saved_state_load_sighashes = false;
      shm_hash_table_pow = 19;
      shm_heap_size = (* 25GB *) 1024 * 1024 * 1024 * 25;
      shm_log_level = 0;
      strict_es6_import_export = false;
      strict_es6_import_export_excludes = [];
      suppress_types = SSet.empty |> SSet.add "$FlowFixMe";
      temp_dir = default_temp_dir;
      env_mode = Options.ClassicEnv [];
      traces = 0;
      trust_mode = Options.NoTrust;
      type_asserts = false;
      wait_for_recheck = false;
      watchman_defer_states = [];
      watchman_survive_restarts = None;
      watchman_sync_timeout = None;
      weak = false;
    }

  let parse_lines : line list -> (raw_options, error) result =
    let rec loop acc lines =
      acc >>= fun map ->
      match lines with
      | [] -> Ok map
      | (line_num, line) :: rest ->
        if Str.string_match (Str.regexp "^\\([a-zA-Z0-9._]+\\)=\\(.*\\)$") line 0 then
          let key = Str.matched_group 1 line in
          let value = Str.matched_group 2 line in
          let map =
            SMap.add
              key
              ((line_num, value)
               ::
               (match SMap.find_opt key map with
               | Some values -> values
               | None -> []))
              map
          in
          loop (Ok map) rest
        else
          Error (line_num, "Unable to parse line.")
    in
    fun lines ->
      let lines =
        lines
        |> Base.List.map ~f:(fun (ln, line) -> (ln, String.trim line))
        |> Base.List.filter ~f:(fun (_, s) -> s <> "")
      in
      loop (Ok SMap.empty) lines

  (**
    * `init` gets called on the options object immediately before
    * parsing the *first* occurrence of the user-specified config option. This
    * is useful in cases where the user's value should blow away the default
    * value (rather than being aggregated to it).
    *
    * For example: We want the default value of 'module.file_ext' to be
    * ['.js'; '.jsx'], but if the user specifies any 'module.file_ext'
    * settings, we want to start from a clean list.
    *)
  let opt =
    let rec loop optparser setter values config =
      match values with
      | [] -> Ok config
      | (line_num, value_str) :: rest ->
        let value =
          optparser value_str
          |> Base.Result.map_error ~f:(fun msg -> (line_num, Failed_to_parse_value msg))
        in
        let config =
          value >>= fun value ->
          setter config value |> Base.Result.map_error ~f:(fun msg -> (line_num, Failed_to_set msg))
        in
        config >>= loop optparser setter rest
    in
    fun (optparser : string -> ('a, string) result)
        ?init
        ?(multiple = false)
        (setter : t -> 'a -> (t, string) result)
        (values : raw_values)
        config : (t, opt_error) result ->
      let config =
        match init with
        | None -> config
        | Some f -> f config
      in
      (* Error when duplicate options were incorrectly given *)
      match (multiple, values) with
      | (false, _ :: (dupe_ln, _) :: _) -> Error (dupe_ln, Duplicate_option)
      | _ -> loop optparser setter values config

  let optparse_string str =
    try Ok (Scanf.unescaped str) with
    | Scanf.Scan_failure reason -> Error (spf "Invalid ocaml string: %s" reason)

  let enum values =
    opt (fun str ->
        let values = Base.List.fold_left ~f:map_add ~init:SMap.empty values in
        match SMap.find_opt str values with
        | Some v -> Ok v
        | None ->
          Error
            (spf
               "Unsupported value: \"%s\". Supported values are: %s"
               str
               (String.concat ", " (SMap.keys values))))

  let filepath = opt (fun str -> Ok (Path.make str))

  let optparse_mapping str =
    let regexp_str = "^'\\([^']*\\)'[ \t]*->[ \t]*'\\([^']*\\)'$" in
    let regexp = Str.regexp regexp_str in
    if Str.string_match regexp str 0 then
      Ok (Str.matched_group 1 str, Str.matched_group 2 str)
    else
      Error ("Expected a mapping of form: " ^ "'single-quoted-string' -> 'single-quoted-string'")

  let boolean = enum [("true", true); ("false", false)]

  let string = opt optparse_string

  let uint =
    opt (fun str ->
        let v = int_of_string str in
        if v < 0 then
          Error "Number cannot be negative!"
        else
          Ok v)

  let mapping fn = opt (fun str -> optparse_mapping str >>= fn)

  let optparse_json str =
    try Ok (Hh_json.json_of_string str) with
    | Hh_json.Syntax_error msg -> Error (spf "Failed to parse JSON: %s" msg)

  let json = opt optparse_json

  (* TODO: remove once .flowconfigs no longer use this setting *)
  let new_check_parser =
    boolean (fun opts v ->
        if v then
          Ok opts
        else
          Error "New check mode can no longer be disabled.")

  let new_merge_parser = boolean (fun opts v -> Ok { opts with new_merge = v })

  let max_files_checked_per_worker_parser =
    uint (fun opts v -> Ok { opts with max_files_checked_per_worker = v })

  let max_seconds_for_check_per_worker_parser =
    uint (fun opts v -> Ok { opts with max_seconds_for_check_per_worker = float v })

  let max_rss_bytes_for_check_per_worker_parser =
    uint (fun opts v -> Ok { opts with max_rss_bytes_for_check_per_worker = v })

  let file_ext_parser =
    string
      ~init:(fun opts -> { opts with module_file_exts = SSet.empty })
      ~multiple:true
      (fun opts v ->
        if String_utils.string_ends_with v Files.flow_ext then
          Error
            ("Cannot use file extension '"
            ^ v
            ^ "' since it ends with the reserved extension '"
            ^ Files.flow_ext
            ^ "'")
        else
          let module_file_exts = SSet.add v opts.module_file_exts in
          Ok { opts with module_file_exts })

  let haste_name_reducers_parser =
    mapping
      ~init:(fun opts -> { opts with haste_name_reducers = [] })
      ~multiple:true
      (fun (pattern, template) -> Ok (Str.regexp pattern, template))
      (fun opts v -> Ok { opts with haste_name_reducers = v :: opts.haste_name_reducers })

  let haste_paths_excludes_parser =
    string
      ~init:(fun opts -> { opts with haste_paths_excludes = [] })
      ~multiple:true
      (fun opts v -> Ok { opts with haste_paths_excludes = v :: opts.haste_paths_excludes })

  let log_saving_parser =
    let init opts = { opts with log_saving = SMap.empty } in
    let multiple = true in
    let parse opts json =
      let open Hh_json in
      let open Base.Result.Let_syntax in
      let%bind (method_name, threshold_time_ms_str, limit_json, rate_str) =
        match json with
        | JSON_Array [JSON_String a; JSON_Number b; c; JSON_Number d] -> Ok (a, b, c, d)
        | _ -> Error "must be of the form [\"method name\", threshold_time_ms, limit, rate]"
      in
      let threshold_time_ms = int_of_string threshold_time_ms_str in
      let%bind limit =
        match limit_json with
        | JSON_Null -> Ok None
        | JSON_Number limit_str -> Ok (Some (int_of_string limit_str))
        | _ -> Error "limit (third element) must be either number or null"
      in
      let rate = float_of_string rate_str in
      let%bind () =
        Base.Result.ok_if_true
          (0. <= rate && rate <= 100.)
          ~error:"rate (fourth element) means a percentage, so must be within 0 to 100"
      in
      let log_saving =
        SMap.add method_name Options.{ threshold_time_ms; limit; rate } opts.log_saving
      in
      return { opts with log_saving }
    in
    json ~init ~multiple parse

  let haste_paths_includes_parser =
    string
      ~init:(fun opts -> { opts with haste_paths_includes = [] })
      ~multiple:true
      (fun opts v -> Ok { opts with haste_paths_includes = v :: opts.haste_paths_includes })

  let enforce_local_inference_annotations =
    boolean (fun opts v -> Ok { opts with enforce_local_inference_annotations = v })

  let local_inference_annotation_dirs =
    string
      ~init:(fun opts -> { opts with local_inference_annotation_dirs = [] })
      ~multiple:true
      (fun opts v ->
        if opts.enforce_local_inference_annotations then
          Ok
            {
              opts with
              local_inference_annotation_dirs = v :: opts.local_inference_annotation_dirs;
            }
        else
          Error
            "Option \"enforce_local_inference_annotations\" must be set to true to set \"local_inference_annotation_dirs\".")

  let reorder_checking_parser =
    enum
      [
        ("lexical", Options.Lexical);
        ("dependency", Options.Dependency);
        ("lexical_with_dependency_validation", Options.LexicalWithDependencyValidation);
      ]
      (fun opts v -> Ok { opts with reorder_checking = v })

  let post_inference_implicit_instantiation_parser =
    boolean (fun opts v -> Ok { opts with run_post_inference_implicit_instantiation = v })

  let abstract_locations_parser =
    boolean (fun opts v -> Ok { opts with abstract_locations = Some v })

  let automatic_require_default_parser =
    boolean (fun opts v -> Ok { opts with automatic_require_default = Some v })

  let babel_loose_array_spread_parser =
    boolean (fun opts v -> Ok { opts with babel_loose_array_spread = Some v })

  let disable_live_non_parse_errors_parser =
    boolean (fun opts v -> Ok { opts with disable_live_non_parse_errors = Some v })

  let enforce_strict_call_arity_parser =
    boolean (fun opts v -> Ok { opts with enforce_strict_call_arity = v })

  let facebook_module_interop_parser =
    boolean (fun opts v -> Ok { opts with facebook_module_interop = v })

  let file_watcher_parser =
    enum [("none", NoFileWatcher); ("dfind", DFind); ("watchman", Watchman)] (fun opts v ->
        Ok { opts with file_watcher = Some v })

  let file_watcher_mergebase_with_parser =
    string (fun opts v -> Ok { opts with file_watcher_mergebase_with = Some v })

  let format_bracket_spacing_parser =
    boolean (fun opts v -> Ok { opts with format_bracket_spacing = Some v })

  let format_single_quotes_parser =
    boolean (fun opts v -> Ok { opts with format_single_quotes = Some v })

  let haste_module_ref_prefix_parser =
    string (fun opts v -> Ok { opts with haste_module_ref_prefix = Some v })

  let haste_use_name_reducers_parser =
    boolean
      ~init:(fun opts -> { opts with haste_use_name_reducers = false })
      (fun opts v -> Ok { opts with haste_use_name_reducers = v })

  let gc_worker_major_heap_increment_parser =
    uint (fun opts v -> Ok { opts with gc_worker_major_heap_increment = Some v })

  let gc_worker_minor_heap_size_parser =
    uint (fun opts v -> Ok { opts with gc_worker_minor_heap_size = Some v })

  let gc_worker_space_overhead_parser =
    uint (fun opts v -> Ok { opts with gc_worker_space_overhead = Some v })

  let gc_worker_window_size_parser =
    uint (fun opts v -> Ok { opts with gc_worker_window_size = Some v })

  let gc_worker_custom_major_ratio_parser =
    uint (fun opts v -> Ok { opts with gc_worker_custom_major_ratio = Some v })

  let gc_worker_custom_minor_ratio_parser =
    uint (fun opts v -> Ok { opts with gc_worker_custom_minor_ratio = Some v })

  let gc_worker_custom_minor_max_size_parser =
    uint (fun opts v -> Ok { opts with gc_worker_custom_minor_max_size = Some v })

  let ignore_non_literal_requires_parser =
    boolean (fun opts v -> Ok { opts with ignore_non_literal_requires = v })

  let lazy_mode_parser =
    enum
      [
        ("true", Lazy);
        ("false", Non_lazy);
        (* legacy, deprecated *)
        ("fs", Lazy);
        ("watchman", Watchman_DEPRECATED);
        ("none", Non_lazy);
      ]
      (fun opts v -> Ok { opts with lazy_mode = Some v })

  let merge_timeout_parser =
    uint (fun opts v ->
        let merge_timeout =
          if v = 0 then
            None
          else
            Some v
        in
        Ok { opts with merge_timeout })

  let module_system_parser =
    enum [("node", Options.Node); ("haste", Options.Haste)] (fun opts v ->
        Ok { opts with module_system = v })

  let name_mapper_parser =
    mapping
      ~multiple:true
      (fun (pattern, template) -> Ok (Str.regexp pattern, template))
      (fun opts v ->
        let module_name_mappers = v :: opts.module_name_mappers in
        Ok { opts with module_name_mappers })

  let name_mapper_extension_parser =
    mapping
      ~multiple:true
      (fun (file_ext, template) ->
        Ok (Str.regexp ("^\\(.*\\)\\." ^ Str.quote file_ext ^ "$"), template))
      (fun opts v ->
        let module_name_mappers = v :: opts.module_name_mappers in
        Ok { opts with module_name_mappers })

  let node_main_field_parser =
    string
      ~init:(fun opts -> { opts with node_main_fields = [] })
      ~multiple:true
      (fun opts v ->
        let node_main_fields = v :: opts.node_main_fields in
        Ok { opts with node_main_fields })

  let node_resolve_dirname_parser =
    string
      ~init:(fun opts -> { opts with node_resolver_dirnames = [] })
      ~multiple:true
      (fun opts v ->
        if v = Filename.current_dir_name || v = Filename.parent_dir_name then
          Error
            (spf
               "%S is not a valid value for `module.system.node.resolve_dirname`. Each value must be a valid directory name. Maybe try `module.system.node.allow_root_relative=true`?"
               v)
        else
          let node_resolver_dirnames = v :: opts.node_resolver_dirnames in
          Ok { opts with node_resolver_dirnames })

  let node_resolver_allow_root_relative_parser =
    boolean (fun opts v -> Ok { opts with node_resolver_allow_root_relative = v })

  let node_resolver_root_relative_dirnames_parser =
    string
      ~init:(fun opts -> { opts with node_resolver_root_relative_dirnames = [] })
      ~multiple:true
      (fun opts v ->
        let node_resolver_root_relative_dirnames = v :: opts.node_resolver_root_relative_dirnames in
        Ok { opts with node_resolver_root_relative_dirnames })

  let prioritize_dependency_checks_parser =
    boolean (fun opts v -> Ok { opts with prioritize_dependency_checks = Some v })

  let react_runtime_parser =
    enum
      [("classic", Options.ReactRuntimeClassic); ("automatic", Options.ReactRuntimeAutomatic)]
      (fun opts react_runtime -> Ok { opts with react_runtime })

  let react_server_component_exts_parser =
    string
      ~init:(fun opts -> { opts with react_server_component_exts = SSet.empty })
      ~multiple:true
      (fun opts v ->
        let react_server_component_exts = SSet.add v opts.react_server_component_exts in
        Ok { opts with react_server_component_exts })

  let root_name_parser =
    string (fun opts v ->
        FlowEventLogger.set_root_name (Some v);
        Ok { opts with root_name = Some v })

  let saved_state_fetcher_parser =
    enum
      [
        ("none", Options.Dummy_fetcher); ("local", Options.Local_fetcher); ("fb", Options.Fb_fetcher);
      ]
      (fun opts saved_state_fetcher -> Ok { opts with saved_state_fetcher })

  let shm_hash_table_pow_parser =
    uint (fun opts shm_hash_table_pow -> Ok { opts with shm_hash_table_pow })

  let strict_es6_import_export_excludes_parser =
    string
      ~init:(fun opts -> { opts with strict_es6_import_export_excludes = [] })
      ~multiple:true
      (fun opts v ->
        Ok
          {
            opts with
            strict_es6_import_export_excludes = v :: opts.strict_es6_import_export_excludes;
          })

  let strict_es6_import_export_parser =
    boolean (fun opts v -> Ok { opts with strict_es6_import_export = v })

  let suppress_types_parser =
    string
      ~init:(fun opts -> { opts with suppress_types = SSet.empty })
      ~multiple:true
      (fun opts v -> Ok { opts with suppress_types = SSet.add v opts.suppress_types })

  let trust_mode_parser =
    enum
      [("check", Options.CheckTrust); ("silent", Options.SilentTrust); ("none", Options.NoTrust)]
      (fun opts trust_mode -> Ok { opts with trust_mode })

  let env_mode_parser =
    string (fun opts s ->
        match String.split_on_char ',' s |> Base.List.filter ~f:(( <> ) "") with
        | [] -> Error "env_mode requires an argument"
        | ["ssa"] -> Ok { opts with env_mode = Options.SSAEnv }
        | ["classic"] -> Ok { opts with env_mode = Options.ClassicEnv [] }
        | options ->
          let options =
            Base.List.fold_result options ~init:[] ~f:(fun acc opt ->
                match opt with
                | "constrain_writes" -> Ok (Options.ConstrainWrites :: acc)
                | "no_variables_without_providers" ->
                  Ok (Options.NoVariablesWithoutProviders :: acc)
                | "ssa" -> Error "\"ssa\" must be the first and only env_mode option if present"
                | "classic" ->
                  Error "\"classic\" must be the first and only env_mode option if present"
                | opt -> Error (spf "\"%s\" is not a valid env_mode option" opt))
          in
          Base.Result.map
            ~f:(fun options -> { opts with env_mode = Options.ClassicEnv options })
            options)

  let watchman_defer_states_parser =
    string ~multiple:true (fun opts v ->
        Ok { opts with watchman_defer_states = v :: opts.watchman_defer_states })

  let watchman_survive_restarts_parser =
    boolean (fun opts v -> Ok { opts with watchman_survive_restarts = Some v })

  let watchman_sync_timeout_parser =
    uint (fun opts v -> Ok { opts with watchman_sync_timeout = Some v })

  let parsers =
    [
      ("all", boolean (fun opts v -> Ok { opts with all = Some v }));
      ("autoimports", boolean (fun opts v -> Ok { opts with autoimports = Some v }));
      ("babel_loose_array_spread", babel_loose_array_spread_parser);
      ("emoji", boolean (fun opts v -> Ok { opts with emoji = Some v }));
      ("enums", boolean (fun opts v -> Ok { opts with enums = v }));
      ("exact_by_default", boolean (fun opts v -> Ok { opts with exact_by_default = v }));
      ("experimental.abstract_locations", abstract_locations_parser);
      ("experimental.const_params", boolean (fun opts v -> Ok { opts with enable_const_params = v }));
      ("experimental.disable_live_non_parse_errors", disable_live_non_parse_errors_parser);
      ("experimental.enforce_local_inference_annotations", enforce_local_inference_annotations);
      ("experimental.local_inference_annotation_dirs", local_inference_annotation_dirs);
      ("experimental.enums", boolean (fun opts v -> Ok { opts with enums = v }));
      ("experimental.facebook_module_interop", facebook_module_interop_parser);
      ("experimental.module.automatic_require_default", automatic_require_default_parser);
      ("experimental.new_check", new_check_parser);
      ("experimental.new_merge", new_merge_parser);
      ("experimental.prioritize_dependency_checks", prioritize_dependency_checks_parser);
      ("experimental.react.server_component_ext", react_server_component_exts_parser);
      ("experimental.refactor", boolean (fun opts v -> Ok { opts with refactor = Some v }));
      ("experimental.reorder_checking", reorder_checking_parser);
      ( "experimental.run_post_inference_implicit_instantiation",
        post_inference_implicit_instantiation_parser );
      ("experimental.strict_call_arity", enforce_strict_call_arity_parser);
      ("experimental.strict_es6_import_export.excludes", strict_es6_import_export_excludes_parser);
      ("experimental.strict_es6_import_export", strict_es6_import_export_parser);
      ("experimental.env_mode", env_mode_parser);
      ("experimental.type_asserts", boolean (fun opts v -> Ok { opts with type_asserts = v }));
      ("facebook.fbs", string (fun opts v -> Ok { opts with facebook_fbs = Some v }));
      ("facebook.fbt", string (fun opts v -> Ok { opts with facebook_fbt = Some v }));
      ("file_watcher_timeout", uint (fun opts v -> Ok { opts with file_watcher_timeout = Some v }));
      ("file_watcher.mergebase_with", file_watcher_mergebase_with_parser);
      ("file_watcher.watchman.defer_state", watchman_defer_states_parser);
      ("file_watcher.watchman.survive_restarts", watchman_survive_restarts_parser);
      ("file_watcher.watchman.sync_timeout", watchman_sync_timeout_parser);
      ("file_watcher", file_watcher_parser);
      ("format.bracket_spacing", format_bracket_spacing_parser);
      ("format.single_quotes", format_single_quotes_parser);
      ("gc.worker.custom_major_ratio", gc_worker_custom_major_ratio_parser);
      ("gc.worker.custom_minor_max_size", gc_worker_custom_minor_max_size_parser);
      ("gc.worker.custom_minor_ratio", gc_worker_custom_minor_ratio_parser);
      ("gc.worker.major_heap_increment", gc_worker_major_heap_increment_parser);
      ("gc.worker.minor_heap_size", gc_worker_minor_heap_size_parser);
      ("gc.worker.space_overhead", gc_worker_space_overhead_parser);
      ("gc.worker.window_size", gc_worker_window_size_parser);
      ("include_warnings", boolean (fun opts v -> Ok { opts with include_warnings = v }));
      ("indexed_access", boolean (fun opts v -> Ok { opts with indexed_access = v }));
      ("lazy_mode", lazy_mode_parser);
      ("log.file", filepath (fun opts v -> Ok { opts with log_file = Some v }));
      ("log_saving", log_saving_parser);
      ("max_header_tokens", uint (fun opts v -> Ok { opts with max_header_tokens = v }));
      ("max_literal_length", uint (fun opts v -> Ok { opts with max_literal_length = v }));
      ("merge_timeout", merge_timeout_parser);
      ("module.file_ext", file_ext_parser);
      ("module.ignore_non_literal_requires", ignore_non_literal_requires_parser);
      ("module.name_mapper.extension", name_mapper_extension_parser);
      ("module.name_mapper", name_mapper_parser);
      ("module.system.haste.module_ref_prefix", haste_module_ref_prefix_parser);
      ("module.system.haste.name_reducers", haste_name_reducers_parser);
      ("module.system.haste.paths.excludes", haste_paths_excludes_parser);
      ("module.system.haste.paths.includes", haste_paths_includes_parser);
      ("module.system.haste.use_name_reducers", haste_use_name_reducers_parser);
      ("module.system.node.allow_root_relative", node_resolver_allow_root_relative_parser);
      ("module.system.node.main_field", node_main_field_parser);
      ("module.system.node.resolve_dirname", node_resolve_dirname_parser);
      ("module.system.node.root_relative_dirname", node_resolver_root_relative_dirnames_parser);
      ("module.system", module_system_parser);
      ("module.use_strict", boolean (fun opts v -> Ok { opts with modules_are_use_strict = v }));
      ("munge_underscores", boolean (fun opts v -> Ok { opts with munge_underscores = v }));
      ("name", root_name_parser);
      ("no_flowlib", boolean (fun opts v -> Ok { opts with no_flowlib = v }));
      ("react.runtime", react_runtime_parser);
      ("recursion_limit", uint (fun opts v -> Ok { opts with recursion_limit = v }));
      ("saved_state.fetcher", saved_state_fetcher_parser);
      ( "saved_state.load_sighashes",
        boolean (fun opts v -> Ok { opts with saved_state_load_sighashes = v }) );
      ("server.max_workers", uint (fun opts v -> Ok { opts with max_workers = v }));
      ("sharedmemory.hash_table_pow", shm_hash_table_pow_parser);
      ("sharedmemory.heap_size", uint (fun opts shm_heap_size -> Ok { opts with shm_heap_size }));
      ("sharedmemory.log_level", uint (fun opts shm_log_level -> Ok { opts with shm_log_level }));
      ("suppress_type", suppress_types_parser);
      ("temp_dir", string (fun opts v -> Ok { opts with temp_dir = v }));
      ("traces", uint (fun opts v -> Ok { opts with traces = v }));
      ("trust_mode", trust_mode_parser);
      ("types_first.max_files_checked_per_worker", max_files_checked_per_worker_parser);
      ("types_first.max_rss_bytes_for_check_per_worker", max_rss_bytes_for_check_per_worker_parser);
      ("types_first.max_seconds_for_check_per_worker", max_seconds_for_check_per_worker_parser);
      ("wait_for_recheck", boolean (fun opts v -> Ok { opts with wait_for_recheck = v }));
      ("weak", boolean (fun opts v -> Ok { opts with weak = v }));
    ]

  let parse =
    let error_of_opt_error key (line_num, opt_error) =
      let msg =
        match opt_error with
        | Failed_to_parse_value msg -> spf "Error parsing value for \"%s\". %s" key msg
        | Failed_to_set msg -> spf "Error setting value for \"%s\". %s" key msg
        | Duplicate_option -> spf "Duplicate option: \"%s\"" key
      in
      (line_num, msg)
    in
    let rec loop
        (acc : (raw_options * t, error) result)
        (parsers : (string * (raw_values -> t -> (t, opt_error) result)) list) =
      acc >>= fun (raw_opts, config) ->
      match parsers with
      | [] -> Ok (raw_opts, config)
      | (key, f) :: rest ->
        let acc =
          match SMap.find_opt key raw_opts with
          | None -> Ok (raw_opts, config)
          | Some values ->
            f values config |> Base.Result.map_error ~f:(error_of_opt_error key) >>= fun config ->
            let new_raw_opts = SMap.remove key raw_opts in
            Ok (new_raw_opts, config)
        in
        loop acc rest
    in
    fun (init : t) (lines : line list) : (t * warning list, error) result ->
      parse_lines lines >>= fun raw_options ->
      loop (Ok (raw_options, init)) parsers >>= warn_on_unknown_opts
end

type rollout = {
  enabled_group: string;
  disabled_groups: SSet.t;
}

type config = {
  rollouts: rollout SMap.t;
  (* completely ignored files (both module resolving and typing) *)
  ignores: string list;
  (* files that should be treated as untyped *)
  untyped: string list;
  (* files that should be treated as declarations *)
  declarations: string list;
  (* non-root include paths *)
  includes: string list;
  (* library paths. no wildcards *)
  libs: string list;
  (* lint severities *)
  lint_severities: Severity.severity LintSettings.t;
  (* strict mode *)
  strict_mode: StrictModeSettings.t;
  (* config options *)
  options: Opts.t;
  (* version constraint *)
  version: string option;
}

type config_result = (config * warning list, error) result

module Pp : sig
  val config : out_channel -> config -> unit
end = struct
  open Printf

  let section_header o section = fprintf o "[%s]\n" section

  let ignores o = Base.List.iter ~f:(fprintf o "%s\n")

  let untyped o = Base.List.iter ~f:(fprintf o "%s\n")

  let declarations o = Base.List.iter ~f:(fprintf o "%s\n")

  let includes o = Base.List.iter ~f:(fprintf o "%s\n")

  let libs o = Base.List.iter ~f:(fprintf o "%s\n")

  let options =
    let pp_opt o name value = fprintf o "%s=%s\n" name value in
    let module_system = function
      | Options.Node -> "node"
      | Options.Haste -> "haste"
    in
    fun o config ->
      Opts.(
        let options = config.options in
        if options.module_system <> default_options.module_system then
          pp_opt o "module.system" (module_system options.module_system);
        if options.all <> default_options.all then
          pp_opt o "all" (string_of_bool (Base.Option.value options.all ~default:false));
        if options.weak <> default_options.weak then pp_opt o "weak" (string_of_bool options.weak);
        if options.temp_dir <> default_options.temp_dir then pp_opt o "temp_dir" options.temp_dir;
        if options.include_warnings <> default_options.include_warnings then
          pp_opt o "include_warnings" (string_of_bool options.include_warnings))

  let lints o config =
    let lint_severities = config.lint_severities in
    let lint_default = LintSettings.get_default lint_severities in
    (* Don't print an 'all' setting if it matches the default setting. *)
    if lint_default <> LintSettings.get_default LintSettings.empty_severities then
      fprintf o "all=%s\n" (Severity.string_of_severity lint_default);
    LintSettings.iter
      (fun kind (state, _) ->
        fprintf o "%s=%s\n" (Lints.string_of_kind kind) (Severity.string_of_severity state))
      lint_severities

  let strict o config =
    Lints.(
      let strict_mode = config.strict_mode in
      StrictModeSettings.iter (fun kind -> fprintf o "%s\n" (string_of_kind kind)) strict_mode)

  let section_if_nonempty o header f = function
    | [] -> ()
    | xs ->
      section_header o header;
      f o xs;
      fprintf o "\n"

  let config o config =
    section_header o "ignore";
    ignores o config.ignores;
    fprintf o "\n";
    section_if_nonempty o "untyped" untyped config.untyped;
    section_if_nonempty o "declarations" declarations config.declarations;
    section_header o "include";
    includes o config.includes;
    fprintf o "\n";
    section_header o "libs";
    libs o config.libs;
    fprintf o "\n";
    section_header o "lints";
    lints o config;
    fprintf o "\n";
    section_header o "options";
    options o config;
    fprintf o "\n";
    section_header o "strict";
    strict o config
end

let empty_config =
  {
    rollouts = SMap.empty;
    ignores = [];
    untyped = [];
    declarations = [];
    includes = [];
    libs = [];
    lint_severities = LintSettings.empty_severities;
    strict_mode = StrictModeSettings.empty;
    options = Opts.default_options;
    version = None;
  }

let group_into_sections : line list -> (section list, error) result =
  let is_section_header = Str.regexp "^\\[\\(.*\\)\\]$" in
  let rec loop acc lines =
    acc >>= fun (seen, sections, (section_name, section_lines)) ->
    match lines with
    | [] ->
      let section = (section_name, Base.List.rev section_lines) in
      Ok (Base.List.rev (section :: sections))
    | (ln, line) :: rest ->
      if Str.string_match is_section_header line 0 then
        let sections = (section_name, Base.List.rev section_lines) :: sections in
        let section_name = Str.matched_group 1 line in
        if SSet.mem section_name seen then
          Error (ln, spf "contains duplicate section: \"%s\"" section_name)
        else
          let seen = SSet.add section_name seen in
          let section = ((ln, section_name), []) in
          let acc = Ok (seen, sections, section) in
          loop acc rest
      else
        let acc = Ok (seen, sections, (section_name, (ln, line) :: section_lines)) in
        loop acc rest
  in
  (fun lines -> loop (Ok (SSet.empty, [], ((0, ""), []))) lines)

let trim_lines lines =
  lines
  |> Base.List.map ~f:(fun (_, line) -> String.trim line)
  |> Base.List.filter ~f:(fun s -> s <> "")

let trim_labeled_lines lines =
  lines
  |> Base.List.map ~f:(fun (label, line) -> (label, String.trim line))
  |> Base.List.filter ~f:(fun (_, s) -> s <> "")

(* parse [include] lines *)
let parse_includes lines config =
  let includes = trim_lines lines in
  Ok ({ config with includes }, [])

let parse_libs lines config : (config * warning list, error) result =
  let libs = trim_lines lines in
  Ok ({ config with libs }, [])

let parse_ignores lines config =
  let ignores = trim_lines lines in
  Ok ({ config with ignores }, [])

let parse_untyped lines config =
  let untyped = trim_lines lines in
  Ok ({ config with untyped }, [])

let parse_declarations lines config =
  let declarations = trim_lines lines in
  Ok ({ config with declarations }, [])

let parse_options lines config : (config * warning list, error) result =
  Opts.parse config.options lines >>= fun (options, warnings) ->
  Ok ({ config with options }, warnings)

let parse_version lines config =
  let potential_versions =
    lines
    |> Base.List.map ~f:(fun (ln, line) -> (ln, String.trim line))
    |> Base.List.filter ~f:(fun (_, s) -> s <> "")
  in
  match potential_versions with
  | (ln, version_str) :: _ ->
    if not (Semver.is_valid_range version_str) then
      Error
        ( ln,
          spf
            "Expected version to match %%d.%%d.%%d, with an optional leading ^, got %s"
            version_str )
    else
      Ok ({ config with version = Some version_str }, [])
  | _ -> Ok (config, [])

let parse_lints lines config : (config * warning list, error) result =
  let lines = trim_labeled_lines lines in
  LintSettings.of_lines config.lint_severities lines >>= fun (lint_severities, warnings) ->
  Ok ({ config with lint_severities }, warnings)

let parse_strict lines config =
  let lines = trim_labeled_lines lines in
  StrictModeSettings.of_lines lines >>= fun strict_mode -> Ok ({ config with strict_mode }, [])

(* Basically fold_left but with early exit when f returns an Error *)
let rec fold_left_stop_on_error
    (l : 'elem list) ~(acc : 'acc) ~(f : 'acc -> 'elem -> ('acc, 'error) result) :
    ('acc, 'error) result =
  match l with
  | [] -> Ok acc
  | elem :: rest -> f acc elem >>= fun acc -> fold_left_stop_on_error rest ~acc ~f

(* Rollouts are based on randomness, but we want it to be stable from run to run. So we seed our
 * pseudo random number generator with
 *
 * 1. The hostname
 * 2. The user
 * 3. The name of the rollout
 *)
let calculate_pct rollout_name =
  let state = Xx.init 0L in
  Xx.update state (Unix.gethostname ());
  Xx.update_int state (Unix.getuid ());
  Xx.update state rollout_name;
  let hash = Xx.digest state in
  Xx.modulo hash 100

(* The optional rollout section has 0 or more lines. Each line defines a single rollout. For example
 *
 * [rollouts]
 *
 * testA=40% on, 60% off
 * testB=50% blue, 20% yellow, 30% pink
 *
 * The first line defines a rollout named "testA" with two groups.
 * The second line defines a rollout named "testB" with three groups.
 *
 * Each rollout's groups must sum to 100.
 *)
let parse_rollouts config lines =
  Base.Option.value_map lines ~default:(Ok config) ~f:(fun lines ->
      let lines = trim_labeled_lines lines in
      fold_left_stop_on_error lines ~acc:SMap.empty ~f:(fun rollouts (line_num, line) ->
          (* A rollout's name is can only contain [a-zA-Z0-9._] *)
          if Str.string_match (Str.regexp "^\\([a-zA-Z0-9._]+\\)=\\(.*\\)$") line 0 then
            let rollout_name = Str.matched_group 1 line in
            let rollout_values_raw = Str.matched_group 2 line in
            let my_pct = calculate_pct rollout_name in
            fold_left_stop_on_error
              (* Groups are delimited with commas *)
              Str.(split (regexp ",") rollout_values_raw)
              ~acc:(None, SSet.empty, 0)
              ~f:(fun (enabled_group, disabled_groups, pct_total) raw_group ->
                let raw_group = String.trim raw_group in
                (* A rollout group has the for "X% label", where label can only contain
                 * [a-zA-Z0-9._] *)
                if Str.string_match (Str.regexp "^\\([0-9]+\\)% \\([a-zA-Z0-9._]+\\)$") raw_group 0
                then
                  let group_pct = Str.matched_group 1 raw_group |> int_of_string in
                  let group_name = Str.matched_group 2 raw_group in
                  if enabled_group = Some group_name || SSet.mem group_name disabled_groups then
                    Error
                      ( line_num,
                        spf
                          "Groups must have unique names. There is more than one %S group"
                          group_name )
                  else
                    let (enabled_group, disabled_groups) =
                      match enabled_group with
                      | None when my_pct < group_pct + pct_total ->
                        (* This is the first group that passes my_pct, so we enable it *)
                        (Some group_name, disabled_groups)
                      | _ ->
                        (* Either we've already chosen the enabled group or we haven't passed my_pct *)
                        (enabled_group, SSet.add group_name disabled_groups)
                    in
                    Ok (enabled_group, disabled_groups, pct_total + group_pct)
                else
                  Error
                    ( line_num,
                      "Malformed rollout group. A group should be a percentage and an identifier, "
                      ^ "like `50% on`" ))
            >>= fun (enabled_group, disabled_groups, pct_total) ->
            if pct_total = 100 then
              if SMap.mem rollout_name rollouts then
                Error
                  ( line_num,
                    spf
                      "Rollouts must have unique names. There already is a %S rollout"
                      rollout_name )
              else
                match enabled_group with
                | None -> Error (line_num, "Invariant violation: failed to choose a group")
                | Some enabled_group ->
                  Ok (SMap.add rollout_name { enabled_group; disabled_groups } rollouts)
            else
              Error
                ( line_num,
                  spf "Rollout groups must sum to 100%%. %S sums to %d%%" rollout_name pct_total )
          else
            Error
              ( line_num,
                "Malformed rollout. A rollout should be an identifier followed by a list of groups, "
                ^ "like `myRollout=10% on, 50% off`" ))
      >>= fun rollouts -> Ok { config with rollouts })

let parse_section config ((section_ln, section), lines) : (config * warning list, error) result =
  match (section, lines) with
  | ("", []) when section_ln = 0 -> Ok (config, [])
  | ("", (ln, _) :: _) when section_ln = 0 -> Error (ln, "Unexpected config line not in any section")
  | ("include", _) -> parse_includes lines config
  | ("ignore", _) -> parse_ignores lines config
  | ("libs", _) -> parse_libs lines config
  | ("lints", _) -> parse_lints lines config
  | ("declarations", _) -> parse_declarations lines config
  | ("strict", _) -> parse_strict lines config
  | ("options", _) -> parse_options lines config
  | ("untyped", _) -> parse_untyped lines config
  | ("version", _) -> parse_version lines config
  | _ -> Ok (config, [(section_ln, spf "Unsupported config section: \"%s\"" section)])

let parse =
  (* Filter every section (except the rollouts section) for disabled rollouts. For example, if a
   * line starts with (my_rollout=on) and the "on" group is not enabled for the "my_rollout"
   * rollout, then drop the line completely.
   *
   * Lines with enabled rollouts just have the prefix stripped
   *)
  let filter_sections_by_rollout sections config =
    (* The rollout prefix looks like `(rollout_name=group_name)` *)
    let rollout_regex = Str.regexp "^(\\([a-zA-Z0-9._]+\\)=\\([a-zA-Z0-9._]+\\))\\(.*\\)$" in
    fold_left_stop_on_error sections ~acc:[] ~f:(fun acc (section_name, lines) ->
        fold_left_stop_on_error lines ~acc:[] ~f:(fun acc (line_num, line) ->
            if Str.string_match rollout_regex line 0 then
              let rollout_name = Str.matched_group 1 line in
              let group_name = Str.matched_group 2 line in
              let line = Str.matched_group 3 line in
              match SMap.find_opt rollout_name config.rollouts with
              | None -> Error (line_num, spf "Unknown rollout %S" rollout_name)
              | Some { enabled_group; disabled_groups } ->
                if enabled_group = group_name then
                  Ok ((line_num, line) :: acc)
                else if SSet.mem group_name disabled_groups then
                  Ok acc
                else
                  Error (line_num, spf "Unknown group %S in rollout %S" group_name rollout_name)
            else
              Ok ((line_num, line) :: acc))
        >>= fun lines -> Ok ((section_name, Base.List.rev lines) :: acc))
    >>= fun sections -> Ok (config, Base.List.rev sections)
  in
  let process_rollouts config sections =
    let rollout_section_lines = ref None in
    let sections =
      Base.List.filter sections ~f:(function
          | ((_, "rollouts"), lines) ->
            rollout_section_lines := Some lines;
            false
          | _ -> true)
    in
    parse_rollouts config !rollout_section_lines >>= filter_sections_by_rollout sections
  in
  let rec loop acc sections =
    acc >>= fun (config, warn_acc) ->
    match sections with
    | [] -> Ok (config, Base.List.rev warn_acc)
    | section :: rest ->
      parse_section config section >>= fun (config, warnings) ->
      let acc = Ok (config, Base.List.rev_append warnings warn_acc) in
      loop acc rest
  in
  fun config lines ->
    group_into_sections lines >>= process_rollouts config >>= fun (config, sections) ->
    loop (Ok (config, [])) sections

let is_not_comment =
  let comment_regexps =
    [
      Str.regexp_string "#";
      (* Line starts with # *)
      Str.regexp_string ";";
      (* Line starts with ; *)
      Str.regexp_string "\240\159\146\169";
      (* Line starts with poop emoji *)
    ]
  in
  fun (_, line) ->
    not (Base.List.exists ~f:(fun regexp -> Str.string_match regexp line 0) comment_regexps)

let read filename =
  let contents = Sys_utils.cat_no_fail filename in
  let hash =
    let xx_state = Xx.init 0L in
    Xx.update xx_state contents;
    Xx.digest xx_state
  in
  let lines =
    contents
    |> Sys_utils.split_lines
    |> Base.List.mapi ~f:(fun i line -> (i + 1, String.trim line))
    |> Base.List.filter ~f:is_not_comment
  in
  (lines, hash)

let init ~ignores ~untyped ~declarations ~includes ~libs ~options ~lints =
  let ( >>= )
      (acc : (config * warning list, error) result)
      (fn : config -> (config * warning list, error) result) =
    let ( >>= ) = Base.Result.( >>= ) in
    acc >>= fun (config, warn_acc) ->
    fn config >>= fun (config, warnings) -> Ok (config, Base.List.rev_append warnings warn_acc)
  in
  let ignores_lines = Base.List.map ~f:(fun s -> (1, s)) ignores in
  let untyped_lines = Base.List.map ~f:(fun s -> (1, s)) untyped in
  let declarations_lines = Base.List.map ~f:(fun s -> (1, s)) declarations in
  let includes_lines = Base.List.map ~f:(fun s -> (1, s)) includes in
  let options_lines = Base.List.map ~f:(fun s -> (1, s)) options in
  let lib_lines = Base.List.map ~f:(fun s -> (1, s)) libs in
  let lint_lines = Base.List.map ~f:(fun s -> (1, s)) lints in
  Ok (empty_config, [])
  >>= parse_ignores ignores_lines
  >>= parse_untyped untyped_lines
  >>= parse_declarations declarations_lines
  >>= parse_includes includes_lines
  >>= parse_options options_lines
  >>= parse_libs lib_lines
  >>= parse_lints lint_lines

let write config oc = Pp.config oc config

(* We should restart every time the config changes, so it's generally cool to cache it *)
let cache = ref None

let get_from_cache ?(allow_cache = true) filename =
  match !cache with
  | Some ((cached_filename, _, _) as cached_data) when allow_cache ->
    assert (filename = cached_filename);
    cached_data
  | _ ->
    let (lines, hash) = read filename in
    let config = { empty_config with lint_severities = LintSettings.default_severities } in
    let config = parse config lines in
    let cached_data = (filename, config, hash) in
    cache := Some cached_data;
    cached_data

let get ?allow_cache filename =
  let (_, config, _) = get_from_cache ?allow_cache filename in
  config

let get_hash ?allow_cache filename =
  let (_, _, hash) = get_from_cache ?allow_cache filename in
  hash

let get_with_hash ?allow_cache filename =
  let (_, config, hash) = get_from_cache ?allow_cache filename in
  (config, hash)

(* Accessors *)

let enabled_rollouts config = SMap.map (fun { enabled_group; _ } -> enabled_group) config.rollouts

(* completely ignored files (both module resolving and typing) *)
let ignores config = config.ignores

(* files that should be treated as untyped *)
let untyped config = config.untyped

(* files that should be treated as declarations *)
let declarations config = config.declarations

(* non-root include paths *)
let includes config = config.includes

(* library paths. no wildcards *)
let libs config = config.libs

(* options *)
let abstract_locations c = c.options.Opts.abstract_locations

let all c = c.options.Opts.all

let autoimports c = c.options.Opts.autoimports

let automatic_require_default c = c.options.Opts.automatic_require_default

let babel_loose_array_spread c = c.options.Opts.babel_loose_array_spread

let disable_live_non_parse_errors c = c.options.Opts.disable_live_non_parse_errors

let emoji c = c.options.Opts.emoji

let format_bracket_spacing c = c.options.Opts.format_bracket_spacing

let format_single_quotes c = c.options.Opts.format_single_quotes

let max_literal_length c = c.options.Opts.max_literal_length

let enable_const_params c = c.options.Opts.enable_const_params

let enforce_local_inference_annotations c = c.options.Opts.enforce_local_inference_annotations

let local_inference_annotation_dirs c = c.options.Opts.local_inference_annotation_dirs

let enforce_strict_call_arity c = c.options.Opts.enforce_strict_call_arity

let enums c = c.options.Opts.enums

let env_mode c = c.options.Opts.env_mode

let exact_by_default c = c.options.Opts.exact_by_default

let file_watcher c = c.options.Opts.file_watcher

let file_watcher_mergebase_with c = c.options.Opts.file_watcher_mergebase_with

let file_watcher_timeout c = c.options.Opts.file_watcher_timeout

let watchman_sync_timeout c = c.options.Opts.watchman_sync_timeout

let watchman_defer_states c = c.options.Opts.watchman_defer_states

let watchman_survive_restarts c = c.options.Opts.watchman_survive_restarts

let facebook_fbs c = c.options.Opts.facebook_fbs

let facebook_fbt c = c.options.Opts.facebook_fbt

let facebook_module_interop c = c.options.Opts.facebook_module_interop

let haste_module_ref_prefix c = c.options.Opts.haste_module_ref_prefix

let haste_name_reducers c = c.options.Opts.haste_name_reducers

let haste_paths_excludes c = c.options.Opts.haste_paths_excludes

let haste_paths_includes c = c.options.Opts.haste_paths_includes

let haste_use_name_reducers c = c.options.Opts.haste_use_name_reducers

let ignore_non_literal_requires c = c.options.Opts.ignore_non_literal_requires

let include_warnings c = c.options.Opts.include_warnings

let indexed_access c = c.options.Opts.indexed_access

let lazy_mode c = c.options.Opts.lazy_mode

let log_file c = c.options.Opts.log_file

let log_saving c = c.options.Opts.log_saving

let max_files_checked_per_worker c = c.options.Opts.max_files_checked_per_worker

let max_header_tokens c = c.options.Opts.max_header_tokens

let max_rss_bytes_for_check_per_worker c = c.options.Opts.max_rss_bytes_for_check_per_worker

let max_seconds_for_check_per_worker c = c.options.Opts.max_seconds_for_check_per_worker

let max_workers c = c.options.Opts.max_workers

let merge_timeout c = c.options.Opts.merge_timeout

let module_file_exts c = c.options.Opts.module_file_exts

let module_name_mappers c = c.options.Opts.module_name_mappers

let module_resource_exts c = c.options.Opts.module_resource_exts

let module_system c = c.options.Opts.module_system

let modules_are_use_strict c = c.options.Opts.modules_are_use_strict

let munge_underscores c = c.options.Opts.munge_underscores

let new_merge c = c.options.Opts.new_merge

let no_flowlib c = c.options.Opts.no_flowlib

let node_main_fields c = c.options.Opts.node_main_fields

let node_resolver_allow_root_relative c = c.options.Opts.node_resolver_allow_root_relative

let node_resolver_dirnames c = c.options.Opts.node_resolver_dirnames

let node_resolver_root_relative_dirnames c = c.options.Opts.node_resolver_root_relative_dirnames

let prioritize_dependency_checks c = c.options.Opts.prioritize_dependency_checks

let react_runtime c = c.options.Opts.react_runtime

let react_server_component_exts c = c.options.Opts.react_server_component_exts

let recursion_limit c = c.options.Opts.recursion_limit

let refactor c = c.options.Opts.refactor

let root_name c = c.options.Opts.root_name

let saved_state_fetcher c = c.options.Opts.saved_state_fetcher

let saved_state_load_sighashes c = c.options.Opts.saved_state_load_sighashes

let shm_hash_table_pow c = c.options.Opts.shm_hash_table_pow

let shm_heap_size c = c.options.Opts.shm_heap_size

let shm_log_level c = c.options.Opts.shm_log_level

let strict_es6_import_export c = c.options.Opts.strict_es6_import_export

let strict_es6_import_export_excludes c = c.options.Opts.strict_es6_import_export_excludes

let suppress_types c = c.options.Opts.suppress_types

let temp_dir c = c.options.Opts.temp_dir

let traces c = c.options.Opts.traces

let trust_mode c = c.options.Opts.trust_mode

let type_asserts c = c.options.Opts.type_asserts

let required_version c = c.version

let reorder_checking c = c.options.Opts.reorder_checking

let run_post_inference_implicit_instantiation c =
  c.options.Opts.run_post_inference_implicit_instantiation

let wait_for_recheck c = c.options.Opts.wait_for_recheck

let weak c = c.options.Opts.weak

(* global defaults for lint severities and strict mode *)
let lint_severities c = c.lint_severities

let strict_mode c = c.strict_mode

let gc_worker_minor_heap_size c = c.options.Opts.gc_worker_minor_heap_size

let gc_worker_major_heap_increment c = c.options.Opts.gc_worker_major_heap_increment

let gc_worker_space_overhead c = c.options.Opts.gc_worker_space_overhead

let gc_worker_window_size c = c.options.Opts.gc_worker_window_size

let gc_worker_custom_major_ratio c = c.options.Opts.gc_worker_custom_major_ratio

let gc_worker_custom_minor_ratio c = c.options.Opts.gc_worker_custom_minor_ratio

let gc_worker_custom_minor_max_size c = c.options.Opts.gc_worker_custom_minor_max_size
