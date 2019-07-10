(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

let (>>=) = Core_result.(>>=)

type line = int * string
type section = line * line list
type warning = int * string
type error = int * string

let default_temp_dir = Filename.concat Sys_utils.temp_dir_name "flow"
let default_shm_dirs =
  try
    Sys.getenv "FLOW_SHMDIR"
    |> Str.(split (regexp ","))
  with _ -> [ "/dev/shm"; default_temp_dir; ]

(* Half a gig *)
let default_shm_min_avail = 1024 * 1024 * 512

let version_regex = Str.regexp_string "<VERSION>"

let less_or_equal_curr_version = Version_regex.less_than_or_equal_to_version (Flow_version.version)

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
    abstract_locations: bool;
    all: bool;
    emoji: bool;
    enable_const_params: bool;
    enforce_strict_call_arity: bool;
    enforce_well_formed_exports: bool;
    enforce_well_formed_exports_whitelist: string list;
    enums: bool;
    esproposal_class_instance_fields: Options.esproposal_feature_mode;
    esproposal_class_static_fields: Options.esproposal_feature_mode;
    esproposal_decorators: Options.esproposal_feature_mode;
    esproposal_export_star_as: Options.esproposal_feature_mode;
    esproposal_nullish_coalescing: Options.esproposal_feature_mode;
    esproposal_optional_chaining: Options.esproposal_feature_mode;
    esproposal_fsharp_pipeline_operator: Options.esproposal_feature_mode;
    facebook_fbs: string option;
    facebook_fbt: string option;
    file_watcher: Options.file_watcher option;
    haste_module_ref_prefix: string option;
    haste_name_reducers: (Str.regexp * string) list;
    haste_paths_blacklist: string list;
    haste_paths_whitelist: string list;
    haste_use_name_reducers: bool;
    ignore_non_literal_requires: bool;
    include_warnings: bool;
    lazy_mode: Options.lazy_mode option;
    log_file: Path.t option;
    max_files_checked_per_worker: int;
    max_header_tokens: int;
    max_literal_length: int;
    max_workers: int;
    merge_timeout: int option;
    module_file_exts: SSet.t;
    module_name_mappers: (Str.regexp * string) list;
    module_resolver: Path.t option;
    module_resource_exts: SSet.t;
    module_system: Options.module_system;
    modules_are_use_strict: bool;
    munge_underscores: bool;
    no_flowlib: bool;
    node_resolver_dirnames: string list;
    recursion_limit: int;
    root_name: string option;
    saved_state_fetcher: Options.saved_state_fetcher;
    shm_dep_table_pow: int;
    shm_dirs: string list;
    shm_hash_table_pow: int;
    shm_heap_size: int;
    shm_log_level: int;
    shm_min_avail: int;
    suppress_comments: Str.regexp list;
    suppress_types: SSet.t;
    temp_dir: string;
    traces: int;
    trust_mode: Options.trust_mode;
    type_asserts: bool;
    types_first: bool;
    wait_for_recheck: bool;
    weak: bool;
  }

  let warn_on_unknown_opts (raw_opts, config) : (t * warning list, error) result =
    (* If the user specified any options that aren't defined, issue a warning *)
    let warnings =
      SMap.elements raw_opts
      |> Core_list.fold_left ~f:(fun acc (k, v) ->
          let msg = spf "Unsupported option specified! (%s)" k in
          Core_list.fold_left ~f:(fun acc (line_num, _) -> (line_num, msg)::acc) ~init:acc v
        ) ~init:[]
    in
    Ok (config, warnings)

  let module_file_exts = SSet.empty
    |> SSet.add ".js"
    |> SSet.add ".jsx"
    |> SSet.add ".json"
    |> SSet.add ".mjs"

  let module_resource_exts = SSet.empty
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

  let default_options = {
    abstract_locations = false;
    all = false;
    emoji = false;
    enable_const_params = false;
    enforce_strict_call_arity = true;
    enforce_well_formed_exports = false;
    enforce_well_formed_exports_whitelist = [];
    enums = false;
    esproposal_class_instance_fields = Options.ESPROPOSAL_ENABLE;
    esproposal_class_static_fields = Options.ESPROPOSAL_ENABLE;
    esproposal_decorators = Options.ESPROPOSAL_WARN;
    esproposal_export_star_as = Options.ESPROPOSAL_WARN;
    esproposal_nullish_coalescing = Options.ESPROPOSAL_WARN;
    esproposal_optional_chaining = Options.ESPROPOSAL_WARN;
    esproposal_fsharp_pipeline_operator = Options.ESPROPOSAL_WARN;
    facebook_fbs = None;
    facebook_fbt = None;
    file_watcher = None;
    haste_module_ref_prefix = None;
    haste_name_reducers = [(Str.regexp "^\\(.*/\\)?\\([a-zA-Z0-9$_.-]+\\)\\.js\\(\\.flow\\)?$", "\\2")];
    haste_paths_blacklist = ["\\(.*\\)?/node_modules/.*"];
    haste_paths_whitelist = ["<PROJECT_ROOT>/.*"];
    haste_use_name_reducers = false;
    ignore_non_literal_requires = false;
    include_warnings = false;
    lazy_mode = None;
    log_file = None;
    max_header_tokens = 10;
    max_files_checked_per_worker = 100;
    max_literal_length = 100;
    max_workers = Sys_utils.nbr_procs;
    merge_timeout = Some 100;
    module_file_exts;
    module_name_mappers = [];
    module_resolver = None;
    module_resource_exts;
    module_system = Options.Node;
    modules_are_use_strict = false;
    munge_underscores = false;
    no_flowlib = false;
    node_resolver_dirnames = ["node_modules"];
    recursion_limit = 10000;
    root_name = None;
    saved_state_fetcher = Options.Dummy_fetcher;
    shm_dep_table_pow = 17;
    shm_dirs = default_shm_dirs;
    shm_hash_table_pow = 19;
    shm_heap_size = 1024 * 1024 * 1024 * 25; (* 25 gigs *)
    shm_log_level = 0;
    shm_min_avail = default_shm_min_avail;
    suppress_comments = [Str.regexp "\\(.\\|\n\\)*\\$FlowFixMe"];
    suppress_types = SSet.empty |> SSet.add "$FlowFixMe";
    temp_dir = default_temp_dir;
    traces = 0;
    trust_mode = Options.NoTrust;
    type_asserts = false;
    types_first = false;
    wait_for_recheck = false;
    weak = false;
  }

  let parse_lines : line list -> (raw_options, error) result =
    let rec loop acc lines = acc >>= (fun map ->
      match lines with
      | [] -> Ok map
      | (line_num, line)::rest ->
        if Str.string_match (Str.regexp "^\\([a-zA-Z0-9._]+\\)=\\(.*\\)$") line 0
        then
          let key = Str.matched_group 1 line in
          let value = Str.matched_group 2 line in
          let map = SMap.add key ((line_num, value)::(
            match SMap.get key map with
            | Some values -> values
            | None -> []
          )) map in
          loop (Ok map) rest
        else
          Error (line_num, "Unable to parse line.")
    ) in

    fun lines ->
      let lines = lines
        |> Core_list.map ~f:(fun (ln, line) -> ln, String.trim line)
        |> Core_list.filter ~f:(fun (_, s) -> s <> "")
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
      | (line_num, value_str)::rest ->
        let value =
          optparser value_str
          |> Core_result.map_error
              ~f:(fun msg -> line_num, Failed_to_parse_value msg)
        in
        let config =
          value >>= fun value ->
          setter config value
          |> Core_result.map_error
              ~f:(fun msg -> line_num, Failed_to_set msg)
        in
        config >>= loop optparser setter rest
    in
    fun
      (optparser: (string -> ('a, string) result))
      ?init
      ?(multiple=false)
      (setter: (t -> 'a -> (t, string) result))
      (values: raw_values)
      config
  : (t, opt_error) result ->
    let config =
      match init with
      | None -> config
      | Some f -> f config
    in
    (* Error when duplicate options were incorrectly given *)
    match multiple, values with
    | false, _::(dupe_ln, _)::_ ->
      Error (dupe_ln, Duplicate_option)
    | _ ->
      loop optparser setter values config

  let optparse_string str =
    try Ok (Scanf.unescaped str)
    with Scanf.Scan_failure reason -> Error (
      spf "Invalid ocaml string: %s" reason
    )

  let optparse_regexp str =
    optparse_string str >>= fun unescaped ->
    try Ok (Str.regexp unescaped)
    with Failure reason -> Error (
      spf "Invalid regex \"%s\" (%s)" unescaped reason
    )

  let enum values =
    opt (fun str ->
      let values = Core_list.fold_left ~f:map_add ~init:SMap.empty values in
      match SMap.get str values with
      | Some v -> Ok v
      | None -> Error (
          spf "Unsupported value: \"%s\". Supported values are: %s"
            str
            (String.concat ", " (SMap.keys values))
        )
    )

  let esproposal_feature_flag ?(allow_enable=false) =
    let values = [
      ("ignore", Options.ESPROPOSAL_IGNORE);
      ("warn", Options.ESPROPOSAL_WARN);
    ] in
    let values =
      if allow_enable
      then ("enable", Options.ESPROPOSAL_ENABLE)::values
      else values
    in
    enum values

  let filepath = opt (fun str -> Ok (Path.make str))

  let optparse_mapping str =
    let regexp_str = "^'\\([^']*\\)'[ \t]*->[ \t]*'\\([^']*\\)'$" in
    let regexp = Str.regexp regexp_str in
    if Str.string_match regexp str 0 then
      Ok (Str.matched_group 1 str, Str.matched_group 2 str)
    else
      Error (
        "Expected a mapping of form: " ^
        "'single-quoted-string' -> 'single-quoted-string'"
      )

  let boolean =
    enum [("true", true); ("false", false)]

  let string =
    opt optparse_string

  let uint =
    opt (fun str ->
      let v = int_of_string str in
      if v < 0 then Error "Number cannot be negative!" else Ok v
    )

  let mapping fn =
    opt (fun str -> optparse_mapping str >>= fn)

  let parsers = [
    "emoji",
      boolean (fun opts v -> Ok { opts with emoji = v });

    "esproposal.class_instance_fields",
      esproposal_feature_flag
        ~allow_enable:true
        (fun opts v -> Ok { opts with esproposal_class_instance_fields = v });

    "esproposal.class_static_fields",
      esproposal_feature_flag
        ~allow_enable:true
        (fun opts v -> Ok { opts with esproposal_class_static_fields = v });

    "esproposal.decorators",
      esproposal_feature_flag
        (fun opts v -> Ok { opts with esproposal_decorators = v });

    "esproposal.export_star_as",
      esproposal_feature_flag
        ~allow_enable:true
        (fun opts v -> Ok { opts with esproposal_export_star_as = v });

    "esproposal.optional_chaining",
      esproposal_feature_flag
        ~allow_enable:true
        (fun opts v -> Ok { opts with esproposal_optional_chaining = v });

    "esproposal.nullish_coalescing",
      esproposal_feature_flag
        ~allow_enable:true
        (fun opts v -> Ok { opts with esproposal_nullish_coalescing = v });

    "esproposal.fsharp_pipeline_operator",
      esproposal_feature_flag
        ~allow_enable:true
        (fun opts v -> Ok { opts with esproposal_fsharp_pipeline_operator = v });

    "facebook.fbs",
      string (fun opts v -> Ok { opts with facebook_fbs = Some v });

    "facebook.fbt",
      string (fun opts v -> Ok { opts with facebook_fbt = Some v });

    "file_watcher",
      enum
        [
          "none", Options.NoFileWatcher;
          "dfind", Options.DFind;
          "watchman", Options.Watchman;
        ]
        (fun opts v -> Ok { opts with file_watcher = Some v });

    "include_warnings",
      boolean (fun opts v -> Ok { opts with include_warnings = v });

    "lazy_mode",
      enum
        [
          "fs", Options.LAZY_MODE_FILESYSTEM;
          "ide", Options.LAZY_MODE_IDE;
          "watchman", Options.LAZY_MODE_WATCHMAN;
          "none", Options.NON_LAZY_MODE;
        ]
        (fun opts v -> Ok { opts with lazy_mode = Some v });

    "merge_timeout",
      uint
        (fun opts v ->
          let merge_timeout = if v = 0 then None else Some v in
          Ok {opts with merge_timeout}
        );

    "module.system.haste.module_ref_prefix",
      string (fun opts v -> Ok { opts with haste_module_ref_prefix = Some v });

    "module.system.haste.name_reducers",
      mapping
        ~init:(fun opts -> { opts with haste_name_reducers = [] })
        ~multiple: true
        (fun (pattern, template) -> Ok (Str.regexp pattern, template))
        (fun opts v -> Ok {
          opts with haste_name_reducers = v::(opts.haste_name_reducers);
        });

    "module.system.haste.paths.blacklist",
      string
        ~init: (fun opts -> { opts with haste_paths_blacklist = [] })
        ~multiple: true
        (fun opts v -> Ok {
          opts with haste_paths_blacklist = v::(opts.haste_paths_blacklist);
        });

    "module.system.haste.paths.whitelist",
      string
        ~init: (fun opts -> { opts with haste_paths_whitelist = [] })
        ~multiple: true
        (fun opts v -> Ok {
          opts with haste_paths_whitelist = v::(opts.haste_paths_whitelist);
        });

    "module.system.haste.use_name_reducers",
      boolean
        ~init: (fun opts -> { opts with haste_use_name_reducers = false })
        (fun opts v -> Ok { opts with haste_use_name_reducers = v });

    "log.file",
      filepath (fun opts v -> Ok { opts with log_file = Some v });

    "max_header_tokens",
      uint (fun opts v -> Ok { opts with max_header_tokens = v });

    "module.ignore_non_literal_requires",
      boolean (fun opts v -> Ok { opts with ignore_non_literal_requires = v });

    "module.file_ext",
      string
        ~init: (fun opts -> { opts with module_file_exts = SSet.empty })
        ~multiple: true
        (fun opts v ->
          if String_utils.string_ends_with v Files.flow_ext
          then Error (
            "Cannot use file extension '" ^
            v ^
            "' since it ends with the reserved extension '"^
            Files.flow_ext^
            "'"
          ) else
            let module_file_exts = SSet.add v opts.module_file_exts in
            Ok {opts with module_file_exts;}
        );

    "module.name_mapper",
      mapping
        ~multiple: true
        (fun (pattern, template) -> Ok (Str.regexp pattern, template))
        (fun opts v ->
          let module_name_mappers = v :: opts.module_name_mappers in
          Ok {opts with module_name_mappers;}
        );

    "module.name_mapper.extension",
      mapping
        ~multiple: true
        (fun (file_ext, template) -> Ok (
          Str.regexp ("^\\(.*\\)\\." ^ (Str.quote file_ext) ^ "$"),
          template
        ))
        (fun opts v ->
          let module_name_mappers = v :: opts.module_name_mappers in
          Ok {opts with module_name_mappers;}
        );

    "module.resolver",
      filepath (fun opts v -> Ok { opts with module_resolver = Some v });

    "module.system",
      enum
        [
          ("node", Options.Node);
          ("haste", Options.Haste);
        ]
        (fun opts v -> Ok { opts with module_system = v });

    "module.system.node.resolve_dirname",
      string
        ~init: (fun opts -> { opts with node_resolver_dirnames = [] })
        ~multiple: true
        (fun opts v ->
          let node_resolver_dirnames = v :: opts.node_resolver_dirnames in
          Ok {opts with node_resolver_dirnames;}
        );

    "module.use_strict",
      boolean (fun opts v -> Ok { opts with modules_are_use_strict = v });

    "munge_underscores",
      boolean (fun opts v -> Ok { opts with munge_underscores = v });

    "name",
      string
        (fun opts v ->
          FlowEventLogger.set_root_name (Some v);
          Ok {opts with root_name = Some v;}
        );

    "server.max_workers",
      uint (fun opts v -> Ok { opts with max_workers = v });

    "all",
      boolean (fun opts v -> Ok { opts with all = v });

    "wait_for_recheck",
      boolean (fun opts v -> Ok { opts with wait_for_recheck = v });

    "weak",
      boolean (fun opts v -> Ok { opts with weak = v });

    "suppress_comment",
      string
        ~init: (fun opts -> { opts with suppress_comments = [] })
        ~multiple: true
        (fun opts v ->
          Str.split_delim version_regex v
          |> String.concat (">=" ^ less_or_equal_curr_version)
          |> String.escaped
          |> Core_result.return
          >>= optparse_regexp
          >>= fun v -> Ok { opts with suppress_comments = v::(opts.suppress_comments) }
        );

    "suppress_type",
      string
        ~init: (fun opts -> { opts with suppress_types = SSet.empty })
        ~multiple: true
        (fun opts v -> Ok { opts with suppress_types = SSet.add v opts.suppress_types });

    "temp_dir",
      string (fun opts v -> Ok { opts with temp_dir = v });

    "saved_state.fetcher",
      enum
        [
          ("none", Options.Dummy_fetcher);
          ("local", Options.Local_fetcher);
          ("fb", Options.Fb_fetcher);
        ]
        (fun opts saved_state_fetcher -> Ok { opts with saved_state_fetcher });

    "sharedmemory.dirs",
      string
        ~multiple: true
        (fun opts v -> Ok { opts with shm_dirs = opts.shm_dirs @ [v] });

    "sharedmemory.minimum_available",
      uint (fun opts shm_min_avail -> Ok { opts with shm_min_avail });

    "sharedmemory.dep_table_pow",
      uint (fun opts shm_dep_table_pow -> Ok { opts with shm_dep_table_pow });

    "sharedmemory.hash_table_pow",
      uint (fun opts shm_hash_table_pow -> Ok { opts with shm_hash_table_pow });

    "sharedmemory.heap_size",
      uint (fun opts shm_heap_size -> Ok { opts with shm_heap_size });

    "sharedmemory.log_level",
      uint (fun opts shm_log_level -> Ok { opts with shm_log_level });

    "traces",
      uint (fun opts v -> Ok { opts with traces = v });

    "max_literal_length",
      uint (fun opts v -> Ok { opts with max_literal_length = v });

    "experimental.const_params",
      boolean (fun opts v -> Ok { opts with enable_const_params = v });

    "experimental.enums",
      boolean (fun opts v -> Ok { opts with enums = v });

    "experimental.strict_call_arity",
      boolean (fun opts v -> Ok { opts with enforce_strict_call_arity = v });

    "experimental.well_formed_exports",
      boolean (fun opts v -> Ok { opts with enforce_well_formed_exports = v });

    "experimental.well_formed_exports.whitelist",
      string
        ~init: (fun opts -> { opts with enforce_well_formed_exports_whitelist = [] })
        ~multiple: true
        (fun opts v ->
          if opts.enforce_well_formed_exports then
            Ok { opts with enforce_well_formed_exports_whitelist =
              v::(opts.enforce_well_formed_exports_whitelist) }
          else
            Error "This option requires \"experimental.enforce_well_formed_exports\" \
            set to \"true\"."
        );

    "experimental.type_asserts",
      boolean (fun opts v -> Ok { opts with type_asserts = v });

    "experimental.types_first",
      boolean (fun opts v -> Ok { opts with types_first = v });

    "experimental.abstract_locations",
      boolean (fun opts v -> Ok { opts with abstract_locations = v });

    "no_flowlib",
      boolean (fun opts v -> Ok { opts with no_flowlib = v });

    "trust_mode",
      enum
        [
          ("check", Options.CheckTrust);
          ("silent", Options.SilentTrust);
          ("none", Options.NoTrust);
        ]
        (fun opts trust_mode -> Ok { opts with trust_mode });

    "recursion_limit",
      uint (fun opts v -> Ok { opts with recursion_limit = v });

    "experimental.types_first.max_files_checked_per_worker",
      uint (fun opts v -> Ok { opts with max_files_checked_per_worker = v });
  ]

  let parse =
    let error_of_opt_error key (line_num, opt_error) =
      let msg = match opt_error with
      | Failed_to_parse_value msg -> spf "Error parsing value for \"%s\". %s" key msg
      | Failed_to_set msg -> spf "Error setting value for \"%s\". %s" key msg
      | Duplicate_option -> spf "Duplicate option: \"%s\"" key
      in
      line_num, msg
    in
    let rec loop
        (acc: ((raw_options * t), error) result)
        (parsers: (string * (raw_values -> t -> (t, opt_error) result)) list)
    =
      acc >>= fun (raw_opts, config) ->
      match parsers with
      | [] -> Ok (raw_opts, config)
      | (key, f)::rest ->
        let acc =
          match SMap.get key raw_opts with
          | None -> Ok (raw_opts, config)
          | Some values ->
            f values config
            |> Core_result.map_error ~f:(error_of_opt_error key)
            >>= fun config ->
            let new_raw_opts = SMap.remove key raw_opts in
            Ok (new_raw_opts, config)
        in
        loop acc rest
    in
    fun (init: t) (lines: line list) : (t * warning list, error) result ->
      parse_lines lines >>= fun raw_options ->
      loop (Ok (raw_options, init)) parsers >>=
      warn_on_unknown_opts
end

type config = {
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

module Pp : sig
  val config : out_channel -> config -> unit
end = struct
  open Printf

  let section_header o section =
    fprintf o "[%s]\n" section

  let ignores o =
    Core_list.iter ~f:(fprintf o "%s\n")

  let untyped o =
    Core_list.iter ~f:(fprintf o "%s\n")

  let declarations o =
    Core_list.iter ~f:(fprintf o "%s\n")

  let includes o =
    Core_list.iter ~f:(fprintf o "%s\n")

  let libs o =
    Core_list.iter ~f:(fprintf o "%s\n")

  let options =
    let pp_opt o name value = fprintf o "%s=%s\n" name value

    in let module_system = function
      | Options.Node -> "node"
      | Options.Haste -> "haste"

    in fun o config ->
      let open Opts in
      let options = config.options in
      if options.module_system <> default_options.module_system
      then pp_opt o "module.system" (module_system options.module_system);
      if options.all <> default_options.all
      then pp_opt o "all" (string_of_bool options.all);
      if options.weak <> default_options.weak
      then pp_opt o "weak" (string_of_bool options.weak);
      if options.temp_dir <> default_options.temp_dir
      then pp_opt o "temp_dir" options.temp_dir;
      if options.include_warnings <> default_options.include_warnings
      then pp_opt o "include_warnings" (string_of_bool options.include_warnings)

  let lints o config =
    let open Lints in
    let open Severity in
    let lint_severities = config.lint_severities in
    let lint_default = LintSettings.get_default lint_severities in
    (* Don't print an 'all' setting if it matches the default setting. *)
    if (lint_default <> LintSettings.get_default LintSettings.empty_severities) then
      fprintf o "all=%s\n" (string_of_severity lint_default);
    LintSettings.iter (fun kind (state, _) ->
        (fprintf o "%s=%s\n"
          (string_of_kind kind)
          (string_of_severity state)))
      lint_severities

  let strict o config =
    let open Lints in
    let strict_mode = config.strict_mode in
    StrictModeSettings.iter (fun kind ->
      (fprintf o "%s\n"
         (string_of_kind kind)))
      strict_mode

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

let empty_config = {
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
  let rec loop acc lines = acc >>= fun (seen, sections, (section_name, section_lines)) ->
    match lines with
    | [] ->
      let section = section_name, Core_list.rev section_lines in
      Ok (Core_list.rev (section::sections))
    | (ln, line)::rest ->
      if Str.string_match is_section_header line 0
      then begin
        let sections = (section_name, Core_list.rev section_lines)::sections in
        let section_name = Str.matched_group 1 line in
        if SSet.mem section_name seen then
          Error (ln, spf "contains duplicate section: \"%s\"" section_name)
        else
          let seen = SSet.add section_name seen in
          let section = (ln, section_name), [] in
          let acc = Ok (seen, sections, section) in
          loop acc rest
      end else
        let acc = Ok (seen, sections, (section_name, (ln, line)::section_lines)) in
        loop acc rest
  in
  fun lines ->
    loop (Ok (SSet.empty, [], ((0, ""), []))) lines

let trim_lines lines =
  lines
  |> Core_list.map ~f:(fun (_, line) -> String.trim line)
  |> Core_list.filter ~f:(fun s -> s <> "")

let trim_labeled_lines lines =
  lines
  |> Core_list.map ~f:(fun (label, line) -> (label, String.trim line))
  |> Core_list.filter ~f:(fun (_, s) -> s <> "")

(* parse [include] lines *)
let parse_includes lines config =
  let includes = trim_lines lines in
  Ok ({ config with includes; }, [])

let parse_libs lines config : (config * warning list, error) result =
  let libs = trim_lines lines in
  Ok ({ config with libs; }, [])

let parse_ignores lines config =
  let ignores = trim_lines lines in
  Ok ({ config with ignores; }, [])

let parse_untyped lines config =
  let untyped = trim_lines lines in
  Ok ({ config with untyped; }, [])

let parse_declarations lines config =
  let declarations = trim_lines lines in
  Ok ({ config with declarations; }, [])

let parse_options lines config : (config * warning list, error) result =
  Opts.parse config.options lines >>= fun (options, warnings) ->
  Ok ({ config with options}, warnings)

let parse_version lines config =
  let potential_versions = lines
  |> Core_list.map ~f:(fun (ln, line) -> ln, String.trim line)
  |> Core_list.filter ~f:(fun (_, s) -> s <> "")
  in

  match potential_versions with
  | (ln, version_str) :: _ ->
    if not (Semver.is_valid_range version_str) then
      Error (ln, (
        spf
          "Expected version to match %%d.%%d.%%d, with an optional leading ^, got %s"
          version_str
      ))
    else
      Ok ({ config with version = Some version_str }, [])
  | _ -> Ok (config, [])

let parse_lints lines config : (config * warning list, error) result =
  let lines = trim_labeled_lines lines in
  LintSettings.of_lines config.lint_severities lines >>= fun lint_severities ->
  Ok ({config with lint_severities}, [])

let parse_strict lines config =
  let lines = trim_labeled_lines lines in
  StrictModeSettings.of_lines lines >>= fun strict_mode ->
  Ok ({config with strict_mode}, [])

let parse_section config ((section_ln, section), lines) : (config * warning list, error) result =
  match section, lines with
  | "", [] when section_ln = 0 -> Ok (config, [])
  | "", (ln, _)::_ when section_ln = 0 ->
      Error (ln, "Unexpected config line not in any section")
  | "include", _ -> parse_includes lines config
  | "ignore", _ -> parse_ignores lines config
  | "libs", _ -> parse_libs lines config
  | "lints", _ -> parse_lints lines config
  | "declarations", _ -> parse_declarations lines config
  | "strict", _ -> parse_strict lines config
  | "options", _ -> parse_options lines config
  | "untyped", _ -> parse_untyped lines config
  | "version", _ -> parse_version lines config
  | _ -> Ok (config, [section_ln, (spf "Unsupported config section: \"%s\"" section)])

let parse =
  let rec loop acc sections =
    acc >>= fun (config, warn_acc) ->
    match sections with
    | [] -> Ok (config, Core_list.rev warn_acc)
    | section::rest ->
      parse_section config section >>= fun (config, warnings) ->
      let acc = Ok (config, Core_list.rev_append warnings warn_acc) in
      loop acc rest
  in
  fun config lines ->
    group_into_sections lines
    >>= loop (Ok (config, []))

let is_not_comment =
  let comment_regexps = [
    Str.regexp_string "#";                (* Line starts with # *)
    Str.regexp_string ";";                (* Line starts with ; *)
    Str.regexp_string "\240\159\146\169"; (* Line starts with poop emoji *)
  ] in
  fun (_, line) ->
    not (Core_list.exists
      ~f:(fun (regexp) -> Str.string_match regexp line 0)
      comment_regexps)

let read filename =
  let contents = Sys_utils.cat_no_fail filename in
  let hash =
    let xx_state = Xx.init () in
    Xx.update xx_state contents;
    Xx.digest xx_state
  in
  let lines = contents
    |> Sys_utils.split_lines
    |> Core_list.mapi ~f:(fun i line -> (i+1, String.trim line))
    |> Core_list.filter ~f:is_not_comment
  in
  lines, hash

let get_empty_config () =
  let lint_severities = Core_list.fold_left ~f:(fun acc (lint, severity) ->
    LintSettings.set_value lint severity acc
  ) ~init:empty_config.lint_severities LintSettings.default_lint_severities in
  { empty_config with lint_severities }

let init ~ignores ~untyped ~declarations ~includes ~libs ~options ~lints =
  let (>>=)
    (acc: (config * warning list, error) result)
    (fn: config -> (config * warning list, error) result)
  =
    let (>>=) = Core_result.(>>=) in
    acc >>= fun (config, warn_acc) ->
    fn config >>= fun (config, warnings) ->
    Ok (config, Core_list.rev_append warnings warn_acc)
  in
  let ignores_lines = Core_list.map ~f:(fun s -> (1, s)) ignores in
  let untyped_lines = Core_list.map ~f:(fun s -> (1, s)) untyped in
  let declarations_lines = Core_list.map ~f:(fun s -> (1, s)) declarations in
  let includes_lines = Core_list.map ~f:(fun s -> (1, s)) includes in
  let options_lines = Core_list.map ~f:(fun s -> (1, s)) options in
  let lib_lines = Core_list.map ~f:(fun s -> (1, s)) libs in
  let lint_lines = Core_list.map ~f:(fun s -> (1, s)) lints in
  Ok (empty_config, [])
  >>= (parse_ignores ignores_lines)
  >>= (parse_untyped untyped_lines)
  >>= (parse_declarations declarations_lines)
  >>= (parse_includes includes_lines)
  >>= (parse_options options_lines)
  >>= (parse_libs lib_lines)
  >>= (parse_lints lint_lines)

let write config oc = Pp.config oc config

(* We should restart every time the config changes, so it's generally cool to cache it *)
let cache = ref None

let get_from_cache ?(allow_cache=true) filename =
  match !cache with
  | Some (cached_filename, _, _ as cached_data) when allow_cache ->
      assert (filename = cached_filename);
      cached_data
  | _ ->
      let lines, hash = read filename in
      let config = parse (get_empty_config ()) lines in
      let cached_data = filename, config, hash in
      cache := Some cached_data;
      cached_data

let get ?allow_cache filename =
  let (_, config, _) = get_from_cache ?allow_cache filename in
  config

let get_hash ?allow_cache filename =
  let (_, _, hash) = get_from_cache ?allow_cache filename in
  hash

(* Accessors *)

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
let emoji c = c.options.Opts.emoji
let max_literal_length c = c.options.Opts.max_literal_length
let enable_const_params c = c.options.Opts.enable_const_params
let enforce_strict_call_arity c = c.options.Opts.enforce_strict_call_arity
let enforce_well_formed_exports c = c.options.Opts.enforce_well_formed_exports
let enforce_well_formed_exports_whitelist c = c.options.Opts.enforce_well_formed_exports_whitelist
let enums c = c.options.Opts.enums
let esproposal_class_instance_fields c = c.options.Opts.esproposal_class_instance_fields
let esproposal_class_static_fields c = c.options.Opts.esproposal_class_static_fields
let esproposal_decorators c = c.options.Opts.esproposal_decorators
let esproposal_export_star_as c = c.options.Opts.esproposal_export_star_as
let esproposal_optional_chaining c = c.options.Opts.esproposal_optional_chaining
let esproposal_nullish_coalescing c = c.options.Opts.esproposal_nullish_coalescing
let esproposal_fsharp_pipeline_operator c = c.options.Opts.esproposal_fsharp_pipeline_operator
let file_watcher c = c.options.Opts.file_watcher
let facebook_fbs c = c.options.Opts.facebook_fbs
let facebook_fbt c = c.options.Opts.facebook_fbt
let haste_module_ref_prefix c = c.options.Opts.haste_module_ref_prefix
let haste_name_reducers c = c.options.Opts.haste_name_reducers
let haste_paths_blacklist c = c.options.Opts.haste_paths_blacklist
let haste_paths_whitelist c = c.options.Opts.haste_paths_whitelist
let haste_use_name_reducers c = c.options.Opts.haste_use_name_reducers
let ignore_non_literal_requires c = c.options.Opts.ignore_non_literal_requires
let include_warnings c = c.options.Opts.include_warnings
let lazy_mode c = c.options.Opts.lazy_mode
let log_file c = c.options.Opts.log_file
let max_files_checked_per_worker c = c.options.Opts.max_files_checked_per_worker
let max_header_tokens c = c.options.Opts.max_header_tokens
let max_workers c = c.options.Opts.max_workers
let merge_timeout c = c.options.Opts.merge_timeout
let module_file_exts c = c.options.Opts.module_file_exts
let module_name_mappers c = c.options.Opts.module_name_mappers
let module_resolver c = c.options.Opts.module_resolver
let module_resource_exts c = c.options.Opts.module_resource_exts
let module_system c = c.options.Opts.module_system
let modules_are_use_strict c = c.options.Opts.modules_are_use_strict
let munge_underscores c = c.options.Opts.munge_underscores
let no_flowlib c = c.options.Opts.no_flowlib
let node_resolver_dirnames c = c.options.Opts.node_resolver_dirnames
let recursion_limit c = c.options.Opts.recursion_limit
let root_name c = c.options.Opts.root_name
let saved_state_fetcher c = c.options.Opts.saved_state_fetcher
let shm_dep_table_pow c = c.options.Opts.shm_dep_table_pow
let shm_dirs c = c.options.Opts.shm_dirs
let shm_hash_table_pow c = c.options.Opts.shm_hash_table_pow
let shm_heap_size c = c.options.Opts.shm_heap_size
let shm_log_level c = c.options.Opts.shm_log_level
let shm_min_avail c = c.options.Opts.shm_min_avail
let suppress_comments c = c.options.Opts.suppress_comments
let suppress_types c = c.options.Opts.suppress_types
let temp_dir c = c.options.Opts.temp_dir
let traces c = c.options.Opts.traces
let trust_mode c = c.options.Opts.trust_mode
let type_asserts c = c.options.Opts.type_asserts
let types_first c = c.options.Opts.types_first
let required_version c = c.version
let wait_for_recheck c = c.options.Opts.wait_for_recheck
let weak c = c.options.Opts.weak

(* global defaults for lint severities and strict mode *)
let lint_severities c = c.lint_severities
let strict_mode c = c.strict_mode
