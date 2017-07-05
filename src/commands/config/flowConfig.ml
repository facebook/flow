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

let default_temp_dir = Filename.concat Sys_utils.temp_dir_name "flow"
let default_shm_dirs =
  try
    Sys.getenv "FLOW_SHMDIR"
    |> Str.(split (regexp ","))
  with _ -> [ "/dev/shm"; default_temp_dir; ]

(* Half a gig *)
let default_shm_min_avail = 1024 * 1024 * 512

let map_add map (key, value) = SMap.add key value map

let multi_error (errs:(int * string) list) =
  let msg =
    errs
    |> List.map (fun (ln, msg) -> spf ".flowconfig:%d %s" ln msg)
    |> String.concat "\n"
  in
  FlowExitStatus.(exit ~msg Invalid_flowconfig)

let error ln msg = multi_error [(ln, msg)]

module Opts = struct
  type t = {
    emoji: bool;
    enable_const_params: bool;
    enable_unsafe_getters_and_setters: bool;
    enforce_strict_type_args: bool;
    enforce_strict_call_arity: bool;
    esproposal_class_instance_fields: Options.esproposal_feature_mode;
    esproposal_class_static_fields: Options.esproposal_feature_mode;
    esproposal_decorators: Options.esproposal_feature_mode;
    esproposal_export_star_as: Options.esproposal_feature_mode;
    facebook_fbt: string option;
    haste_name_reducers: (Str.regexp * string) list;
    haste_paths_blacklist: string list;
    haste_paths_whitelist: string list;
    haste_use_name_reducers: bool;
    ignore_non_literal_requires: bool;
    module_system: Options.module_system;
    module_name_mappers: (Str.regexp * string) list;
    node_resolver_dirnames: string list;
    munge_underscores: bool;
    module_file_exts: SSet.t;
    module_resource_exts: SSet.t;
    modules_are_use_strict: bool;
    suppress_comments: Str.regexp list;
    suppress_types: SSet.t;
    traces: int;
    all: bool;
    weak: bool;
    log_file: Path.t option;
    max_header_tokens: int;
    max_workers: int;
    no_flowlib: bool;
    temp_dir: string;
    shm_global_size: int;
    shm_heap_size: int;
    shm_dirs: string list;
    shm_min_avail: int;
    shm_dep_table_pow: int;
    shm_hash_table_pow: int;
    shm_log_level: int;
    version: string option;
  }

  type initializer_ =
    | USE_DEFAULT
    | INIT_FN of (t -> t)

  type option_flag =
    | ALLOW_DUPLICATE

  type 'a option_definition = {
    (**
     * The initializer_ gets set on the options object immediately before
     * parsing the *first* occurrence of the user-specified config option. This
     * is useful in cases where the user's value should blow away the default
     * value (rather than being aggregated to it).
     *
     * For example: We want the default value of 'module.file_ext' to be
     * ['.js'; '.jsx'], but if the user specifies any 'module.file_ext'
     * settings, we want to start from a clean list.
     *)
    initializer_: initializer_;
    flags: option_flag list;
    setter: (t -> 'a -> (t, string) result);
    optparser: (string -> ('a, string) result);
  }

  let get_defined_opts (raw_opts, config) =
    (* If the user specified any options that aren't defined, issue an error *)
    if SMap.cardinal raw_opts > 0 then (
      let errors =
        SMap.elements raw_opts
        |> List.map (fun (k, v) ->
          let msg = spf "Unsupported option specified! (%s)" k in
          List.map (fun (line_num, _) -> (line_num, msg)) v
        )
        |> List.flatten
        |> List.rev
      in
      multi_error errors
    );

    config

  let module_file_exts = SSet.empty
    |> SSet.add ".js"
    |> SSet.add ".jsx"
    |> SSet.add ".json"

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
    emoji = false;
    enable_const_params = false;
    enable_unsafe_getters_and_setters = false;
    enforce_strict_type_args = true;
    enforce_strict_call_arity = true;
    esproposal_class_instance_fields = Options.ESPROPOSAL_ENABLE;
    esproposal_class_static_fields = Options.ESPROPOSAL_ENABLE;
    esproposal_decorators = Options.ESPROPOSAL_WARN;
    esproposal_export_star_as = Options.ESPROPOSAL_WARN;
    facebook_fbt = None;
    haste_name_reducers = [(Str.regexp "^\\(.*/\\)?\\([a-zA-Z0-9$_.-]+\\)\\.js\\(\\.flow\\)?$", "\\2")];
    haste_paths_blacklist = ["\\(.*\\)?/node_modules/.*"];
    haste_paths_whitelist = ["<PROJECT_ROOT>/.*"];
    haste_use_name_reducers = false;
    ignore_non_literal_requires = false;
    module_system = Options.Node;
    module_name_mappers = [];
    node_resolver_dirnames = ["node_modules"];
    munge_underscores = false;
    module_file_exts;
    module_resource_exts;
    modules_are_use_strict = false;
    suppress_comments = [Str.regexp "\\(.\\|\n\\)*\\$FlowFixMe"];
    suppress_types = SSet.empty |> SSet.add "$FlowFixMe";
    traces = 0;
    all = false;
    weak = false;
    log_file = None;
    max_header_tokens = 10;
    max_workers = Sys_utils.nbr_procs;
    no_flowlib = false;
    temp_dir = default_temp_dir;
    shm_global_size = 1024 * 1024 * 1024; (* 1 gig *)
    shm_heap_size = 1024 * 1024 * 1024 * 25; (* 25 gigs *)
    shm_dirs = default_shm_dirs;
    shm_min_avail = default_shm_min_avail;
    shm_dep_table_pow = 17;
    shm_hash_table_pow = 19;
    shm_log_level = 0;
    version = None;
  }

  let parse =
    let parse_line map (line_num, line) =
      if Str.string_match (Str.regexp "^\\([a-zA-Z0-9._]+\\)=\\(.*\\)$") line 0
      then
        let key = Str.matched_group 1 line in
        let value = Str.matched_group 2 line in
        SMap.add key ((line_num, value)::(
          match SMap.get key map with
          | Some values -> values
          | None -> []
        )) map
      else error line_num "Unable to parse line."
    in

    fun config lines ->
      let lines = lines
        |> List.map (fun (ln, line) -> ln, String.trim line)
        |> List.filter (fun (_, s) -> s <> "")
      in
      let raw_options = List.fold_left parse_line SMap.empty lines in
      (raw_options, config)

  let define_opt key definition (raw_opts, config) =
    let new_raw_opts = SMap.remove key raw_opts in

    match SMap.get key raw_opts with
    | None -> (new_raw_opts, config)
    | Some values ->
        let config = (
          match definition.initializer_ with
          | USE_DEFAULT -> config
          | INIT_FN f -> f config
        ) in

        (* Error when duplicate options were incorrectly given *)
        let allow_dupes = List.mem ALLOW_DUPLICATE definition.flags in
        if (not allow_dupes) && (List.length values) > 1 then (
          let line_num = fst (List.nth values 1) in
          error line_num (spf "Duplicate option: \"%s\"" key)
        );

        let config = List.fold_left (fun config (line_num, value_str) ->
          let value =
            match definition.optparser value_str with
            | Ok value -> value
            | Error msg -> error line_num (
              spf "Error parsing value for \"%s\". %s" key msg
            )
          in
          match definition.setter config value with
          | Ok config -> config
          | Error msg -> error line_num (
            spf "Error setting value for \"%s\". %s" key msg
          )
        ) config values in

        (new_raw_opts, config)

  let optparse_enum values str =
    let values = List.fold_left map_add SMap.empty values in
    match SMap.get str values with
    | Some v -> Ok v
    | None -> Error (
        spf "Unsupported value: \"%s\". Supported values are: %s"
          str
          (String.concat ", " (SMap.keys values))
      )

  let optparse_boolean = optparse_enum [
    ("true", true);
    ("false", false);
  ]

  let optparse_uint str =
    let v = int_of_string str in
    if v < 0 then Error "Number cannot be negative!" else Ok v

  let optparse_string str =
    try Ok (Scanf.unescaped str)
    with Scanf.Scan_failure reason -> Error (
      spf "Invalid ocaml string: %s" reason
    )

  let optparse_regexp str =
    match optparse_string str with
    | Ok unescaped ->
      begin try Ok (Str.regexp unescaped)
      with Failure reason -> Error (
        spf "Invalid regex \"%s\" (%s)" unescaped reason
      )
      end
    | Error _ as err -> err

  let optparse_esproposal_feature_flag ?(allow_enable=false) =
    let values = [
      ("ignore", Options.ESPROPOSAL_IGNORE);
      ("warn", Options.ESPROPOSAL_WARN);
    ] in
    let values =
      if allow_enable
      then ("enable", Options.ESPROPOSAL_ENABLE)::values
      else values
    in
    optparse_enum values

  let optparse_filepath str = Ok (Path.make str)

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

end

type config = {
  (* file blacklist *)
  ignores: string list;
  (* non-root include paths *)
  includes: string list;
  (* library paths. no wildcards *)
  libs: string list;
  (* config options *)
  options: Opts.t;
  (* lint settings *)
  lint_settings: LintSettings.t
}

module Pp : sig
  val config : out_channel -> config -> unit
end = struct
  open Printf

  let section_header o section =
    fprintf o "[%s]\n" section

  let ignores o ignores =
    List.iter (fun ex -> (fprintf o "%s\n" ex)) ignores

  let includes o includes =
    List.iter (fun inc -> (fprintf o "%s\n" inc)) includes

  let libs o libs =
    List.iter (fun lib -> (fprintf o "%s\n" lib)) libs

  let options =
    let opt o name value = fprintf o "%s=%s\n" name value

    in let module_system = function
      | Options.Node -> "node"
      | Options.Haste -> "haste"

    in fun o config -> Opts.(
      let options = config.options in
      if options.module_system <> default_options.module_system
      then opt o "module.system" (module_system options.module_system);
      if options.all <> default_options.all
      then opt o "all" (string_of_bool options.all);
      if options.weak <> default_options.weak
      then opt o "weak" (string_of_bool options.weak);
      if options.temp_dir <> default_options.temp_dir
      then opt o "temp_dir" options.temp_dir
    )

  let lints o config =
    let lint_settings = config.lint_settings in
    let lint_default = LintSettings.get_default lint_settings in
    (* Don't print an 'all' setting if it matches the default setting. *)
    if (lint_default <> LintSettings.get_default LintSettings.default_settings) then
      fprintf o "all=%s\n" (LintSettings.string_of_state lint_default);
    LintSettings.iter (fun kind (state, _) ->
        (fprintf o "%s=%s\n"
          (LintSettings.string_of_kind kind)
          (LintSettings.string_of_state state)))
      lint_settings

  let config o config =
    section_header o "ignore";
    ignores o config.ignores;
    fprintf o "\n";
    section_header o "include";
    includes o config.includes;
    fprintf o "\n";
    section_header o "libs";
    libs o config.libs;
    fprintf o "\n";
    section_header o "options";
    options o config;
    fprintf o "\n";
    section_header o "lints";
    lints o config
end

let empty_config = {
  ignores = [];
  includes = [];
  libs = [];
  options = Opts.default_options;
  lint_settings = LintSettings.default_settings
}

let group_into_sections lines =
  let is_section_header = Str.regexp "^\\[\\(.*\\)\\]$" in
  let _, sections, section =
    List.fold_left (fun (seen, sections, (section, lines)) (ln, line) ->
      if Str.string_match is_section_header line 0
      then begin
        let sections = (section, List.rev lines)::sections in
        let section_name = Str.matched_group 1 line in
        if SSet.mem section_name seen
        then error ln (spf "contains duplicate section: \"%s\"" section_name);
        SSet.add section_name seen, sections, ((ln, section_name), [])
      end else
        seen, sections, (section, (ln, line)::lines)
    ) (SSet.empty, [], ((0, ""), [])) lines in
  let (section, section_lines) = section in
  List.rev ((section, List.rev section_lines)::sections)

let trim_lines lines =
  lines
  |> List.map (fun (_, line) -> String.trim line)
  |> List.filter (fun s -> s <> "")

let trim_labeled_lines lines =
  lines
  |> List.map (fun (label, line) -> (label, String.trim line))
  |> List.filter (fun (_, s) -> s <> "")

(* parse [include] lines *)
let parse_includes config lines =
  let includes = trim_lines lines in
  { config with includes; }

let parse_libs config lines =
  let libs = trim_lines lines in
  { config with libs; }

let parse_ignores config lines =
  let ignores = trim_lines lines in
  { config with ignores; }

let parse_options config lines =
  let open Opts in
  let options = parse config.options lines
    |> define_opt "emoji" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        Ok {opts with emoji = v;}
      );
    }

    |> define_opt "esproposal.class_instance_fields" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_esproposal_feature_flag ~allow_enable:true;
      setter = (fun opts v -> Ok {
        opts with esproposal_class_instance_fields = v;
      });
    }

    |> define_opt "esproposal.class_static_fields" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_esproposal_feature_flag ~allow_enable:true;
      setter = (fun opts v -> Ok {
        opts with esproposal_class_static_fields = v;
      });
    }

    |> define_opt "esproposal.decorators" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_esproposal_feature_flag;
      setter = (fun opts v -> Ok {
        opts with esproposal_decorators = v;
      });
    }

    |> define_opt "esproposal.export_star_as" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_esproposal_feature_flag ~allow_enable:true;
      setter = (fun opts v -> Ok {
        opts with esproposal_export_star_as = v;
      });
    }

    |> define_opt "facebook.fbt" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_string;
      setter = (fun opts v -> Ok {
        opts with facebook_fbt = Some v;
      });
    }

    |> define_opt "module.system.haste.name_reducers" {
      initializer_ = INIT_FN (fun opts -> {
        opts with haste_name_reducers = [];
      });
      flags = [ALLOW_DUPLICATE];
      optparser = (fun str ->
        match optparse_mapping str with
        | Ok (pattern, template) -> Ok (Str.regexp pattern, template)
        | Error _ as err -> err
      );
      setter = (fun opts v -> Ok {
        opts with haste_name_reducers = v::(opts.haste_name_reducers);
      });
    }

    |> define_opt "module.system.haste.paths.blacklist" {
      initializer_ = INIT_FN (fun opts -> {
        opts with haste_paths_blacklist = [];
      });
      flags = [ALLOW_DUPLICATE];
      optparser = optparse_string;
      setter = (fun opts v -> Ok {
        opts with haste_paths_blacklist = v::(opts.haste_paths_blacklist);
      });
    }

    |> define_opt "module.system.haste.paths.whitelist" {
      initializer_ = INIT_FN (fun opts -> {
        opts with haste_paths_whitelist = [];
      });
      flags = [ALLOW_DUPLICATE];
      optparser = optparse_string;
      setter = (fun opts v -> Ok {
        opts with haste_paths_whitelist = v::(opts.haste_paths_whitelist);
      });
    }

    |> define_opt "module.system.haste.use_name_reducers" {
      initializer_ = INIT_FN (fun opts -> {
        opts with haste_use_name_reducers = false;
      });
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        Ok {opts with haste_use_name_reducers = v;}
      );
    }

    |> define_opt "log.file" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_filepath;
      setter = (fun opts v -> Ok {
        opts with log_file = Some v;
      });
    }

    |> define_opt "max_header_tokens" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_uint;
      setter = (fun opts v ->
        Ok {opts with max_header_tokens = v;}
      );
    }

    |> define_opt "module.ignore_non_literal_requires" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        Ok {opts with ignore_non_literal_requires = v;}
      );
    }

    |> define_opt "module.file_ext" {
      initializer_ = INIT_FN (fun opts -> {
        opts with module_file_exts = SSet.empty;
      });
      flags = [ALLOW_DUPLICATE];
      optparser = optparse_string;
      setter = (fun opts v ->
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
    }

    |> define_opt "module.name_mapper" {
      initializer_ = USE_DEFAULT;
      flags = [ALLOW_DUPLICATE];
      optparser = (fun str ->
        match optparse_mapping str with
        | Ok (pattern, template) -> Ok (Str.regexp pattern, template)
        | Error _ as err -> err
      );
      setter = (fun opts v ->
        let module_name_mappers = v :: opts.module_name_mappers in
        Ok {opts with module_name_mappers;}
      );
    }

    |> define_opt "module.name_mapper.extension" {
      initializer_ = USE_DEFAULT;
      flags = [ALLOW_DUPLICATE];
      optparser = (fun str ->
        match optparse_mapping str with
        | Ok (file_ext, template) -> Ok (
            Str.regexp ("^\\(.*\\)\\." ^ (Str.quote file_ext) ^ "$"),
            template
          )
        | Error _ as err -> err
      );
      setter = (fun opts v ->
        let module_name_mappers = v :: opts.module_name_mappers in
        Ok {opts with module_name_mappers;}
      );
    }

    |> define_opt "module.system" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_enum [
        ("node", Options.Node);
        ("haste", Options.Haste);
      ];
      setter = (fun opts v -> Ok {
        opts with module_system = v;
      });
    }

    |> define_opt "module.system.node.resolve_dirname" {
      initializer_ = INIT_FN (fun opts -> {
        opts with node_resolver_dirnames = [];
      });
      flags = [ALLOW_DUPLICATE];
      optparser = optparse_string;
      setter = (fun opts v ->
        let node_resolver_dirnames = v :: opts.node_resolver_dirnames in
        Ok {opts with node_resolver_dirnames;}
      );
    }

    |> define_opt "module.use_strict" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        Ok {opts with modules_are_use_strict = v;}
      );
    }

    |> define_opt "munge_underscores" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        Ok {opts with munge_underscores = v;}
      );
    }

    |> define_opt "server.max_workers" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_uint;
      setter = (fun opts v ->
        Ok {opts with max_workers = v;}
      );
    }

    |> define_opt "all" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        Ok {opts with all = v;}
      );
    }

    |> define_opt "weak" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        Ok {opts with weak = v;}
      );
    }

    |> define_opt "suppress_comment" {
      initializer_ = INIT_FN (fun opts ->
        {opts with suppress_comments = [];}
      );
      flags = [ALLOW_DUPLICATE];
      optparser = optparse_regexp;
      setter = (fun opts v -> Ok {
        opts with suppress_comments = v::(opts.suppress_comments);
      });
    }

    |> define_opt "suppress_type" {
      initializer_ = INIT_FN (fun opts ->
        {opts with suppress_types = SSet.empty;}
      );
      flags = [ALLOW_DUPLICATE];
      optparser = optparse_string;
      setter = (fun opts v -> Ok {
        opts with suppress_types = SSet.add v opts.suppress_types;
      });
    }

    |> define_opt "temp_dir" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_string;
      setter = (fun opts v -> Ok {
        opts with temp_dir = v;
      });
    }

    |> define_opt "sharedmemory.dirs" {
      initializer_ = USE_DEFAULT;
      flags = [ALLOW_DUPLICATE];
      optparser = optparse_string;
      setter = (fun opts v -> Ok {
        opts with shm_dirs = opts.shm_dirs @ [v];
      });
    }

    |> define_opt "sharedmemory.minimum_available" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_uint;
      setter = (fun opts shm_min_avail -> Ok {
        opts with shm_min_avail;
      });
    }

    |> define_opt "sharedmemory.dep_table_pow" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_uint;
      setter = (fun opts shm_dep_table_pow -> Ok {
        opts with shm_dep_table_pow;
      });
    }

    |> define_opt "sharedmemory.hash_table_pow" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_uint;
      setter = (fun opts shm_hash_table_pow -> Ok {
        opts with shm_hash_table_pow;
      });
    }

    |> define_opt "sharedmemory.log_level" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_uint;
      setter = (fun opts shm_log_level -> Ok {
        opts with shm_log_level;
      });
    }

    |> define_opt "traces" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_uint;
      setter = (fun opts v ->
        Ok {opts with traces = v;}
      );
    }

    |> define_opt "unsafe.enable_getters_and_setters" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        Ok {opts with enable_unsafe_getters_and_setters = v;}
      );
    }

    |> define_opt "experimental.const_params" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        Ok {opts with enable_const_params = v;}
      );
    }

    |> define_opt "experimental.strict_type_args" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        Ok {opts with enforce_strict_type_args = v;}
      );
    }

    |> define_opt "experimental.strict_call_arity" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        Ok {opts with enforce_strict_call_arity = v;}
      );
    }

    |> define_opt "no_flowlib" {
      initializer_ = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        Ok {opts with no_flowlib = v;}
      );
    }

    |> get_defined_opts
  in
  {config with options}

let parse_version config lines =
  let potential_versions = lines
  |> List.map (fun (ln, line) -> ln, String.trim line)
  |> List.filter (fun (_, s) -> s <> "")
  in

  match potential_versions with
  | (ln, version_str) :: _ ->
    if not (Semver.is_valid_range version_str) then
      error ln (
        spf
          "Expected version to match %%d.%%d.%%d, with an optional leading ^, got %s"
          version_str
      );

    let options = { config.options with Opts.version = Some version_str } in
    { config with options }
  | _ -> config

let parse_lints config lines =
  match lines |> trim_labeled_lines |> LintSettings.of_lines LintSettings.default_settings with
  | Ok lint_settings -> {config with lint_settings}
  | Error (ln, msg) -> error ln msg

let parse_section config ((section_ln, section), lines) =
  match section, lines with
  | "", [] when section_ln = 0 -> config
  | "", (ln, _)::_ when section_ln = 0 ->
      error ln "Unexpected config line not in any section"
  | "include", _ -> parse_includes config lines
  | "ignore", _ -> parse_ignores config lines
  | "libs", _ -> parse_libs config lines
  | "options", _ -> parse_options config lines
  | "version", _ -> parse_version config lines
  | "lints", _ -> parse_lints config lines
  | _ -> error section_ln (spf "Unsupported config section: \"%s\"" section)

let parse config lines =
  let sections = group_into_sections lines in
  List.fold_left parse_section config sections

let is_not_comment =
  let comment_regexps = [
    Str.regexp_string "#";                (* Line starts with # *)
    Str.regexp_string ";";                (* Line starts with ; *)
    Str.regexp_string "\240\159\146\169"; (* Line starts with poop emoji *)
  ] in
  fun (_, line) ->
    not (List.exists
      (fun (regexp) -> Str.string_match regexp line 0)
      comment_regexps)

let read filename =
  let lines = Sys_utils.cat_no_fail filename
    |> Sys_utils.split_lines
    |> List.mapi (fun i line -> (i+1, String.trim line))
    |> List.filter is_not_comment in
  parse empty_config lines

let init ~ignores ~includes ~libs ~options ~lints =
  let ignores_lines = List.map (fun s -> (1, s)) ignores in
  let includes_lines = List.map (fun s -> (1, s)) includes in
  let options_lines = List.map (fun s -> (1, s)) options in
  let lib_lines = List.map (fun s -> (1, s)) libs in
  let lint_lines = List.map (fun s -> (1, s)) lints in
  let config = parse_ignores empty_config ignores_lines in
  let config = parse_includes config includes_lines in
  let config = parse_options config options_lines in
  let config = parse_libs config lib_lines in
  let config = parse_lints config lint_lines in
  config

let write config oc = Pp.config oc config

(* We should restart every time the config changes, so it's cool to cache it *)
let cache = ref None

let get filename =
  match !cache with
  | None ->
      let config = read filename in
      cache := Some (filename, config);
      config
  | Some (cached_filename, config) ->
      assert (filename = cached_filename);
      config

let restore (filename, config) = cache := Some (filename, config)


(* Accessors *)

(* file blacklist *)
let ignores config = config.ignores
(* non-root include paths *)
let includes config = config.includes
(* library paths. no wildcards *)
let libs config = config.libs

(* options *)
let all c = c.options.Opts.all
let emoji c = c.options.Opts.emoji
let enable_const_params c = c.options.Opts.enable_const_params
let enable_unsafe_getters_and_setters c = c.options.Opts.enable_unsafe_getters_and_setters
let enforce_strict_type_args c = c.options.Opts.enforce_strict_type_args
let enforce_strict_call_arity c = c.options.Opts.enforce_strict_call_arity
let esproposal_class_instance_fields c = c.options.Opts.esproposal_class_instance_fields
let esproposal_class_static_fields c = c.options.Opts.esproposal_class_static_fields
let esproposal_decorators c = c.options.Opts.esproposal_decorators
let esproposal_export_star_as c = c.options.Opts.esproposal_export_star_as
let facebook_fbt c = c.options.Opts.facebook_fbt
let haste_name_reducers c = c.options.Opts.haste_name_reducers
let haste_paths_blacklist c = c.options.Opts.haste_paths_blacklist
let haste_paths_whitelist c = c.options.Opts.haste_paths_whitelist
let haste_use_name_reducers c = c.options.Opts.haste_use_name_reducers
let ignore_non_literal_requires c = c.options.Opts.ignore_non_literal_requires
let log_file c = c.options.Opts.log_file
let max_header_tokens c = c.options.Opts.max_header_tokens
let max_workers c = c.options.Opts.max_workers
let module_file_exts c = c.options.Opts.module_file_exts
let module_name_mappers c = c.options.Opts.module_name_mappers
let module_resource_exts c = c.options.Opts.module_resource_exts
let module_system c = c.options.Opts.module_system
let modules_are_use_strict c = c.options.Opts.modules_are_use_strict
let munge_underscores c = c.options.Opts.munge_underscores
let no_flowlib c = c.options.Opts.no_flowlib
let node_resolver_dirnames c = c.options.Opts.node_resolver_dirnames
let shm_dep_table_pow c = c.options.Opts.shm_dep_table_pow
let shm_dirs c = c.options.Opts.shm_dirs
let shm_global_size c = c.options.Opts.shm_global_size
let shm_hash_table_pow c = c.options.Opts.shm_hash_table_pow
let shm_heap_size c = c.options.Opts.shm_heap_size
let shm_log_level c = c.options.Opts.shm_log_level
let shm_min_avail c = c.options.Opts.shm_min_avail
let suppress_comments c = c.options.Opts.suppress_comments
let suppress_types c = c.options.Opts.suppress_types
let temp_dir c = c.options.Opts.temp_dir
let traces c = c.options.Opts.traces
let required_version c = c.options.Opts.version
let weak c = c.options.Opts.weak

(* global defaults for lint suppressions *)
let lint_settings c = c.lint_settings
