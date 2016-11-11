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

let version = "0.35.0"

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

let project_root_token = Str.regexp_string "<PROJECT_ROOT>";

module Opts = struct
  exception UserError of string

  type moduleSystem = Node | Haste

  type t = {
    enable_const_params: bool;
    enable_unsafe_getters_and_setters: bool;
    enforce_strict_type_args: bool;
    esproposal_class_instance_fields: Options.esproposal_feature_mode;
    esproposal_class_static_fields: Options.esproposal_feature_mode;
    esproposal_decorators: Options.esproposal_feature_mode;
    esproposal_export_star_as: Options.esproposal_feature_mode;
    facebook_fbt: string option;
    ignore_non_literal_requires: bool;
    moduleSystem: moduleSystem;
    module_name_mappers: (Str.regexp * string) list;
    node_resolver_dirnames: string list;
    munge_underscores: bool;
    module_file_exts: SSet.t;
    module_resource_exts: SSet.t;
    modules_are_use_strict: bool;
    suppress_comments: Str.regexp list;
    suppress_types: SSet.t;
    traces: int;
    strip_root: bool;
    all: bool;
    log_file: Path.t option;
    max_header_tokens: int;
    max_workers: int;
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

  type _initializer =
    | USE_DEFAULT
    | INIT_FN of (t -> t)

  type option_flag =
    | ALLOW_DUPLICATE

  type 'a option_definition = {
    (**
     * The _initializer gets set on the options object immediately before
     * parsing the *first* occurrence of the user-specified config option. This
     * is useful in cases where the user's value should blow away the default
     * value (rather than being aggregated to it).
     *
     * For example: We want the default value of 'module.file_ext' to be
     * ['.js'; '.jsx'], but if the user specifies any 'module.file_ext'
     * settings, we want to start from a clean list.
     *)
    _initializer: _initializer;
    flags: option_flag list;
    setter: (t -> 'a -> t);
    optparser: (string -> 'a);
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
    enable_const_params = false;
    enable_unsafe_getters_and_setters = false;
    enforce_strict_type_args = true;
    esproposal_class_instance_fields = Options.ESPROPOSAL_ENABLE;
    esproposal_class_static_fields = Options.ESPROPOSAL_ENABLE;
    esproposal_decorators = Options.ESPROPOSAL_WARN;
    esproposal_export_star_as = Options.ESPROPOSAL_WARN;
    facebook_fbt = None;
    ignore_non_literal_requires = false;
    moduleSystem = Node;
    module_name_mappers = [];
    node_resolver_dirnames = ["node_modules"];
    munge_underscores = false;
    module_file_exts;
    module_resource_exts;
    modules_are_use_strict = false;
    suppress_comments = [Str.regexp "\\(.\\|\n\\)*\\$FlowFixMe"];
    suppress_types = SSet.empty |> SSet.add "$FlowFixMe";
    traces = 0;
    strip_root = false;
    all = false;
    log_file = None;
    max_header_tokens = 10;
    max_workers = Sys_utils.nbr_procs;
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
          match definition._initializer with
          | USE_DEFAULT -> config
          | INIT_FN f ->
              try f config
              with UserError msg ->
                let line_num = fst (List.hd values) in
                error line_num (
                  spf "Error initializing config option \"%s\". %s" key msg
                )
        ) in

        (* Error when duplicate options were incorrectly given *)
        let allow_dupes = List.mem ALLOW_DUPLICATE definition.flags in
        if (not allow_dupes) && (List.length values) > 1 then (
          let line_num = fst (List.nth values 1) in
          error line_num (spf "Duplicate option: \"%s\"" key)
        );

        let config = List.fold_left (fun config (line_num, value_str) ->
          let value =
            try definition.optparser value_str
            with UserError msg -> error line_num (
              spf "Error parsing value for \"%s\". %s" key msg
            )
          in
          try definition.setter config value
          with UserError msg -> error line_num (
            spf "Error setting value for \"%s\". %s" key msg
          )
        ) config values in

        (new_raw_opts, config)

  let optparse_enum values str =
    let values = List.fold_left map_add SMap.empty values in
    match SMap.get str values with
    | Some v -> v
    | None -> raise (UserError (
        spf "Unsupported value: \"%s\". Supported values are: %s"
          str
          (String.concat ", " (SMap.keys values))
      ))

  let optparse_boolean = optparse_enum [
    ("true", true);
    ("false", false);
  ]

  let optparse_uint str =
    let v = int_of_string str in
    if v < 0 then raise (UserError "Number cannot be negative!") else v

  let optparse_string str =
    try Scanf.unescaped str
    with Scanf.Scan_failure reason -> raise (UserError (
      spf "Invalid ocaml string: %s" reason
    ))

  let optparse_regexp str =
    let unescaped = optparse_string str in
    try Str.regexp unescaped
    with Failure reason -> raise (UserError (
      spf "Invalid regex \"%s\" (%s)" unescaped reason
    ))

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

  let optparse_filepath str =
    Path.make str
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
      | Opts.Node -> "node"
      | Opts.Haste -> "haste"

    in fun o config -> Opts.(
      let options = config.options in
      if options.moduleSystem <> default_options.moduleSystem
      then opt o "module.system" (module_system options.moduleSystem);
      if options.all <> default_options.all
      then opt o "all" (string_of_bool options.all);
      if options.temp_dir <> default_options.temp_dir
      then opt o "temp_dir" options.temp_dir
    )

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
    options o config
end

let empty_config = {
  ignores = [];
  includes = [];
  libs = [];
  options = Opts.default_options;
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
    |> define_opt "esproposal.class_instance_fields" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_esproposal_feature_flag ~allow_enable:true;
      setter = (fun opts v -> {
        opts with esproposal_class_instance_fields = v;
      });
    }

    |> define_opt "esproposal.class_static_fields" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_esproposal_feature_flag ~allow_enable:true;
      setter = (fun opts v -> {
        opts with esproposal_class_static_fields = v;
      });
    }

    |> define_opt "esproposal.decorators" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_esproposal_feature_flag;
      setter = (fun opts v -> {
        opts with esproposal_decorators = v;
      });
    }

    |> define_opt "esproposal.export_star_as" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_esproposal_feature_flag ~allow_enable:true;
      setter = (fun opts v -> {
        opts with esproposal_export_star_as = v;
      });
    }

    |> define_opt "facebook.fbt" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_string;
      setter = (fun opts v -> {
        opts with facebook_fbt = Some v;
      });
    }

    |> define_opt "log.file" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_filepath;
      setter = (fun opts v -> {
        opts with log_file = Some v;
      });
    }

    |> define_opt "max_header_tokens" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_uint;
      setter = (fun opts v ->
        {opts with max_header_tokens = v;}
      );
    }

    |> define_opt "module.ignore_non_literal_requires" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        {opts with ignore_non_literal_requires = v;}
      );
    }

    |> define_opt "module.file_ext" {
      _initializer = INIT_FN (fun opts -> {
        opts with module_file_exts = SSet.empty;
      });
      flags = [ALLOW_DUPLICATE];
      optparser = optparse_string;
      setter = (fun opts v ->
        if String_utils.string_ends_with v Files.flow_ext
        then raise (Opts.UserError (
          "Cannot use file extension '" ^
          v ^
          "' since it ends with the reserved extension '"^
          Files.flow_ext^
          "'"
        ));
        let module_file_exts = SSet.add v opts.module_file_exts in
        {opts with module_file_exts;}
      );
    }

    |> define_opt "module.name_mapper" {
      _initializer = USE_DEFAULT;
      flags = [ALLOW_DUPLICATE];
      optparser = (fun str ->
        let regexp_str = "^'\\([^']*\\)'[ \t]*->[ \t]*'\\([^']*\\)'$" in
        let regexp = Str.regexp regexp_str in
        (if not (Str.string_match regexp str 0) then
          raise (Opts.UserError (
            "Expected a mapping of form: " ^
            "'single-quoted-string' -> 'single-quoted-string'"
          ))
        );

        let pattern = Str.matched_group 1 str in
        let template = Str.matched_group 2 str in

        (Str.regexp pattern, template)
      );
      setter = (fun opts v ->
        let module_name_mappers = v :: opts.module_name_mappers in
        {opts with module_name_mappers;}
      );
    }

    |> define_opt "module.name_mapper.extension" {
      _initializer = USE_DEFAULT;
      flags = [ALLOW_DUPLICATE];
      optparser = (fun str ->
        let regexp_str = "^'\\([^']*\\)'[ \t]*->[ \t]*'\\([^']*\\)'$" in
        let regexp = Str.regexp regexp_str in
        (if not (Str.string_match regexp str 0) then
          raise (Opts.UserError (
            "Expected a mapping of form: " ^
            "'single-quoted-string' -> 'single-quoted-string'"
          ))
        );

        let file_ext = Str.matched_group 1 str in
        let template = Str.matched_group 2 str in

        (Str.regexp ("^\\(.*\\)\\." ^ (Str.quote file_ext) ^ "$"), template)
      );
      setter = (fun opts v ->
        let module_name_mappers = v :: opts.module_name_mappers in
        {opts with module_name_mappers;}
      );
    }

    |> define_opt "module.system" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_enum [
        ("node", Node);
        ("haste", Haste);
      ];
      setter = (fun opts v -> {
        opts with moduleSystem = v;
      });
    }

    |> define_opt "module.system.node.resolve_dirname" {
      _initializer = INIT_FN (fun opts -> {
        opts with node_resolver_dirnames = [];
      });
      flags = [ALLOW_DUPLICATE];
      optparser = optparse_string;
      setter = (fun opts v ->
        let node_resolver_dirnames = v :: opts.node_resolver_dirnames in
        {opts with node_resolver_dirnames;}
      );
    }

    |> define_opt "module.use_strict" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        {opts with modules_are_use_strict = v;}
      );
    }

    |> define_opt "munge_underscores" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        {opts with munge_underscores = v;}
      );
    }

    |> define_opt "server.max_workers" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_uint;
      setter = (fun opts v ->
        {opts with max_workers = v;}
      );
    }

    |> define_opt "strip_root" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        {opts with strip_root = v;}
      );
    }

    |> define_opt "all" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        {opts with all = v;}
      );
    }

    |> define_opt "suppress_comment" {
      _initializer = INIT_FN (fun opts ->
        {opts with suppress_comments = [];}
      );
      flags = [ALLOW_DUPLICATE];
      optparser = optparse_regexp;
      setter = (fun opts v -> {
        opts with suppress_comments = v::(opts.suppress_comments);
      });
    }

    |> define_opt "suppress_type" {
      _initializer = INIT_FN (fun opts ->
        {opts with suppress_types = SSet.empty;}
      );
      flags = [ALLOW_DUPLICATE];
      optparser = optparse_string;
      setter = (fun opts v -> {
        opts with suppress_types = SSet.add v opts.suppress_types;
      });
    }

    |> define_opt "temp_dir" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_string;
      setter = (fun opts v -> {
        opts with temp_dir = v;
      });
    }

    |> define_opt "sharedmemory.dirs" {
      _initializer = USE_DEFAULT;
      flags = [ALLOW_DUPLICATE];
      optparser = optparse_string;
      setter = (fun opts v -> {
        opts with shm_dirs = opts.shm_dirs @ [v];
      });
    }

    |> define_opt "sharedmemory.minimum_available" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_uint;
      setter = (fun opts shm_min_avail -> {
        opts with shm_min_avail;
      });
    }

    |> define_opt "sharedmemory.dep_table_pow" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_uint;
      setter = (fun opts shm_dep_table_pow -> {
        opts with shm_dep_table_pow;
      });
    }

    |> define_opt "sharedmemory.hash_table_pow" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_uint;
      setter = (fun opts shm_hash_table_pow -> {
        opts with shm_hash_table_pow;
      });
    }

    |> define_opt "sharedmemory.log_level" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_uint;
      setter = (fun opts shm_log_level -> {
        opts with shm_log_level;
      });
    }

    |> define_opt "traces" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_uint;
      setter = (fun opts v ->
        {opts with traces = v;}
      );
    }

    |> define_opt "unsafe.enable_getters_and_setters" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        {opts with enable_unsafe_getters_and_setters = v;}
      );
    }

    |> define_opt "experimental.const_params" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        {opts with enable_const_params = v;}
      );
    }

    |> define_opt "experimental.strict_type_args" {
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        {opts with enforce_strict_type_args = v;}
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

let init ~ignores ~includes ~libs ~options =
  let ignores_lines = List.map (fun s -> (1, s)) ignores in
  let includes_lines = List.map (fun s -> (1, s)) includes in
  let options_lines = List.map (fun s -> (1, s)) options in
  let lib_lines = List.map (fun s -> (1, s)) libs in
  let config = parse_ignores empty_config ignores_lines in
  let config = parse_includes config includes_lines in
  let config = parse_options config options_lines in
  let config = parse_libs config lib_lines in
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
