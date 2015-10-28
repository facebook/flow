(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils
open Sys_utils

let version = "0.18.1"
let flow_ext = ".flow"

let default_temp_dir =
  Path.to_string @@
  Path.concat Path.temp_dir_name "flow"

let default_shm_dir =
  try Sys.getenv "FLOW_SHMDIR"
  with _ -> "/dev/shm"

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
  exception UserError of string

  type moduleSystem = Node | Haste

  type esproposal_feature_mode =
    | ESPROPOSAL_ENABLE
    | ESPROPOSAL_IGNORE
    | ESPROPOSAL_WARN

  type t = {
    enable_unsafe_getters_and_setters: bool;
    esproposal_class_instance_fields: esproposal_feature_mode;
    esproposal_class_static_fields: esproposal_feature_mode;
    esproposal_decorators: esproposal_feature_mode;
    ignore_non_literal_requires: bool;
    moduleSystem: moduleSystem;
    module_name_mappers: (Str.regexp * string) list;
    node_resolver_dirnames: string list;
    munge_underscores: bool;
    module_file_exts: SSet.t;
    suppress_comments: Str.regexp list;
    suppress_types: SSet.t;
    traces: int;
    strip_root: bool;
    log_file: Path.t option;
    max_workers: int;
    temp_dir: Path.t;
    shm_dir: Path.t;
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
          List.map (fun (line_num, value) -> (line_num, msg)) v
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

  let default_options = {
    enable_unsafe_getters_and_setters = false;
    esproposal_class_instance_fields = ESPROPOSAL_WARN;
    esproposal_class_static_fields = ESPROPOSAL_WARN;
    esproposal_decorators = ESPROPOSAL_WARN;
    ignore_non_literal_requires = false;
    moduleSystem = Node;
    module_name_mappers = [];
    node_resolver_dirnames = ["node_modules"];
    munge_underscores = false;
    module_file_exts;
    suppress_comments = [];
    suppress_types = SSet.empty;
    traces = 0;
    strip_root = false;
    log_file = None;
    max_workers = Sys_utils.nbr_procs;
    temp_dir = Path.make default_temp_dir;
    shm_dir = Path.make default_shm_dir;
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
        |> List.filter (fun (ln, s) -> s <> "")
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
      ("ignore", ESPROPOSAL_IGNORE);
      ("warn", ESPROPOSAL_WARN);
    ] in
    let values =
      if allow_enable
      then ("enable", ESPROPOSAL_ENABLE)::values
      else values
    in
    optparse_enum values

  let optparse_filepath str =
    Path.make str
end

module PathMap : MapSig with type key = Path.t = MyMap(struct
  type t = Path.t
  let compare p1 p2 =
    String.compare (Path.to_string p1) (Path.to_string p2)
end)

type config = {
  (* file blacklist *)
  excludes: (string * Str.regexp) list;
  (* non-root include paths. may contain wildcards *)
  includes: Path.t list;
  (* stems extracted from includes *)
  include_stems: Path.t list;
  (* map from include_stems to list of (original path, regexified path) *)
  include_map: ((string * Str.regexp) list) PathMap.t;
  (* library paths. no wildcards *)
  libs: Path.t list;
  (* config options *)
  options: Opts.t;
  (* root path *)
  root: Path.t;
}

let add_dir_sep dir =
  let open Filename in
  if check_suffix dir dir_sep
  then dir
  else dir ^ dir_sep

let file_of_root extension ~tmp_dir root =
  (* TODO: move this to places that write this file *)
  mkdir_no_fail tmp_dir;
  let tmp_dir = tmp_dir |> Path.make |> Path.to_string |> add_dir_sep in
  let root_part = Path.slash_escaped_string_of_path root in
  Printf.sprintf "%s%s.%s" tmp_dir root_part extension

let init_file   = file_of_root "init"
let lock_file   = file_of_root "lock"
let pids_file   = file_of_root "pids"
let socket_file = file_of_root "sock"
let log_file ~tmp_dir root opts =
  match opts.Opts.log_file with
  | Some x -> x
  | None -> Path.make (file_of_root "log" ~tmp_dir root)

module Pp : sig
  val config : out_channel -> config -> unit
end = struct
  open Printf

  let section_header o section =
    fprintf o "[%s]\n" section

  let excludes o excludes =
    List.iter (fun ex -> (fprintf o "%s\n" (fst ex))) excludes

  let includes o includes =
    List.iter (fun inc -> (fprintf o "%s\n" (Path.to_string inc))) includes

  let libs o libs =
    List.iter (fun lib -> (fprintf o "%s\n" (Path.to_string lib))) libs

  let options =
    let opt o name value = fprintf o "%s=%s\n" name value

    in let module_system = function
      | Opts.Node -> "node"
      | Opts.Haste -> "haste"

    in fun o config -> Opts.(
      let options = config.options in
      if options.moduleSystem <> default_options.moduleSystem
      then opt o "module.system" (module_system options.moduleSystem)
    )

  let config o config =
    section_header o "ignore";
    excludes o config.excludes;
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

let empty_config root = {
  excludes = [];
  includes = [];
  include_stems = [];
  include_map = PathMap.empty;
  libs = [];
  options = Opts.default_options;
  root;
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

(* given a path, return the max prefix not containing a wildcard
   or a terminal filename.
 *)
let path_stem =
  let wc = Str.regexp "^[^*?]*[*?]" in
  (fun path ->
    (* strip filename *)
    let path = if Path.file_exists path && not (Path.is_directory path)
      then Path.parent path else path in
    let path_str = Path.to_string path in
    (* strip back to non-wc prefix *)
    let stem = if Str.string_match wc path_str 0
      then Filename.dirname (Str.matched_string path_str)
    else path_str in
    Path.make stem)

let dir_sep = Str.regexp_string Filename.dir_sep

(* translate a path with wildcards into a regex *)
let path_patt =
  let star = Str.regexp_string "*" in
  let star2 = Str.regexp_string "**" in
  let qmark = Str.regexp_string "?" in
  fun path ->
    let str = Path.to_string path in
    (* because we accept both * and **, convert in 2 steps *)
    let results = Str.full_split star2 str in
    let results = List.map (fun r -> match r with
      | Str.Text s ->
          (* note: unix path specifiers only *)
          let s = Str.global_replace star "[^/]*" s in
          Str.global_replace qmark "." s
      | Str.Delim _ -> ".*") results in
    let str = String.concat "" results in
    Str.regexp str

(* helper - eliminate noncanonical entries where possible.
   no other normalization is done *)
let fixup_path p =
  let s = Path.to_string p in
  let is_normalized = match realpath s with
      | Some s' -> s' = s
      | None -> false in
  if is_normalized then p else
  let abs = not (Filename.is_relative s) in
  let entries = Str.split_delim dir_sep s in
  let rec loop revbase entries =
    match entries with
    | h :: t when h = Filename.current_dir_name ->
        loop revbase t
    | h :: t when h = Filename.parent_dir_name -> (
        match revbase with
        | rh :: rt -> loop rt t
        | _ -> loop (h :: revbase) t
      )
    | h :: t -> loop (h :: revbase) t
    | [] -> List.rev revbase
  in
  let entries = loop [] entries in
  let s = List.fold_left Filename.concat "" entries in
  let s = if abs then Filename.dir_sep ^ s else s in
  Path.make s

let make_path_absolute config path =
  if Filename.is_relative path
  then Path.concat config.root path
  else Path.make path

(* parse include lines and set config's
   includes (a list of path specs) and
   include_map (a map from stems to (spec, regex) pairs *)
let parse_includes config lines =
  let includes = lines
    |> List.map (fun (ln, line) -> String.trim line)
    |> List.filter (fun s -> s <> "")
    |> List.map (make_path_absolute config)
    |> List.map fixup_path
  in
  let include_stems, include_map = List.fold_left (fun (stems, map) path ->
    let stem = path_stem path in
    let patt = path_patt path in
    let pstr = Path.to_string path in
    match PathMap.get stem map with
      | None ->
          let map = PathMap.add stem [pstr, patt] map in
          (stem :: stems), map
      | Some entries ->
          let map = PathMap.add stem ((pstr, patt) :: entries) map in
          stems, map
  ) ([], PathMap.empty) includes
  in
  { config with includes; include_stems; include_map; }

(* find a prefix for f in a list of paths, or none *)
let rec find_prefix f = function
| [] -> None
| h :: _ when str_starts_with f (Path.to_string h) -> Some h
| h :: t -> find_prefix f t

(* find a match for f in a list of patterns, or none *)
let rec match_patt f = function
| [] -> None
| (path, patt) :: _ when Str.string_match patt f 0 -> Some path
| (path, patt) :: t -> match_patt f t

(* try to find a match for f in our include paths *)
let is_included config f =
  match find_prefix f config.include_stems with
  | None -> false
  | Some stem ->
      let patts = PathMap.find_unsafe stem config.include_map in
      match_patt f patts != None

(* true if path matches one of our excludes *)
let is_excluded config =
  let list = List.map snd config.excludes in
  fun path -> List.exists (fun rx -> Str.string_match rx path 0) list

let parse_libs config lines =
  let libs = lines
  |> List.map (fun (ln, line) -> String.trim line)
  |> List.filter (fun s -> s <> "")
  |> List.map (make_path_absolute config) in
  { config with libs; }

let parse_excludes config lines =
  let excludes = lines
  |> List.map (fun (ln, line) -> String.trim line)
  |> List.filter (fun s -> s <> "")
  |> List.map (fun s -> (s, Str.regexp s)) in
  { config with excludes; }

let file_extension = Str.regexp "^\\(\\.[^ \t]+\\)+$"

let parse_options config lines = Opts.(
  let options = Opts.parse config.options lines
    |> Opts.define_opt "esproposal.class_instance_fields" Opts.({
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_esproposal_feature_flag ~allow_enable:true;
      setter = (fun opts v -> {
        opts with esproposal_class_instance_fields = v;
      });
    })

    |> Opts.define_opt "esproposal.class_static_fields" Opts.({
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_esproposal_feature_flag ~allow_enable:true;
      setter = (fun opts v -> {
        opts with esproposal_class_static_fields = v;
      });
    })

    |> Opts.define_opt "esproposal.decorators" Opts.({
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_esproposal_feature_flag;
      setter = (fun opts v -> {
        opts with esproposal_decorators = v;
      });
    })

    |> Opts.define_opt "log.file" Opts.({
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_filepath;
      setter = (fun opts v -> {
        opts with log_file = Some v;
      });
    })

    |> Opts.define_opt "module.ignore_non_literal_requires" Opts.({
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        {opts with ignore_non_literal_requires = v;}
      );
    })

    |> Opts.define_opt "module.file_ext" Opts.({
      _initializer = INIT_FN (fun opts -> {
        opts with module_file_exts = SSet.empty;
      });
      flags = [ALLOW_DUPLICATE];
      optparser = optparse_string;
      setter = (fun opts v ->
        if str_ends_with v flow_ext
        then raise (Opts.UserError (
          "Cannot use file extension '" ^
          v ^
          "' since it ends with the reserved extension '"^
          flow_ext^
          "'"
        ));
        let module_file_exts = SSet.add v opts.module_file_exts in
        {opts with module_file_exts;}
      );
    })

    |> Opts.define_opt "module.name_mapper" Opts.({
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

        ((Str.regexp pattern, template), str)
      );
      setter = (fun opts v ->
        let (v, str) = v in
        let module_name_mappers = v :: opts.module_name_mappers in
        {opts with module_name_mappers;}
      );
    })

    |> Opts.define_opt "module.system" Opts.({
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_enum [
        ("node", Node);
        ("haste", Haste);
      ];
      setter = (fun opts v -> {
        opts with moduleSystem = v;
      });
    })

    |> Opts.define_opt "module.system.node.resolve_dirname" Opts.({
      _initializer = INIT_FN (fun opts -> {
        opts with node_resolver_dirnames = [];
      });
      flags = [ALLOW_DUPLICATE];
      optparser = optparse_string;
      setter = (fun opts v ->
        let node_resolver_dirnames = v :: opts.node_resolver_dirnames in
        {opts with node_resolver_dirnames;}
      );
    })

    |> Opts.define_opt "munge_underscores" Opts.({
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        {opts with munge_underscores = v;}
      );
    })

    |> Opts.define_opt "server.max_workers" Opts.({
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_uint;
      setter = (fun opts v ->
        {opts with traces = v;}
      );
    })

    |> Opts.define_opt "strip_root" Opts.({
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        {opts with strip_root = v;}
      );
    })

    |> Opts.define_opt "suppress_comment" Opts.({
      _initializer = USE_DEFAULT;
      flags = [ALLOW_DUPLICATE];
      optparser = optparse_regexp;
      setter = (fun opts v -> {
        opts with suppress_comments = v::(opts.suppress_comments);
      });
    })

    |> Opts.define_opt "suppress_type" Opts.({
      _initializer = USE_DEFAULT;
      flags = [ALLOW_DUPLICATE];
      optparser = optparse_string;
      setter = (fun opts v -> {
        opts with suppress_types = SSet.add v opts.suppress_types;
      });
    })

    |> Opts.define_opt "temp_dir" Opts.({
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_filepath;
      setter = (fun opts v -> {
        opts with temp_dir = v;
      });
    })

    |> Opts.define_opt "shm_dir" Opts.({
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_filepath;
      setter = (fun opts v -> {
        opts with shm_dir = v;
      });
    })

    |> Opts.define_opt "traces" Opts.({
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_uint;
      setter = (fun opts v ->
        {opts with traces = v;}
      );
    })

    |> Opts.define_opt "unsafe.enable_getters_and_setters" Opts.({
      _initializer = USE_DEFAULT;
      flags = [];
      optparser = optparse_boolean;
      setter = (fun opts v ->
        {opts with enable_unsafe_getters_and_setters = v;}
      );
    })

    |> get_defined_opts
  in
  {config with options}
)

let assert_version (ln, line) =
  try
    if not (Semver.satisfies line version)
    then error ln (
      spf
        "Wrong version of Flow. The config specifies version %s but this is version %s"
        line
        version
    )
  with Semver.Parse_error _ ->
    error ln (
      spf
        "Expected version to match %%d.%%d.%%d, with an optional leading ^, got %s"
        line
    )

let parse_version config lines =
  lines
    |> List.map (fun (ln, line) -> ln, String.trim line)
    |> List.filter (fun (ln, s) -> s <> "")
    |> List.iter assert_version;
  config

let parse_section config ((section_ln, section), lines) =
  match section, lines with
  | "", [] when section_ln = 0 -> config
  | "", (ln, line)::lines when section_ln = 0 ->
      error ln "Unexpected config line not in any section"
  | "include", _ -> parse_includes config lines
  | "ignore", _ -> parse_excludes config lines
  | "libs", _ -> parse_libs config lines
  | "options", _ -> parse_options config lines
  | "version", _ -> parse_version config lines
  | _ -> error section_ln (spf "Unsupported config section: \"%s\"" section)

let parse config lines =
  let sections = group_into_sections lines in
  List.fold_left parse_section config sections

let fullpath root =
  Path.to_string (Path.concat root ".flowconfig")

let read root =
  let filename = fullpath root in
  let lines = cat_no_fail filename |> split_lines in
  let config = empty_config root in
  let lines = List.mapi (fun i line -> (i+1, String.trim line)) lines in
  parse config lines

let init root options =
  let file = fullpath root in
  if Sys.file_exists file
  then begin
    let msg = spf "Error: \"%s\" already exists!\n%!" file in
    FlowExitStatus.(exit ~msg Invalid_flowconfig)
  end;
  let options_lines = List.map (fun s -> (1, s)) options in
  let config = parse_options (empty_config root) options_lines in
  let out = open_out_no_fail (fullpath root) in
  Pp.config out config;
  close_out_no_fail (fullpath root) out

(* We should restart every time the config changes, so it's cool to cache it *)
let cache = ref None

let get root =
  match !cache with
  | None ->
      let config = read root in
      cache := Some config;
      config
  | Some config ->
      assert (root = config.root);
      config

let get_unsafe () =
  match !cache with
  | Some config -> config
  | None -> failwith "No config loaded"

let restore config = cache := Some config
