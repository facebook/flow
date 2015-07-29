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

module Json = Hh_json

let version = "0.13.1"

type moduleSystem = Node | Haste

type options = {
  enable_unsafe_getters_and_setters: bool;
  moduleSystem: moduleSystem;
  module_name_mappers: (Str.regexp * string) list;
  munge_underscores: bool;
  suppress_comments: Str.regexp list;
  suppress_types: SSet.t;
  traces: int;
  strip_root: bool;
  log_file: Path.t;
}

module PathMap : MapSig with type key = Path.t
= MyMap(struct
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
  options: options;
  (* root path *)
  root: Path.t;
}

let tmp_dir = "/tmp/flow/"

let file_of_root root extension =
  let tmp_dir = if tmp_dir.[String.length tmp_dir - 1] <> '/'
    then tmp_dir ^ "/"
    else tmp_dir in
  Tmp.mkdir tmp_dir; (* TODO: move this to places that write this file *)
  let root_part = Path.slash_escaped_string_of_path root in
  Printf.sprintf "%s%s.%s" tmp_dir root_part extension

let init_file root = file_of_root root "init"
let lock_file root = file_of_root root "lock"
let pids_file root = file_of_root root "pids"
let socket_file root = file_of_root root "sock"

let default_log_file root =
  let root_part = Path.slash_escaped_string_of_path root in
  Path.make (Printf.sprintf "%s%s.log" tmp_dir root_part)

let default_module_system = Node

let default_options root = {
  enable_unsafe_getters_and_setters = false;
  moduleSystem = default_module_system;
  module_name_mappers = [];
  munge_underscores = false;
  suppress_comments = [];
  suppress_types = SSet.empty;
  traces = 0;
  strip_root = false;
  log_file = default_log_file root;
}

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
      | Node -> "node"
      | Haste -> "haste"

    in fun o config ->
      let options = config.options in
      let default_opts = default_options config.root in
      if options.moduleSystem <> default_opts.moduleSystem
      then opt o "module.system" (module_system options.moduleSystem)

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
  options = (default_options root);
  root;
}

let error ln msg =
  Printf.printf ".flowconfig:%d %s\n" ln msg;
  exit 4

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

module OptionsParser = struct
  type option_parser = (options -> string -> (int * string) -> options)
  type t = option_parser SMap.t
  type option_flag =
    | ALLOW_DUPLICATE
  type option_entry = {
    flags: option_flag list;
    _parser: option_parser;
  }

  let map_add map (key, value) = SMap.add key value map

  let configure configuration =
    List.fold_left map_add SMap.empty configuration

  let unescape opt (ln, value) =
    try Scanf.unescaped value
    with Scanf.Scan_failure reason ->
      let msg = spf "Invalid ocaml string for %s: %s" opt reason in
      error ln msg

  let raw_string option_setter =
    fun options opt (ln, value) ->
      (* Process escape characters as if the string were written in ocaml *)
      let unescaped_value = unescape opt (ln, value) in
      option_setter options (ln, unescaped_value)

  let regexp option_setter =
    fun options opt (ln, value) ->
      try
        (* Process escape characters as if the string were written in ocaml *)
        let unescaped_value = unescape opt (ln, value) in
        let r = Str.regexp unescaped_value in
        option_setter options (ln, r)
      with Failure reason ->
        let msg = spf "Invalid regex for %s: %s" opt reason in
        error ln msg

  (* Generic option parser constructor. Makes an option parser given a string
     description `supported` of the target datatype and a partial function
     `converter` that parsers strings to that datatype. *)
  let generic (supported, converter) option_setter =
    fun options opt (ln, value) ->
      match converter value with
      | Some value -> option_setter options (ln, value)
      | None ->
          let msg = spf
            "Unsupported value for %s: \"%s\"\nSupported values: %s"
            opt value supported in
          error ln msg

  let boolean =
    generic ("true, false", fun s -> try Some (bool_of_string s) with _ -> None)

  (* Option parser constructor for finite sets. Reuses the generic option parser
     constructor, passing the appropriate `supported` and `converter`. *)
  let enum values option_setter =
    let map = List.fold_left map_add SMap.empty values in
    let converter = fun value -> SMap.get value map in
    let supported = values
          |> List.map fst
          |> List.map (spf "\"%s\"")
          |> String.concat ", " in
    generic (supported, converter) option_setter

  let str_to_str_mapper option_setter =
    fun options opt (ln, value) ->
      let value = String.trim value in

      let regexp_str = "^'\\([^']*\\)'[ \t]*->[ \t]*'\\([^']*\\)'$" in
      let regexp = Str.regexp regexp_str in
      (if not (Str.string_match regexp value 0) then
        error ln (
          "Expected a mapping of form: " ^
          "'single-quoted-string' -> 'single-quoted-string'"
        )
      );

      let left = Str.matched_group 1 value in
      let right = Str.matched_group 2 value in

      option_setter options (ln, (left, right))

  let contains_flag flags flag =
    List.fold_left (fun contains_flag maybe_flag ->
      (maybe_flag = flag) || contains_flag
    ) false flags

  let parse_line p (options, seen) (ln, line) =
    if Str.string_match (Str.regexp "^\\([a-zA-Z._]+\\)=\\(.*\\)$") line 0
    then
      let opt = Str.matched_group 1 line in
      if SMap.mem opt p
      then
        let opt_entry = SMap.find_unsafe opt p in
        let allows_dupes = contains_flag opt_entry.flags ALLOW_DUPLICATE in
        let seen = if SSet.mem opt seen && not allows_dupes
          then error ln (spf "Duplicate option: \"%s\"" opt)
          else SSet.add opt seen
        in
        let _parser = opt_entry._parser in
        let options = _parser options opt (ln, (Str.matched_group 2 line)) in
        (options, seen)
      else error ln (spf "Unsupported option: \"%s\"" opt)
    else error ln "Unable to parse line"

  let parse config p lines =
    let seen = SSet.empty in
    let options, _ =
      List.fold_left (parse_line p) (default_options config.root, seen) lines in
    options
end

let options_parser = OptionsParser.configure [
  ("suppress_comment", OptionsParser.({
    flags = [ALLOW_DUPLICATE];
    _parser = regexp (fun options (ln, suppress_comment) ->
      let suppress_comments = suppress_comment::(options.suppress_comments) in
      { options with suppress_comments; }
    );
  }));

  ("suppress_type", OptionsParser.({
    flags = [ALLOW_DUPLICATE];
    _parser = raw_string (fun options (ln, suppress_type) ->
      let suppress_types = options.suppress_types in
      if SSet.mem suppress_type suppress_types
      then error ln (spf "Duplicate suppress_type value %s" suppress_type);
      let suppress_types = SSet.add suppress_type suppress_types in
      { options with suppress_types; }
    );
  }));

  ("log.file", OptionsParser.({
    flags = [];
    _parser = generic
      ("string", fun s -> Some (Path.make s))
      (fun opts (_, log_file) -> { opts with log_file });
  }));

  ("module.system", OptionsParser.({
    flags = [];
    _parser = enum ["node", Node; "haste", Haste] (fun opts (_, moduleSystem) ->
      {opts with moduleSystem}
    );
  }));

  ("module.name_mapper", OptionsParser.({
    flags = [ALLOW_DUPLICATE];
    _parser = str_to_str_mapper (fun options (ln, (pattern, template)) ->
      let rewriter = (Str.regexp pattern, template) in
      let module_name_mappers = options.module_name_mappers @ [rewriter] in
      {options with module_name_mappers }
    );
  }));

  ("munge_underscores", OptionsParser.({
    flags = [];
    _parser = boolean
      (fun opts (_, munge_underscores) ->
        { opts with munge_underscores });
  }));

  ("unsafe.enable_getters_and_setters", OptionsParser.({
    flags = [];
    _parser = boolean
      (fun opts (_, enable_unsafe_getters_and_setters) ->
        { opts with enable_unsafe_getters_and_setters });
  }));

  ("traces", OptionsParser.({
    flags = [];
    _parser = generic
      ("integer", fun s -> try Some (int_of_string s) with _ -> None)
      (fun opts (_, traces) -> { opts with traces });
  }));

  ("strip_root", OptionsParser.({
    flags = [];
    _parser = boolean
      (fun opts (_, strip_root) -> { opts with strip_root });
  }));
]

let parse_options config lines =
  let lines = lines
    |> List.map (fun (ln, line) -> ln, String.trim line)
    |> List.filter (fun (ln, s) -> s <> "") in
  let options = OptionsParser.parse config options_parser lines in
  { config with options }

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
  let lines = List.mapi (fun i line -> (i+1, String.trim line)) lines in
  let config = empty_config root in
  parse config lines

let init root options =
  let file = fullpath root in
  if Sys.file_exists file
  then begin
    Printf.printf "Error: \"%s\" already exists!\n%!" file;
    exit 4
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
  | none -> failwith "No config loaded"
