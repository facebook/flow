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

let version = "0.9.2"

type moduleSystem = Node | Haste

type options = {
  moduleSystem: moduleSystem;
  traces: bool;
}

module PathMap : MapSig with type key = Path.path
= MyMap(struct
  type t = Path.path with show
  let compare p1 p2 =
    String.compare (Path.string_of_path p1) (Path.string_of_path p2)
end)

type config = {
  (* file blacklist *)
  excludes: (string * Str.regexp) list;
  (* non-root include paths. may contain wildcards *)
  includes: Path.path list;
  (* stems extracted from includes *)
  include_stems: Path.path list;
  (* map from include_stems to list of (original path, regexified path) *)
  include_map: ((string * Str.regexp) list) PathMap.t;
  (* library paths. no wildcards *)
  libs: Path.path list;
  (* config options *)
  options: options;
  (* root path *)
  root: Path.path;
}

let default_options = {
  moduleSystem = Node;
  traces = false;
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
    List.iter (fun inc -> (fprintf o "%s\n" (Path.string_of_path inc))) includes

  let libs o libs =
    List.iter (fun lib -> (fprintf o "%s\n" (Path.string_of_path lib))) libs

  let options =
    let opt o name value = fprintf o "%s=%s\n" name value

    in let module_system = function
      | Node -> "node"
      | Haste -> "haste"

    in fun o options ->
      if options.moduleSystem <> default_options.moduleSystem
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
    options o config.options
end

let empty_config root = {
  excludes = [];
  includes = [];
  include_stems = [];
  include_map = PathMap.empty;
  libs = [];
  options = default_options;
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
        let section = Str.matched_group 1 line in
        if SSet.mem section seen
        then error ln (spf "contains duplicate section: \"%s\"" section);
        SSet.add section seen, sections, ((ln, section), [])
      end else
        seen, sections, (section, (ln, line)::lines)
    ) (SSet.empty, [], ((0, ""), [])) lines in
  List.rev (section::sections)

(* given a path, return the max prefix not containing a wildcard *)
let path_stem =
  let wc = Str.regexp "^[^*?]*[*?]" in
  (fun path ->
    let path_str = Path.string_of_path path in
    let stem = if Str.string_match wc path_str 0
    then Filename.dirname (Str.matched_string path_str)
    else path_str in
    Path.mk_path stem)

let dir_sep = Str.regexp_string Filename.dir_sep

(* translate a path with wildcards into a regex *)
let path_patt =
  let star = Str.regexp_string "*" in
  let star2 = Str.regexp_string "**" in
  let qmark = Str.regexp_string "?" in
  fun path ->
    let str = Path.string_of_path path in
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
let fixup_path p = if Path.is_normalized p then p else
  let s = Path.string_of_path p in
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
  Path.mk_path s

let make_path_absolute config path =
  if Filename.is_relative path
  then Path.concat config.root path
  else Path.mk_path path

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
    let pstr = Path.string_of_path path in
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
| h :: _ when str_starts_with f (Path.string_of_path h) -> Some h
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

module OptionsParser : sig
  type t
  type option_parser
  (* Parses a list of lines into an options object *)
  val parse : t -> (int * string) list -> options
  (* Pass configure a list of option names and option parsers *)
  val configure : (string * option_parser) list -> t

  (******* Option parser constructors ********)
  val enum : (string * 'a) list -> (options -> 'a -> options) -> option_parser
end = struct
  type option_parser = (options -> string -> (int * string) -> options)
  type t = option_parser SMap.t

  let map_add map (key, value) = SMap.add key value map

  let configure configuration =
    List.fold_left map_add SMap.empty configuration

  let enum values option_setter =
    let map = List.fold_left map_add SMap.empty values in
    fun options opt (ln, value) ->
      if SMap.mem value map
      then option_setter options (SMap.find_unsafe value map)
      else
        let supported = values
          |> List.map fst
          |> List.map (spf "\"%s\"")
          |> String.concat ", " in
        let msg = spf
          "Unsupported enum value for %s: \"%s\"\nSupprted values: %s"
          opt value supported in
        error ln msg

  let parse_line p (options, seen) (ln, line) =
    if Str.string_match (Str.regexp "^\\([a-zA-Z.]+\\)=\\(.*\\)$") line 0
    then
      let opt = Str.matched_group 1 line in
      let seen = if SSet.mem opt seen
        then error ln (spf "Duplicate option: \"%s\"" opt)
        else SSet.add opt seen in
      if SMap.mem opt p
      then
        let options =
          (SMap.find_unsafe opt p) options opt (ln, (Str.matched_group 2 line))
        in options, seen
      else error ln (spf "Unsupported option: \"%s\"" opt)
    else error ln "Unable to parse line"

  let parse p lines =
    let seen = SSet.empty in
    let options, _ =
      List.fold_left (parse_line p) (default_options, seen) lines in
    options
end

let options_parser = OptionsParser.configure [
  "module.system", OptionsParser.enum (["node", Node; "haste", Haste]) (fun
    options moduleSystem -> { options with moduleSystem });
  "traces", OptionsParser.enum (["true", true; "false", false]) (fun
    options traces -> { options with traces });
]

let parse_options config lines =
  let lines = lines
    |> List.map (fun (ln, line) -> ln, String.trim line)
    |> List.filter (fun (ln, s) -> s <> "") in
  let options = OptionsParser.parse options_parser lines in
  { config with options }

let assert_version (ln, line) =
  if line <> version
  then error ln (
    spf
      "Wrong version of Flow. The config specifies version %s but this is version %s"
      line
      version
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
  Path.string_of_path (Path.concat root ".flowconfig")

let read root =
  let filename = fullpath root in
  let lines = cat_no_fail filename |> split_lines in
  let lines = List.mapi (fun i line -> (i+1, line)) lines in
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
