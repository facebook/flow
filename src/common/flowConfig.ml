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

type moduleSystem = Node | Haste

type options = {
  moduleSystem: moduleSystem;
  traces: bool;
}

type config = {
  excludes: (string * Str.regexp) list;
  includes: Path.path list;
  libs: Path.path list;
  options: options;
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

let parse_includes config lines =
  let includes = lines
  |> List.map (fun (ln, line) -> String.trim line)
  |> List.filter (fun s -> s <> "")
  |> List.map (Path.concat config.root) in
  { config with includes; }

let parse_libs config lines =
  let libs = lines
  |> List.map (fun (ln, line) -> String.trim line)
  |> List.filter (fun s -> s <> "")
  |> List.map (Path.concat config.root) in
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

let parse_section config ((section_ln, section), lines) =
  match section, lines with
  | "", [] when section_ln = 0 -> config
  | "", (ln, line)::lines when section_ln = 0 ->
      error ln "Unexpected config line not in any section"
  | "include", _ -> parse_includes config lines
  | "ignore", _ -> parse_excludes config lines
  | "libs", _ -> parse_libs config lines
  | "options", _ -> parse_options config lines
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
  | Some config -> config
