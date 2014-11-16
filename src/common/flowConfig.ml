(**
 *  Copyright 2014 Facebook.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *)

open Utils

type config = {
  excludes: (string * Str.regexp) list;
  includes: Path.path list;
  root: Path.path;
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

  let config o config =
    section_header o "ignore";
    excludes o config.excludes;
    fprintf o "\n";
    section_header o "include";
    includes o config.includes
end

let empty_config root = {
  excludes = [];
  includes = [];
  root;
}

let error ln msg =
  failwith (spf ".flowconfig:%d %s" ln msg)

let group_into_sections lines =
  let is_section_header = Str.regexp "^\\[\\(.*\\)\\]$" in
  let _, sections, section =
    List.fold_left (fun (seen, sections, (section, lines)) (ln, line) ->
      if Str.string_match is_section_header line 0
      then begin
        let sections = (section, List.rev lines)::sections in
        let section = Str.matched_group 1 line in
        if SSet.mem section seen
        then error ln (spf "contains duplicate section: %s" section);
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

let parse_excludes config lines =
  let excludes = lines
  |> List.map (fun (ln, line) -> String.trim line)
  |> List.filter (fun s -> s <> "")
  |> List.map (fun s -> (s, Str.regexp s)) in
  { config with excludes; }

let parse_section config ((section_ln, section), lines) =
  match section, lines with
  | "", [] when section_ln = 0 -> config
  | "", (ln, line)::lines when section_ln = 0 ->
      error ln "Unexpected config line not in any section"
  | "include", _ -> parse_includes config lines
  | "ignore", _ -> parse_excludes config lines
  | _ -> error section_ln (spf "Unsupported config section: %s" section)

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

let init root =
  let file = fullpath root in
  if Sys.file_exists file
  then begin
    Printf.printf "Error: %s already exists!\n%!" file;
    exit 2
  end;
  let config = empty_config root in
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
