(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module SMap = Map.Make(String)
module SSet = Set.Make(String)

type t = {
  file: string option;
  source_root: string option;
  sources: SSet.t;
  names: SSet.t;
  mappings: mapping list;
  sources_contents: string SMap.t;
}
and mapping = {
  name: string option;
  source: string;
  original: line_col;
  generated: line_col;
}
and line_col = {
  line: int;
  col: int;
}

type mapping_state = {
  prev_gen_col: int;
  prev_gen_line: int;
  prev_orig_col: int;
  prev_orig_line: int;
  prev_name: int;
  prev_source: int;
  prev_mapping: mapping option;
}

let create ?file ?source_root () = {
  file;
  source_root;
  sources = SSet.empty;
  names = SSet.empty;
  mappings = [];
  sources_contents = SMap.empty;
}

let add_mapping ?name ~source ~original ~generated map =
  let sources = SSet.add source map.sources in
  let names = match name with
  | Some name -> SSet.add name map.names
  | None -> map.names
  in
  let mappings = { name; source; original; generated }::map.mappings in
  { map with sources; mappings; names }

let add_source_content ~source ~content map =
  let sources_contents = SMap.add source content map.sources_contents in
  { map with sources_contents }

let rec add_string_repeat ~times buf str =
  if times = 0 then () else begin
    Buffer.add_string buf str;
    add_string_repeat ~times:(times - 1) buf str
  end

let index_map set =
  let _, map = SSet.fold (fun item (i, map) ->
    i + 1, SMap.add item i map
  ) set (0, SMap.empty) in
  map

let string_of_mappings map =
  let state = {
    prev_gen_col = 0;
    prev_gen_line = 1;
    prev_orig_col = 0;
    prev_orig_line = 0;
    prev_name = 0;
    prev_source = 0;
    prev_mapping = None;
  } in
  let source_indexes = index_map map.sources in
  let name_indexes = index_map map.names in
  let buf = Buffer.create 127 in (* arbitrary length *)
  let (buf, _) = List.fold_left (fun (buf, state) mapping ->
    let state =
      let line_diff = mapping.generated.line - state.prev_gen_line in
      let skip, state =
        if line_diff > 0 then begin
          add_string_repeat ~times:line_diff buf ";";
          false, { state with
            prev_gen_col = 0; prev_gen_line = mapping.generated.line
          }
        end else begin
          let skip = match state.prev_mapping with
          | Some prev_mapping ->
            if mapping.source = prev_mapping.source &&
               mapping.original = prev_mapping.original then
              true
            else begin
              Buffer.add_string buf ",";
              false
            end
          | None -> false
          in
          skip, state
        end
      in
      if skip then state else begin
        Vlq.Base64.encode buf (mapping.generated.col - state.prev_gen_col);

        let source_idx = SMap.find mapping.source source_indexes in
        Vlq.Base64.encode buf (source_idx - state.prev_source);

        (* lines are stored 0-based in SourceMap spec version 3 *)
        Vlq.Base64.encode buf (mapping.original.line - 1 - state.prev_orig_line);
        Vlq.Base64.encode buf (mapping.original.col - state.prev_orig_col);

        let prev_name =
          match mapping.name with
          | Some name ->
            let name_idx = SMap.find name name_indexes in
            Vlq.Base64.encode buf (name_idx - state.prev_name);
            name_idx
          | None ->
            state.prev_name
        in

        { state with
          prev_gen_col = mapping.generated.col;
          prev_source = source_idx;
          prev_orig_line = mapping.original.line - 1;
          prev_orig_col = mapping.original.col;
          prev_name;
        }
      end
    in
    (buf, { state with prev_mapping = Some mapping })
  ) (buf, state) (List.rev map.mappings) in
  Buffer.contents buf

let sources_contents map =
  SSet.elements map.sources
  |> List.map (fun source ->
    if SMap.mem source map.sources_contents
      then Some (SMap.find source map.sources_contents)
      else None
  )

let version _map = "3"
let names map = SSet.elements map.names
let source_root map = map.source_root
let sources map = SSet.elements map.sources

module type Json_serializer_intf = sig
  type t
  val string: string -> t
  val obj: (string * t) list -> t
  val array: t list -> t
  val number: string -> t
  val null: t
end

module type Json = sig
  type json
  val json_of_sourcemap: t -> json
end

module Make_json (Serializer : Json_serializer_intf) : (Json with type json = Serializer.t) = struct
  type json = Serializer.t

  let json_of_sources_contents map =
    sources_contents map
    |> List.map (function Some content -> Serializer.string content | None -> Serializer.null)
    |> Serializer.array

  let json_of_sourcemap map =
    (* build props in reverse, "version" *must* end up first! *)
    let rev_props = [
      "mappings", Serializer.string (string_of_mappings map);
      "names", Serializer.array (List.map Serializer.string (names map));
      "sources", Serializer.array (List.map Serializer.string (sources map));
      "version", Serializer.number (version map);
    ] in
    let rev_props = match map.file with
    | Some file -> ("file", Serializer.string file)::rev_props
    | None -> rev_props
    in
    let rev_props = match source_root map with
    | Some root -> ("sourceRoot", Serializer.string root)::rev_props
    | None -> rev_props
    in
    let rev_props = if SMap.is_empty map.sources_contents
      then rev_props
      else ("sourcesContent", json_of_sources_contents map)::rev_props
    in
    Serializer.obj (List.rev rev_props)
end
