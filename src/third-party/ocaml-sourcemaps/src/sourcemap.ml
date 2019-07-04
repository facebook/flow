(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module SMap = Map.Make(String)
module SSet = Set.Make(String)

type t = {
  file: string option;
  source_root: string option;
  names: SSet.t;
  mappings: mapping_holder; (* assumed to be sorted. switch to a Map? *)
  sources_contents: string SMap.t;
}
and mapping = {
  original: original option;
  generated_loc: line_col;
}
and original = {
  source: string;
  original_loc: line_col;
  name: string option;
}
and line_col = {
  line: int;
  col: int;
}
and mapping_holder =
  | Addition of mapping list
  | Lookup of lookup_context
and lookup_context = {
  mapping_arr: mapping array;
  mutable last_ind: int;
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
  names = SSet.empty;
  mappings = Addition [];
  sources_contents = SMap.empty;
}

module MappingHolder = struct
  exception MappingHolderException of string

  let fold_left func init holder =
    match holder with
    | Addition l -> List.fold_left func init l
    | Lookup {mapping_arr; _} -> Array.fold_left func init mapping_arr

  let make_lookup mapping_arr = Lookup {
    mapping_arr;
    last_ind = -1;
  }

  let addition_list holder =
    match holder with
    | Addition l -> l
    | _ -> raise (MappingHolderException "Cannot get list from frozen holder")
end

let freeze_for_lookup map =
  match map.mappings with
  | Addition l -> { map with mappings = MappingHolder.make_lookup (Array.of_list l) }
  | _ -> map

let sources_set map =
  MappingHolder.fold_left (fun acc mapping ->
    match mapping with
    | { original = Some { source; _ }; _ } -> SSet.add source acc
    | { original = None; _ } -> acc
  ) SSet.empty map.mappings

let sources map = SSet.elements (sources_set map)

(* Searches for `needle` and the index it's found at in `arr`. If `needle` doesn't exist,
   returns the closest lower bound. *)
let rec binary_search ~cmp needle arr l u =
  let len = Array.length arr in
  if len = 0 then None
  else if l >= len then Some (arr.(len - 1), len - 1)
  else if u < l then Some (arr.(l), l)
  else
    let i = (l + u) / 2 in
    let k = cmp needle arr.(i) in
    if k = 0 then Some (arr.(i), i)
    else if k < 0 then binary_search ~cmp needle arr l (i - 1)
    else binary_search ~cmp needle arr (i + 1) u

let original_from_found = function
  | Some ({ original; _ },_) -> original
  | None -> None

let find_original_unknown_arr mappings_arr generated =
  let len = Array.length mappings_arr in
  let cmp = fun { line = a_line; col = a_col } { generated_loc = { line = b_line; col = b_col }; _ } ->
    let k = b_line - a_line in
    if k = 0 then b_col - a_col
    else k
  in
  binary_search ~cmp generated mappings_arr 0 (len - 1)

(* Try to inform local search based on last found index, else binary search *)
let find_original_from_context c generated =
  let len = Array.length c.mapping_arr in
  if len = 0 then None else
  let line_for_index ind = c.mapping_arr.(ind).generated_loc.line in
  let column_for_index ind = c.mapping_arr.(ind).generated_loc.col in
  let try_local_search =
    if c.last_ind < 0 then None else
    let recent_line = line_for_index c.last_ind in
    if generated.line <> recent_line then None else
    let look_distance = 5 in
    (* Left and right can be deceiving. The array is sorted descending. *)
    let rec look_right cur_ind looks_remaining =
      if cur_ind = (len - 1) ||
         (line_for_index (cur_ind + 1)) <> recent_line
      then
        Some (c.mapping_arr.(cur_ind), cur_ind)
      else if (column_for_index (cur_ind + 1)) <= generated.col
      then
        Some (c.mapping_arr.(cur_ind + 1), cur_ind + 1)
      else if looks_remaining = 0 then None
      else look_right (cur_ind + 1) (looks_remaining - 1) in
    let rec look_left cur_ind looks_remaining =
      if cur_ind = 0 ||
         (line_for_index (cur_ind - 1)) <> recent_line ||
         (column_for_index (cur_ind - 1)) > generated.col
      then
        Some (c.mapping_arr.(cur_ind), cur_ind)
      else if looks_remaining = 0 then None
      else look_left (cur_ind - 1) (looks_remaining - 1) in
    let cur_col = column_for_index c.last_ind in
    if generated.col < cur_col then
      look_right c.last_ind look_distance
    else
      look_left c.last_ind look_distance
  in
  let findings =
    match try_local_search with
    | Some _ -> try_local_search
    | _ -> find_original_unknown_arr c.mapping_arr generated
  in
  let () = match findings with
    | Some (_, ind) -> c.last_ind <- ind
    | None -> ()
  in
  original_from_found findings


let find_original map generated =
  match map.mappings with
  | Addition l ->
      let found = find_original_unknown_arr (Array.of_list l) generated in
      original_from_found found
  | Lookup c -> find_original_from_context c generated

(* for each mapping in `map`, update to the `original` info corresponding to
   that loc in map2 *)
let compose map map2 =
  let mappings, names, sources_contents =
    List.fold_left (fun (mappings, names, sources_contents) mapping ->
      match mapping.original with
      | Some { original_loc; _ } ->
        begin match find_original map2 original_loc with
        | Some ({ name; source; _ } as original) ->
          let mapping = { mapping with original = Some original } in
          let names = match name with Some name -> SSet.add name names | None -> names in
          let sources_contents =
            match SMap.find_opt source map2.sources_contents with
            | Some content -> SMap.add source content sources_contents
            | _ -> sources_contents
          in
          mapping::mappings, names, sources_contents
        | None -> mappings, names, sources_contents
        end
      | None -> mappings, names, sources_contents
  ) ([], SSet.empty, SMap.empty) (MappingHolder.addition_list map.mappings) in
  { map with mappings = Addition (List.rev mappings); names; sources_contents;
    source_root = map2.source_root }


let add_mapping ~original ~generated map =
  let names = match original.name with
  | Some name -> SSet.add name map.names
  | None -> map.names
  in
  let mapping = {
    original = Some original;
    generated_loc = generated;
  } in
  let mappings = mapping::(MappingHolder.addition_list map.mappings) in
  { map with mappings = Addition mappings; names }

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
  let source_indexes = index_map (sources_set map) in
  let name_indexes = index_map map.names in
  let buf = Buffer.create 127 in (* arbitrary length *)
  let (buf, _) = List.fold_left (fun (buf, state) mapping ->
    let state =
      let line_diff = mapping.generated_loc.line - state.prev_gen_line in
      let skip, state =
        if line_diff > 0 then begin
          add_string_repeat ~times:line_diff buf ";";
          false, { state with
            prev_gen_col = 0; prev_gen_line = mapping.generated_loc.line
          }
        end else begin
          let skip = match state.prev_mapping with
          | Some prev_mapping ->
            begin match prev_mapping.original, mapping.original with
            | Some { source = prev_source; original_loc = prev_loc; name = _ },
              Some { source; original_loc = loc; name = _ }
              when source = prev_source && loc = prev_loc ->
                true
            | _ ->
              Buffer.add_string buf ",";
              false
            end
          | None -> false
          in
          skip, state
        end
      in
      if skip then state else begin
        Vlq.Base64.encode buf (mapping.generated_loc.col - state.prev_gen_col);
        let state = { state with prev_gen_col = mapping.generated_loc.col } in

        let state = match mapping.original with
        | Some { source; original_loc; name } ->
          let source_idx = SMap.find source source_indexes in
          Vlq.Base64.encode buf (source_idx - state.prev_source);

          (* lines are stored 0-based in SourceMap spec version 3 *)
          Vlq.Base64.encode buf (original_loc.line - 1 - state.prev_orig_line);
          Vlq.Base64.encode buf (original_loc.col - state.prev_orig_col);

          let prev_name =
            match name with
            | Some name ->
              let name_idx = SMap.find name name_indexes in
              Vlq.Base64.encode buf (name_idx - state.prev_name);
              name_idx
            | None ->
              state.prev_name
          in

          { state with
            prev_source = source_idx;
            prev_orig_line = original_loc.line - 1;
            prev_orig_col = original_loc.col;
            prev_name;
          }
        | None ->
          state
        in

        state
      end
    in
    (buf, { state with prev_mapping = Some mapping })
  ) (buf, state) (List.rev (MappingHolder.addition_list map.mappings)) in
  Buffer.contents buf

let mappings_of_stream =
  let state = {
    prev_gen_col = 0;
    prev_gen_line = 1;
    prev_orig_col = 0;
    prev_orig_line = 1;
    prev_name = 0;
    prev_source = 0;
    prev_mapping = None;
  } in
  let next_is_separator stream =
    match Stream.peek stream with
    | None
    | Some ';'
    | Some ',' -> true
    | _ -> false
  in
  let rec helper acc state stream sources names =
    match Stream.peek stream with
    | None ->
      (* end of stream *)
      acc
    | Some ';' ->
      (* newline *)
      let state = { state with
        prev_gen_line = state.prev_gen_line + 1;
        prev_gen_col = 0;
      } in
      Stream.junk stream;
      helper acc state stream sources names
    | Some ',' ->
      (* separator between segments *)
      Stream.junk stream;
      helper acc state stream sources names
    | Some _ ->
      (* start of a segment, which consists of 1, 4 or 5 VLQs *)

      let offset = Vlq.Base64.decode stream in
      let generated_loc = {
        line = state.prev_gen_line;
        col = offset + state.prev_gen_col;
      } in
      let state = { state with prev_gen_col = generated_loc.col } in

      let state, original = if next_is_separator stream then state, None else
        let source_idx = Vlq.Base64.decode stream + state.prev_source in
        let source = List.nth sources source_idx in
        let line_offset = Vlq.Base64.decode stream in
        let col_offset = Vlq.Base64.decode stream in
        let original_loc = {
          line = line_offset + state.prev_orig_line;
          col = col_offset + state.prev_orig_col;
        } in
        let name_idx, name = if next_is_separator stream then state.prev_name, None else
          let idx = Vlq.Base64.decode stream + state.prev_name in
          idx, Some (List.nth names idx)
        in
        let state = { state with
          prev_orig_col = original_loc.col;
          prev_orig_line = original_loc.line;
          prev_name = name_idx;
          prev_source = source_idx;
        } in
        state, Some { name; source; original_loc }
      in
      let mapping = { generated_loc; original } in
      helper (mapping::acc) state stream sources names
  in
  fun ~sources ~names stream -> helper [] state stream sources names

let mappings_of_string ~sources ~names str =
  mappings_of_stream ~sources ~names (Stream.of_string str)

let sources_contents map =
  sources map
  |> List.map (fun source ->
    if SMap.mem source map.sources_contents
      then Some (SMap.find source map.sources_contents)
      else None
  )

let version _map = "3"
let names map = SSet.elements map.names
let source_root map = map.source_root
let file map = map.file

module type Json_writer_intf = sig
  type t
  val of_string: string -> t
  val of_obj: (string * t) list -> t
  val of_array: t list -> t
  val of_number: string -> t
  val null: t
end

module type Json_reader_intf = sig
  type t
  val to_string: t -> string
  val to_obj: t -> (string * t) list
  val to_array: t -> t list
  val to_number: t -> string
  val is_null: t -> bool
end

module type Json_writer = sig
  type json
  val json_of_sourcemap: t -> json
end

module type Json_reader = sig
  type json
  val sourcemap_of_json: json -> t
end

module Make_json_writer (Json : Json_writer_intf) : (Json_writer with type json = Json.t) = struct
  type json = Json.t

  let json_of_sources_contents map =
    sources_contents map
    |> List.map (function Some content -> Json.of_string content | None -> Json.null)
    |> Json.of_array

  let json_of_sourcemap map =
    (* build props in reverse, "version" *must* end up first! *)
    let rev_props = [
      "mappings", Json.of_string (string_of_mappings map);
      "names", Json.of_array (List.map Json.of_string (names map));
      "sources", Json.of_array (List.map Json.of_string (sources map));
      "version", Json.of_number (version map);
    ] in
    let rev_props = match map.file with
    | Some file -> ("file", Json.of_string file)::rev_props
    | None -> rev_props
    in
    let rev_props = match source_root map with
    | Some root -> ("sourceRoot", Json.of_string root)::rev_props
    | None -> rev_props
    in
    let rev_props = if SMap.is_empty map.sources_contents
      then rev_props
      else ("sourcesContent", json_of_sources_contents map)::rev_props
    in
    Json.of_obj (List.rev rev_props)
end

module Make_json_reader (Json : Json_reader_intf) : (Json_reader with type json = Json.t) = struct
  type json = Json.t

  let sources_list_of_json json =
    List.map Json.to_string (Json.to_array json)

  let names_list_of_json json =
    List.map Json.to_string (Json.to_array json)

  let mappings_of_json ~sources ~names json =
    mappings_of_string ~sources ~names (Json.to_string json)

  let sources_contents_of_json sources json =
    let contents =
      Json.to_array json
      |> List.map (fun x -> if Json.is_null x then None else Some (Json.to_string x))
    in
    List.fold_left2 (fun acc source content ->
      match content with
      | Some content -> SMap.add source content acc
      | None -> acc
    ) SMap.empty sources contents

  let find_opt key map = SMap.find_opt key map
  let opt f key map = match find_opt key map with Some x -> Some (f x) | None -> None

  let sourcemap_of_json json =
    let props = Json.to_obj json in
    let smap = List.fold_left (fun map (key, value) -> SMap.add key value map) SMap.empty props in
    let file = opt Json.to_string "file" smap in
    let source_root = opt Json.to_string "sourceRoot" smap in
    let sources = sources_list_of_json (SMap.find "sources" smap) in
    let names = names_list_of_json (SMap.find "names" smap) in
    let mappings = mappings_of_json ~sources ~names (SMap.find "mappings" smap) in
    let sources_contents = opt (sources_contents_of_json sources) "sourcesContent" smap in
    {
      file;
      source_root;
      names = SSet.of_list names;
      mappings = Addition mappings;
      sources_contents = match sources_contents with Some x -> x | None -> SMap.empty;
    }
end
