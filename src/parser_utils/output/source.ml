(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module LocMap = Loc_collections.LocMap

type t = {
  buffer: Buffer.t;
  sourcemap: Sourcemap.t option;
  pos: Sourcemap.line_col;
  loc_stack: Loc.t list;
  names: Source_map_config.names option;
}

let create ~source_maps () =
  let (sourcemap, names) =
    Option.value_map source_maps ~default:(None, None) ~f:(fun { Source_map_config.names } ->
        (Some (Sourcemap.create ()), Some names))
  in
  {
    buffer = Buffer.create 127;
    (* no idea the best value for this *)
    sourcemap;
    names;
    pos = { Sourcemap.line = 1; col = 0 };
    loc_stack = [];
  }

let push_loc loc source = { source with loc_stack = loc :: source.loc_stack }

let pop_loc source =
  (* raises if you call pop more than push *)
  let loc_stack = List.tl source.loc_stack in
  { source with loc_stack }

let pos_new_line pos = Sourcemap.{ line = pos.line + 1; col = 0 }

let pos_add_string =
  let rec count n i str =
    try
      let i' = String.index_from str i '\n' in
      count (n + 1) (i' + 1) str
    with Not_found -> (n, String.length str - i)
  in
  fun { Sourcemap.line; col } str ->
    let (newlines, remaining_cols) = count 0 0 str in
    let line = line + newlines in
    let col =
      if newlines > 0 then
        remaining_cols
      else
        col + remaining_cols
    in
    { Sourcemap.line; col }

let add_string ?name str src =
  Buffer.add_string src.buffer str;
  let sourcemap =
    Option.map src.sourcemap ~f:(fun sourcemap ->
        match src.loc_stack with
        | [] -> sourcemap
        | loc :: _ ->
          let source =
            match Loc.source loc with
            | Some filename -> File_key.to_string filename
            | None -> "<stdin>"
          in
          let original =
            Sourcemap.
              {
                name;
                source;
                original_loc = { line = loc.Loc.start.Loc.line; col = loc.Loc.start.Loc.column };
              }
          in
          Sourcemap.add_mapping ~original ~generated:src.pos sourcemap)
  in
  let pos = pos_add_string src.pos str in
  { src with sourcemap; pos }

let add_identifier loc str src =
  (* If no name is found or its the same as the original name don't set it *)
  let default = None in
  let name =
    Option.value_map src.names ~default ~f:(fun names ->
        Option.value_map (LocMap.get loc names) ~default ~f:(fun name ->
            if name = str then
              None
            else
              Some name))
  in
  src |> push_loc loc |> add_string ?name str |> pop_loc

(* TODO: Remove any right trailing whitespace *)
let add_newline source =
  Buffer.add_string source.buffer "\n";
  { source with pos = pos_new_line source.pos }

let add_space num b = add_string (String.make num ' ') b

let contents b = Buffer.contents b.buffer

let sourcemap b = b.sourcemap
