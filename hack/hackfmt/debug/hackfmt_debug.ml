(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module SyntaxTree = Full_fidelity_syntax_tree
module SourceText = Full_fidelity_source_text

open Core

type debug_config = {
  print_ast: bool;
  print_nesting_graph: bool;
  chunk_ids: int list option;
}

let debug_config = ref {
  print_ast = false;
  print_nesting_graph = false;
  chunk_ids = None;
}

let init_with_options () = [
  "--ast",
  Arg.Unit (fun () -> debug_config := { !debug_config with print_ast = true }),
  " Print out an ast dump before the formatted result ";
  "--ids",
  Arg.String (fun s ->
    debug_config := { !debug_config with chunk_ids = Some (
      try List.map (Str.split (Str.regexp ",") s) ~f:int_of_string
      with Failure _ -> raise (Failure "Invalid id list specification")
    )}
  ),
  " Comma separate list of chunk ids to inspect (default is all)";
  "--nesting",
  Arg.Unit (fun () ->
    debug_config := { !debug_config with print_nesting_graph = true }),
  " Print out a reprsentation of the nesting graph below each chung group";
]

let debug_nesting chunk_group =
  List.iteri chunk_group.Chunk_group.chunks ~f:(fun i c ->
    let nesting_list =
      Nesting.get_self_and_parent_list (Some c.Chunk.nesting) in
    let list_string = String.concat ", "
      @@ List.map nesting_list ~f:string_of_int in
    Printf.printf "Chunk %d - [ %s ]\n" i list_string;
  )

let debug_chunk_groups chunk_groups =
  let get_range cg =
    let chunks = cg.Chunk_group.chunks in
    let a, b, c = Chunk_group.(match cg.print_range with
      | No -> "No", -1, -1
      | All -> "All", 0, List.length chunks
      | Range (s, e) ->  "Range", s, e
      | StartAt s -> "StartAt", s, List.length chunks
      | EndAt e -> "EndAt", 0, e
    ) in
    Printf.sprintf "%s %d %d" a b c
  in

  let print_chunk = match !debug_config.chunk_ids with
    | None -> (fun id c -> Some (id, c))
    | Some id_list -> (fun id c ->
        if List.exists id_list (fun x -> x = id) then Some (id, c) else None
      )
  in

  let chunk_groups = List.filter_mapi chunk_groups ~f:print_chunk in
  List.iter chunk_groups ~f:(fun (i, cg) ->
    Printf.printf "Group Id: %d\n" i;
    Printf.printf "Indentation: %d\n" cg.Chunk_group.block_indentation;
    Printf.printf "Chunk count: %d\n" (List.length cg.Chunk_group.chunks);
    Printf.printf "%s\n" @@ get_range cg;
    List.iteri cg.Chunk_group.chunks ~f:(fun i c ->
      Printf.printf "\t%d - %s - Nesting:%d Pending:%d\n"
        i (Chunk.to_string c) (Chunk.get_nesting_id c)
        (Option.value ~default:(-1) c.Chunk.comma_rule)
    );
    Printf.printf "Rule count %d\n"
      (IMap.cardinal cg.Chunk_group.rule_map);
    IMap.iter (fun k v ->
      Printf.printf "\t%d - %s\n" k (Rule.to_string v);
    ) cg.Chunk_group.rule_map;

    if !debug_config.print_nesting_graph then debug_nesting cg;

    Printf.printf "%s" @@ Line_splitter.solve [cg];
  );
  ()

let debug_full_text source_text =
  Printf.printf "%s\n" (SourceText.get_text source_text)

let debug_ast syntax_tree =
  Printf.printf "%s\n" @@ Debug.dump_full_fidelity syntax_tree

let debug_text_range source_text start_char end_char =
  Printf.printf "Subrange passed:\n%s\n" @@
    String.sub source_text.SourceText.text start_char (end_char - start_char)

let debug _source_text syntax_tree chunk_groups =
  if !debug_config.print_ast then debug_ast syntax_tree;
  debug_chunk_groups chunk_groups
