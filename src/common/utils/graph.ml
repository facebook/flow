(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Make (Set : Set.S) (Map : WrappedMap.S with type key = Set.elt) = struct
  type node = {
    forward: Set.t;
    backward: Set.t;
  }

  type t = node Map.t

  type elt = Map.key

  type set = Set.t

  type map = Set.t Map.t

  let empty_entry = { forward = Set.empty; backward = Set.empty }

  (* Updates the entries corresponding to `keys_to_update` by applying `f` to them. If the key is
   * not already present, an empty entry is constructed and passed to `f`. *)
  let update_entries f keys_to_update graph =
    Set.fold
      (fun key_to_update graph ->
        Map.update
          key_to_update
          (fun entry ->
            let entry =
              match entry with
              | None -> empty_entry
              | Some entry -> entry
            in
            Some (f entry))
          graph)
      keys_to_update
      graph

  (* Adds backward edges pointing from every key in `keys_to_update` to `key`. *)
  let add_backward_edges key keys_to_update graph =
    update_entries
      (fun entry -> { entry with backward = Set.add key entry.backward })
      keys_to_update
      graph

  (* Removes backwards edges pointing from every key in `keys_to_update` to `key`. *)
  let remove_backward_edges key keys_to_update graph =
    update_entries
      (fun entry -> { entry with backward = Set.remove key entry.backward })
      keys_to_update
      graph

  (* Removes forward edges pointing from every key in `keys_to_update` to `key`. *)
  let remove_forward_edges key keys_to_update graph =
    update_entries
      (fun entry -> { entry with forward = Set.remove key entry.forward })
      keys_to_update
      graph

  let of_map map =
    (* First, fill in the forward edges *)
    let graph = Map.map (fun forward -> { forward; backward = Set.empty }) map in
    (* Next, fill in the backward edges *)
    let graph = Map.fold add_backward_edges map graph in
    graph

  let update_from_map graph map ~to_remove:keys_to_remove =
    (* First, make changes as needed based on `map`. This includes updating dependency edges and
     * adding entirely new nodes. *)
    let graph =
      Map.fold
        (fun key forward_edges graph ->
          let previous_entry =
            match Map.find_opt key graph with
            | None -> empty_entry
            | Some entry -> entry
          in
          let previous_forward_edges = previous_entry.forward in
          let additional_forward_edges = Set.diff forward_edges previous_forward_edges in
          let removed_forward_edges = Set.diff previous_forward_edges forward_edges in
          (* For each new forward edge, add a backward edge from that node to this one *)
          let graph = add_backward_edges key additional_forward_edges graph in
          (* For each removed forward edge, remove its corresponding backward edge from that node to this one. *)
          let graph = remove_backward_edges key removed_forward_edges graph in
          let graph = Map.add key { previous_entry with forward = forward_edges } graph in
          graph)
        map
        graph
    in
    (* Now, remove nodes as needed based on `keys_to_remove`. This requires fixing up both
     * forward edges and backward edges which point to the entries to remove, as well as removing
     * the entries themselves. *)
    let graph =
      Set.fold
        (fun key_to_remove graph ->
          (* In practice we sometimes get asked to remove nodes that aren't present to begin
           * with. That's a bit weird, but let's just tolerate that by doing nothing. *)
          match Map.find_opt key_to_remove graph with
          | None -> graph
          | Some node ->
            (* Remove forward dependency edges that refer to this key *)
            let graph = remove_forward_edges key_to_remove node.backward graph in
            (* Remove backward dependency edges that refer to this key *)
            let graph = remove_backward_edges key_to_remove node.forward graph in
            (* Remove this key's entry. We can do this as part of this step because we've already
             * removed forward and backward edges pointing towards this node, so we won't try to
             * look it up when removing a future node. *)
            let graph = Map.remove key_to_remove graph in
            graph)
        keys_to_remove
        graph
    in
    graph

  let to_map graph = Map.map (fun { forward; _ } -> forward) graph

  let to_backward_map graph = Map.map (fun { backward; _ } -> backward) graph

  let find elt graph = (Map.find elt graph).forward

  let find_opt elt graph =
    match Map.find_opt elt graph with
    | None -> None
    | Some { forward; _ } -> Some forward

  let find_backward elt graph = (Map.find elt graph).backward

  let find_backward_opt elt graph =
    match Map.find_opt elt graph with
    | None -> None
    | Some { backward; _ } -> Some backward

  let fold f graph init = Map.fold (fun elt { forward; _ } acc -> f elt forward acc) graph init
end
