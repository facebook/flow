(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* For a detailed description of the algorithm, see:
   http://en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm

   The code below is mostly a transcription of the above. In addition to
   computing strongly connected components, we also compute their "heights": the
   height of a strongly connected component is the length of the longest path of
   strongly connected components from it in the residual graph. *)

module type NODE = sig
  type t
  val compare: t -> t -> int
  val to_string: t -> string
end

module Make
  (N: NODE)
  (NMap: MyMap.S with type key = N.t)
  (NSet: Set.S with type elt = N.t) = struct

  (** Nodes are N.t. Edges are dependencies. **)
  type topsort_state = {
    (* nodes not yet visited *)
    mutable not_yet_visited: NSet.t NMap.t;
    (* number of nodes visited *)
    mutable visit_count: int;
    (* visit ordering *)
    mutable indices: int NMap.t;
    (* nodes in a strongly connected component *)
    mutable stack: N.t list;
    (* back edges to earliest visited nodes *)
    mutable lowlinks: int NMap.t;
    (* heights *)
    mutable heights: int NMap.t;
    (* components *)
    mutable components: N.t list NMap.t;
  }

  let initial_state nodes = {
    not_yet_visited = nodes;
    visit_count = 0;
    indices = NMap.empty;
    stack = [];
    lowlinks = NMap.empty;
    heights = NMap.empty;
    components = NMap.empty;
  }

  (* Compute strongly connected component for node m with requires rs. *)
  let rec strongconnect state m rs =
    let i = state.visit_count in
    state.visit_count <- i + 1;

    (* visit m *)
    state.indices <- NMap.add m i state.indices;
    state.not_yet_visited <- NMap.remove m state.not_yet_visited;

    (* push on stack *)
    state.stack <- m :: state.stack;

    (* initialize lowlink *)
    let lowlink = ref i in

    (* initialize height *)
    let height = ref 0 in

    (* for each require r in rs: *)
    rs |> NSet.iter (fun r ->
      match NMap.get r state.not_yet_visited with
      | Some rs_ ->
          (* recursively compute strongly connected component of r *)
          let h = strongconnect state r rs_ in

          (* update height with that of r *)
          height := max !height h;

          (* update lowlink with that of r *)
          let lowlink_r = NMap.find_unsafe r state.lowlinks in
          lowlink := min !lowlink lowlink_r

      | None ->
          if (List.mem r state.stack) then
            (** either back edge, or cross edge where strongly connected component
                is not yet complete **)
            (* update lowlink with index of r *)
            let index_r = NMap.find_unsafe r state.indices in
            lowlink := min !lowlink index_r
          else
            match NMap.get r state.heights with
            | Some h ->
                (** cross edge where strongly connected component is complete **)
                (* update height *)
                height := max !height h
            | None -> ()
    );

    state.lowlinks <- NMap.add m !lowlink state.lowlinks;
    if (!lowlink = i) then (
      (* strongly connected component *)
      let h = !height + 1 in
      let c = component state m h in
      state.components <- NMap.add m c state.components;
      h
    )
    else !height

  (* Return component strongly connected to m. *)
  and component state m h =
    (* pop stack until m is found *)
    let m_ = List.hd state.stack in
    state.stack <- List.tl state.stack;
    state.heights <- state.heights |> NMap.add m_ h;
    if (m = m_) then []
    else m_ :: (component state m h)

  (** main loop **)
  let tarjan state =
    while not (NMap.is_empty state.not_yet_visited) do
      (* choose a node, compute its strongly connected component *)
      (** NOTE: this choice is non-deterministic, so any computations that depend
          on the visit order, such as heights, are in general non-repeatable. **)
      let m, rs =
         match NMap.choose state.not_yet_visited with
         | Some (m, rs) -> m, rs
         | None -> failwith "choose should always work on a non empty node map" in
      strongconnect state m rs |> ignore
    done

  (* Order nodes by their computed heights. It is guaranteed that height ordering
     implies topological sort ordering; in particular, nodes at a particular
     height are guaranteed to only depend on nodes at lower heights, so we can
     partition nodes by their heights, and run a series of parallel jobs, one for
     each partition, by height. *)
  let partition heights components =
    NMap.fold (fun m c map ->
      let mc = m::c in
      let height = NMap.find_unsafe m heights in
      match IMap.get height map with
      | None -> IMap.add height [mc] map
      | Some mcs -> IMap.add height (mc::mcs) map
    ) components IMap.empty

  let topsort nodes =
    let state = initial_state nodes in
    tarjan state;
    partition state.heights state.components

  let reverse nodes =
    nodes
    |> NMap.map (fun _ -> NSet.empty)
    |> NMap.fold (fun from_f ->
         NSet.fold (fun to_f rev_nodes ->
           let from_fs = NMap.find_unsafe to_f rev_nodes in
           NMap.add to_f (NSet.add from_f from_fs) rev_nodes
         )
        ) nodes

  let log =
    IMap.iter (fun _ mcs ->
      List.iter (fun mc ->
        (* Show cycles, which are components with more than one node. *)
        if List.length mc > 1
        then
          let nodes = mc
          |> List.map N.to_string
          |> String.concat "\n\t"
          in
          Printf.ksprintf prerr_endline
            "cycle detected among the following nodes:\n\t%s" nodes
      ) mcs;
    )

  let component_map partition =
    IMap.fold (fun _ components acc ->
      List.fold_left (fun acc component ->
        NMap.add (List.hd component) component acc
      ) acc components
    ) partition NMap.empty
end
