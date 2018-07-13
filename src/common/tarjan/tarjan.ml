(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* For a detailed description of the algorithm, see:
   http://en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm

   The code below is mostly a transcription of the above. *)

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
    graph: NSet.t NMap.t;
    (* nodes not yet visited *)
    mutable not_yet_visited: NSet.t;
    (* number of nodes visited *)
    mutable visit_count: int;
    (* visit ordering *)
    indices: (N.t, int) Hashtbl.t;
    (* nodes in a strongly connected component *)
    mutable stack: N.t list;
    mem_stack: (N.t, bool) Hashtbl.t;
    (* back edges to earliest visited nodes *)
    lowlinks: (N.t, int) Hashtbl.t;
    (* components *)
    mutable components: N.t Nel.t list;
  }

  let initial_state ~roots graph = {
    graph;
    not_yet_visited = roots;
    visit_count = 0;
    indices = Hashtbl.create 0;
    stack = [];
    mem_stack = Hashtbl.create 0;
    lowlinks = Hashtbl.create 0;
    components = [];
  }

  (* Compute strongly connected component for node m with requires rs. *)
  let rec strongconnect state m rs =
    let i = state.visit_count in
    state.visit_count <- i + 1;

    (* visit m *)
    Hashtbl.replace state.indices m i;
    state.not_yet_visited <- NSet.remove m state.not_yet_visited;

    (* push on stack *)
    state.stack <- m :: state.stack;
    Hashtbl.replace state.mem_stack m true;

    (* initialize lowlink *)
    let lowlink = ref i in

    (* for each require r in rs: *)
    rs |> NSet.iter (fun r ->
      if Hashtbl.mem state.indices r
      then begin
        if (Hashtbl.find state.mem_stack r) then
          (** either back edge, or cross edge where strongly connected component
              is not yet complete **)
          (* update lowlink with index of r *)
          let index_r = Hashtbl.find state.indices r in
          lowlink := min !lowlink index_r
      end else match NMap.get r state.graph with
        | Some rs_ ->
          (* recursively compute strongly connected component of r *)
          strongconnect state r rs_;

          (* update lowlink with that of r *)
          let lowlink_r = Hashtbl.find state.lowlinks r in
          lowlink := min !lowlink lowlink_r

        | None -> ()
    );

    Hashtbl.replace state.lowlinks m !lowlink;
    if (!lowlink = i) then
      (* strongly connected component *)
      let c = component state m in
      state.components <- (m, c) :: state.components

  (* Return component strongly connected to m. *)
  and component state m =
    (* pop stack until m is found *)
    let m_ = List.hd state.stack in
    state.stack <- List.tl state.stack;
    Hashtbl.replace state.mem_stack m_ false;
    if (m = m_) then []
    else m_ :: (component state m)

  (** main loop **)
  let tarjan state =
    while not (NSet.is_empty state.not_yet_visited) do
      (* choose a node, compute its strongly connected component *)
      (** NOTE: this choice is non-deterministic, so any computations that depend
          on the visit order, such as heights, are in general non-repeatable. **)
      let m = NSet.choose state.not_yet_visited in
      let rs = NMap.find_unsafe m state.graph in
      strongconnect state m rs
    done

  let topsort ~roots graph =
    let state = initial_state ~roots graph in
    tarjan state;
    state.components

  let log =
    List.iter (fun mc ->
      (* Show cycles, which are components with more than one node. *)
      if Nel.length mc > 1
      then
        let nodes = mc
        |> Nel.to_list
        |> List.map N.to_string
        |> String.concat "\n\t"
        in
        Printf.ksprintf prerr_endline
          "cycle detected among the following nodes:\n\t%s" nodes
    )

  let reverse nodes =
    nodes
    |> NMap.map (fun _ -> NSet.empty)
    |> NMap.fold (fun from_f ->
         NSet.fold (fun to_f rev_nodes ->
           let from_fs = NMap.find_unsafe to_f rev_nodes in
           NMap.add to_f (NSet.add from_f from_fs) rev_nodes
         )
        ) nodes

end
