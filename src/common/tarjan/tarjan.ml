(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* For a detailed description of the algorithm, see:
   http://en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm

   The code below is mostly a transcription of the above. *)

module type NODE = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

module Make
    (N : NODE)
    (NMap : WrappedMap.S with type key = N.t)
    (NSet : Flow_set.S with type elt = N.t) =
struct
  type node = {
    value: N.t;
    (* visit order, -1 if unvisited *)
    mutable index: int;
    (* back edge to earliest visited node, -1 when unvisited *)
    mutable lowlink: int;
    mutable on_stack: bool;
  }

  (** Nodes are N.t. Edges are dependencies. **)
  type topsort_state = {
    graph: NSet.t NMap.t;
    (* nodes, created on demand *)
    nodes: (N.t, node) Hashtbl.t;
    (* number of nodes visited *)
    mutable visit_count: int;
    (* nodes in a strongly connected component *)
    mutable stack: node list;
    (* accumulated components *)
    mutable components: N.t Nel.t list;
  }

  let initial_state graph =
    let nodes = Hashtbl.create 0 in
    { graph; nodes; visit_count = 0; stack = []; components = [] }

  let find_or_create_node state value =
    match Hashtbl.find_opt state.nodes value with
    | Some n -> n
    | None ->
      let n = { value; index = -1; lowlink = -1; on_stack = false } in
      Hashtbl.add state.nodes value n;
      n

  (* Return component strongly connected to v. *)
  let rec collect_scc state v acc = function
    | [] -> failwith "unexpected empty stack"
    | w :: stack ->
      w.on_stack <- false;
      if w == v then (
        state.stack <- stack;
        state.components <- (w.value, List.rev acc) :: state.components
      ) else
        collect_scc state v (w.value :: acc) stack

  (* Compute strongly connected component for node m with requires rs. *)
  let rec strongconnect state v =
    let i = state.visit_count in
    state.visit_count <- i + 1;

    (* visit node *)
    assert (v.index = -1);
    v.index <- i;
    v.lowlink <- i;

    (* push on stack *)
    state.stack <- v :: state.stack;
    v.on_stack <- true;

    (* for each edge e:
       If the edge has not yet been visited, recurse in a depth-first manner.
       If the edge has been visited, it is a back-edge iff it is on the stack,
       otherwise it's a cross-edge and can be ignored. *)
    state.graph
    |> NMap.find v.value
    |> NSet.iter (fun e ->
           let w = find_or_create_node state e in
           if w.index = -1 then (
             strongconnect state w;
             v.lowlink <- min v.lowlink w.lowlink
           ) else if w.on_stack then
             v.lowlink <- min v.lowlink w.index
       );

    if v.lowlink = v.index then
      (* strongly connected component *)
      collect_scc state v [] state.stack

  (** main loop **)
  let tarjan ~roots state =
    NSet.iter
      (fun x ->
        let v = find_or_create_node state x in
        if v.index = -1 then strongconnect state v)
      roots

  let topsort ~roots graph =
    let state = initial_state graph in
    tarjan ~roots state;
    state.components

  let log =
    List.iter (fun mc ->
        (* Show cycles, which are components with more than one node. *)
        if Nel.length mc > 1 then
          let nodes = mc |> Nel.to_list |> Base.List.map ~f:N.to_string |> String.concat "\n\t" in
          Printf.ksprintf prerr_endline "cycle detected among the following nodes:\n\t%s" nodes
    )
end
