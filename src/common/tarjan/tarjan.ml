(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
    edges: NSet.t;
    (* visit order, -1 if unvisited *)
    mutable index: int;
    (* back edge to earliest visited node, -1 when unvisited *)
    mutable lowlink: int;
    mutable on_stack: bool;
  }

  (** Nodes are N.t. Edges are dependencies. **)
  type topsort_state = {
    graph: node NMap.t;
    (* number of nodes visited *)
    mutable visit_count: int;
    (* nodes in a strongly connected component *)
    mutable stack: node list;
    (* accumulated components *)
    mutable components: N.t Nel.t list;
  }

  let initial_state graph =
    let graph =
      NMap.mapi
        (fun value edges -> { value; edges; index = -1; lowlink = -1; on_stack = false })
        graph
    in
    { graph; visit_count = 0; stack = []; components = [] }

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
    v.edges
    |> NSet.iter (fun e ->
           let w = NMap.find e state.graph in
           if w.index = -1 then (
             strongconnect state w;
             v.lowlink <- min v.lowlink w.lowlink
           ) else if w.on_stack then
             v.lowlink <- min v.lowlink w.index
       );

    if v.lowlink = v.index then
      (* strongly connected component *)
      let c = component state v in
      state.components <- (v.value, c) :: state.components

  (* Return component strongly connected to v. *)
  and component state v =
    (* pop stack until m is found *)
    let w = List.hd state.stack in
    state.stack <- List.tl state.stack;
    w.on_stack <- false;
    if v.value = w.value then
      []
    else
      w.value :: component state v

  (** main loop **)
  let tarjan ~roots state =
    NSet.iter
      (fun x ->
        let v = NMap.find x state.graph in
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
