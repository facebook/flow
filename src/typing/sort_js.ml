(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

(*****************************************)
(* topological sort of file dependencies *)
(*****************************************)

(* For a detailed description of the algorithm, see:
   http://en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm

   The code below is mostly a transcription of the above. In addition to
   computing strongly connected components, we also compute their "heights": the
   height of a strongly connected component is the length of the longest path of
   strongly connected components from it in the residual graph. *)

(** Nodes are files. Edges are dependencies. **)
type topsort_state = {
  (* nodes not yet visited *)
  mutable not_yet_visited: FilenameSet.t FilenameMap.t;
  (* number of nodes visited *)
  mutable visit_count: int;
  (* visit ordering *)
  mutable indices: int FilenameMap.t;
  (* nodes in a strongly connected component *)
  mutable stack: filename list;
  (* back edges to earliest visited nodes *)
  mutable lowlinks: int FilenameMap.t;
  (* heights *)
  mutable heights: int FilenameMap.t;
  (* components *)
  mutable components: filename list FilenameMap.t;
}

let initial_state files = {
  not_yet_visited = files;
  visit_count = 0;
  indices = FilenameMap.empty;
  stack = [];
  lowlinks = FilenameMap.empty;
  heights = FilenameMap.empty;
  components = FilenameMap.empty;
}

(* Compute strongly connected component for file m with requires rs. *)
let rec strongconnect state m rs =
  let i = state.visit_count in
  state.visit_count <- i + 1;

  (* visit m *)
  state.indices <- FilenameMap.add m i state.indices;
  state.not_yet_visited <- FilenameMap.remove m state.not_yet_visited;

  (* push on stack *)
  state.stack <- m :: state.stack;

  (* initialize lowlink *)
  let lowlink = ref i in

  (* initialize height *)
  let height = ref 0 in

  (* for each require r in rs: *)
  rs |> FilenameSet.iter (fun r ->
    match FilenameMap.get r state.not_yet_visited with
    | Some rs_ ->
        (* recursively compute strongly connected component of r *)
        let h = strongconnect state r rs_ in

        (* update height with that of r *)
        height := max !height h;

        (* update lowlink with that of r *)
        let lowlink_r = FilenameMap.find_unsafe r state.lowlinks in
        lowlink := min !lowlink lowlink_r

    | None ->
        if (List.mem r state.stack) then
          (** either back edge, or cross edge where strongly connected component
              is not yet complete **)
          (* update lowlink with index of r *)
          let index_r = FilenameMap.find_unsafe r state.indices in
          lowlink := min !lowlink index_r
        else
          match FilenameMap.get r state.heights with
          | Some h ->
              (** cross edge where strongly connected component is complete **)
              (* update height *)
              height := max !height h
          | None -> ()
  );

  state.lowlinks <- FilenameMap.add m !lowlink state.lowlinks;
  if (!lowlink = i) then (
    (* strongly connected component *)
    let h = !height + 1 in
    let c = component state m h in
    state.components <- FilenameMap.add m c state.components;
    h
  )
  else !height

(* Return component strongly connected to m. *)
and component state m h =
  (* pop stack until m is found *)
  let m_ = List.hd state.stack in
  state.stack <- List.tl state.stack;
  state.heights <- state.heights |> FilenameMap.add m_ h;
  if (m = m_) then []
  else m_ :: (component state m h)

(** main loop **)
let tarjan state =
  while not (FilenameMap.is_empty state.not_yet_visited) do
    (* choose a node, compute its strongly connected component *)
    (** NOTE: this choice is non-deterministic, so any computations that depend
        on the visit order, such as heights, are in general non-repeatable. **)
    let m, rs =
       match FilenameMap.choose state.not_yet_visited with
       | Some (m, rs) -> m, rs
       | None -> failwith "choose should always work on a non empty file map" in
    strongconnect state m rs |> ignore
  done

(* Order nodes by their computed heights. It is guaranteed that height ordering
   implies topological sort ordering; in particular, nodes at a particular
   height are guaranteed to only depend on nodes at lower heights, so we can
   partition nodes by their heights, and run a series of parallel jobs, one for
   each partition, by height. *)
let partition heights components =
  FilenameMap.fold (fun m c map ->
    let mc = m::c in
    let height = FilenameMap.find_unsafe m heights in
    match IMap.get height map with
    | None -> IMap.add height [mc] map
    | Some mcs -> IMap.add height (mc::mcs) map
  ) components IMap.empty

let topsort files =
  let state = initial_state files in
  tarjan state;
  partition state.heights state.components

let reverse files =
  files
  |> FilenameMap.map (fun _ -> FilenameSet.empty)
  |> FilenameMap.fold (fun from_f ->
       FilenameSet.fold (fun to_f rev_files ->
         let from_fs = FilenameMap.find_unsafe to_f rev_files in
         FilenameMap.add to_f (FilenameSet.add from_f from_fs) rev_files
       )
      ) files

let log =
  IMap.iter (fun _ mcs ->
    List.iter (fun mc ->
      (* Show cycles, which are components with more than one node. *)
      if List.length mc > 1
      then
        let files = mc
        |> List.map string_of_filename
        |> String.concat "\n\t"
        in
        prerr_endlinef "cycle detected among the following files:\n\t%s" files
    ) mcs;
  )
