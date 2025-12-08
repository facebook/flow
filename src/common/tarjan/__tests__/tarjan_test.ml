(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

module StringNode = struct
  type t = string

  let compare = String.compare

  let to_string x = x
end

module StringMap = struct
  type key = string

  type value = SSet.t

  type t = SSet.t SMap.t

  let find k m = SMap.find k m
end

module StringTarjan = Tarjan.Make (StringNode) (SSet) (StringMap)

(* Helper: convert list of edges to a graph map *)
let graph_of_list edges =
  List.fold_left (fun acc (k, vs) -> SMap.add k (SSet.of_list vs) acc) SMap.empty edges

(* Helper: create a set from a list *)
let roots_of_list lst = SSet.of_list lst

(* Helper: normalize an SCC (sort elements within each component) *)
let normalize_scc (hd, tl) =
  let all = hd :: tl in
  let sorted = List.sort String.compare all in
  match sorted with
  | [] -> failwith "impossible: empty SCC"
  | x :: xs -> (x, xs)

(* Helper: compare two SCCs for equality (order within component doesn't matter) *)
let scc_equal a b =
  let a_norm = normalize_scc a in
  let b_norm = normalize_scc b in
  a_norm = b_norm

(* Helper: compare two lists of SCCs *)
let sccs_equal expected actual =
  List.length expected = List.length actual && List.for_all2 scc_equal expected actual

(* Helper: pretty print an SCC *)
let string_of_scc (hd, tl) =
  let all = hd :: tl in
  Printf.sprintf "[%s]" (String.concat ", " all)

(* Helper: pretty print a list of SCCs *)
let string_of_sccs sccs = Printf.sprintf "[%s]" (String.concat "; " (List.map string_of_scc sccs))

(* Helper: assert SCCs equal with nice error messages *)
let assert_sccs_equal ~ctxt expected actual =
  assert_equal ~ctxt ~cmp:sccs_equal ~printer:string_of_sccs expected actual

let tests =
  "tarjan"
  >::: [
         (* Basic Cases *)
         ( "empty_roots" >:: fun ctxt ->
           let graph = graph_of_list [] in
           let roots = roots_of_list [] in
           let result = StringTarjan.topsort ~roots graph in
           assert_sccs_equal ~ctxt [] result
         );
         ( "single_node_no_edges" >:: fun ctxt ->
           let graph = graph_of_list [("a", [])] in
           let roots = roots_of_list ["a"] in
           let result = StringTarjan.topsort ~roots graph in
           assert_sccs_equal ~ctxt [("a", [])] result
         );
         ( "single_node_self_loop" >:: fun ctxt ->
           let graph = graph_of_list [("a", ["a"])] in
           let roots = roots_of_list ["a"] in
           let result = StringTarjan.topsort ~roots graph in
           assert_sccs_equal ~ctxt [("a", [])] result
         );
         ( "two_nodes_no_edges" >:: fun ctxt ->
           let graph = graph_of_list [("a", []); ("b", [])] in
           let roots = roots_of_list ["a"; "b"] in
           let result = StringTarjan.topsort ~roots graph in
           (* Order may vary, so check both possibilities *)
           let result_normalized = List.map normalize_scc result |> List.sort compare in
           let expected = [("a", []); ("b", [])] |> List.map normalize_scc |> List.sort compare in
           assert_equal ~ctxt expected result_normalized
         );
         ( "two_nodes_one_edge" >:: fun ctxt ->
           let graph = graph_of_list [("a", ["b"]); ("b", [])] in
           let roots = roots_of_list ["a"] in
           let result = StringTarjan.topsort ~roots graph in
           (* Dependent first order: [A], [B] *)
           assert_sccs_equal ~ctxt [("a", []); ("b", [])] result
         );
         ( "two_nodes_cycle" >:: fun ctxt ->
           let graph = graph_of_list [("a", ["b"]); ("b", ["a"])] in
           let roots = roots_of_list ["a"] in
           let result = StringTarjan.topsort ~roots graph in
           (* Single SCC containing both nodes *)
           assert_equal ~ctxt 1 (List.length result);
           let (hd, tl) = List.hd result in
           let all_nodes = hd :: tl |> List.sort String.compare in
           assert_equal ~ctxt ["a"; "b"] all_nodes
         );
         (* Chain/Linear Cases *)
         ( "linear_chain_3_nodes" >:: fun ctxt ->
           let graph = graph_of_list [("a", ["b"]); ("b", ["c"]); ("c", [])] in
           let roots = roots_of_list ["a"] in
           let result = StringTarjan.topsort ~roots graph in
           (* Dependent first order: A, B, C *)
           assert_sccs_equal ~ctxt [("a", []); ("b", []); ("c", [])] result
         );
         ( "linear_chain_5_nodes" >:: fun ctxt ->
           let graph =
             graph_of_list [("a", ["b"]); ("b", ["c"]); ("c", ["d"]); ("d", ["e"]); ("e", [])]
           in
           let roots = roots_of_list ["a"] in
           let result = StringTarjan.topsort ~roots graph in
           (* Dependent first order: A, B, C, D, E *)
           assert_sccs_equal ~ctxt [("a", []); ("b", []); ("c", []); ("d", []); ("e", [])] result
         );
         (* Cycle Cases *)
         ( "triangle_cycle" >:: fun ctxt ->
           let graph = graph_of_list [("a", ["b"]); ("b", ["c"]); ("c", ["a"])] in
           let roots = roots_of_list ["a"] in
           let result = StringTarjan.topsort ~roots graph in
           (* Single SCC containing all three nodes *)
           assert_equal ~ctxt 1 (List.length result);
           let (hd, tl) = List.hd result in
           let all_nodes = hd :: tl |> List.sort String.compare in
           assert_equal ~ctxt ["a"; "b"; "c"] all_nodes
         );
         ( "cycle_with_tail" >:: fun ctxt ->
           let graph = graph_of_list [("a", ["b"]); ("b", ["c"]); ("c", ["b"])] in
           let roots = roots_of_list ["a"] in
           let result = StringTarjan.topsort ~roots graph in
           (* Two SCCs: {A} and {B, C} *)
           assert_equal ~ctxt 2 (List.length result);
           (* First SCC should be {A} *)
           let (hd1, tl1) = List.nth result 0 in
           assert_equal ~ctxt "a" hd1;
           assert_equal ~ctxt [] tl1;
           (* Second SCC should be {B, C} *)
           let (hd2, tl2) = List.nth result 1 in
           let scc2 = hd2 :: tl2 |> List.sort String.compare in
           assert_equal ~ctxt ["b"; "c"] scc2
         );
         ( "two_separate_cycles" >:: fun ctxt ->
           let graph = graph_of_list [("a", ["b"]); ("b", ["a"]); ("c", ["d"]); ("d", ["c"])] in
           let roots = roots_of_list ["a"; "c"] in
           let result = StringTarjan.topsort ~roots graph in
           (* Two SCCs: {A, B} and {C, D} *)
           assert_equal ~ctxt 2 (List.length result);
           (* Check both SCCs, order may vary *)
           let sccs =
             result
             |> List.map (fun (hd, tl) -> hd :: tl |> List.sort String.compare)
             |> List.sort compare
           in
           assert_equal ~ctxt [["a"; "b"]; ["c"; "d"]] sccs
         );
         (* Complex Graph Patterns *)
         ( "diamond_pattern" >:: fun ctxt ->
           let graph = graph_of_list [("a", ["b"; "c"]); ("b", ["d"]); ("c", ["d"]); ("d", [])] in
           let roots = roots_of_list ["a"] in
           let result = StringTarjan.topsort ~roots graph in
           (* Four SCCs: A, then B and C (order may vary), then D *)
           assert_equal ~ctxt 4 (List.length result);
           (* A should be first (no dependents) *)
           let (hd0, tl0) = List.nth result 0 in
           assert_equal ~ctxt "a" hd0;
           assert_equal ~ctxt [] tl0;
           (* D should be last (no dependencies) *)
           let (hd3, tl3) = List.nth result 3 in
           assert_equal ~ctxt "d" hd3;
           assert_equal ~ctxt [] tl3
         );
         ( "figure_8" >:: fun ctxt ->
           (* A→B→C, B→A (first cycle), C→D→E, D→C (second cycle) *)
           let graph =
             graph_of_list [("a", ["b"]); ("b", ["a"; "c"]); ("c", ["d"]); ("d", ["c"])]
           in
           let roots = roots_of_list ["a"] in
           let result = StringTarjan.topsort ~roots graph in
           (* Two SCCs: {A, B} and {C, D} *)
           assert_equal ~ctxt 2 (List.length result);
           let sccs = result |> List.map (fun (hd, tl) -> hd :: tl |> List.sort String.compare) in
           (* {A, B} should come before {C, D} because {A,B} depends on {C,D} *)
           assert_equal ~ctxt ["a"; "b"] (List.nth sccs 0);
           assert_equal ~ctxt ["c"; "d"] (List.nth sccs 1)
         );
         ( "nested_cycles" >:: fun ctxt ->
           (* Outer: A→B→C→A, Inner: B→D→B *)
           let graph =
             graph_of_list [("a", ["b"]); ("b", ["c"; "d"]); ("c", ["a"]); ("d", ["b"])]
           in
           let roots = roots_of_list ["a"] in
           let result = StringTarjan.topsort ~roots graph in
           (* All four nodes in one SCC *)
           assert_equal ~ctxt 1 (List.length result);
           let (hd, tl) = List.hd result in
           let all_nodes = hd :: tl |> List.sort String.compare in
           assert_equal ~ctxt ["a"; "b"; "c"; "d"] all_nodes
         );
         ( "multiple_sccs_varying_sizes" >:: fun ctxt ->
           (* {A} alone, {B, C} cycle, {D, E, F} cycle *)
           let graph =
             graph_of_list
               [
                 ("a", ["b"]);
                 ("b", ["c"]);
                 ("c", ["b"; "d"]);
                 ("d", ["e"]);
                 ("e", ["f"]);
                 ("f", ["d"]);
               ]
           in
           let roots = roots_of_list ["a"] in
           let result = StringTarjan.topsort ~roots graph in
           (* Three SCCs *)
           assert_equal ~ctxt 3 (List.length result);
           let sccs = result |> List.map (fun (hd, tl) -> hd :: tl |> List.sort String.compare) in
           (* Check SCC sizes *)
           let scc_sizes = List.map List.length sccs |> List.sort compare in
           assert_equal ~ctxt [1; 2; 3] scc_sizes
         );
         ( "large_scc" >:: fun ctxt ->
           (* Six nodes all in one big cycle: A→B→C→D→E→F→A *)
           let graph =
             graph_of_list
               [("a", ["b"]); ("b", ["c"]); ("c", ["d"]); ("d", ["e"]); ("e", ["f"]); ("f", ["a"])]
           in
           let roots = roots_of_list ["a"] in
           let result = StringTarjan.topsort ~roots graph in
           (* Single SCC with 6 nodes *)
           assert_equal ~ctxt 1 (List.length result);
           let (hd, tl) = List.hd result in
           let all_nodes = hd :: tl |> List.sort String.compare in
           assert_equal ~ctxt ["a"; "b"; "c"; "d"; "e"; "f"] all_nodes
         );
         (* Edge Cases *)
         ( "partial_reachability" >:: fun ctxt ->
           (* Graph has A→B, C→D, but only A is a root *)
           let graph = graph_of_list [("a", ["b"]); ("b", []); ("c", ["d"]); ("d", [])] in
           let roots = roots_of_list ["a"] in
           let result = StringTarjan.topsort ~roots graph in
           (* Only A and B should be visited *)
           assert_sccs_equal ~ctxt [("a", []); ("b", [])] result
         );
         ( "subset_of_roots" >:: fun ctxt ->
           (* A→B→C, only A is a root, B and C discovered via edges *)
           let graph = graph_of_list [("a", ["b"]); ("b", ["c"]); ("c", [])] in
           let roots = roots_of_list ["a"] in
           let result = StringTarjan.topsort ~roots graph in
           assert_sccs_equal ~ctxt [("a", []); ("b", []); ("c", [])] result
         );
         ( "all_nodes_as_roots" >:: fun ctxt ->
           (* A→B, B→C, but all three are roots *)
           let graph = graph_of_list [("a", ["b"]); ("b", ["c"]); ("c", [])] in
           let roots = roots_of_list ["a"; "b"; "c"] in
           let result = StringTarjan.topsort ~roots graph in
           (* Same result as if only A was root *)
           assert_sccs_equal ~ctxt [("a", []); ("b", []); ("c", [])] result
         );
         ( "star_pattern" >:: fun ctxt ->
           (* Hub A points to B, C, D, E (no back edges) *)
           let graph =
             graph_of_list [("a", ["b"; "c"; "d"; "e"]); ("b", []); ("c", []); ("d", []); ("e", [])]
           in
           let roots = roots_of_list ["a"] in
           let result = StringTarjan.topsort ~roots graph in
           (* Five SCCs, all singleton, A should be first *)
           assert_equal ~ctxt 5 (List.length result);
           let (hd0, tl0) = List.nth result 0 in
           assert_equal ~ctxt "a" hd0;
           assert_equal ~ctxt [] tl0;
           (* Last four should be B, C, D, E in some order *)
           let last_four =
             List.filteri (fun i _ -> i > 0) result
             |> List.map (fun (hd, _) -> hd)
             |> List.sort String.compare
           in
           assert_equal ~ctxt ["b"; "c"; "d"; "e"] last_four
         );
         ( "reverse_star" >:: fun ctxt ->
           (* B, C, D, E all point to hub A *)
           let graph =
             graph_of_list [("b", ["a"]); ("c", ["a"]); ("d", ["a"]); ("e", ["a"]); ("a", [])]
           in
           let roots = roots_of_list ["b"; "c"; "d"; "e"] in
           let result = StringTarjan.topsort ~roots graph in
           (* Five SCCs, A should appear somewhere (exact position depends on visit order) *)
           assert_equal ~ctxt 5 (List.length result);
           (* Verify A is present as a singleton SCC *)
           let has_a_singleton = result |> List.exists (fun (hd, tl) -> hd = "a" && tl = []) in
           assert_bool "A should be a singleton SCC" has_a_singleton;
           (* Verify all nodes are present *)
           let all_nodes = result |> List.map (fun (hd, _) -> hd) |> List.sort String.compare in
           assert_equal ~ctxt ["a"; "b"; "c"; "d"; "e"] all_nodes
         );
         (* Topological Order Verification *)
         ( "verify_reverse_topo_order" >:: fun _ctxt ->
           (* A→B→C→D, verify dependents appear before dependencies *)
           let graph = graph_of_list [("a", ["b"]); ("b", ["c"]); ("c", ["d"]); ("d", [])] in
           let roots = roots_of_list ["a"] in
           let result = StringTarjan.topsort ~roots graph in
           (* Build index map: node -> position in result *)
           let indices =
             result |> List.mapi (fun i (hd, _) -> (hd, i)) |> List.to_seq |> Hashtbl.of_seq
           in
           (* For each edge u→v in graph, verify index[u] < index[v] (dependent before dependency) *)
           let check_edge u v =
             let u_idx = Hashtbl.find indices u in
             let v_idx = Hashtbl.find indices v in
             assert_bool
               (Printf.sprintf "Edge %s→%s: expected %d < %d" u v u_idx v_idx)
               (u_idx < v_idx)
           in
           check_edge "a" "b";
           check_edge "b" "c";
           check_edge "c" "d"
         );
         ( "cross_edges_dont_create_sccs" >:: fun ctxt ->
           (* A→B→C with cross-edge A→C (doesn't create cycle) *)
           let graph = graph_of_list [("a", ["b"; "c"]); ("b", ["c"]); ("c", [])] in
           let roots = roots_of_list ["a"] in
           let result = StringTarjan.topsort ~roots graph in
           (* Three separate SCCs (cross-edge doesn't form cycle) *)
           assert_sccs_equal ~ctxt [("a", []); ("b", []); ("c", [])] result
         );
       ]

let () = run_test_tt_main tests
