(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Cycle_hash

let print_hash = Printf.sprintf "%Lx"

let assert_changed = assert_equal ~printer:print_hash ~cmp:( <> )

let assert_unchanged = assert_equal ~printer:print_hash

(* We use laziness to tie the knot of recursive structures in this test file. A
 * unary operator makes things a bit less cluttered. *)
let ( !! ) = Lazy.force

(* Define a node in a graph `g` with edges to nodes at the given offsets. *)
let mk_node g hash edges =
  let read_hash () = !hash in
  let write_hash = ( := ) hash in
  let visit edge _ = List.iter (fun i -> edge !!g.(i)) edges in
  create_node visit read_hash write_hash

let run nodes roots =
  let cx = create_cx () in
  let rec g = lazy (Array.map (fun (hash, deps) -> mk_node g hash deps) nodes) in
  List.iter (fun i -> root cx !!g.(i)) roots

(* special case for unitary graph *)
let run1 hash = run [| (hash, []) |] [0]

(* Test that a unit graph's hash is stable *)
let unit_unchanged_test _ =
  (* init *)
  let h1 = ref 0L in
  run1 h1;

  (* unchanged *)
  let h2 = ref 0L in
  run1 h2;

  assert_unchanged !h1 !h2

(* Test that a unit graph's hash updates when the inputs change *)
let unit_changed_test _ =
  (* init *)
  let h1 = ref 0L in
  run1 h1;

  (* changed 0->1 *)
  let h2 = ref 1L in
  run1 h2;

  assert_changed !h1 !h2

(* Test that B is stable in B->A when A is stable *)
let acyclic_unchanged_test _ =
  (* init *)
  let h1a = ref 0L in
  let h1b = ref 0L in
  run [| (h1a, []); (h1b, [0]) |] [1];

  (* unchanged *)
  let h2a = ref 0L in
  let h2b = ref 0L in
  run [| (h2a, []); (h2b, [0]) |] [1];

  assert_unchanged !h1a !h2a;
  assert_unchanged !h1b !h2b

(* Test that B changes in B->A when A changes *)
let acyclic_changed_test _ =
  (* init *)
  let h1a = ref 0L in
  let h1b = ref 0L in
  run [| (h1a, []); (h1b, [0]) |] [1];

  (* changed A 0->1 *)
  let h2a = ref 1L in
  let h2b = ref 0L in
  run [| (h2a, []); (h2b, [0]) |] [1];

  assert_changed !h1a !h2a;
  assert_changed !h1b !h2b

(* Test that A,B is stable in B<->A when inputs do not change *)
let cyclic_unchanged_test _ =
  (* init *)
  let h1a = ref 0L in
  let h1b = ref 0L in
  run [| (h1a, [1]); (h1b, [0]) |] [1];

  (* unchanged *)
  let h2a = ref 0L in
  let h2b = ref 0L in
  run [| (h2a, [1]); (h2b, [0]) |] [1];

  assert_unchanged !h1a !h2a;
  assert_unchanged !h1b !h2b

(* Test that A,B change in B<->A when inputs change *)
let cyclic_changed_test _ =
  (* init *)
  let h1a = ref 0L in
  let h1b = ref 0L in
  run [| (h1a, [1]); (h1b, [0]) |] [1];

  (* changed A 0->1 *)
  let h2a = ref 1L in
  let h2b = ref 0L in
  run [| (h2a, [1]); (h2b, [0]) |] [1];

  assert_changed !h1a !h2a;
  assert_changed !h1b !h2b

let cycle_order_test _ =
  (* A = ... C ... *
   * B = ... C ... *
   * C = [A, B]
   *)
  let h1a = ref 0L in
  let h1b = ref 1L in
  let h1c = ref 2L in
  run [| (h1a, [2]); (h1b, [2]); (h1c, [0; 1]) |] [2];

  (* A = ... C ... *
   * B = ... C ... *
   * C = [B, A]
   *)
  let h2a = ref 0L in
  let h2b = ref 1L in
  let h2c = ref 2L in
  run [| (h2a, [2]); (h2b, [2]); (h2c, [1; 0]) |] [2];

  assert_changed !h1c !h2c

let cycle_canon_test _ =
  (* visit A<->B cycle starting at A *)
  let h1a = ref 0L in
  let h1b = ref 1L in
  run [| (h1a, [1]); (h1b, [0]) |] [0];

  (* A<->B cycle unchanged, but visit starting at B *)
  let h2a = ref 0L in
  let h2b = ref 1L in
  run [| (h2a, [1]); (h2b, [0]) |] [1];

  assert_unchanged !h1a !h2a;
  assert_unchanged !h1b !h2b

let tests =
  "cycle_hash"
  >::: [
         "unit_unchanged" >:: unit_unchanged_test;
         "unit_changed" >:: unit_changed_test;
         "acyclic_unchanged" >:: acyclic_unchanged_test;
         "acyclic_changed" >:: acyclic_changed_test;
         "cyclic_unchanged" >:: cyclic_unchanged_test;
         "cyclic_changed" >:: cyclic_changed_test;
         "cycle_order" >:: cycle_order_test;
         "cycle_canon" >:: cycle_canon_test;
       ]

let () = run_test_tt_main tests
