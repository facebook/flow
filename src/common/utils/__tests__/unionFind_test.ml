(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let assert_same_elements ~ctxt lst1 lst2 =
  let sort = List.fast_sort (fun x y -> x - y) in
  assert_equal ~ctxt (sort lst1) (sort lst2)

let assert_one_of ~ctxt item lst = assert_equal ~ctxt (List.mem item lst) true

let assert_raises ~ctxt ~f expected_exception =
  let exn = ref None in
  (try f () with e -> exn := Some e);
  assert_equal ~ctxt !exn (Some expected_exception)

let makeUnionFind () =
  let x = UnionFind.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9] in
  UnionFind.union x 1 3;
  UnionFind.union x 2 6;
  UnionFind.union x 7 9;
  UnionFind.union x 9 10;
  UnionFind.union x 4 3;
  UnionFind.union x 1 10;
  x

let tests =
  "union_find"
  >::: [
         ( "add" >:: fun ctxt ->
           let x = UnionFind.make () in
           UnionFind.add x 3;
           UnionFind.add x 5;
           assert_equal ~ctxt (UnionFind.members x 3) [3];
           assert_equal ~ctxt (UnionFind.members x 5) [5] );
         ( "union_new_values" >:: fun ctxt ->
           let x = UnionFind.make () in
           UnionFind.union x 1 3;
           assert_same_elements ~ctxt (UnionFind.members x 1) [1; 3] );
         ( "union_duplicate" >:: fun ctxt ->
           let x = UnionFind.make () in
           UnionFind.add x 1;
           UnionFind.add x 2;
           UnionFind.union x 1 2;
           UnionFind.union x 1 2;
           assert_same_elements ~ctxt (UnionFind.members x 1) [1; 2] );
         ( "find" >:: fun ctxt ->
           let x = makeUnionFind () in
           assert_one_of ~ctxt (UnionFind.find x 1) [1; 3; 4; 7; 9; 10];
           assert_one_of ~ctxt (UnionFind.find x 6) [2; 6];
           let y = UnionFind.make () in
           UnionFind.add y 1;
           assert_equal ~ctxt (UnionFind.find y 1) 1 );
         ( "multiple_groups" >:: fun ctxt ->
           let x = makeUnionFind () in
           assert_same_elements ~ctxt (UnionFind.members x 1) [1; 3; 4; 7; 9; 10];
           assert_same_elements ~ctxt (UnionFind.members x 6) [2; 6] );
         ( "grow" >:: fun ctxt ->
           let x = UnionFind.of_list [1; 2] in
           (* Should grow here *)
           UnionFind.add x 3;
           UnionFind.add x 4;

           (* Should grow here too *)
           UnionFind.add x 5;
           UnionFind.union x 1 5;
           UnionFind.union x 3 4;
           UnionFind.union x 4 2;
           assert_same_elements ~ctxt (UnionFind.members x 1) [1; 5];
           assert_same_elements ~ctxt (UnionFind.members x 4) [3; 4; 2] );
         ( "find_not_found" >:: fun ctxt ->
           let x = UnionFind.make () in
           assert_raises ~ctxt Not_found ~f:(fun () -> ignore (UnionFind.find x 1)) );
         ( "members_not_found" >:: fun ctxt ->
           let x = UnionFind.make () in
           assert_raises ~ctxt Not_found ~f:(fun () -> ignore (UnionFind.members x 1)) );
         ( "exercise_union_by_rank" >:: fun ctxt ->
           let x = UnionFind.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9] in
           (* Two sets have the same rank (0), now [1; 2] has rank 1. *)
           UnionFind.union x 1 2;

           (* Put the set with rank 1 first. Resulting set should still have rank 1. *)
           UnionFind.union x 2 3;

           (* Put the set with rank 1 second. Resulting set should still have rank 1. *)
           UnionFind.union x 4 3;
           assert_same_elements ~ctxt (UnionFind.members x 1) [1; 2; 3; 4] );
         ( "of_list" >:: fun ctxt ->
           (* Exercise power-of-two edge cases looking for off-by-ones *)
           let x = UnionFind.of_list [] in
           UnionFind.union x 1 2;
           assert_same_elements ~ctxt (UnionFind.members x 1) [1; 2];

           let x = UnionFind.of_list [1] in
           UnionFind.union x 1 2;
           assert_same_elements ~ctxt (UnionFind.members x 1) [1; 2];

           let x = UnionFind.of_list [1; 2] in
           UnionFind.union x 1 2;
           UnionFind.union x 1 3;
           UnionFind.union x 4 5;
           assert_same_elements ~ctxt (UnionFind.members x 1) [1; 2; 3] );
       ]
