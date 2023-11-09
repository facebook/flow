(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

(* Unsafe *)
let of_list lst =
  match Nel.of_list lst with
  | Some nel -> nel
  | None -> raise Not_found

let assert_identical ~ctxt x y = assert_equal ~ctxt (x == y) true

let id x = x

(* [6; 4; 2] *)
let lst = Nel.one 2 |> Nel.cons 4 |> Nel.cons 6

(* [3; 5] *)
let lst2 = Nel.one 5 |> Nel.cons 3

let tests =
  "nel"
  >::: [
         ( "all_one_size_16" >:: fun ctxt ->
           let bitset = Bitset.all_one 16 in
           for i = 0 to 15 do
             assert_equal ~ctxt ~printer:string_of_bool true (Bitset.mem i bitset)
           done
         );
         ( "all_one_size_17" >:: fun ctxt ->
           let bitset = Bitset.all_one 17 in
           for i = 0 to 16 do
             assert_equal ~ctxt ~printer:string_of_bool true (Bitset.mem i bitset)
           done
         );
         ( "start_with_all_zero_size_16" >:: fun ctxt ->
           let bitset = Bitset.all_zero 16 in
           for i = 0 to 15 do
             assert_equal ~ctxt ~printer:string_of_bool false (Bitset.mem i bitset)
           done;
           Bitset.set bitset 3;
           assert_equal ~ctxt ~printer:string_of_bool true (Bitset.mem 3 bitset);
           assert_equal ~ctxt ~printer:string_of_bool false (Bitset.mem 4 bitset)
         );
         ( "start_with_all_zero_size_17" >:: fun ctxt ->
           let bitset = Bitset.all_zero 17 in
           for i = 0 to 16 do
             assert_equal ~ctxt ~printer:string_of_bool false (Bitset.mem i bitset)
           done;
           Bitset.set bitset 16;
           assert_equal ~ctxt ~printer:string_of_bool true (Bitset.mem 16 bitset);
           assert_equal ~ctxt ~printer:string_of_bool false (Bitset.mem 15 bitset)
         );
         ( "start_with_all_one_size_16" >:: fun ctxt ->
           let bitset = Bitset.all_one 16 in
           for i = 0 to 15 do
             assert_equal ~ctxt ~printer:string_of_bool true (Bitset.mem i bitset)
           done;
           Bitset.unset bitset 3;
           assert_equal ~ctxt ~printer:string_of_bool false (Bitset.mem 3 bitset);
           assert_equal ~ctxt ~printer:string_of_bool true (Bitset.mem 4 bitset)
         );
         ( "start_with_all_one_size_17" >:: fun ctxt ->
           let bitset = Bitset.all_one 17 in
           for i = 0 to 16 do
             assert_equal ~ctxt ~printer:string_of_bool true (Bitset.mem i bitset)
           done;
           Bitset.unset bitset 16;
           assert_equal ~ctxt ~printer:string_of_bool false (Bitset.mem 16 bitset);
           assert_equal ~ctxt ~printer:string_of_bool true (Bitset.mem 15 bitset)
         );
         ( "out_of_bounds_all_one_bitset_are_unset" >:: fun ctxt ->
           let bitset = Bitset.all_one 17 in
           for i = 17 to 23 do
             assert_equal ~ctxt ~printer:string_of_bool false (Bitset.mem i bitset)
           done;
           let bitset = Bitset.all_one 18 in
           for i = 18 to 23 do
             assert_equal ~ctxt ~printer:string_of_bool false (Bitset.mem i bitset)
           done;
           let bitset = Bitset.all_one 19 in
           for i = 19 to 23 do
             assert_equal ~ctxt ~printer:string_of_bool false (Bitset.mem i bitset)
           done;
           let bitset = Bitset.all_one 20 in
           for i = 20 to 23 do
             assert_equal ~ctxt ~printer:string_of_bool false (Bitset.mem i bitset)
           done;
           let bitset = Bitset.all_one 21 in
           for i = 21 to 23 do
             assert_equal ~ctxt ~printer:string_of_bool false (Bitset.mem i bitset)
           done;
           let bitset = Bitset.all_one 22 in
           for i = 22 to 23 do
             assert_equal ~ctxt ~printer:string_of_bool false (Bitset.mem i bitset)
           done;
           let bitset = Bitset.all_one 23 in
           assert_equal ~ctxt ~printer:string_of_bool false (Bitset.mem 23 bitset)
         );
         ( "subset_test" >:: fun ctxt ->
           let open Bitset in
           assert_equal ~ctxt ~printer:string_of_bool false (is_subset (all_one 31) (all_zero 31));
           assert_equal ~ctxt ~printer:string_of_bool true (is_subset (all_zero 31) (all_one 31));
           assert_equal ~ctxt ~printer:string_of_bool true (is_subset (all_zero 31) (all_zero 31));
           assert_equal ~ctxt ~printer:string_of_bool true (is_subset (all_one 31) (all_one 31));
           let () =
             let a = all_zero 21 in
             let b = all_zero 21 in
             set b 3;
             set a 10;
             set b 10;
             set a 11;
             set b 11;
             set a 17;
             set b 17;
             set b 18;
             assert_equal ~ctxt ~printer:string_of_bool true (is_subset a b)
           in
           let () =
             let a = all_zero 21 in
             let b = all_zero 21 in
             set b 3;
             set a 10;
             set b 10;
             set a 11;
             set b 11;
             set a 17;
             set b 17;
             set a 18;
             assert_equal ~ctxt ~printer:string_of_bool false (is_subset a b)
           in
           let () =
             let a = all_zero 21 in
             let b = all_zero 21 in
             set a 3;
             set a 10;
             set b 10;
             set a 11;
             set b 11;
             set a 17;
             set b 17;
             set b 18;
             assert_equal ~ctxt ~printer:string_of_bool false (is_subset a b)
           in
           ()
         );
         ( "no_overlap_test" >:: fun ctxt ->
           let open Bitset in
           assert_equal ~ctxt ~printer:string_of_bool true (no_overlap (all_one 31) (all_zero 31));
           assert_equal ~ctxt ~printer:string_of_bool true (no_overlap (all_zero 31) (all_one 31));
           assert_equal ~ctxt ~printer:string_of_bool true (no_overlap (all_zero 31) (all_zero 31));
           assert_equal ~ctxt ~printer:string_of_bool false (no_overlap (all_one 31) (all_one 31));
           let () =
             let a = all_zero 21 in
             let b = all_zero 21 in
             set b 3;
             set a 10;
             set b 11;
             set a 17;
             set b 18;
             assert_equal ~ctxt ~printer:string_of_bool true (no_overlap a b)
           in
           let () =
             let a = all_zero 21 in
             let b = all_zero 21 in
             set b 3;
             set a 3;
             set a 11;
             set a 17;
             set a 18;
             assert_equal ~ctxt ~printer:string_of_bool false (is_subset a b)
           in
           ()
         );
       ]
