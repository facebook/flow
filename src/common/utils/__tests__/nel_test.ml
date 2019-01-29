(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

(* Unsafe *)
let of_list lst = match Nel.of_list lst with
  | Some nel -> nel
  | None -> raise Not_found

let assert_identical ~ctxt x y =
  assert_equal ~ctxt (x == y) true

let id x = x

(* [6; 4; 2] *)
let lst = Nel.one 2 |> Nel.cons 4 |> Nel.cons 6
(* [3; 5] *)
let lst2 = Nel.one 5 |> Nel.cons 3

let tests = "nel" >::: [
  "to_list" >:: begin fun ctxt ->
    assert_equal ~ctxt (Nel.to_list lst) [6; 4; 2]
  end;

  "mem" >:: begin fun ctxt ->
    assert_equal ~ctxt (Nel.mem 4 lst) true;
    assert_equal ~ctxt (Nel.mem 5 lst) false
  end;

  "exists" >:: begin fun ctxt ->
    assert_equal ~ctxt (Nel.exists (( = ) 4) lst) true;
    assert_equal ~ctxt (Nel.exists (( = ) 5) lst) false
  end;

  "iter" >:: begin fun ctxt ->
    let x = ref 0 in
    Nel.iter (fun y -> x := !x + y) lst;
    assert_equal ~ctxt !x 12
  end;

  "map" >:: begin fun ctxt ->
    assert_equal ~ctxt (Nel.map (( * ) 2) lst |> Nel.to_list) [12; 8; 4]
  end;

  "ident_map" >:: begin fun ctxt ->
    assert_identical ~ctxt (Nel.ident_map id lst) lst;
    assert_equal ~ctxt (Nel.ident_map (( * ) 2) lst |> Nel.to_list) [12; 8; 4]
  end;

  "concat" >:: begin fun ctxt ->
    let x = Nel.one lst2 |> Nel.cons lst in
    assert_equal ~ctxt (Nel.concat x |> Nel.to_list) [6; 4; 2; 3; 5]
  end;

  "map_concat" >:: begin fun ctxt ->
    let f x = Nel.one (x - 1) |> Nel.cons x in
    assert_equal ~ctxt (Nel.map_concat f lst |> Nel.to_list) [6; 5; 4; 3; 2; 1]
  end;

  "rev" >:: begin fun ctxt ->
    assert_equal ~ctxt (Nel.rev lst |> Nel.to_list) [2; 4; 6]
  end;

  "rev_map" >:: begin fun ctxt ->
    assert_equal ~ctxt (Nel.rev_map (( + ) 1) lst |> Nel.to_list) [3; 5; 7]
  end;

  "rev_append" >:: begin fun ctxt ->
    assert_equal ~ctxt (Nel.rev_append lst lst2 |> Nel.to_list) [2; 4; 6; 3; 5]
  end;

  "append" >:: begin fun ctxt ->
    assert_equal ~ctxt (Nel.append lst lst2 |> Nel.to_list) [6; 4; 2; 3; 5]
  end;

  "length" >:: begin fun ctxt ->
    assert_equal ~ctxt (Nel.length lst) 3
  end;

  "fold_left" >:: begin fun ctxt ->
    assert_equal ~ctxt (Nel.fold_left ( * ) 1 lst) 48
  end;

  "hd" >:: begin fun ctxt ->
    assert_equal ~ctxt (Nel.hd lst) 6
  end;

  "nth" >:: begin fun ctxt ->
    assert_equal ~ctxt (Nel.nth lst 0) 6;
    assert_equal ~ctxt (Nel.nth lst 1) 4;
    assert_equal ~ctxt (Nel.nth lst 2) 2
  end;

  "cat_maybes" >:: begin fun ctxt ->
    assert_equal ~ctxt (Nel.cat_maybes (of_list [None])) None;
    assert_equal ~ctxt (Nel.cat_maybes (of_list [Some 1; None])) (Some (of_list [1]));
    assert_equal ~ctxt (Nel.cat_maybes (of_list [Some 0; None; Some 1])) (Some (of_list [0; 1]))
  end;
]
