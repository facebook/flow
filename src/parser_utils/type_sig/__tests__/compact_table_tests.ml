(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Option = Base.Option

module Tbl = Compact_table.Make ()

type t = {
  label: char;
  refs: t Tbl.node option ref list;
}

let rec mark { refs; _ } = List.iter (fun x -> Tbl.mark (Option.value_exn !x) mark) refs

let merge x0 x1 =
  if x0.label = x1.label then
    let refs = x0.refs @ x1.refs in
    Some { label = x0.label; refs }
  else
    None

let compact { label; refs } =
  let refs = List.map (fun x -> Tbl.index_exn (Option.value_exn !x)) refs in
  (label, refs)

let print_tbl =
  Tbl.iteri (fun (i : Tbl.index) (label, refs) ->
      List.map (fun (j : Tbl.index) -> string_of_int (j :> int)) refs
      |> String.concat " "
      |> Printf.printf "%d| %c -> %s\n" (i :> int) label)

(* A -> [D]
 * B -> [A]
 * C -> [A]
 * D -> [B] *)
let%expect_test "cycle" =
  let builder = Tbl.create () in
  let a_ref = ref None in
  let b_ref = ref None in
  let d_ref = ref None in
  let a = Tbl.push builder { label = 'A'; refs = [d_ref] } in
  let b = Tbl.push builder { label = 'B'; refs = [a_ref] } in
  let _ = Tbl.push builder { label = 'C'; refs = [a_ref] } in
  let d = Tbl.push builder { label = 'D'; refs = [b_ref] } in
  a_ref := Some a;
  b_ref := Some b;
  d_ref := Some d;
  Tbl.mark a mark;
  let indexed = Tbl.compact builder in
  let copy = Tbl.copy compact indexed in
  print_tbl copy;
  [%expect {|
    0| A -> 2
    1| B -> 0
    2| D -> 1
  |}]

let%expect_test "empty" =
  let builder = Tbl.create () in
  let indexed = Tbl.compact builder in
  let copy = Tbl.copy compact indexed in
  print_int (Tbl.length copy);
  [%expect {| 0 |}]

let%expect_test "singleton_unmarked" =
  let builder = Tbl.create () in
  let _ = Tbl.push builder { label = 'A'; refs = [] } in
  let indexed = Tbl.compact builder in
  let copy = Tbl.copy compact indexed in
  print_int (Tbl.length copy);
  [%expect {| 0 |}]

let%expect_test "singleton_marked" =
  let builder = Tbl.create () in
  let a = Tbl.push builder { label = 'A'; refs = [] } in
  Tbl.mark a mark;
  let indexed = Tbl.compact builder in
  let copy = Tbl.copy compact indexed in
  print_tbl copy;
  [%expect {|
    0| A ->
  |}]

let%expect_test "splice" =
  let builder = Tbl.create () in
  let a = Tbl.push builder { label = 'A'; refs = [] } in
  let d = Tbl.push builder { label = 'D'; refs = [] } in
  let (b, c) =
    Tbl.splice a (fun builder ->
        let b = Tbl.push builder { label = 'B'; refs = [] } in
        let c = Tbl.push builder { label = 'C'; refs = [] } in
        (b, c))
  in
  List.iter (fun x -> Tbl.mark x mark) [a; b; c; d];
  let indexed = Tbl.compact builder in
  let copy = Tbl.copy compact indexed in
  print_tbl copy;
  [%expect {|
    0| A ->
    1| B ->
    2| C ->
    3| D ->
  |}]

let%expect_test "compact_merge" =
  let builder = Tbl.create () in
  let a_ref = ref None in
  let b0_ref = ref None in
  let b1_ref = ref None in
  let c_ref = ref None in
  let a = Tbl.push builder { label = 'A'; refs = [b0_ref] } in
  let b0 = Tbl.push builder { label = 'B'; refs = [a_ref] } in
  let b1 = Tbl.push builder { label = 'B'; refs = [c_ref] } in
  let c = Tbl.push builder { label = 'C'; refs = [b1_ref] } in
  a_ref := Some a;
  b0_ref := Some b0;
  b1_ref := Some b1;
  c_ref := Some c;
  List.iter (fun x -> Tbl.mark x mark) [a; b0; b1; c];
  let indexed = Tbl.compact ~merge builder in
  let copy = Tbl.copy compact indexed in
  print_tbl copy;
  [%expect {|
    0| A -> 1
    1| B -> 0 2
    2| C -> 1
  |}]
