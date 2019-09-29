(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
(* open Sourcemaps *)

let bar = Sourcemap.({
  source = "bar.js";
  original_loc = { line = 1; col = 1 };
  name = None;
})
let foo = Sourcemap.({
  source = "foo.js";
  original_loc = { line = 1; col = 1 };
  name = None;
})
let map =
  Sourcemap.create ()
  |> Sourcemap.add_mapping ~original:bar ~generated:{ Sourcemap.line = 3; col = 1 }
  |> Sourcemap.add_mapping ~original:foo ~generated:{ Sourcemap.line = 3; col = 5 }
  |> Sourcemap.freeze_for_lookup

let print_option f = function
  | Some x -> "Some " ^ (f x)
  | None -> "None"

let print_original { Sourcemap.source; original_loc = { Sourcemap.line; col }; name } =
  let name = print_option (fun x -> x) name in
  Printf.sprintf
    "{ source = %S; original_loc = { line = %d; col = %d }; name = %s }"
    source line col name

let tests = "original_loc_frozen" >::: [
  "matches_start" >:: begin fun ctxt ->
    let expected = Some bar in
    let actual = Sourcemap.find_original map { Sourcemap.line = 3; col = 1 } in
    assert_equal ~ctxt ~printer:(print_option print_original) expected actual;

    let expected = Some foo in
    let actual = Sourcemap.find_original map { Sourcemap.line = 3; col = 5 } in
    assert_equal ~ctxt ~printer:(print_option print_original) expected actual;
  end;

  "midpoint" >:: begin fun ctxt ->
    let expected = Some bar in
    let actual = Sourcemap.find_original map { Sourcemap.line = 3; col = 3 } in
    assert_equal ~ctxt ~printer:(print_option print_original) expected actual;
  end;

  "before_start" >:: begin fun ctxt ->
    let expected = Some bar in
    let actual = Sourcemap.find_original map { Sourcemap.line = 1; col = 2 } in
    assert_equal ~ctxt ~printer:(print_option print_original) expected actual;
  end;

  "past_end" >:: begin fun ctxt ->
    let expected = Some foo in
    let actual = Sourcemap.find_original map { Sourcemap.line = 3; col = 8 } in
    assert_equal ~ctxt ~printer:(print_option print_original) expected actual;
  end;

  "empty" >:: begin fun ctxt ->
    let expected = None in
    let map = Sourcemap.freeze_for_lookup (Sourcemap.create ()) in
    let actual = Sourcemap.find_original map { Sourcemap.line = 3; col = 3 } in

    assert_equal ~ctxt ~printer:(print_option print_original) expected actual;
  end;
]
