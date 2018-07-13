(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
(* open Sourcemaps *)
open Test_utils

let bar = Sourcemap.({
  source = "bar.js";
  original_loc = { line = 10; col = 5 };
  name = None;
})
let foo = Sourcemap.({
  source = "foo.js";
  original_loc = { line = 3; col = 1 };
  name = None;
})
let map =
  Sourcemap.create ()
  |> Sourcemap.add_mapping ~original:foo ~generated:{ Sourcemap.line = 1; col = 1 }
let map2 =
  Sourcemap.create ()
  |> Sourcemap.add_mapping ~original:bar ~generated:{ Sourcemap.line = 3; col = 1 }

let tests = "compose" >::: [
  "basic" >:: begin fun ctxt ->
    let expected =
      Sourcemap.create ()
      |> Sourcemap.add_mapping ~original:bar ~generated:{ Sourcemap.line = 1; col = 1 }
    in
    let map3 = Sourcemap.compose map map2 in
    assert_equal_sourcemaps ~ctxt expected map3
  end;
]
