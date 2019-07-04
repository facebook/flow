(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
(* open Sourcemaps *)
open Test_utils

(**
 * Imagine we have three versions of the same file from a transformation pipeline.
 * /v2_files/v2.js was generated from /v1_files/v1.js and produced a map: map_1_2
 * /v3_files/v3.js was generated from /v2_files/v2.js and produced a map: map_2_3
 * We want to reconstruct map_1_3 by composing the two maps.
 *)

let v1_source = "a\nb"
let v2_source = "// Preamble\nA B"
let v3_source = "a\n\nb"

let v1_a = Sourcemap.({
  source = "v1.js";
  original_loc = { line = 1; col = 0 };
  name = Some "a";
})

let v1_b = Sourcemap.({
  source = "v1.js";
  original_loc = { line = 2; col = 0 };
  name = Some "b";
})

let v2_a = Sourcemap.({
  source = "v2.js";
  original_loc = { line = 2; col = 0 };
  name = Some "A";
})

let v2_b = Sourcemap.({
  source = "v2.js";
  original_loc = { line = 1; col = 2 };
  name = Some "B";
})

let v3_a = Sourcemap.({
  source = "v3.js";
  original_loc = { line = 1; col = 0 };
  name = Some "a";
})

let v3_b = Sourcemap.({
  source = "v3.js";
  original_loc = { line = 3; col = 0 };
  name = Some "b";
})

let map_1_2 =
  Sourcemap.create () ~file:"v2.js" ~source_root:"/v1_files/"
  |> Sourcemap.add_mapping ~original:v1_b ~generated:v2_b.Sourcemap.original_loc
  |> Sourcemap.add_mapping ~original:v1_a ~generated:v2_a.Sourcemap.original_loc
  |> Sourcemap.add_source_content ~source:"v1.js" ~content:v1_source

let map_2_3 =
  Sourcemap.create () ~file:"v3.js" ~source_root:"/v2_files/"
  |> Sourcemap.add_mapping ~original:v2_a ~generated:v3_a.Sourcemap.original_loc
  |> Sourcemap.add_mapping ~original:v2_b ~generated:v3_b.Sourcemap.original_loc
  |> Sourcemap.add_source_content ~source:"v2.js" ~content:v2_source

let tests = "compose" >::: [
  "basic" >:: begin fun ctxt ->
    let expected =
      Sourcemap.create () ~file:"v3.js" ~source_root:"/v1_files/"
      |> Sourcemap.add_mapping ~original:v1_a ~generated:v3_a.Sourcemap.original_loc
      |> Sourcemap.add_mapping ~original:v1_b ~generated:v3_b.Sourcemap.original_loc
      |> Sourcemap.add_source_content ~source:"v1.js" ~content:v1_source
    in
    let map_1_3 = Sourcemap.compose map_2_3 map_1_2 in
    assert_equal_sourcemaps ~ctxt expected map_1_3
  end;
  "empty_map_2_3" >:: begin fun ctxt ->
    let expected =
      Sourcemap.create () ~file:"v3.js" ~source_root:"/v1_files/"
    in
    let map_2_3 = Sourcemap.create () ~file:"v3.js" ~source_root:"/v2_files/" in
    let map_1_3 = Sourcemap.compose map_2_3 map_1_2 in
    assert_equal_sourcemaps ~ctxt expected map_1_3
  end;
  "empty_map_1_2" >:: begin fun ctxt ->
    let expected =
      Sourcemap.create () ~file:"v3.js" ~source_root:"/v1_files/"
    in
    let map_1_2 = Sourcemap.create () ~file:"v2.js" ~source_root:"/v1_files/" in
    let map_1_3 = Sourcemap.compose map_2_3 map_1_2 in
    assert_equal_sourcemaps ~ctxt expected map_1_3
  end;
]
