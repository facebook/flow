(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let mk_source ?(source_maps = Some Source_map_config.default) () = Source.create ~source_maps ()

let mk_loc (start_line, start_col) (end_line, end_col) =
  {
    Loc.none with
    Loc.start = { Loc.line = start_line; column = start_col };
    _end = { Loc.line = end_line; column = end_col };
  }

let assert_contents_equal =
  let printer x = x in
  fun ~ctxt (expected : string) (source : Source.t) ->
    assert_equal ~ctxt ~printer expected (Source.contents source)

let assert_sourcemaps_equal =
  let printer = function
    | Some map -> map |> Json_sourcemap.json_of_sourcemap |> Hh_json.json_to_string ~pretty:true
    | None -> "None"
  in
  fun ~ctxt (expected : string option) (source : Source.t) ->
    let expected =
      match expected with
      | Some expected -> Some (Json_sourcemap.sourcemap_of_string expected)
      | None -> None
    in
    assert_equal ~ctxt ~printer expected (Source.sourcemap source)

let tests =
  "source"
  >::: [
         ( "simple_string" >:: fun ctxt ->
           let s =
             mk_source ()
             |> Source.push_loc (mk_loc (1, 0) (1, 3))
             |> Source.add_string "foo;"
             |> Source.pop_loc
           in
           assert_contents_equal ~ctxt "foo;" s;
           assert_sourcemaps_equal
             ~ctxt
             (Some
                {|{
        "version": 3,
        "sources": ["<stdin>"],
        "names": [],
        "mappings": "AAAA"
      }|})
             s );
         ( "two_strings" >:: fun ctxt ->
           let s =
             mk_source ()
             |> Source.push_loc (mk_loc (1, 0) (1, 3))
             |> Source.add_string "foo;"
             |> Source.pop_loc
             |> Source.push_loc (mk_loc (1, 4) (1, 7))
             |> Source.add_string "bar;"
             |> Source.pop_loc
           in
           assert_contents_equal ~ctxt "foo;bar;" s;
           assert_sourcemaps_equal
             ~ctxt
             (Some
                {|{
        "version": 3,
        "sources": ["<stdin>"],
        "names": [],
        "mappings": "AAAA,IAAI"
      }|})
             s );
       ]
