(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Test_utils
module LocMap = Loc_collections.LocMap
module LocSet = Loc_collections.LocSet

let mk_ssa_builder_test contents expected_values ctxt =
  let refined_reads = Env_builder.With_Loc.program (parse contents) in
  let printer locmap =
    let kvlist = LocMap.bindings locmap in
    let strlist =
      Base.List.map
        ~f:(fun (read_loc, refinement) ->
          Printf.sprintf
            "%s => { %s }"
            (Loc.debug_to_string read_loc)
            (Env_builder.show_refinement refinement))
        kvlist
    in
    Printf.sprintf "[ %s ]" (String.concat "; " strlist)
  in
  assert_equal
    ~ctxt
    ~cmp:(eq printer)
    ~printer
    ~msg:"SSA values don't match!"
    expected_values
    refined_reads

let mk_write (pos1, pos2) = Ssa_api.Write (mk_loc pos1 pos2)

let tests =
  let open Env_builder in
  "env_builder"
  >::: [
         "logical_expr"
         >:: mk_ssa_builder_test
               "let x = null;
let y = null;
(x && (y = x)) + x"
               LocMap.(empty |> add (mk_loc (3, 11) (3, 12)) Truthy);
       ]
