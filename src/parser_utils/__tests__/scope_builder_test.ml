(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


open OUnit2
open Test_utils

let mk_scope_builder_all_uses_test contents expected_all_uses =
  begin fun ctxt ->
    let info = Scope_builder.program (parse contents) in
    let all_uses = Scope_builder.Utils.all_uses info in
    let printer = print_list Loc.to_string in
    assert_equal ~ctxt
      ~cmp:(eq printer)
      ~printer
      ~msg:"All uses don't match!"
      expected_all_uses all_uses
  end

let mk_scope_builder_locs_of_defs_of_all_uses_test contents expected_locs_of_defs =
  begin fun ctxt ->
    let info = Scope_builder.program (parse contents) in
    let all_uses = Scope_builder.Utils.all_uses info in
    let defs = List.map (Scope_builder.Utils.def_of_use info) all_uses in
    let locs_of_defs = List.map (
      fun { Scope_builder.Def.loc; _ } -> loc
    ) defs in
    let printer = print_list Loc.to_string in
    assert_equal ~ctxt
      ~cmp:(eq printer)
      ~printer
      ~msg:"Defs of all uses don't match!"
      expected_locs_of_defs locs_of_defs
  end

let mk_scope_builder_uses_of_all_uses_test contents expected_uses =
  begin fun ctxt ->
    let info = Scope_builder.program (parse contents) in
    let all_uses = Scope_builder.Utils.all_uses info in
    let uses = List.map (Scope_builder.Utils.uses_of_use ~exclude_def:true info) all_uses in
    let printer = print_list @@ (fun list ->
      Printf.sprintf "[%s]" (print_list Loc.to_string list)
    ) in
    assert_equal ~ctxt
      ~cmp:(eq printer)
      ~printer
      ~msg:"Uses of all uses don't match!"
      expected_uses uses
  end

let tests = "scope_builder" >::: [
  "let_all_uses" >:: mk_scope_builder_all_uses_test
    "function foo() { \
       let x = 0; \
       return x; \
     }"
    [mk_loc (1, 35) (1, 36);
     mk_loc (1, 21) (1, 22);
     mk_loc (1, 9) (1, 12)];
  "let_locs_of_defs_of_all_uses" >:: mk_scope_builder_locs_of_defs_of_all_uses_test
    "function foo() { \
       let x = 0; \
       return x; \
     }"
    [mk_loc (1, 21) (1, 22);
     mk_loc (1, 21) (1, 22);
     mk_loc (1, 9) (1, 12)];
  "let_uses_of_all_uses" >:: mk_scope_builder_uses_of_all_uses_test
    "function foo() { \
       let x = 0; \
       return x; \
     }"
    [[mk_loc (1, 35) (1, 36)];
     [mk_loc (1, 35) (1, 36)];
     []];
]
