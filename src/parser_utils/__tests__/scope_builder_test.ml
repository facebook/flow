(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)


open OUnit2
open Test_utils

let mk_scope_builder_all_uses_test contents expected_all_uses =
  begin fun ctxt ->
    let info = Scope_builder.program (parse contents) in
    let all_uses = Scope_api.all_uses info in
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
    let all_uses = Scope_api.all_uses info in
    let defs = List.map (Scope_api.def_of_use info) all_uses in
    let locs_of_defs = List.map (
      fun { Scope_api.Def.locs; _ } -> locs
    ) defs in
    let printer = print_list @@ print_list Loc.to_string in
    assert_equal ~ctxt
      ~cmp:(eq printer)
      ~printer
      ~msg:"Defs of all uses don't match!"
      expected_locs_of_defs locs_of_defs
  end

let mk_scope_builder_uses_of_all_uses_test contents expected_uses =
  begin fun ctxt ->
    let info = Scope_builder.program (parse contents) in
    let all_uses = Scope_api.all_uses info in
    let uses = List.map (Scope_api.uses_of_use ~exclude_def:true info) all_uses in
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
    [[mk_loc (1, 21) (1, 22)];
     [mk_loc (1, 21) (1, 22)];
     [mk_loc (1, 9) (1, 12)]];
  "let_uses_of_all_uses" >:: mk_scope_builder_uses_of_all_uses_test
    "function foo() { \
       let x = 0; \
       return x; \
     }"
    [[mk_loc (1, 35) (1, 36)];
     [mk_loc (1, 35) (1, 36)];
     []];
  "var_locs_of_defs_of_all_uses" >:: mk_scope_builder_locs_of_defs_of_all_uses_test
    "function foo({y}) { \
       var {x} = y; \
       return x; \
     }"
    [[mk_loc (1, 25) (1, 26)];
     [mk_loc (1, 14) (1, 15)];
     [mk_loc (1, 25) (1, 26)];
     [mk_loc (1, 14) (1, 15)];
     [mk_loc (1, 9) (1, 12)]];
  "var_uses_of_all_uses" >:: mk_scope_builder_uses_of_all_uses_test
    "function foo({y}) { \
       var {x} = y; \
       return x; \
     }"
    [[mk_loc (1, 40) (1, 41)];
     [mk_loc (1, 30) (1, 31)];
     [mk_loc (1, 40) (1, 41)];
     [mk_loc (1, 30) (1, 31)];
     []];
  "var_locs_of_defs_of_all_uses2" >:: mk_scope_builder_locs_of_defs_of_all_uses_test
    "function foo() { \
       var { x, y } = { x: 0, y: 0 }; \
       var { x: _x, y: _y } = { x, y }; \
       return ({ x: _x, y: _y }); \
     }"
    [[mk_loc (1, 64) (1, 66)];
     [mk_loc (1, 57) (1, 59)];
     [mk_loc (1, 26) (1, 27)];
     [mk_loc (1, 23) (1, 24)];
     [mk_loc (1, 64) (1, 66)];
     [mk_loc (1, 57) (1, 59)];
     [mk_loc (1, 26) (1, 27)];
     [mk_loc (1, 23) (1, 24)];
     [mk_loc (1, 9) (1, 12)]];
  "let_uses_of_all_uses2" >:: mk_scope_builder_uses_of_all_uses_test
    "function foo() { \
       let { x, y } = { x: 0, y: 0 }; \
       let { x: _x, y: _y } = { x, y }; \
       return ({ x: _x, y: _y }); \
     }"
    [[mk_loc (1, 101) (1, 103)];
     [mk_loc (1, 94) (1, 96)];
     [mk_loc (1, 76) (1, 77)];
     [mk_loc (1, 73) (1, 74)];
     [mk_loc (1, 101) (1, 103)];
     [mk_loc (1, 94) (1, 96)];
     [mk_loc (1, 76) (1, 77)];
     [mk_loc (1, 73) (1, 74)];
     []];
]
