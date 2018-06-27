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
    let all_uses = Utils_js.LocSet.elements @@ Scope_api.all_uses info in
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
    let all_uses = Utils_js.LocSet.elements @@ Scope_api.all_uses info in
    let defs = List.map (Scope_api.def_of_use info) all_uses in
    let locs_of_defs = List.map (
      fun { Scope_api.Def.locs; _ } -> Nel.to_list locs
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
    let all_uses = Utils_js.LocSet.elements @@ Scope_api.all_uses info in
    let uses = List.map (fun use ->
      Utils_js.LocSet.elements @@ Scope_api.uses_of_use ~exclude_def:true info use
    ) all_uses in
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
    [mk_loc (1, 9) (1, 12);
     mk_loc (1, 21) (1, 22);
     mk_loc (1, 35) (1, 36)];
  "let_locs_of_defs_of_all_uses" >:: mk_scope_builder_locs_of_defs_of_all_uses_test
    "function foo() { \
       let x = 0; \
       return x; \
     }"
    [[mk_loc (1, 9) (1, 12)];
     [mk_loc (1, 21) (1, 22)];
     [mk_loc (1, 21) (1, 22)]];
  "let_uses_of_all_uses" >:: mk_scope_builder_uses_of_all_uses_test
    "function foo() { \
       let x = 0; \
       return x; \
     }"
    [[];
     [mk_loc (1, 35) (1, 36)];
     [mk_loc (1, 35) (1, 36)]];
  "var_locs_of_defs_of_all_uses" >:: mk_scope_builder_locs_of_defs_of_all_uses_test
    "function foo({y}) { \
       var {x} = y; \
       return x; \
     }"
    [[mk_loc (1, 9) (1, 12)];
     [mk_loc (1, 14) (1, 15)];
     [mk_loc (1, 25) (1, 26)];
     [mk_loc (1, 14) (1, 15)];
     [mk_loc (1, 25) (1, 26)]];
  "var_uses_of_all_uses" >:: mk_scope_builder_uses_of_all_uses_test
    "function foo({y}) { \
       var {x} = y; \
       return x; \
     }"
    [[];
     [mk_loc (1, 30) (1, 31)];
     [mk_loc (1, 40) (1, 41)];
     [mk_loc (1, 30) (1, 31)];
     [mk_loc (1, 40) (1, 41)]];
  "var_locs_of_defs_of_all_uses2" >:: mk_scope_builder_locs_of_defs_of_all_uses_test
    "function foo() { \
       var { x, y } = { x: 0, y: 0 }; \
       var { x: _x, y: _y } = { x, y }; \
       return ({ x: _x, y: _y }); \
     }"
    [[mk_loc (1, 9) (1, 12)];
     [mk_loc (1, 23) (1, 24)];
     [mk_loc (1, 26) (1, 27)];
     [mk_loc (1, 57) (1, 59)];
     [mk_loc (1, 64) (1, 66)];
     [mk_loc (1, 23) (1, 24)];
     [mk_loc (1, 26) (1, 27)];
     [mk_loc (1, 57) (1, 59)];
     [mk_loc (1, 64) (1, 66)]];
  "let_uses_of_all_uses2" >:: mk_scope_builder_uses_of_all_uses_test
    "function foo() { \
       let { x, y } = { x: 0, y: 0 }; \
       let { x: _x, y: _y } = { x, y }; \
       return ({ x: _x, y: _y }); \
     }"
    [[];
     [mk_loc (1, 73) (1, 74)];
     [mk_loc (1, 76) (1, 77)];
     [mk_loc (1, 94) (1, 96)];
     [mk_loc (1, 101) (1, 103)];
     [mk_loc (1, 73) (1, 74)];
     [mk_loc (1, 76) (1, 77)];
     [mk_loc (1, 94) (1, 96)];
     [mk_loc (1, 101) (1, 103)]];
  "jsx_uses_of_all_uses" >:: mk_scope_builder_all_uses_test
    "class Foo {}; <Foo></Foo>; <Foo/>"
    [mk_loc (1, 6) (1, 9);
     mk_loc (1, 15) (1, 18);
     mk_loc (1, 21) (1, 24);
     mk_loc (1, 28) (1, 31)];
  "declare_var" >:: mk_scope_builder_all_uses_test
    "declare var foo: number; foo"
    [mk_loc (1, 12) (1, 15);
     mk_loc (1, 25) (1, 28)];
  "declare_class" >:: mk_scope_builder_all_uses_test
    "declare class Foo {}; new Foo()"
    [mk_loc (1, 14) (1, 17);
     mk_loc (1, 26) (1, 29)];
  "declare_function" >:: mk_scope_builder_all_uses_test
    "declare function foo(): void; foo()"
    [mk_loc (1, 17) (1, 20);
     mk_loc (1, 30) (1, 33)];
  "export_named_function" >:: mk_scope_builder_all_uses_test
    "export function foo() {}; foo()"
    [mk_loc (1, 16) (1, 19);
     mk_loc (1, 26) (1, 29)];
  "export_named_class" >:: mk_scope_builder_all_uses_test
    "export class Foo {}; new Foo()"
    [mk_loc (1, 13) (1, 16);
     mk_loc (1, 25) (1, 28)];
  "export_named_binding" >:: mk_scope_builder_all_uses_test
    "export const foo = () => {}; foo()"
    [mk_loc (1, 13) (1, 16);
     mk_loc (1, 29) (1, 32)];
  "export_default_function" >:: mk_scope_builder_all_uses_test
    "export default function foo() {}; foo()"
    [mk_loc (1, 24) (1, 27);
     mk_loc (1, 34) (1, 37)];
  "export_default_class" >:: mk_scope_builder_all_uses_test
    "export default class Foo {} new Foo()"
    [mk_loc (1, 21) (1, 24);
     mk_loc (1, 32) (1, 35)];
  "computed_property_destructuring" >:: mk_scope_builder_all_uses_test
    "const x = {}; const foo = ''; const {[foo]: bar} = x;"
    [mk_loc (1, 6) (1, 7);
     mk_loc (1, 20) (1, 23);
     (* TODO: should include the use of `foo` *)
     (* mk_loc (1, 38) (1, 41); *)
     mk_loc (1, 44) (1, 47);
     mk_loc (1, 51) (1, 52)];
]
