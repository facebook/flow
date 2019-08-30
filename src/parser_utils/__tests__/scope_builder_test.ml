(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)


open OUnit2
open Test_utils

module Scope_api = Scope_api.With_Loc

let mk_scope_builder_all_uses_test contents expected_all_uses =
  begin fun ctxt ->
    let info = Scope_builder.program (parse contents) in
    let all_uses = Loc_collections.LocSet.elements @@ Scope_api.all_uses info in
    let printer = print_list Loc.debug_to_string in
    assert_equal ~ctxt
      ~cmp:(eq printer)
      ~printer
      ~msg:"All uses don't match!"
      expected_all_uses all_uses
  end

let mk_scope_builder_locs_of_defs_of_all_uses_test contents expected_locs_of_defs =
  begin fun ctxt ->
    let info = Scope_builder.program (parse contents) in
    let all_uses = Loc_collections.LocSet.elements @@ Scope_api.all_uses info in
    let defs = Core_list.map ~f:(Scope_api.def_of_use info) all_uses in
    let locs_of_defs = Core_list.map ~f:(
      fun { Scope_api.Def.locs; _ } -> Nel.to_list locs
    ) defs in
    let printer = print_list @@ print_list Loc.debug_to_string in
    assert_equal ~ctxt
      ~cmp:(eq printer)
      ~printer
      ~msg:"Defs of all uses don't match!"
      expected_locs_of_defs locs_of_defs
  end

let mk_scope_builder_uses_of_all_uses_test contents expected_uses =
  begin fun ctxt ->
    let info = Scope_builder.program (parse contents) in
    let all_uses = Loc_collections.LocSet.elements @@ Scope_api.all_uses info in
    let uses = Core_list.map ~f:(fun use ->
      Loc_collections.LocSet.elements @@ Scope_api.uses_of_use ~exclude_def:true info use
    ) all_uses in
    let printer = print_list @@ (fun list ->
      Printf.sprintf "[%s]" (print_list Loc.debug_to_string list)
    ) in
    assert_equal ~ctxt
      ~cmp:(eq printer)
      ~printer
      ~msg:"Uses of all uses don't match!"
      expected_uses uses
  end

let mk_scope_builder_scope_loc_test contents expected_scope_locs =
  begin fun ctxt ->
    let info = Scope_builder.program (parse contents) in
    let scope_locs = IMap.elements (
      IMap.map
        (fun scope -> scope.Scope_api.Scope.loc)
        info.Scope_api.scopes
      )
    in
    let scope_locs = List.rev scope_locs in
    let printer = (fun list ->
      Printf.sprintf "[%s]" (print_list (fun (id, loc) -> Printf.sprintf "%d: %s" id (Loc.debug_to_string loc)) list)
    ) in
    assert_equal ~ctxt
      ~cmp:(eq printer)
      ~printer
      ~msg:"Uses of all uses don't match!"
      expected_scope_locs scope_locs
  end

let tests = "scope_builder" >::: [
  "let_all_uses" >:: mk_scope_builder_all_uses_test
    ("function foo(x, ...y) {\n"^
     "  let z = 0;\n"^
     "  x, y;\n"^
     "  return z;\n"^
     "}")
    [mk_loc (1, 9) (1, 12); (* foo *)
     mk_loc (1, 13) (1, 14); (* x def *)
     mk_loc (1, 19) (1, 20); (* y def *)
     mk_loc (2, 6) (2, 7); (* z def *)
     mk_loc (3, 2) (3, 3); (* x use *)
     mk_loc (3, 5) (3, 6); (* y use *)
     mk_loc (4, 9) (4, 10)]; (* z use *)
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
  "declare_export_var" >:: mk_scope_builder_all_uses_test
    "declare export var bar; bar"
    [mk_loc (1, 19) (1, 22);
     mk_loc (1, 24) (1, 27)];
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
     mk_loc (1, 38) (1, 41);
     mk_loc (1, 44) (1, 47);
     mk_loc (1, 51) (1, 52)];
  "switch" >:: mk_scope_builder_all_uses_test
    "switch ('') { case '': const foo = ''; foo; };"
    [];
    (* TODO this should be the output, but there is a bug:
    [mk_loc (1, 29) (1, 32);
     mk_loc (1, 39) (1, 42)];
    *)
  "scope_loc_function_declaration" >:: mk_scope_builder_scope_loc_test
    "function a() {};"
    [0, mk_loc (1, 0) (1, 16); (* program *)
     1, mk_loc (1, 0) (1, 16); (* program (lexical) *)
     2, mk_loc (1, 0) (1, 15); (* function params and body *)
     3, mk_loc (1, 13) (1, 15)]; (* block (lexical) *)
  "scope_loc_function_expression" >:: mk_scope_builder_scope_loc_test
    "const x = function() {};"
    [0, mk_loc (1, 0) (1, 24); (* program *)
     1, mk_loc (1, 0) (1, 24); (* program (lexical) *)
     2, mk_loc (1, 10) (1, 23); (* function name (lexical) *)
     3, mk_loc (1, 10) (1, 23); (* function params and body *)
     4, mk_loc (1, 21) (1, 23)]; (* block (lexical) *)
  "scope_loc_arrow_function" >:: mk_scope_builder_scope_loc_test
    "const x = () => 1;"
    [0, mk_loc (1, 0) (1, 18); (* program *)
     1, mk_loc (1, 0) (1, 18); (* program (lexical) *)
     2, mk_loc (1, 10) (1, 17); (* function name (lexical) *)
     3, mk_loc (1, 10) (1, 17)]; (* function params and body *)
  "scope_loc_for_in" >:: mk_scope_builder_scope_loc_test
    "for (let a in b) {}"
    [0, mk_loc (1, 0) (1, 19); (* program *)
     1, mk_loc (1, 0) (1, 19); (* program (lexical) *)
     2, mk_loc (1, 0) (1, 19); (* for in (lexical) *)
     3, mk_loc (1, 17) (1, 19)]; (* block (lexical) *)
]
