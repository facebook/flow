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

let print_locs locs =
  LocSet.elements locs
  |> List.map Loc.debug_to_string
  |> String.concat ", "
  |> Printf.sprintf "[%s]"

let printer with_locs locmap =
  let kvlist = LocMap.bindings locmap in
  let strlist =
    Base.List.map
      ~f:(fun (read_loc, (locs, refinement)) ->
        Printf.sprintf
          "%s => { %s%s }"
          (Loc.debug_to_string read_loc)
          (if with_locs then
            Printf.sprintf "%s, " (print_locs locs)
          else
            "")
          (Env_builder.With_Loc.show_refinement_kind refinement))
      kvlist
  in
  Printf.sprintf "[ %s ]" (String.concat "; " strlist)

let mk_sources_test contents expected_values ctxt =
  let program = parse contents in
  let (_, info) = Env_builder.With_Loc.program_with_scope ~ignore_toplevel:false program in
  let printer locmap =
    let kvlist = LocMap.bindings locmap in
    let strlist =
      Base.List.map
        ~f:(fun (read_loc, set) ->
          Printf.sprintf
            "%s => { %s }"
            (Loc.debug_to_string read_loc)
            (LocSet.elements set |> Base.List.map ~f:Loc.debug_to_string |> String.concat ", "))
        kvlist
    in
    Printf.sprintf "[ %s ]" (String.concat "; " strlist)
  in
  let use_kinds = Env_builder.With_Loc.source_bindings info in
  assert_equal
    ~ctxt
    ~cmp:(eq printer)
    ~printer
    ~msg:"SSA values don't match!"
    expected_values
    use_kinds

let mk_source_of_use_test contents target_loc expected_values ctxt =
  let (_, info) = Env_builder.With_Loc.program_with_scope ~ignore_toplevel:false (parse contents) in
  let locs = Env_builder.With_Loc.sources_of_use info target_loc in
  assert_equal
    ~ctxt
    ~cmp:(eq print_locs)
    ~printer:print_locs
    ~msg:"Refiner locations don't match!"
    expected_values
    locs

let mk_order_test contents expected_values ctxt =
  let ((_, { Flow_ast.Program.statements; _ }) as program) = parse contents in
  let (_, info) = Env_builder.With_Loc.program_with_scope ~ignore_toplevel:false program in
  let deps = Order_builder.With_Loc.mk_order info statements in
  let deps_string =
    Base.List.map
      ~f:(function
        | (i, []) -> string_of_int i
        | (i, js) ->
          Utils_js.spf "cycle(%d,%s)" i (Base.List.map ~f:string_of_int js |> String.concat ","))
      deps
    |> String.concat " -> "
  in
  assert_equal
    ~ctxt
    ~cmp:( = )
    ~printer:(fun x -> x)
    ~msg:"Dependencies don't match!"
    expected_values
    deps_string

let tests =
  [
    "refiner_of_use_1"
    >:: mk_source_of_use_test
          "let x = 42;
(x && x)"
          (mk_loc (2, 6) (2, 7))
          (LocSet.of_list [mk_loc (1, 4) (1, 5); mk_loc (2, 1) (2, 2)]);
    "refiner_of_use_2"
    >:: mk_source_of_use_test
          "
function f() {
  let x = 42;
  if (condition) {
    x = false;
  }
  (x !== true && x);
}"
          (mk_loc (7, 17) (7, 18))
          (LocSet.of_list [mk_loc (3, 6) (3, 7); mk_loc (5, 4) (5, 5); mk_loc (7, 3) (7, 13)]);
    "constlike_havoc"
    >:: mk_source_of_use_test
          "
function invalidate() {}

var x = 10;
x = 4;
invalidate();
x;
x = 20;
"
          (mk_loc (7, 0) (7, 1))
          (LocSet.of_list [mk_loc (5, 0) (5, 1)]);
    "nonconstlike_havoc"
    >:: mk_source_of_use_test
          "
function invalidate() { x = null; }

var x = 10;
x = 4;
invalidate();
x;
x = 20;
"
          (mk_loc (7, 0) (7, 1))
          (LocSet.of_list [mk_loc (4, 4) (4, 5)]);
    "before"
    >:: mk_source_of_use_test
          "
function f() { x };
var x = 10;
"
          (mk_loc (2, 15) (2, 16))
          (LocSet.of_list [mk_loc (3, 4) (3, 5)]);
    "merge"
    >:: mk_source_of_use_test
          "
function f() { x = 42 };
var x = 10;
x = 42;
f();
(x !== 'number' && x);
"
          (mk_loc (6, 19) (6, 20))
          (LocSet.of_list [mk_loc (3, 4) (3, 5); mk_loc (6, 1) (6, 15)]);
    "merge_nonhavoc"
    >:: mk_source_of_use_test
          "
function f() { };
var x = 10;
x = 42;
f();
(x !== 'number' && x);
"
          (mk_loc (6, 19) (6, 20))
          (LocSet.of_list [mk_loc (4, 0) (4, 1); mk_loc (6, 1) (6, 15)]);
    (* These tests are intentionally commented out in this diff so that Jordan can
     * address handling globals in the EnvBuilder given the new way of modeling
     * refinements as writes
    "global1" >:: mk_source_of_use_test "
x
" (mk_loc (2, 0) (2, 1)) (LocSet.of_list []);
    "global2"
    >:: mk_source_of_use_test
          "
(x && x)
"
          (mk_loc (2, 6) (2, 7))
          (LocSet.of_list [mk_loc (2, 1) (2, 2)]);
    "global_havoc"
    >:: mk_source_of_use_test
          "
(x && function() { x })
"
          (mk_loc (2, 19) (2, 20))
          (LocSet.of_list []);
    "global_merge"
    >:: mk_source_of_use_test "
(x || foo) && x;
" (mk_loc (2, 19) (2, 20)) (LocSet.of_list []); *)
    "order1" >:: mk_order_test "let x = 42;
x;" "0 -> 1";
    "order2" >:: mk_order_test "function f() { g() }
function g(){ f() }" "cycle(0,1)";
    "order3" >:: mk_order_test "
let x = null;
x = 42;
x = 100;
         " "0 -> 1 -> 2";
    "order4"
    >:: mk_order_test "
function f() {
  x;
}
var x = 42;
var y = f();
         " "1 -> 0 -> 2";
    "order5"
    >:: mk_order_test
          "
function havoc() {
  x = 10;
}
var x = null;
havoc();
x;
x = 42;"
          "1 -> 4 -> 0 -> 2 -> 3";
  ]

let general_or_specific_tests =
  [
    "no_havocs"
    >:: mk_sources_test
          "let x = null;
(typeof x == `boolean`) && x"
          (let write_x = mk_loc (1, 4) (1, 5) in
           let refine_x = mk_loc (2, 1) (2, 22) in
           LocMap.(
             empty
             |> add (mk_loc (2, 8) (2, 9)) (LocSet.of_list [write_x])
             |> add (mk_loc (2, 27) (2, 28)) (LocSet.of_list [write_x; refine_x])));
    "havoc_call"
    >:: mk_sources_test
          "let x = null;
function invalidate() { x = 42; }
x;
invalidate();
x"
          (let write_x = mk_loc (1, 4) (1, 5) in
           let write_invalidate = mk_loc (2, 9) (2, 19) in
           LocMap.(
             empty
             |> add (mk_loc (3, 0) (3, 1)) (LocSet.of_list [write_x])
             |> add (mk_loc (4, 0) (4, 10)) (LocSet.of_list [write_invalidate])
             |> add (mk_loc (5, 0) (5, 1)) (LocSet.of_list [mk_loc (1, 4) (1, 5)])));
    "enter_function_var_scope"
    >:: mk_sources_test
          "let x = null;
function new_scope() {
  x;
  x = 42;
}
x;
new_scope();
x"
          (let write_x = mk_loc (1, 4) (1, 5) in
           let write_new_scope = mk_loc (2, 9) (2, 18) in
           LocMap.(
             empty
             |> add (mk_loc (3, 2) (3, 3)) (LocSet.of_list [mk_loc (1, 4) (1, 5)])
             |> add (mk_loc (6, 0) (6, 1)) (LocSet.of_list [write_x])
             |> add (mk_loc (7, 0) (7, 9)) (LocSet.of_list [write_new_scope])
             |> add (mk_loc (8, 0) (8, 1)) (LocSet.of_list [mk_loc (1, 4) (1, 5)])));
    "control_flow"
    >:: mk_sources_test
          "let x = null;
function new_scope() {
  x;
}
if (true) {
  x;
  x = 7;
} else {
  new_scope();
}
x"
          (let write_x = mk_loc (1, 4) (1, 5) in
           let reassign_x = mk_loc (7, 2) (7, 3) in
           let write_new_scope = mk_loc (2, 9) (2, 18) in
           LocMap.(
             empty
             |> add (mk_loc (3, 2) (3, 3)) (LocSet.of_list [write_x; reassign_x])
             |> add (mk_loc (6, 2) (6, 3)) (LocSet.of_list [write_x])
             |> add (mk_loc (9, 2) (9, 11)) (LocSet.of_list [write_new_scope])
             |> add (mk_loc (11, 0) (11, 1)) (LocSet.of_list [write_x; reassign_x])));
  ]

let tests = "env_builder" >::: tests @ general_or_specific_tests
