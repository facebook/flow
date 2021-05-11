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
          ( if with_locs then
            Printf.sprintf "%s, " (print_locs locs)
          else
            "" )
          (Env_builder.With_Loc.show_refinement_kind refinement))
      kvlist
  in
  Printf.sprintf "[ %s ]" (String.concat "; " strlist)

let mk_ssa_builder_test contents expected_values ctxt =
  let refined_reads = Env_builder.With_Loc.program (parse contents) in
  let expected_values = LocMap.map (fun refkind -> (LocSet.empty, refkind)) expected_values in
  assert_equal
    ~ctxt
    ~cmp:(eq (printer false))
    ~printer:(printer false)
    ~msg:"SSA values don't match!"
    expected_values
    refined_reads

let mk_ssa_builder_location_test contents expected_values ctxt =
  let refined_reads = Env_builder.With_Loc.program (parse contents) in
  assert_equal
    ~ctxt
    ~cmp:(eq (printer true))
    ~printer:(printer true)
    ~msg:"SSA values don't match!"
    expected_values
    refined_reads

let mk_refiner_of_use_test contents target_loc expected_values ctxt =
  let info = Env_builder.With_Loc.program_with_scope ~ignore_toplevel:false (parse contents) in
  let locs = Env_builder.With_Loc.refiners_of_use info target_loc in

  assert_equal
    ~ctxt
    ~cmp:(eq print_locs)
    ~printer:print_locs
    ~msg:"Refiner locations don't match!"
    expected_values
    locs

let mk_order_test contents expected_values ctxt =
  let ((_, { Flow_ast.Program.statements; _ }) as program) = parse contents in
  let (scope, _) = Provider_api.LocProviders.find_providers program in
  let info = Env_builder.With_Loc.program_with_scope ~ignore_toplevel:false program in
  let stmt_deps = Order_builder.With_Loc.calc_index_deps info scope statements in
  let deps =
    Base.List.map stmt_deps ~f:(fun (i, js) ->
        ISet.elements js |> Base.List.map ~f:(Printf.sprintf "%d -> %d" i))
    |> Base.List.concat
    |> String.concat ", "
  in
  assert_equal
    ~ctxt
    ~cmp:( = )
    ~printer:(fun x -> x)
    ~msg:"Dependencies don't match!"
    expected_values
    deps

let mk_write (pos1, pos2) = Ssa_api.Write (mk_loc pos1 pos2)

let tests =
  let open Env_builder.With_Loc in
  "env_builder"
  >::: [
         "logical_expr"
         >:: mk_ssa_builder_location_test
               "let x = null;
let y = null;
(x && (y = x)) + x"
               LocMap.(
                 empty
                 |> add (mk_loc (3, 11) (3, 12)) (LocSet.singleton (mk_loc (3, 1) (3, 2)), Truthy));
         "logical_expr_successive"
         >:: mk_ssa_builder_location_test
               "let x = null;
x && (x && x)"
               LocMap.(
                 empty
                 |> add (mk_loc (2, 6) (2, 7)) (LocSet.singleton (mk_loc (2, 0) (2, 1)), Truthy)
                 |> add
                      (mk_loc (2, 11) (2, 12))
                      ( LocSet.of_list [mk_loc (2, 0) (2, 1); mk_loc (2, 6) (2, 7)],
                        And (Truthy, Truthy) ));
         "logical_or"
         >:: mk_ssa_builder_location_test
               "let x = null;
x || x"
               LocMap.(
                 empty
                 |> add (mk_loc (2, 5) (2, 6)) (LocSet.singleton (mk_loc (2, 0) (2, 1)), Not Truthy));
         "logical_nested_right"
         >:: mk_ssa_builder_location_test
               "let x = null;
x || (x || x)"
               LocMap.(
                 empty
                 |> add (mk_loc (2, 6) (2, 7)) (LocSet.singleton (mk_loc (2, 0) (2, 1)), Not Truthy)
                 |> add
                      (mk_loc (2, 11) (2, 12))
                      ( LocSet.of_list [mk_loc (2, 0) (2, 1); mk_loc (2, 6) (2, 7)],
                        And (Not Truthy, Not Truthy) ));
         "logical_nested"
         >:: mk_ssa_builder_test
               "let x = null;
(x || x) && x"
               LocMap.(
                 empty
                 |> add (mk_loc (2, 6) (2, 7)) (Not Truthy)
                 |> add (mk_loc (2, 12) (2, 13)) (Or (Truthy, Truthy)));
         "logical_nested2"
         >:: mk_ssa_builder_test
               "let x = null;
(x && x) || (x && x)"
               LocMap.(
                 empty
                 |> add (mk_loc (2, 6) (2, 7)) Truthy
                 |> add (mk_loc (2, 13) (2, 14)) (Not (And (Truthy, Truthy)))
                 |> add (mk_loc (2, 18) (2, 19)) (And (Not (And (Truthy, Truthy)), Truthy)));
         "assignment_truthy"
         >:: mk_ssa_builder_test
               "let x = null;
(x = null) && x"
               LocMap.(empty |> add (mk_loc (2, 14) (2, 15)) Truthy);
         "eq_null"
         >:: mk_ssa_builder_test
               "let x = null;
(x == null) && x"
               LocMap.(empty |> add (mk_loc (2, 15) (2, 16)) Maybe);
         "neq_null"
         >:: mk_ssa_builder_test
               "let x = null;
(x != null) && x"
               LocMap.(empty |> add (mk_loc (2, 15) (2, 16)) (Not Maybe));
         "strict_eq_null"
         >:: mk_ssa_builder_test
               "let x = null;
(x === null) && x"
               LocMap.(empty |> add (mk_loc (2, 16) (2, 17)) Null);
         "strit_neq_null"
         >:: mk_ssa_builder_test
               "let x = null;
(x !== null) && x"
               LocMap.(empty |> add (mk_loc (2, 16) (2, 17)) (Not Null));
         "eq_undefined"
         >:: mk_ssa_builder_test
               "let x = undefined;
(x == undefined) && x"
               LocMap.(empty |> add (mk_loc (2, 20) (2, 21)) Maybe);
         "neq_undefined"
         >:: mk_ssa_builder_test
               "let x = undefined;
(x != undefined) && x"
               LocMap.(empty |> add (mk_loc (2, 20) (2, 21)) (Not Maybe));
         "strict_eq_undefined"
         >:: mk_ssa_builder_test
               "let x = undefined;
(x === undefined) && x"
               LocMap.(empty |> add (mk_loc (2, 21) (2, 22)) Undefined);
         "strit_neq_undefined"
         >:: mk_ssa_builder_test
               "let x = undefined;
(x !== undefined) && x"
               LocMap.(empty |> add (mk_loc (2, 21) (2, 22)) (Not Undefined));
         "undefined_already_bound"
         >:: mk_ssa_builder_test
               "let undefined = 3;
let x = null;
(x !== undefined) && x"
               LocMap.empty;
         "eq_void"
         >:: mk_ssa_builder_test
               "let x = undefined;
(x == void 0) && x"
               LocMap.(empty |> add (mk_loc (2, 17) (2, 18)) Maybe);
         "neq_void"
         >:: mk_ssa_builder_test
               "let x = undefined;
(x != void 0) && x"
               LocMap.(empty |> add (mk_loc (2, 17) (2, 18)) (Not Maybe));
         "strict_eq_void"
         >:: mk_ssa_builder_test
               "let x = undefined;
(x === void 0) && x"
               LocMap.(empty |> add (mk_loc (2, 18) (2, 19)) Undefined);
         "strit_neq_void"
         >:: mk_ssa_builder_test
               "let x = undefined;
(x !== void 0) && x"
               LocMap.(empty |> add (mk_loc (2, 18) (2, 19)) (Not Undefined));
         "instanceof"
         >:: mk_ssa_builder_test
               "let x = undefined;
(x instanceof Object) && x"
               LocMap.(empty |> add (mk_loc (2, 25) (2, 26)) (InstanceOf (mk_loc (2, 14) (2, 20))));
         "Array.isArray"
         >:: mk_ssa_builder_test
               "let x = undefined;
(Array.isArray(x)) && x"
               LocMap.(empty |> add (mk_loc (2, 22) (2, 23)) IsArray);
         "unary_negation"
         >:: mk_ssa_builder_test
               "let x = undefined;
(!Array.isArray(x)) && x;
!x && x;
!(x || x) && x;"
               LocMap.(
                 empty
                 |> add (mk_loc (2, 23) (2, 24)) (Not IsArray)
                 |> add (mk_loc (3, 6) (3, 7)) (Not Truthy)
                 |> add (mk_loc (4, 7) (4, 8)) (Not Truthy)
                 |> add (mk_loc (4, 13) (4, 14)) (Not (Or (Truthy, Truthy))));
         "typeof_bool"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x == \"boolean\") && x"
               LocMap.(empty |> add (mk_loc (2, 27) (2, 28)) BoolR);
         "not_typeof_bool"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x != \"boolean\") && x"
               LocMap.(empty |> add (mk_loc (2, 27) (2, 28)) (Not BoolR));
         "typeof_number"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x == \"number\") && x"
               LocMap.(empty |> add (mk_loc (2, 26) (2, 27)) NumberR);
         "not_typeof_number"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x != \"number\") && x"
               LocMap.(empty |> add (mk_loc (2, 26) (2, 27)) (Not NumberR));
         "typeof_function"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x == \"function\") && x"
               LocMap.(empty |> add (mk_loc (2, 28) (2, 29)) FunctionR);
         "not_typeof_function"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x != \"function\") && x"
               LocMap.(empty |> add (mk_loc (2, 28) (2, 29)) (Not FunctionR));
         "typeof_object"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x == \"object\") && x"
               LocMap.(empty |> add (mk_loc (2, 26) (2, 27)) ObjectR);
         "not_typeof_object"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x != \"object\") && x"
               LocMap.(empty |> add (mk_loc (2, 26) (2, 27)) (Not ObjectR));
         "typeof_string"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x == \"string\") && x"
               LocMap.(empty |> add (mk_loc (2, 26) (2, 27)) StringR);
         "not_typeof_string"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x != \"string\") && x"
               LocMap.(empty |> add (mk_loc (2, 26) (2, 27)) (Not StringR));
         "typeof_symbol"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x == \"symbol\") && x"
               LocMap.(empty |> add (mk_loc (2, 26) (2, 27)) SymbolR);
         "not_typeof_symbol"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x != \"symbol\") && x"
               LocMap.(empty |> add (mk_loc (2, 26) (2, 27)) (Not SymbolR));
         "typeof_bool_template"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x == `boolean`) && x"
               LocMap.(empty |> add (mk_loc (2, 27) (2, 28)) BoolR);
         "not_typeof_bool_template"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x != `boolean`) && x"
               LocMap.(empty |> add (mk_loc (2, 27) (2, 28)) (Not BoolR));
         "typeof_number_template"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x == `number`) && x"
               LocMap.(empty |> add (mk_loc (2, 26) (2, 27)) NumberR);
         "not_typeof_number_template"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x != `number`) && x"
               LocMap.(empty |> add (mk_loc (2, 26) (2, 27)) (Not NumberR));
         "typeof_function_template"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x == `function`) && x"
               LocMap.(empty |> add (mk_loc (2, 28) (2, 29)) FunctionR);
         "not_typeof_function_template"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x != `function`) && x"
               LocMap.(empty |> add (mk_loc (2, 28) (2, 29)) (Not FunctionR));
         "typeof_object_template"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x == `object`) && x"
               LocMap.(empty |> add (mk_loc (2, 26) (2, 27)) ObjectR);
         "not_typeof_object_template"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x != `object`) && x"
               LocMap.(empty |> add (mk_loc (2, 26) (2, 27)) (Not ObjectR));
         "typeof_string_template"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x == `string`) && x"
               LocMap.(empty |> add (mk_loc (2, 26) (2, 27)) StringR);
         "not_typeof_string_template"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x != `string`) && x"
               LocMap.(empty |> add (mk_loc (2, 26) (2, 27)) (Not StringR));
         "typeof_symbol_template"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x == `symbol`) && x"
               LocMap.(empty |> add (mk_loc (2, 26) (2, 27)) SymbolR);
         "not_typeof_symbol_template"
         >:: mk_ssa_builder_test
               "let x = undefined;
(typeof x != `symbol`) && x"
               LocMap.(empty |> add (mk_loc (2, 26) (2, 27)) (Not SymbolR));
         "singleton_bool"
         >:: mk_ssa_builder_test
               "let x = undefined;
(x === true) && x"
               LocMap.(empty |> add (mk_loc (2, 16) (2, 17)) (SingletonBoolR true));
         "singleton_str"
         >:: mk_ssa_builder_test
               "let x = undefined;
(x === \"str\") && x"
               LocMap.(empty |> add (mk_loc (2, 17) (2, 18)) (SingletonStrR "str"));
         "singleton_str_template"
         >:: mk_ssa_builder_test
               "let x = undefined;
(x === `str`) && x"
               LocMap.(empty |> add (mk_loc (2, 17) (2, 18)) (SingletonStrR "str"));
         "singleton_num"
         >:: mk_ssa_builder_test
               "let x = undefined;
(x === 3) && x"
               LocMap.(empty |> add (mk_loc (2, 13) (2, 14)) (SingletonNumR "3"));
         "singleton_num_neg"
         >:: mk_ssa_builder_test
               "let x = undefined;
(x === -3) && x"
               LocMap.(empty |> add (mk_loc (2, 14) (2, 15)) (SingletonNumR "-3"));
         "refiner_of_use_1"
         >:: mk_refiner_of_use_test
               "let x = 42;
(x && x)"
               (mk_loc (2, 6) (2, 7))
               (LocSet.of_list [mk_loc (1, 4) (1, 5); mk_loc (2, 1) (2, 2)]);
         "refiner_of_use_2"
         >:: mk_refiner_of_use_test
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
         >:: mk_refiner_of_use_test
               "
function invalidate() {}

var x = 10;
invalidate();
x;
x = 20;
"
               (mk_loc (6, 0) (6, 1))
               (LocSet.of_list [mk_loc (4, 4) (4, 5)]);
         "nonconstlike_havoc"
         >:: mk_refiner_of_use_test
               "
function invalidate() { x = null; }

var x = 10;
invalidate();
x;
x = 20;
"
               (mk_loc (6, 0) (6, 1))
               (LocSet.of_list [mk_loc (2, 24) (2, 25); mk_loc (4, 4) (4, 5); mk_loc (7, 0) (7, 1)]);
         "order1" >:: mk_order_test "let x = 42;
x;" "1 -> 0";
         "order2" >:: mk_order_test "function f() { g() }
function g(){ f() }" "0 -> 1, 1 -> 0";
         "order3" >:: mk_order_test "
let x = null;
x = 42;
x = 100;
         " "2 -> 0, 2 -> 1";
       ]
