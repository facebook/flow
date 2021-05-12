(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Test_utils
module LocMap = Loc_collections.LocMap
module LocSet = Loc_collections.LocSet

let print_locs locs =
  LocSet.elements locs
  |> List.map (Loc.debug_to_string ~include_source:false)
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
  Printf.printf "[ %s ]" (String.concat "; " strlist)

(* TODO: ocamlformat mangles the ppx syntax. *)
[@@@ocamlformat "disable=true"]

let print_ssa_test contents =
  let refined_reads = Env_builder.With_Loc.program (parse contents) in
  printer true refined_reads

let%expect_test "logical_expr" =
  print_ssa_test {|let x = null;
let y = null;
(x && (y = x)) + x|};
  [%expect {| [ (3, 11) to (3, 12) => { [(3, 1) to (3, 2)], Truthy } ] |}]

let%expect_test "logical_expr_successive" =
  print_ssa_test {|let x = null;
x && (x && x)|};
  [%expect {| [ (2, 6) to (2, 7) => { [(2, 0) to (2, 1)], Truthy }; (2, 11) to (2, 12) => { [(2, 0) to (2, 1), (2, 6) to (2, 7)], (And (Truthy, Truthy)) } ] |}]

let%expect_test "logical_or" =
  print_ssa_test {|let x = null;
x || x|};
  [%expect {| [ (2, 5) to (2, 6) => { [(2, 0) to (2, 1)], (Not Truthy) } ] |}]

let%expect_test "logical_nc_and" =
  print_ssa_test {|let x = null;
(x ?? x) && x|};
  [%expect {| [ (2, 6) to (2, 7) => { [(2, 1) to (2, 2)], Maybe }; (2, 12) to (2, 13) => { [(2, 1) to (2, 2), (2, 6) to (2, 7)], (Or ((And (Truthy, (Not Maybe))), Truthy)) } ] |}]

let%expect_test "logical_nested_right" =
  print_ssa_test {|let x = null;
x || (x || x)|};
  [%expect {| [ (2, 6) to (2, 7) => { [(2, 0) to (2, 1)], (Not Truthy) }; (2, 11) to (2, 12) => { [(2, 0) to (2, 1), (2, 6) to (2, 7)], (And ((Not Truthy), (Not Truthy))) } ] |}]

let%expect_test "logical_nested" =
  print_ssa_test {|let x = null;
(x || x) && x|};
  [%expect {| [ (2, 6) to (2, 7) => { [(2, 1) to (2, 2)], (Not Truthy) }; (2, 12) to (2, 13) => { [(2, 1) to (2, 2), (2, 6) to (2, 7)], (Or (Truthy, Truthy)) } ] |}]

let%expect_test "logical_nested2" =
  print_ssa_test {|let x = null;
(x && x) || (x && x)|};
  [%expect {| [ (2, 6) to (2, 7) => { [(2, 1) to (2, 2)], Truthy }; (2, 13) to (2, 14) => { [(2, 1) to (2, 2), (2, 6) to (2, 7)], (Not (And (Truthy, Truthy))) }; (2, 18) to (2, 19) => { [(2, 1) to (2, 2), (2, 6) to (2, 7), (2, 13) to (2, 14)], (And ((Not (And (Truthy, Truthy))), Truthy)) } ] |}]

let%expect_test "assignment_truthy" =
  print_ssa_test {|let x = null;
(x = null) && x|};
  [%expect {| [ (2, 14) to (2, 15) => { [(2, 1) to (2, 9)], Truthy } ] |}]

let%expect_test "eq_null" =
  print_ssa_test {|let x = null;
(x == null) && x|};
  [%expect {| [ (2, 15) to (2, 16) => { [(2, 1) to (2, 10)], Maybe } ] |}]

let%expect_test "neq_null" =
  print_ssa_test {|let x = null;
(x != null) && x|};
  [%expect {| [ (2, 15) to (2, 16) => { [(2, 1) to (2, 10)], (Not Maybe) } ] |}]

let%expect_test "strict_eq_null" =
  print_ssa_test {|let x = null;
(x === null) && x|};
  [%expect {| [ (2, 16) to (2, 17) => { [(2, 1) to (2, 11)], Null } ] |}]

let%expect_test "strict_neq_null" =
  print_ssa_test {|let x = null;
(x !== null) && x|};
  [%expect {| [ (2, 16) to (2, 17) => { [(2, 1) to (2, 11)], (Not Null) } ] |}]

let%expect_test "eq_undefined" =
  print_ssa_test {|let x = undefined;
(x == undefined) && x|};
  [%expect {| [ (2, 20) to (2, 21) => { [(2, 1) to (2, 15)], Maybe } ] |}]

let%expect_test "neq_undefined" =
  print_ssa_test {|let x = undefined;
(x != undefined) && x|};
  [%expect {| [ (2, 20) to (2, 21) => { [(2, 1) to (2, 15)], (Not Maybe) } ] |}]

let%expect_test "strict_eq_undefined" =
  print_ssa_test {|let x = undefined;
(x === undefined) && x|};
  [%expect {| [ (2, 21) to (2, 22) => { [(2, 1) to (2, 16)], Undefined } ] |}]

let%expect_test "strict_neq_undefined" =
  print_ssa_test {|let x = undefined;
(x !== undefined) && x|};
  [%expect {| [ (2, 21) to (2, 22) => { [(2, 1) to (2, 16)], (Not Undefined) } ] |}]

let%expect_test "undefined_already_bound" =
  print_ssa_test {|let undefined = 3;
let x = null;
(x !== undefined) && x|};
  [%expect {| [  ] |}]

let%expect_test "eq_void" =
  print_ssa_test {|let x = undefined;
(x == void 0) && x|};
  [%expect {| [ (2, 17) to (2, 18) => { [(2, 1) to (2, 12)], Maybe } ] |}]

let%expect_test "neq_void" =
  print_ssa_test {|let x = undefined;
(x != void 0) && x|};
  [%expect {| [ (2, 17) to (2, 18) => { [(2, 1) to (2, 12)], (Not Maybe) } ] |}]

let%expect_test "strict_eq_void" =
  print_ssa_test {|let x = undefined;
(x === void 0) && x|};
  [%expect {| [ (2, 18) to (2, 19) => { [(2, 1) to (2, 13)], Undefined } ] |}]

let%expect_test "strict_neq_void" =
  print_ssa_test {|let x = undefined;
(x !== void 0) && x|};
  [%expect {| [ (2, 18) to (2, 19) => { [(2, 1) to (2, 13)], (Not Undefined) } ] |}]

let%expect_test "instanceof" =
  print_ssa_test {|let x = undefined;
(x instanceof Object) && x|};
  [%expect {|
    [ (2, 25) to (2, 26) => { [(2, 1) to (2, 20)], (InstanceOf
       { Loc.source = None; start = { Loc.line = 2; column = 14 };
         _end = { Loc.line = 2; column = 20 } }) } ] |}]

let%expect_test "Array.isArray" =
  print_ssa_test {|let x = undefined;
(Array.isArray(x)) && x|};
  [%expect {| [ (2, 22) to (2, 23) => { [(2, 1) to (2, 17)], IsArray } ] |}]

let%expect_test "unary_negation" =
  print_ssa_test {|let x = undefined;
(!Array.isArray(x)) && x;
!x && x;
!(x || x) && x;|};
  [%expect {| [ (2, 23) to (2, 24) => { [(2, 2) to (2, 18)], (Not IsArray) }; (3, 6) to (3, 7) => { [(3, 1) to (3, 2)], (Not Truthy) }; (4, 7) to (4, 8) => { [(4, 2) to (4, 3)], (Not Truthy) }; (4, 13) to (4, 14) => { [(4, 2) to (4, 3), (4, 7) to (4, 8)], (Not (Or (Truthy, Truthy))) } ] |}]

let%expect_test "typeof_bool" =
  print_ssa_test {|let x = undefined;
(typeof x == "boolean") && x|};
  [%expect {| [ (2, 27) to (2, 28) => { [(2, 1) to (2, 22)], BoolR } ] |}]

let%expect_test "not_typeof_bool" =
  print_ssa_test {|let x = undefined;
(typeof x != "boolean") && x|};
  [%expect {| [ (2, 27) to (2, 28) => { [(2, 1) to (2, 22)], (Not BoolR) } ] |}]

let%expect_test "typeof_number" =
  print_ssa_test {|let x = undefined;
(typeof x == "number") && x|};
  [%expect {| [ (2, 26) to (2, 27) => { [(2, 1) to (2, 21)], NumberR } ] |}]

let%expect_test "not_typeof_number" =
  print_ssa_test {|let x = undefined;
(typeof x != "number") && x|};
  [%expect {| [ (2, 26) to (2, 27) => { [(2, 1) to (2, 21)], (Not NumberR) } ] |}]

let%expect_test "typeof_function" =
  print_ssa_test {|let x = undefined;
(typeof x == "function") && x|};
  [%expect {| [ (2, 28) to (2, 29) => { [(2, 1) to (2, 23)], FunctionR } ] |}]

let%expect_test "not_typeof_function" =
  print_ssa_test {|let x = undefined;
(typeof x != "function") && x|};
  [%expect {| [ (2, 28) to (2, 29) => { [(2, 1) to (2, 23)], (Not FunctionR) } ] |}]

let%expect_test "typeof_object" =
  print_ssa_test {|let x = undefined;
(typeof x == "object") && x|};
  [%expect {| [ (2, 26) to (2, 27) => { [(2, 1) to (2, 21)], ObjectR } ] |}]

let%expect_test "not_typeof_object" =
  print_ssa_test {|let x = undefined;
(typeof x != "object") && x|};
  [%expect {| [ (2, 26) to (2, 27) => { [(2, 1) to (2, 21)], (Not ObjectR) } ] |}]

let%expect_test "typeof_string" =
  print_ssa_test {|let x = undefined;
(typeof x == "string") && x|};
  [%expect {| [ (2, 26) to (2, 27) => { [(2, 1) to (2, 21)], StringR } ] |}]

let%expect_test "not_typeof_string" =
  print_ssa_test {|let x = undefined;
(typeof x != "string") && x|};
  [%expect {| [ (2, 26) to (2, 27) => { [(2, 1) to (2, 21)], (Not StringR) } ] |}]

let%expect_test "typeof_symbol" =
  print_ssa_test {|let x = undefined;
(typeof x == "symbol") && x|};
  [%expect {| [ (2, 26) to (2, 27) => { [(2, 1) to (2, 21)], SymbolR } ] |}]

let%expect_test "not_typeof_symbol" =
  print_ssa_test {|let x = undefined;
(typeof x != "symbol") && x|};
  [%expect {| [ (2, 26) to (2, 27) => { [(2, 1) to (2, 21)], (Not SymbolR) } ] |}]

let%expect_test "typeof_bool_template" =
  print_ssa_test {|let x = undefined;
(typeof x == `boolean`) && x|};
  [%expect {| [ (2, 27) to (2, 28) => { [(2, 1) to (2, 22)], BoolR } ] |}]

let%expect_test "not_typeof_bool_template" =
  print_ssa_test {|let x = undefined;
(typeof x != `boolean`) && x|};
  [%expect {| [ (2, 27) to (2, 28) => { [(2, 1) to (2, 22)], (Not BoolR) } ] |}]

let%expect_test "typeof_number_template" =
  print_ssa_test {|let x = undefined;
(typeof x == `number`) && x|};
  [%expect {| [ (2, 26) to (2, 27) => { [(2, 1) to (2, 21)], NumberR } ] |}]

let%expect_test "not_typeof_number_template" =
  print_ssa_test {|let x = undefined;
(typeof x != `number`) && x|};
  [%expect {| [ (2, 26) to (2, 27) => { [(2, 1) to (2, 21)], (Not NumberR) } ] |}]

let%expect_test "typeof_function_template" =
  print_ssa_test {|let x = undefined;
(typeof x == `function`) && x|};
  [%expect {| [ (2, 28) to (2, 29) => { [(2, 1) to (2, 23)], FunctionR } ] |}]

let%expect_test "not_typeof_function_template" =
  print_ssa_test {|let x = undefined;
(typeof x != `function`) && x|};
  [%expect {| [ (2, 28) to (2, 29) => { [(2, 1) to (2, 23)], (Not FunctionR) } ] |}]

let%expect_test "typeof_object_template" =
  print_ssa_test {|let x = undefined;
(typeof x == `object`) && x|};
  [%expect {| [ (2, 26) to (2, 27) => { [(2, 1) to (2, 21)], ObjectR } ] |}]

let%expect_test "not_typeof_object_template" =
  print_ssa_test {|let x = undefined;
(typeof x != `object`) && x|};
  [%expect {| [ (2, 26) to (2, 27) => { [(2, 1) to (2, 21)], (Not ObjectR) } ] |}]

let%expect_test "typeof_string_template" =
  print_ssa_test {|let x = undefined;
(typeof x == `string`) && x|};
  [%expect {| [ (2, 26) to (2, 27) => { [(2, 1) to (2, 21)], StringR } ] |}]

let%expect_test "not_typeof_string_template" =
  print_ssa_test {|let x = undefined;
(typeof x != `string`) && x|};
  [%expect {| [ (2, 26) to (2, 27) => { [(2, 1) to (2, 21)], (Not StringR) } ] |}]

let%expect_test "typeof_symbol_template" =
  print_ssa_test {|let x = undefined;
(typeof x == `symbol`) && x|};
  [%expect {| [ (2, 26) to (2, 27) => { [(2, 1) to (2, 21)], SymbolR } ] |}]

let%expect_test "not_typeof_symbol_template" =
  print_ssa_test {|let x = undefined;
(typeof x != `symbol`) && x|};
  [%expect {| [ (2, 26) to (2, 27) => { [(2, 1) to (2, 21)], (Not SymbolR) } ] |}]

let%expect_test "singleton_bool" =
  print_ssa_test {|let x = undefined;
(x === true) && x|};
  [%expect {| [ (2, 16) to (2, 17) => { [(2, 1) to (2, 11)], (SingletonBoolR true) } ] |}]

let%expect_test "singleton_str" =
  print_ssa_test {|let x = undefined;
(x === "str") && x|};
  [%expect{| [ (2, 17) to (2, 18) => { [(2, 1) to (2, 12)], (SingletonStrR "str") } ] |}]

let%expect_test "singleton_str_template" =
  print_ssa_test {|let x = undefined;
(x === `str`) && x|};
  [%expect {| [ (2, 17) to (2, 18) => { [(2, 1) to (2, 12)], (SingletonStrR "str") } ] |}]

let%expect_test "singleton_num" =
  print_ssa_test {|let x = undefined;
(x === 3) && x|};
  [%expect {| [ (2, 13) to (2, 14) => { [(2, 1) to (2, 8)], (SingletonNumR "3") } ] |}]

let%expect_test "singleton_num_neg" =
  print_ssa_test {|let x = undefined;
(x === -3) && x|};
  [%expect {| [ (2, 14) to (2, 15) => { [(2, 1) to (2, 9)], (SingletonNumR "-3") } ] |}]
