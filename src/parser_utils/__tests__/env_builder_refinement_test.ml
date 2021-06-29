(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Test_utils
module LocMap = Loc_collections.LocMap
module LocSet = Loc_collections.LocSet

let print_values refinement_of_id =
  let open Env_builder.With_Loc.Env_api in
  let rec print_value write_loc =
    match write_loc with
    | Uninitialized -> "(uninitialized)"
    | Write reason ->
      let loc = Reason.poly_loc_of_reason reason in
      Utils_js.spf
        "%s: (%s)"
        (L.debug_to_string loc)
        Reason.(desc_of_reason reason |> string_of_desc)
    | Refinement { refinement_id; writes } ->
      let refinement = refinement_of_id refinement_id in
      let refinement_str =
        Env_builder.With_Loc.show_refinement_kind_without_locs (snd refinement)
      in
      let writes_str = String.concat "," (List.map print_value writes) in
      Printf.sprintf "{refinement = %s; writes = %s}" refinement_str writes_str
  in
  fun values ->
    let kvlist = L.LMap.bindings values in
    let strlist =
      Base.List.map
        ~f:(fun (read_loc, write_locs) ->
          Printf.sprintf
            "%s => { %s }"
            (L.debug_to_string read_loc)
            (String.concat ", " @@ Base.List.map ~f:print_value write_locs))
        kvlist
    in
    Printf.printf "[ %s ]" (String.concat "; " strlist)

(* TODO: ocamlformat mangles the ppx syntax. *)
[@@@ocamlformat "disable=true"]

let print_ssa_test contents =
  let refined_reads, refinement_of_id = Env_builder.With_Loc.program (parse contents) in
  print_values refinement_of_id refined_reads

let%expect_test "logical_expr" =
  print_ssa_test {|let x = null;
let y = null;
(x && (y = x)) + x|};
  [%expect {|
    [ (3, 1) to (3, 2) => { (1, 4) to (1, 5): (`x`) }; (3, 11) to (3, 12) => { {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)} }; (3, 17) to (3, 18) => { (1, 4) to (1, 5): (`x`) } ] |}]

let%expect_test "logical_expr_successive" =
  print_ssa_test {|let x = null;
x && (x && x)|};
  [%expect {|
    [ (2, 0) to (2, 1) => { (1, 4) to (1, 5): (`x`) }; (2, 6) to (2, 7) => { {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)} }; (2, 11) to (2, 12) => { {refinement = Truthy; writes = {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}} } ] |}]

let%expect_test "logical_or" =
  print_ssa_test {|let x = null;
x || x|};
  [%expect {|
    [ (2, 0) to (2, 1) => { (1, 4) to (1, 5): (`x`) }; (2, 5) to (2, 6) => { {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "logical_nc_and" =
  print_ssa_test {|let x = null;
(x ?? x) && x|};
  [%expect{| [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 6) to (2, 7) => { {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)} }; (2, 12) to (2, 13) => { {refinement = Or (And (Not (Maybe), Truthy), Truthy); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "logical_nc_no_key" =
  print_ssa_test {|let x = null;
((x != null) ?? x) && x|};
  [%expect {|
    [ (2, 2) to (2, 3) => { (1, 4) to (1, 5): (`x`) }; (2, 16) to (2, 17) => { (1, 4) to (1, 5): (`x`) }; (2, 22) to (2, 23) => { {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "logical_nested_right" =
  print_ssa_test {|let x = null;
x || (x || x)|};
  [%expect {|
    [ (2, 0) to (2, 1) => { (1, 4) to (1, 5): (`x`) }; (2, 6) to (2, 7) => { {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)} }; (2, 11) to (2, 12) => { {refinement = Not (Truthy); writes = {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}} } ] |}]

let%expect_test "logical_nested" =
  print_ssa_test {|let x = null;
(x || (x != null)) && x|};
  [%expect {|
    [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 7) to (2, 8) => { {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)} }; (2, 22) to (2, 23) => { {refinement = Or (Truthy, Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "logical_nested2" =
  print_ssa_test {|let x = null;
(x && x) || (x && x)|};
  [%expect {|
    [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 6) to (2, 7) => { {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)} }; (2, 13) to (2, 14) => { {refinement = Not (And (Truthy, Truthy)); writes = (1, 4) to (1, 5): (`x`)} }; (2, 18) to (2, 19) => { {refinement = Truthy; writes = {refinement = Not (And (Truthy, Truthy)); writes = (1, 4) to (1, 5): (`x`)}} } ] |}]

let%expect_test "assignment_truthy" =
  print_ssa_test {|let x = null;
(x = null) && x|};
  [%expect {|
    [ (2, 14) to (2, 15) => { {refinement = Truthy; writes = (2, 1) to (2, 2): (`x`)} } ] |}]

let%expect_test "eq_null" =
  print_ssa_test {|let x = null;
(x == null) && x|};
  [%expect {|
    [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 15) to (2, 16) => { {refinement = Maybe; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "neq_null" =
  print_ssa_test {|let x = null;
(x != null) && x|};
  [%expect {|
    [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 15) to (2, 16) => { {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "strict_eq_null" =
  print_ssa_test {|let x = null;
(x === null) && x|};
  [%expect {|
    [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 16) to (2, 17) => { {refinement = Null; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "strict_neq_null" =
  print_ssa_test {|let x = null;
(x !== null) && x|};
  [%expect {|
    [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 16) to (2, 17) => { {refinement = Not (Null); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "eq_undefined" =
  print_ssa_test {|let x = undefined;
(x == undefined) && x|};
  [%expect {|
    [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 20) to (2, 21) => { {refinement = Maybe; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "neq_undefined" =
  print_ssa_test {|let x = undefined;
(x != undefined) && x|};
  [%expect {|
    [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 20) to (2, 21) => { {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "strict_eq_undefined" =
  print_ssa_test {|let x = undefined;
(x === undefined) && x|};
  [%expect {|
    [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 21) to (2, 22) => { {refinement = Undefined; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "strict_neq_undefined" =
  print_ssa_test {|let x = undefined;
(x !== undefined) && x|};
  [%expect {|
    [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 21) to (2, 22) => { {refinement = Not (Undefined); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "undefined_already_bound" =
  print_ssa_test {|let undefined = 3;
let x = null;
(x !== undefined) && x|};
  [%expect {| [ (3, 1) to (3, 2) => { (2, 4) to (2, 5): (`x`) }; (3, 7) to (3, 16) => { (1, 4) to (1, 13): (`undefined`) }; (3, 21) to (3, 22) => { (2, 4) to (2, 5): (`x`) } ] |}]

let%expect_test "eq_void" =
  print_ssa_test {|let x = undefined;
(x == void 0) && x|};
  [%expect {|
    [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 17) to (2, 18) => { {refinement = Maybe; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "neq_void" =
  print_ssa_test {|let x = undefined;
(x != void 0) && x|};
  [%expect {|
    [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 17) to (2, 18) => { {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "strict_eq_void" =
  print_ssa_test {|let x = undefined;
(x === void 0) && x|};
  [%expect {|
    [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 18) to (2, 19) => { {refinement = Undefined; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "strict_neq_void" =
  print_ssa_test {|let x = undefined;
(x !== void 0) && x|};
  [%expect {|
    [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 18) to (2, 19) => { {refinement = Not (Undefined); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "instanceof" =
  print_ssa_test {|let x = undefined;
(x instanceof Object) && x|};
  [%expect {|
    [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 25) to (2, 26) => { {refinement = instanceof; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "Array.isArray" =
  print_ssa_test {|let x = undefined;
(Array.isArray(x)) && x|};
  [%expect {|
    [ (2, 15) to (2, 16) => { (1, 4) to (1, 5): (`x`) }; (2, 22) to (2, 23) => { {refinement = isArray; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "unary_negation" =
  print_ssa_test {|let x = undefined;
(!Array.isArray(x)) && x;
!x && x;
!(x || x) && x;|};
  [%expect {|
    [ (2, 16) to (2, 17) => { (1, 4) to (1, 5): (`x`) }; (2, 23) to (2, 24) => { {refinement = Not (isArray); writes = (1, 4) to (1, 5): (`x`)} }; (3, 1) to (3, 2) => { (1, 4) to (1, 5): (`x`) }; (3, 6) to (3, 7) => { {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)} }; (4, 2) to (4, 3) => { (1, 4) to (1, 5): (`x`) }; (4, 7) to (4, 8) => { {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)} }; (4, 13) to (4, 14) => { {refinement = Not (Or (Truthy, Truthy)); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "typeof_bool" =
  print_ssa_test {|let x = undefined;
(typeof x == "boolean") && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 27) to (2, 28) => { {refinement = bool; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "not_typeof_bool" =
  print_ssa_test {|let x = undefined;
(typeof x != "boolean") && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 27) to (2, 28) => { {refinement = Not (bool); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "typeof_number" =
  print_ssa_test {|let x = undefined;
(typeof x == "number") && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 26) to (2, 27) => { {refinement = number; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "not_typeof_number" =
  print_ssa_test {|let x = undefined;
(typeof x != "number") && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 26) to (2, 27) => { {refinement = Not (number); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "typeof_function" =
  print_ssa_test {|let x = undefined;
(typeof x == "function") && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 28) to (2, 29) => { {refinement = function; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "not_typeof_function" =
  print_ssa_test {|let x = undefined;
(typeof x != "function") && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 28) to (2, 29) => { {refinement = Not (function); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "typeof_object" =
  print_ssa_test {|let x = undefined;
(typeof x == "object") && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 26) to (2, 27) => { {refinement = object; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "not_typeof_object" =
  print_ssa_test {|let x = undefined;
(typeof x != "object") && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 26) to (2, 27) => { {refinement = Not (object); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "typeof_string" =
  print_ssa_test {|let x = undefined;
(typeof x == "string") && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 26) to (2, 27) => { {refinement = string; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "not_typeof_string" =
  print_ssa_test {|let x = undefined;
(typeof x != "string") && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 26) to (2, 27) => { {refinement = Not (string); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "typeof_symbol" =
  print_ssa_test {|let x = undefined;
(typeof x == "symbol") && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 26) to (2, 27) => { {refinement = symbol; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "not_typeof_symbol" =
  print_ssa_test {|let x = undefined;
(typeof x != "symbol") && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 26) to (2, 27) => { {refinement = Not (symbol); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "typeof_bool_template" =
  print_ssa_test {|let x = undefined;
(typeof x == `boolean`) && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 27) to (2, 28) => { {refinement = bool; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "not_typeof_bool_template" =
  print_ssa_test {|let x = undefined;
(typeof x != `boolean`) && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 27) to (2, 28) => { {refinement = Not (bool); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "typeof_number_template" =
  print_ssa_test {|let x = undefined;
(typeof x == `number`) && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 26) to (2, 27) => { {refinement = number; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "not_typeof_number_template" =
  print_ssa_test {|let x = undefined;
(typeof x != `number`) && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 26) to (2, 27) => { {refinement = Not (number); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "typeof_function_template" =
  print_ssa_test {|let x = undefined;
(typeof x == `function`) && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 28) to (2, 29) => { {refinement = function; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "not_typeof_function_template" =
  print_ssa_test {|let x = undefined;
(typeof x != `function`) && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 28) to (2, 29) => { {refinement = Not (function); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "typeof_object_template" =
  print_ssa_test {|let x = undefined;
(typeof x == `object`) && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 26) to (2, 27) => { {refinement = object; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "not_typeof_object_template" =
  print_ssa_test {|let x = undefined;
(typeof x != `object`) && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 26) to (2, 27) => { {refinement = Not (object); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "typeof_string_template" =
  print_ssa_test {|let x = undefined;
(typeof x == `string`) && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 26) to (2, 27) => { {refinement = string; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "not_typeof_string_template" =
  print_ssa_test {|let x = undefined;
(typeof x != `string`) && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 26) to (2, 27) => { {refinement = Not (string); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "typeof_symbol_template" =
  print_ssa_test {|let x = undefined;
(typeof x == `symbol`) && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 26) to (2, 27) => { {refinement = symbol; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "not_typeof_symbol_template" =
  print_ssa_test {|let x = undefined;
(typeof x != `symbol`) && x|};
  [%expect {|
    [ (2, 8) to (2, 9) => { (1, 4) to (1, 5): (`x`) }; (2, 26) to (2, 27) => { {refinement = Not (symbol); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "singleton_bool" =
  print_ssa_test {|let x = undefined;
(x === true) && x|};
  [%expect {|
    [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 16) to (2, 17) => { {refinement = true; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "singleton_str" =
  print_ssa_test {|let x = undefined;
(x === "str") && x|};
  [%expect{|
    [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 17) to (2, 18) => { {refinement = str; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "singleton_str_template" =
  print_ssa_test {|let x = undefined;
(x === `str`) && x|};
  [%expect {|
    [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 17) to (2, 18) => { {refinement = str; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "singleton_num" =
  print_ssa_test {|let x = undefined;
(x === 3) && x|};
  [%expect {|
    [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 13) to (2, 14) => { {refinement = 3; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "singleton_num_neg" =
  print_ssa_test {|let x = undefined;
(x === -3) && x|};
  [%expect {|
    [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 14) to (2, 15) => { {refinement = -3; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "sentinel_lit" =
  print_ssa_test {|let x = undefined;
(x.foo === 3) && x|}; 
    [%expect {| [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 17) to (2, 18) => { {refinement = SentinelR foo; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "sentinel_lit_indexed " =
  print_ssa_test {|let x = undefined;
(x["foo"] === 3) && x|};
    [%expect {| [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 20) to (2, 21) => { {refinement = SentinelR foo; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "sentinel_nonlit" =
  print_ssa_test {|let x = undefined;
let y = undefined;
(x.foo === y) && x|};
    [%expect {| [ (3, 1) to (3, 2) => { (1, 4) to (1, 5): (`x`) }; (3, 17) to (3, 18) => { {refinement = SentinelR foo; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "sentinel_nonlit_indexed" =
  print_ssa_test {|let x = undefined;
let y = undefined;
(x["foo"] === y) && x|};
    [%expect {| [ (3, 1) to (3, 2) => { (1, 4) to (1, 5): (`x`) }; (3, 20) to (3, 21) => { {refinement = SentinelR foo; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "optional_chain_lit" =
  print_ssa_test {|let x = undefined;
(x?.foo === 3) && x|}; 
    [%expect{| [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 18) to (2, 19) => { {refinement = And (SentinelR foo, Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "optional_chain_member_base" =
  print_ssa_test {|let x = undefined;
(x.foo?.bar === 3) && x|}; 
    [%expect {| [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 22) to (2, 23) => { (1, 4) to (1, 5): (`x`) } ] |}]

let%expect_test "optional_chain_with_call" =
  print_ssa_test {|let x = undefined;
(x?.foo().bar === 3) && x|}; 
    [%expect {| [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 24) to (2, 25) => { {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "optional_multiple_chains" =
  print_ssa_test {|let x = undefined;
(x?.foo?.bar.baz?.qux === 3) && x|}; 
    [%expect {| [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 32) to (2, 33) => { {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "optional_base_call" =
  print_ssa_test {|let x = undefined;
(x?.().foo?.bar.baz?.qux === 3) && x|}; 
    [%expect {| [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 35) to (2, 36) => { (1, 4) to (1, 5): (`x`) } ] |}]

let%expect_test "sentinel_standalone" =
  print_ssa_test {|let x = undefined;
x.foo && x|}; 
    [%expect {| [ (2, 9) to (2, 10) => { {refinement = SentinelR foo; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "optional_chain_standalone" =
  print_ssa_test {|let x = undefined;
x?.foo && x|}; 
    [%expect {| [ (2, 10) to (2, 11) => { {refinement = And (SentinelR foo, Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "conditional_expression" =
  print_ssa_test {|let x = undefined;
(x ? x: x) && x|}; 
    [%expect {| [ (2, 1) to (2, 2) => { (1, 4) to (1, 5): (`x`) }; (2, 5) to (2, 6) => { {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)} }; (2, 8) to (2, 9) => { {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)} }; (2, 14) to (2, 15) => { (1, 4) to (1, 5): (`x`) } ] |}]

let%expect_test "if_else_statement" =
  print_ssa_test {|let x = undefined;
if (x) {
  x;
} else {
  x;
}
x;|};
    [%expect {| [ (2, 4) to (2, 5) => { (1, 4) to (1, 5): (`x`) }; (3, 2) to (3, 3) => { {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)} }; (5, 2) to (5, 3) => { {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)} }; (7, 0) to (7, 1) => { (1, 4) to (1, 5): (`x`) } ] |}]

let%expect_test "if_no_else_statement" =
  print_ssa_test {|let x = undefined;
if (x) {
  x;
} 
x;|};
    [%expect {| [ (2, 4) to (2, 5) => { (1, 4) to (1, 5): (`x`) }; (3, 2) to (3, 3) => { {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)} }; (5, 0) to (5, 1) => { (1, 4) to (1, 5): (`x`) } ] |}]

let%expect_test "if_no_else_statement_with_assignment" =
  print_ssa_test {|let x = undefined;
if (x !== null) {
  x = null;
} 
x;|};
    [%expect {| [ (2, 4) to (2, 5) => { (1, 4) to (1, 5): (`x`) }; (5, 0) to (5, 1) => { (3, 2) to (3, 3): (`x`), {refinement = Not (Not (Null)); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "if_throw_else_statement" =
  print_ssa_test {|let x = undefined;
if (x) {
  throw 'error';
} else {
  x;
}
x;|};
    [%expect {| [ (2, 4) to (2, 5) => { (1, 4) to (1, 5): (`x`) }; (5, 2) to (5, 3) => { {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)} }; (7, 0) to (7, 1) => { {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "if_else_throw_statement" =
  print_ssa_test {|let x = undefined;
if (x) {
  x;
} else {
  throw 'error';
}
x;|};
    [%expect {| [ (2, 4) to (2, 5) => { (1, 4) to (1, 5): (`x`) }; (3, 2) to (3, 3) => { {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)} }; (7, 0) to (7, 1) => { {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "if_return_else_statement" =
  print_ssa_test {|function f() {
  let x = undefined;
  if (x) {
    return;
  } else {
    x;
  }
  x;
}
|};
    [%expect {| [ (3, 6) to (3, 7) => { (2, 6) to (2, 7): (`x`) }; (6, 4) to (6, 5) => { {refinement = Not (Truthy); writes = (2, 6) to (2, 7): (`x`)} }; (8, 2) to (8, 3) => { {refinement = Not (Truthy); writes = (2, 6) to (2, 7): (`x`)} } ] |}]

let%expect_test "if_else_return_statement" =
  print_ssa_test {|function f() {
  let x = undefined;
  if (x) {
    x;
  } else {
    return;
  }
  x;
}
|};
    [%expect {| [ (3, 6) to (3, 7) => { (2, 6) to (2, 7): (`x`) }; (4, 4) to (4, 5) => { {refinement = Truthy; writes = (2, 6) to (2, 7): (`x`)} }; (8, 2) to (8, 3) => { {refinement = Truthy; writes = (2, 6) to (2, 7): (`x`)} } ] |}]

let%expect_test "nested_if_else_statement" =
  print_ssa_test {|let x = undefined;
if (x) {
  if (x === null) {
    throw 'error';
  }
  x; 
} else {
  if (x === null) {
    x;
  } else {
    throw 'error';
  }
  x;
}
x;|};
    [%expect {| [ (2, 4) to (2, 5) => { (1, 4) to (1, 5): (`x`) }; (3, 6) to (3, 7) => { {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)} }; (6, 2) to (6, 3) => { {refinement = Not (Null); writes = {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}} }; (8, 6) to (8, 7) => { {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)} }; (9, 4) to (9, 5) => { {refinement = Null; writes = {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}} }; (13, 2) to (13, 3) => { {refinement = Null; writes = {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}} }; (15, 0) to (15, 1) => { {refinement = Not (Null); writes = {refinement = Truthy; writes = (1, 4) to (1, 5): (`x`)}}, {refinement = Null; writes = {refinement = Not (Truthy); writes = (1, 4) to (1, 5): (`x`)}} } ] |}]

let%expect_test "while" =
  print_ssa_test {|let x = undefined;
while (x != null) {
  x;
}
x;|};
    [%expect {| [ (2, 7) to (2, 8) => { (1, 4) to (1, 5): (`x`) }; (3, 2) to (3, 3) => { {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)} }; (5, 0) to (5, 1) => { {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "while_throw" =
  print_ssa_test {|let x = undefined;
while (x != null) {
  throw 'error';
}
x;|};
    [%expect {| [ (2, 7) to (2, 8) => { (1, 4) to (1, 5): (`x`) }; (5, 0) to (5, 1) => { {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "while_break_with_control_flow_writes" =
  print_ssa_test {|let x = undefined;
let y = undefined;
while (x != null) {
  if (y == null) {
    break;
  }
  y;
}
y;
x;|};
    [%expect {| [ (3, 7) to (3, 8) => { (1, 4) to (1, 5): (`x`) }; (4, 6) to (4, 7) => { (2, 4) to (2, 5): (`y`) }; (7, 2) to (7, 3) => { {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`y`)} }; (9, 0) to (9, 1) => { (2, 4) to (2, 5): (`y`), {refinement = Maybe; writes = (2, 4) to (2, 5): (`y`)}, {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`y`)} }; (10, 0) to (10, 1) => { (1, 4) to (1, 5): (`x`), {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "while_with_runtime_writes" =
  print_ssa_test {|let x = undefined;
let y = undefined;
while (x != null) {
  if (y == null) {
    x = 2;
  }
  y;
}
y;
x;|};
    [%expect {| [ (3, 7) to (3, 8) => { (1, 4) to (1, 5): (`x`) }; (4, 6) to (4, 7) => { (2, 4) to (2, 5): (`y`) }; (7, 2) to (7, 3) => { (2, 4) to (2, 5): (`y`) }; (9, 0) to (9, 1) => { (2, 4) to (2, 5): (`y`) }; (10, 0) to (10, 1) => { {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`),(5, 4) to (5, 5): (`x`)} } ] |}]

let%expect_test "while_continue" =
  print_ssa_test {|let x = undefined;
while (x != null) {
  continue;
}
x;|};
    [%expect {| [ (2, 7) to (2, 8) => { (1, 4) to (1, 5): (`x`) }; (5, 0) to (5, 1) => { {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "while_continue_with_control_flow_writes" =
  print_ssa_test {|let x = undefined;
let y = undefined;
while (x != null) {
  if (y == null) {
    continue;
  }
  y;
}
y;
x;|};
    [%expect {| [ (3, 7) to (3, 8) => { (1, 4) to (1, 5): (`x`) }; (4, 6) to (4, 7) => { (2, 4) to (2, 5): (`y`) }; (7, 2) to (7, 3) => { {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`y`)} }; (9, 0) to (9, 1) => { (2, 4) to (2, 5): (`y`), {refinement = Maybe; writes = (2, 4) to (2, 5): (`y`)}, {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`y`)} }; (10, 0) to (10, 1) => { {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "while_phi_node_refinement" =
  print_ssa_test {|let x = undefined;
while (x != null) {
  if (x === 3) {
    continue;
  }
  x;
}
x;|};
    [%expect {| [ (2, 7) to (2, 8) => { (1, 4) to (1, 5): (`x`) }; (3, 6) to (3, 7) => { {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)} }; (6, 2) to (6, 3) => { {refinement = Not (3); writes = {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)}} }; (8, 0) to (8, 1) => { {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`),{refinement = 3; writes = (1, 4) to (1, 5): (`x`)},{refinement = Not (3); writes = (1, 4) to (1, 5): (`x`)}} } ] |}]

let%expect_test "do_while" =
  print_ssa_test {|let x = undefined;
do {
  x;
} while (x != null);
x;|};
    [%expect {| [ (3, 2) to (3, 3) => { (1, 4) to (1, 5): (`x`) }; (4, 9) to (4, 10) => { (1, 4) to (1, 5): (`x`) }; (5, 0) to (5, 1) => { {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "do_while_break_with_control_flow_writes" =
  print_ssa_test {|let x = undefined;
let y = undefined;
do {
  if (y == null) {
    break;
  }
  y;
} while (x != null);
y;
x;|};
    [%expect {| [ (4, 6) to (4, 7) => { (2, 4) to (2, 5): (`y`) }; (7, 2) to (7, 3) => { {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`y`)} }; (8, 9) to (8, 10) => { (1, 4) to (1, 5): (`x`) }; (9, 0) to (9, 1) => { {refinement = Maybe; writes = (2, 4) to (2, 5): (`y`)}, {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`y`)} }; (10, 0) to (10, 1) => { (1, 4) to (1, 5): (`x`) } ] |}]

let%expect_test "do_while_with_runtime_writes" =
  print_ssa_test {|let x = undefined;
let y = undefined;
do {
  if (y == null) {
    x = 2;
  }
  y;
} while (x != null);
y;
x;|};
    [%expect {| [ (4, 6) to (4, 7) => { (2, 4) to (2, 5): (`y`) }; (7, 2) to (7, 3) => { (2, 4) to (2, 5): (`y`) }; (8, 9) to (8, 10) => { (1, 4) to (1, 5): (`x`), (5, 4) to (5, 5): (`x`) }; (9, 0) to (9, 1) => { (2, 4) to (2, 5): (`y`) }; (10, 0) to (10, 1) => { {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`),(5, 4) to (5, 5): (`x`)} } ] |}]

let%expect_test "do_while_continue" =
  print_ssa_test {|let x = undefined;
do {
  continue;
} while (x != null);
x;|};
    [%expect {| [ (4, 9) to (4, 10) => { (1, 4) to (1, 5): (`x`) }; (5, 0) to (5, 1) => { {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "do_while_continue_with_control_flow_writes" =
  print_ssa_test {|let x = undefined;
let y = undefined;
do {
  if (y == null) {
    continue;
  }
  y;
} while (x != null);
y;
x;|};
    [%expect {| [ (4, 6) to (4, 7) => { (2, 4) to (2, 5): (`y`) }; (7, 2) to (7, 3) => { {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`y`)} }; (8, 9) to (8, 10) => { (1, 4) to (1, 5): (`x`) }; (9, 0) to (9, 1) => { {refinement = Maybe; writes = (2, 4) to (2, 5): (`y`)}, {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`y`)} }; (10, 0) to (10, 1) => { {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "do_while_phi_node_refinement" =
  print_ssa_test {|let x = undefined;
do {
  if (x === 3) {
    continue;
  }
  x;
} while (x != null);
x;|};
    [%expect {| [ (3, 6) to (3, 7) => { (1, 4) to (1, 5): (`x`) }; (6, 2) to (6, 3) => { {refinement = Not (3); writes = (1, 4) to (1, 5): (`x`)} }; (7, 9) to (7, 10) => { {refinement = 3; writes = (1, 4) to (1, 5): (`x`)}, {refinement = Not (3); writes = (1, 4) to (1, 5): (`x`)} }; (8, 0) to (8, 1) => { {refinement = Not (Not (Maybe)); writes = {refinement = 3; writes = (1, 4) to (1, 5): (`x`)},{refinement = Not (3); writes = (1, 4) to (1, 5): (`x`)}} } ] |}]

let%expect_test "for_no_init_no_update" =
  print_ssa_test {|let x = undefined;
for (;x != null;) {
  x;
}
x;|};
    [%expect {| [ (2, 6) to (2, 7) => { (1, 4) to (1, 5): (`x`) }; (3, 2) to (3, 3) => { {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)} }; (5, 0) to (5, 1) => { {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "for_no_init_no_update_throw" =
  print_ssa_test {|let x = undefined;
for (;x != null;) {
  throw 'error';
}
x;|};
    [%expect {| [ (2, 6) to (2, 7) => { (1, 4) to (1, 5): (`x`) }; (5, 0) to (5, 1) => { {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "for_no_init_no_update_break_with_control_flow_writes" =
  print_ssa_test {|let x = undefined;
let y = undefined;
for (; x != null; ) {
  if (y == null) {
    break;
  }
  y;
}
y;
x;|};
    [%expect {| [ (3, 7) to (3, 8) => { (1, 4) to (1, 5): (`x`) }; (4, 6) to (4, 7) => { (2, 4) to (2, 5): (`y`) }; (7, 2) to (7, 3) => { {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`y`)} }; (9, 0) to (9, 1) => { (2, 4) to (2, 5): (`y`), {refinement = Maybe; writes = (2, 4) to (2, 5): (`y`)}, {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`y`)} }; (10, 0) to (10, 1) => { (1, 4) to (1, 5): (`x`), {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "for_no_init_no_update_with_runtime_writes" =
  print_ssa_test {|let x = undefined;
let y = undefined;
for (;x != null;) {
  if (y == null) {
    x = 2;
  }
  y;
}
y;
x;|};
    [%expect {| [ (3, 6) to (3, 7) => { (1, 4) to (1, 5): (`x`) }; (4, 6) to (4, 7) => { (2, 4) to (2, 5): (`y`) }; (7, 2) to (7, 3) => { (2, 4) to (2, 5): (`y`) }; (9, 0) to (9, 1) => { (2, 4) to (2, 5): (`y`) }; (10, 0) to (10, 1) => { {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`),(5, 4) to (5, 5): (`x`)} } ] |}]

let%expect_test "for_no_init_no_update_continue" =
  print_ssa_test {|let x = undefined;
for (; x != null; ) {
  continue;
}
x;|};
    [%expect {| [ (2, 7) to (2, 8) => { (1, 4) to (1, 5): (`x`) }; (5, 0) to (5, 1) => { {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "for_no_init_no_update_continue_with_control_flow_writes" =
  print_ssa_test {|let x = undefined;
let y = undefined;
for (;x != null;) {
  if (y == null) {
    continue;
  }
  y;
}
y;
x;|};
    [%expect {| [ (3, 6) to (3, 7) => { (1, 4) to (1, 5): (`x`) }; (4, 6) to (4, 7) => { (2, 4) to (2, 5): (`y`) }; (7, 2) to (7, 3) => { {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`y`)} }; (9, 0) to (9, 1) => { (2, 4) to (2, 5): (`y`), {refinement = Maybe; writes = (2, 4) to (2, 5): (`y`)}, {refinement = Not (Maybe); writes = (2, 4) to (2, 5): (`y`)} }; (10, 0) to (10, 1) => { {refinement = Not (Not (Maybe)); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "for_no_init_no_update_phi_refinement" =
  print_ssa_test {|let x = undefined;
for (; x != null; ) {
  if (x === 3) {
    break;
  }
  x;
}
x;|};
    [%expect {| [ (2, 7) to (2, 8) => { (1, 4) to (1, 5): (`x`) }; (3, 6) to (3, 7) => { {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)} }; (6, 2) to (6, 3) => { {refinement = Not (3); writes = {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)}} }; (8, 0) to (8, 1) => { (1, 4) to (1, 5): (`x`), {refinement = 3; writes = {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`x`)}}, {refinement = Not (3); writes = (1, 4) to (1, 5): (`x`)} } ] |}]

let%expect_test "for_shadow" =
  print_ssa_test {|let x = undefined;
for (let x = null; x != null; x++) {
}
x;|};
    [%expect {| [ (2, 19) to (2, 20) => { (2, 9) to (2, 10): (`x`) }; (2, 30) to (2, 31) => { {refinement = Not (Maybe); writes = (2, 9) to (2, 10): (`x`)} }; (4, 0) to (4, 1) => { (1, 4) to (1, 5): (`x`) } ] |}]

let%expect_test "for" =
  print_ssa_test {|for (let x = 3; x != null; x++) {
  x;
}|};
    [%expect {| [ (1, 16) to (1, 17) => { (1, 9) to (1, 10): (`x`) }; (1, 27) to (1, 28) => { {refinement = Not (Maybe); writes = (1, 9) to (1, 10): (`x`)} }; (2, 2) to (2, 3) => { {refinement = Not (Maybe); writes = (1, 9) to (1, 10): (`x`)} } ] |}]

let%expect_test "for_throw" =
  print_ssa_test {|for (let x = 3; x != null; x++) {
  throw 'error';
}|};
    [%expect {| [ (1, 16) to (1, 17) => { (1, 9) to (1, 10): (`x`) } ] |}]

let%expect_test "for_break_with_control_flow_writes" =
  print_ssa_test {|let y = undefined;
for (let x = 3; x != null; x++) {
  if (y == null) {
    break;
  }
  y;
}
y;|};
    [%expect {| [ (2, 16) to (2, 17) => { (2, 9) to (2, 10): (`x`) }; (2, 27) to (2, 28) => { {refinement = Not (Maybe); writes = (2, 9) to (2, 10): (`x`)} }; (3, 6) to (3, 7) => { (1, 4) to (1, 5): (`y`) }; (6, 2) to (6, 3) => { {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`y`)} }; (8, 0) to (8, 1) => { (1, 4) to (1, 5): (`y`), {refinement = Maybe; writes = (1, 4) to (1, 5): (`y`)}, {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`y`)} } ] |}]

let%expect_test "for_with_runtime_writes" =
  print_ssa_test {|let y = undefined;
for (let x = 3; x != null; x++) {
  if (y == null) {
    x = 2;
  }
  y;
}
y;|};
    [%expect {| [ (2, 16) to (2, 17) => { (2, 9) to (2, 10): (`x`) }; (2, 27) to (2, 28) => { (4, 4) to (4, 5): (`x`), {refinement = Not (Maybe); writes = (2, 9) to (2, 10): (`x`)} }; (3, 6) to (3, 7) => { (1, 4) to (1, 5): (`y`) }; (6, 2) to (6, 3) => { (1, 4) to (1, 5): (`y`) }; (8, 0) to (8, 1) => { (1, 4) to (1, 5): (`y`) } ] |}]

let%expect_test "for_continue" =
  print_ssa_test {|for (let x = 3; x != null; x++) {
  continue;
}
x;|};
    [%expect {| [ (1, 16) to (1, 17) => { (1, 9) to (1, 10): (`x`) }; (1, 27) to (1, 28) => { {refinement = Not (Maybe); writes = (1, 9) to (1, 10): (`x`)} } ] |}]

let%expect_test "for_continue_with_control_flow_writes" =
  print_ssa_test {|let y = undefined;
for (let x = 3; x != null; x++) {
  if (y == null) {
    continue;
  }
  y;
}
y;|};
    [%expect {| [ (2, 16) to (2, 17) => { (2, 9) to (2, 10): (`x`) }; (2, 27) to (2, 28) => { {refinement = Not (Maybe); writes = (2, 9) to (2, 10): (`x`)} }; (3, 6) to (3, 7) => { (1, 4) to (1, 5): (`y`) }; (6, 2) to (6, 3) => { {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`y`)} }; (8, 0) to (8, 1) => { (1, 4) to (1, 5): (`y`), {refinement = Maybe; writes = (1, 4) to (1, 5): (`y`)}, {refinement = Not (Maybe); writes = (1, 4) to (1, 5): (`y`)} } ] |}]
