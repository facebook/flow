(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Test_utils
open Name_def.With_Loc
open Utils_js
module Ast = Flow_ast
module LocMap = Loc_collections.LocMap
module LocSet = Loc_collections.LocSet

let string_of_root = function
  | Contextual _ -> "contextual"
  | Annotation (loc, _) -> spf "annot %s" (L.debug_to_string loc)
  | Value (loc, _) -> spf "val %s" (L.debug_to_string loc)

let string_of_selector = function
  | Elem n -> spf "[%d]" n
  | Prop { prop; _ } -> spf ".%s" prop
  | Computed _ -> ".[computed]"
  | ObjRest _ -> "{ ... }"
  | ArrRest _ -> "[...]"
  | Default _ -> "<with default>"

let rec string_of_binding = function
  | Root r -> string_of_root r
  | Select (sel, src) -> spf "(%s)%s" (string_of_binding src) (string_of_selector sel)

let string_of_source = function
  | Binding (_, b) -> string_of_binding b
  | Function (InferredFunction { Ast.Function.id; _ }) ->
    spf
      "fun/inferred %s"
      (Base.Option.value_map
         ~f:(fun (_, { Ast.Identifier.name; _ }) -> name)
         ~default:"<anonymous>"
         id
      )
  | Function (ReturnAnnotatedFunction _) -> "fun/annotated"
  | TypeAlias { Ast.Statement.TypeAlias.right = (loc, _); _ } ->
    spf "alias %s" (L.debug_to_string loc)
  | TypeParam (loc, _) -> spf "tparam %s" (L.debug_to_string loc)

let print_values values =
  let kvlist = L.LMap.bindings values in
  let strlist =
    Base.List.map
      ~f:(fun (def_loc, init) ->
        Printf.sprintf "%s => %s" (L.debug_to_string def_loc) (string_of_source init))
      kvlist
  in
  Printf.printf "[\n  %s\n]" (String.concat ";\n  " strlist)

let print_order lst =
  let open Name_def_ordering.With_Loc in
  let string_of_why why =
    match why with
    | (loc, []) -> L.debug_to_string loc
    | (loc, _) -> Printf.sprintf "[%s, ...]" (L.debug_to_string loc)
  in
  let msg =
    Base.List.map
      ~f:(function
        | Singleton key -> Key.to_string key
        | ReflCycle (key, why) ->
          Printf.sprintf "illegal self-cycle: (%s via %s)" (Key.to_string key) (string_of_why why)
        | IllegalCycle ((fst_key, fst_why), rest) ->
          Printf.sprintf
            "illegal cycle: (%s -> \n  %s)"
            (Key.to_string fst_key)
            (List.map
               (fun (key, why) ->
                 Printf.sprintf "(via %s) %s" (string_of_why why) (Key.to_string key))
               (rest @ [(fst_key, fst_why)])
            |> String.concat " -> \n  "
            )
        | TypeCycle keys ->
          Printf.sprintf
            "legal cycle: ((%s))"
            (Nel.map Key.to_string keys |> Nel.to_list |> String.concat "); ("))
      lst
    |> String.concat " => \n"
  in
  print_string msg

let print_init_test contents =
  let inits = Name_def.With_Loc.find_defs (parse contents) in
  print_values inits

let print_order_test contents =
  let module Name_def = Name_def_ordering.With_Loc.Name_def in
  let ast = parse contents in
  let (_, env) = Name_resolver.With_Loc.program_with_scope () ast in
  let inits = Name_def.find_defs ast in
  let order = Name_def_ordering.With_Loc.build_ordering env inits in
  print_order order

(* TODO: ocamlformat mangles the ppx syntax. *)
[@@@ocamlformat "disable=true"]

let%expect_test "decl" =
  print_init_test {|
let x = 42;
  |};
  [%expect {|
    [
      (2, 4) to (2, 5) => val (2, 8) to (2, 10)
    ] |}]

let%expect_test "decl_annot" =
  print_init_test {|
let x: string = 42;
  |};
  [%expect {|
    [
      (2, 4) to (2, 5) => annot (2, 5) to (2, 13)
    ] |}]

let%expect_test "decl_annot_no_init" =
  print_init_test {|
let x: number;
  |};
  [%expect {|
    [
      (2, 4) to (2, 5) => annot (2, 5) to (2, 13)
    ] |}]

let%expect_test "decl_nothing" =
  print_init_test {|
let x;
  |};
  [%expect {|
    [

    ] |}]

let%expect_test "assign" =
  print_init_test {|
x = 42;
  |};
  [%expect {|
    [
      (2, 0) to (2, 1) => val (2, 4) to (2, 6)
    ] |}]

let%expect_test "elems" =
  print_init_test {|
let [a,b] = 42;
  |};
  [%expect {|
    [
      (2, 5) to (2, 6) => (val (2, 12) to (2, 14))[0];
      (2, 7) to (2, 8) => (val (2, 12) to (2, 14))[1]
    ] |}]

let%expect_test "elems_hole" =
  print_init_test {|
let [a,,b] = 42;
  |};
  [%expect {|
    [
      (2, 5) to (2, 6) => (val (2, 13) to (2, 15))[0];
      (2, 8) to (2, 9) => (val (2, 13) to (2, 15))[2]
    ] |}]

let%expect_test "elems_rest" =
  print_init_test {|
let [a,,b,...c] = 42;
  |};
  [%expect {|
    [
      (2, 5) to (2, 6) => (val (2, 18) to (2, 20))[0];
      (2, 8) to (2, 9) => (val (2, 18) to (2, 20))[2];
      (2, 13) to (2, 14) => (val (2, 18) to (2, 20))[...]
    ] |}]

let%expect_test "elems_def" =
  print_init_test {|
let [a=42] = 42;
  |};
  [%expect {|
    [
      (2, 5) to (2, 6) => ((val (2, 13) to (2, 15))[0])<with default>
    ] |}]

let%expect_test "props" =
  print_init_test {|
let {a, b} = 42;
  |};
  [%expect {|
    [
      (2, 5) to (2, 6) => (val (2, 13) to (2, 15)).a;
      (2, 8) to (2, 9) => (val (2, 13) to (2, 15)).b
    ] |}]

let%expect_test "props_lit" =
  print_init_test {|
let {a, '42':b} = 42;
  |};
  [%expect {|
    [
      (2, 5) to (2, 6) => (val (2, 18) to (2, 20)).a;
      (2, 13) to (2, 14) => (val (2, 18) to (2, 20)).42
    ] |}]

let%expect_test "props_comp_rest" =
  print_init_test {|
let {a, [foo()]: b, ...c} = 42;
  |};
  [%expect {|
    [
      (2, 5) to (2, 6) => (val (2, 28) to (2, 30)).a;
      (2, 17) to (2, 18) => (val (2, 28) to (2, 30)).[computed];
      (2, 23) to (2, 24) => (val (2, 28) to (2, 30)){ ... }
    ] |}]

let%expect_test "function_def" =
  print_init_test {|
function f(y, z: number) {
  x = 42;
}
  |};
  [%expect {|
    [
      (2, 9) to (2, 10) => fun/inferred f;
      (2, 11) to (2, 12) => contextual;
      (2, 14) to (2, 15) => annot (2, 15) to (2, 23);
      (3, 2) to (3, 3) => val (3, 6) to (3, 8)
    ] |}]

let%expect_test "function_exp" =
  print_init_test {|
var w = function f(y, z: number) {
  x = 42;
}
  |};
  [%expect {|
    [
      (2, 4) to (2, 5) => val (2, 8) to (4, 1);
      (2, 17) to (2, 18) => fun/inferred f;
      (2, 19) to (2, 20) => contextual;
      (2, 22) to (2, 23) => annot (2, 23) to (2, 31);
      (3, 2) to (3, 3) => val (3, 6) to (3, 8)
    ] |}]

let%expect_test "fun_tparam" =
  print_init_test {|
var w = function <X, Y:number>() { }
  |};
  [%expect {|
    [
      (2, 4) to (2, 5) => val (2, 8) to (2, 36);
      (2, 18) to (2, 19) => tparam (2, 18) to (2, 19);
      (2, 21) to (2, 22) => tparam (2, 21) to (2, 29)
    ] |}]

let%expect_test "type_alias" =
  print_init_test {|
type T = number;
type P<X> = X;
  |};
  [%expect {|
    [
      (2, 5) to (2, 6) => alias (2, 9) to (2, 15);
      (3, 5) to (3, 6) => alias (3, 12) to (3, 13);
      (3, 7) to (3, 8) => tparam (3, 7) to (3, 8)
    ] |}]

let%expect_test "deps" =
  print_order_test {|
let x = 1;
let y = x;
  |};
  [%expect {|
    term((2, 4) to (2, 5)) =>
    term((3, 4) to (3, 5)) |}]

let%expect_test "deps_on_type" =
  print_order_test {|
type T = number;
let x: T = 1;
let y = x;
  |};
  [%expect {|
    type((2, 5) to (2, 6)) =>
    term((3, 4) to (3, 5)) =>
    term((4, 4) to (4, 5)) |}]

let%expect_test "deps_recur" =
  print_order_test {|
type T = number;
let x: T;
function f() {
  x = x;
}
  |};
  [%expect {|
    type((2, 5) to (2, 6)) =>
    term((3, 4) to (3, 5)) =>
    term((5, 2) to (5, 3)) =>
    term((4, 9) to (4, 10)) |}]

let%expect_test "recur_func" =
  print_order_test {|
function f() {
  return f();
}
  |};
  [%expect {|
    illegal self-cycle: (term((2, 9) to (2, 10)) via (3, 9) to (3, 10)) |}]

let%expect_test "recur_func_anno" =
  print_order_test {|
function f(): void {
  return f();
}
  |};
  [%expect {|
    term((2, 9) to (2, 10)) |}]

let%expect_test "recur_fun_order" =
  print_order_test {|
function f() {
  return f();
}
  |};
  [%expect {|
    illegal self-cycle: (term((2, 9) to (2, 10)) via (3, 9) to (3, 10)) |}]

let%expect_test "deps1" =
  print_order_test {|
type T = number;
let x: T;
function f() {
  x = x;
}
  |};
  [%expect {|
    type((2, 5) to (2, 6)) =>
    term((3, 4) to (3, 5)) =>
    term((5, 2) to (5, 3)) =>
    term((4, 9) to (4, 10)) |}]

let%expect_test "deps2" =
  print_order_test {|
type T = Array<T>;
type S = W;
type W = S;
let x, y, z;
function nested() {
  x = y;
  y = z;
  z = x;
}
  |};
  [%expect {|
    illegal cycle: (term((7, 2) to (7, 3)) ->
      (via (7, 6) to (7, 7)) term((8, 2) to (8, 3)) ->
      (via (8, 6) to (8, 7)) term((9, 2) to (9, 3)) ->
      (via (9, 6) to (9, 7)) term((7, 2) to (7, 3))) =>
    term((6, 9) to (6, 15)) =>
    type((2, 5) to (2, 6)) =>
    legal cycle: ((type((3, 5) to (3, 6))); (type((4, 5) to (4, 6)))) |}]

let%expect_test "deps2" =
  print_order_test {|
type T = Array<T>;
type S = W;
type W = S;
let x, y, z;
function nested() {
  x = y;
  y = z;
  z = x;
}
  |};
  [%expect {|
    illegal cycle: (term((7, 2) to (7, 3)) ->
      (via (7, 6) to (7, 7)) term((8, 2) to (8, 3)) ->
      (via (8, 6) to (8, 7)) term((9, 2) to (9, 3)) ->
      (via (9, 6) to (9, 7)) term((7, 2) to (7, 3))) =>
    term((6, 9) to (6, 15)) =>
    type((2, 5) to (2, 6)) =>
    legal cycle: ((type((3, 5) to (3, 6))); (type((4, 5) to (4, 6)))) |}]

let%expect_test "deps2" =
  print_order_test {|
let x = f();
function f() {
  return x;
}
  |};
  [%expect {|
    illegal cycle: (term((2, 4) to (2, 5)) ->
      (via (2, 8) to (2, 9)) term((3, 9) to (3, 10)) ->
      (via (4, 9) to (4, 10)) term((2, 4) to (2, 5))) |}]

let%expect_test "deps3" =
  print_order_test {|
function invalidate_x() {
  x = null;
}

var x = null;
invalidate_x();
invariant(typeof x !== 'number');
(x: null);
x = 42;
(x: number);
  |};
  [%expect {|
    term((6, 4) to (6, 5)) =>
    term((10, 0) to (10, 1)) =>
    term((3, 2) to (3, 3)) =>
    term((2, 9) to (2, 21)) |}]

let%expect_test "typeof1" =
  print_order_test {|
var x = 42;
type T = typeof x;
var z: T = 100;
  |};
  [%expect {|
    term((2, 4) to (2, 5)) =>
    type((3, 5) to (3, 6)) =>
    term((4, 4) to (4, 5)) |}]

let%expect_test "typeof2" =
  print_order_test {|
var x: T = 42;
type T = typeof x;
  |};
  [%expect {|
    illegal cycle: (term((2, 4) to (2, 5)) ->
      (via (2, 7) to (2, 8)) type((3, 5) to (3, 6)) ->
      (via (3, 16) to (3, 17)) term((2, 4) to (2, 5))) |}]
