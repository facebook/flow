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
  | Binding b -> string_of_binding b
  | Function { Ast.Function.id; _ } ->
    spf
      "fun %s"
      (Base.Option.value_map
         ~f:(fun (_, { Ast.Identifier.name; _ }) -> name)
         ~default:"<anonymous>"
         id
      )
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

(* TODO: ocamlformat mangles the ppx syntax. *)
[@@@ocamlformat "disable=true"]

let print_init_test contents =
  let inits = Name_def.With_Loc.find_defs (parse contents) in
  print_values inits

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
      (2, 9) to (2, 10) => fun f;
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
      (2, 17) to (2, 18) => fun f;
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
