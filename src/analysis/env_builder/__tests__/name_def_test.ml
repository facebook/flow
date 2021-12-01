(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Test_utils
open Name_def.With_ALoc
open Utils_js
module Ast = Flow_ast

module Name_resolver = Name_resolver.Make_Test_With_Cx (struct
  type t = unit

  let jsx _cx = Options.Jsx_react

  let react_runtime _cx = Options.ReactRuntimeClassic
end)

let string_of_root = function
  | Contextual _ -> "contextual"
  | Catch -> "catch"
  | Annotation (loc, _) -> spf "annot %s" (L.debug_to_string loc)
  | Value (loc, _) -> spf "val %s" (L.debug_to_string loc)
  | For (In, (loc, _)) -> spf "for in %s" (L.debug_to_string loc)
  | For (Of, (loc, _)) -> spf "for of %s" (L.debug_to_string loc)

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

let string_of_import_kind =
  let open Ast.Statement.ImportDeclaration in
  function
  | ImportTypeof -> "typeof "
  | ImportType -> "type "
  | ImportValue -> ""

let string_of_import =
  let open Ast.Statement.ImportDeclaration in
  function
  | Named { kind; remote } ->
    spf "%s%s" (Base.Option.value_map ~f:string_of_import_kind ~default:"" kind) remote
  | Namespace -> "namespace"
  | Default -> "default"

let string_of_source = function
  | Binding (_, b) -> string_of_binding b
  | Update (loc, _) -> spf "crement %s" (L.debug_to_string loc)
  | OpAssign (loc, _, _) -> spf "opassign %s" (L.debug_to_string loc)
  | Function { function_ = { Ast.Function.id; _ }; _ } ->
    spf
      "fun %s"
      (Base.Option.value_map
         ~f:(fun (_, { Ast.Identifier.name; _ }) -> name)
         ~default:"<anonymous>"
         id
      )
  | DeclaredClass { Ast.Statement.DeclareClass.id = (_, { Ast.Identifier.name; _ }); _ } ->
    spf "declared class %s" name
  | Class { class_ = { Ast.Class.id; _ }; _ } ->
    spf
      "class %s"
      (Base.Option.value_map
         ~f:(fun (_, { Ast.Identifier.name; _ }) -> name)
         ~default:"<anonymous>"
         id
      )
  | TypeAlias { Ast.Statement.TypeAlias.right = (loc, _); _ } ->
    spf "alias %s" (L.debug_to_string loc)
  | OpaqueType { Ast.Statement.OpaqueType.id = (loc, _); _ } ->
    spf "opaque %s" (L.debug_to_string loc)
  | TypeParam (loc, _) -> spf "tparam %s" (L.debug_to_string loc)
  | Enum (loc, _) -> spf "enum %s" (L.debug_to_string loc)
  | Interface _ -> "interface"
  | Import { import_kind; source; import } ->
    spf "import %s%s from %s" (string_of_import_kind import_kind) (string_of_import import) source

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
  let open Name_def_ordering.With_ALoc in
  let string_of_why why =
    match why with
    | (loc, []) -> L.debug_to_string loc
    | (loc, _) -> Printf.sprintf "[%s, ...]" (L.debug_to_string loc)
  in
  let msg =
    Base.List.map
      ~f:(function
        | Singleton key -> L.debug_to_string key
        | ReflCycle (key, why) ->
          Printf.sprintf
            "illegal self-cycle: (%s via %s)"
            (L.debug_to_string key)
            (string_of_why why)
        | IllegalCycle ((fst_key, fst_why), rest) ->
          Printf.sprintf
            "illegal cycle: (%s -> \n  %s)"
            (L.debug_to_string fst_key)
            (List.map
               (fun (key, why) ->
                 Printf.sprintf "(via %s) %s" (string_of_why why) (L.debug_to_string key))
               (rest @ [(fst_key, fst_why)])
            |> String.concat " -> \n  "
            )
        | TypeCycle keys ->
          Printf.sprintf
            "legal cycle: ((%s))"
            (Nel.map (L.debug_to_string ?include_source:None) keys
            |> Nel.to_list
            |> String.concat "); ("
            ))
      lst
    |> String.concat " => \n"
  in
  print_string msg

let print_init_test contents =
  let inits = Name_def.With_ALoc.find_defs (parse_with_alocs contents) in
  print_values inits

let print_order_test contents =
  let module Name_def = Name_def_ordering.With_ALoc.Name_def in
  let ast = parse_with_alocs contents in
  let (_, env) = Name_resolver.program_with_scope () ast in
  let inits = Name_def.find_defs ast in
  let order = Name_def_ordering.With_ALoc.build_ordering env inits in
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

let%expect_test "deps" =
  print_order_test {|
let x = 1;
let y = x;
  |};
  [%expect {|
    (2, 4) to (2, 5) =>
    (3, 4) to (3, 5) |}]

let%expect_test "deps_on_type" =
  print_order_test {|
type T = number;
let x: T = 1;
let y = x;
  |};
  [%expect {|
    (2, 5) to (2, 6) =>
    (3, 4) to (3, 5) =>
    (4, 4) to (4, 5) |}]

let%expect_test "deps_recur" =
  print_order_test {|
type T = number;
let x: T;
function f() {
  x = x;
}
  |};
  [%expect {|
    (2, 5) to (2, 6) =>
    (3, 4) to (3, 5) =>
    (5, 2) to (5, 3) =>
    (4, 9) to (4, 10) |}]

let%expect_test "recur_func" =
  print_order_test {|
function f() {
  return f();
}
  |};
  [%expect {|
    illegal self-cycle: ((2, 9) to (2, 10) via (3, 9) to (3, 10)) |}]

let%expect_test "recur_func_anno" =
  print_order_test {|
function f(): void {
  return f();
}
  |};
  [%expect {|
    (2, 9) to (2, 10) |}]

let%expect_test "recur_fun_order" =
  print_order_test {|
function f() {
  return f();
}
  |};
  [%expect {|
    illegal self-cycle: ((2, 9) to (2, 10) via (3, 9) to (3, 10)) |}]

let%expect_test "deps1" =
  print_order_test {|
type T = number;
let x: T;
function f() {
  x = x;
}
  |};
  [%expect {|
    (2, 5) to (2, 6) =>
    (3, 4) to (3, 5) =>
    (5, 2) to (5, 3) =>
    (4, 9) to (4, 10) |}]

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
    (2, 5) to (2, 6) =>
    legal cycle: (((3, 5) to (3, 6)); ((4, 5) to (4, 6))) =>
    illegal cycle: ((7, 2) to (7, 3) ->
      (via (7, 6) to (7, 7)) (8, 2) to (8, 3) ->
      (via (8, 6) to (8, 7)) (9, 2) to (9, 3) ->
      (via (9, 6) to (9, 7)) (7, 2) to (7, 3)) =>
    (6, 9) to (6, 15) |}]

let%expect_test "deps2a" =
  print_order_test {|
let x = f();
function f() {
  return x;
}
  |};
  [%expect {|
    illegal cycle: ((2, 4) to (2, 5) ->
      (via (2, 8) to (2, 9)) (3, 9) to (3, 10) ->
      (via (4, 9) to (4, 10)) (2, 4) to (2, 5)) |}]

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
    (6, 4) to (6, 5) =>
    (10, 0) to (10, 1) =>
    (3, 2) to (3, 3) =>
    (2, 9) to (2, 21) |}]

let%expect_test "typeof1" =
  print_order_test {|
var x = 42;
type T = typeof x;
var z: T = 100;
  |};
  [%expect {|
    (2, 4) to (2, 5) =>
    (3, 5) to (3, 6) =>
    (4, 4) to (4, 5) |}]

let%expect_test "typeof2" =
  print_order_test {|
var x: T = 42;
type T = typeof x;
  |};
  [%expect {|
    illegal cycle: ((2, 4) to (2, 5) ->
      (via (2, 7) to (2, 8)) (3, 5) to (3, 6) ->
      (via (3, 16) to (3, 17)) (2, 4) to (2, 5)) |}]

let%expect_test "func_exp" =
  print_order_test {|
var y = function f(): number {
  return 42;
}
  |};
  [%expect {|
    (2, 17) to (2, 18) =>
    (2, 4) to (2, 5) |}]

let%expect_test "class_def" =
  print_init_test {|
let x;
class C {
  foo() { x = 42; }
}
x = 10;
  |};
  [%expect {|
    [
      (3, 6) to (3, 7) => class C;
      (4, 10) to (4, 11) => val (4, 14) to (4, 16);
      (6, 0) to (6, 1) => val (6, 4) to (6, 6)
    ] |}]

let%expect_test "class_def2" =
  print_init_test {|
var x = 42;
let foo = class C<Y: typeof x> { };
  |};
  [%expect {|
    [
      (2, 4) to (2, 5) => val (2, 8) to (2, 10);
      (3, 4) to (3, 7) => val (3, 10) to (3, 34);
      (3, 16) to (3, 17) => class C;
      (3, 18) to (3, 19) => tparam (3, 18) to (3, 29)
    ] |}]

let%expect_test "class1" =
  print_order_test {|
let x;
class C {
  foo() { x = 42; }
}
x = 10;
  |};
  [%expect {|
    (6, 0) to (6, 1) =>
    (4, 10) to (4, 11) =>
    (3, 6) to (3, 7) |}]

let%expect_test "class2" =
  print_order_test {|
var x = 42;
let foo = class C<Y: typeof x> { };
  |};
  [%expect {|
    (2, 4) to (2, 5) =>
    (3, 18) to (3, 19) =>
    (3, 16) to (3, 17) =>
    (3, 4) to (3, 7) |}]

let%expect_test "class3" =
  print_order_test {|
class C {
  foo: D;
}
class D extends C {
  bar;
}
  |};
  [%expect {|
    illegal cycle: ((2, 6) to (2, 7) ->
      (via (3, 7) to (3, 8)) (5, 6) to (5, 7) ->
      (via (5, 16) to (5, 17)) (2, 6) to (2, 7)) |}]

let%expect_test "class3_anno" =
  print_order_test {|
class C {
  foo: D;
}
class D extends C {
  bar: C;
}
  |};
  [%expect {|
    legal cycle: (((2, 6) to (2, 7)); ((5, 6) to (5, 7))) |}]

let%expect_test "enum" =
  print_order_test {|
function havoced() {
  var x: E = E.Foo
}
enum E {
  Foo
}
  |};
  [%expect {|
    (5, 5) to (5, 6) =>
    (3, 6) to (3, 7) =>
    (2, 9) to (2, 16) |}]

let%expect_test "interface" =
  print_order_test {|
interface I extends J { x: J }
interface J { h: number }
  |};
  [%expect {|
    (3, 10) to (3, 11) =>
    (2, 10) to (2, 11) |}]

let%expect_test "interface_class_anno_cycle" =
  print_order_test {|
interface I extends J { x: J }
interface J { h: C }
class C implements I { }
  |};
  [%expect {|
    legal cycle: (((2, 10) to (2, 11)); ((4, 6) to (4, 7)); ((3, 10) to (3, 11))) |}]

let%expect_test "import" =
  print_init_test {|
import typeof B, * as A from 'x';
import type C, * as D from 'x';
import E from 'x';
import F, {type G, typeof H, J } from 'x';
  |};
  [%expect {|
    [
      (2, 14) to (2, 15) => import typeof default from x;
      (2, 22) to (2, 23) => import typeof namespace from x;
      (3, 12) to (3, 13) => import type default from x;
      (3, 20) to (3, 21) => import type namespace from x;
      (4, 7) to (4, 8) => import default from x;
      (5, 7) to (5, 8) => import default from x;
      (5, 16) to (5, 17) => import type G from x;
      (5, 26) to (5, 27) => import typeof H from x;
      (5, 29) to (5, 30) => import J from x
    ] |}]

let%expect_test "refi_instanceof" =
  print_order_test {|
class C {
  foo() { y }
}
declare var x: mixed;
var y;

if (x instanceof C) {
  y = x;
}
  |};
  [%expect {|
    (5, 12) to (5, 13) =>
    illegal cycle: ((2, 6) to (2, 7) ->
      (via (3, 10) to (3, 11)) (9, 2) to (9, 3) ->
      (via (9, 6) to (9, 7)) (2, 6) to (2, 7)) |}]

let%expect_test "refi_latent" =
  print_order_test {|
function f() { return y; }
declare var x: mixed;
var y;

if (f(x)) {
  y = x;
}
  |};
  [%expect {|
    (3, 12) to (3, 13) =>
    illegal cycle: ((2, 9) to (2, 10) ->
      (via (2, 22) to (2, 23)) (7, 2) to (7, 3) ->
      (via (7, 6) to (7, 7)) (2, 9) to (2, 10)) |}]

let%expect_test "declare_class" =
  print_order_test {|
var f: C = 42;
type S = typeof f;
declare class C mixins S { }
  |};
  [%expect {|
    illegal cycle: ((2, 4) to (2, 5) ->
      (via (2, 7) to (2, 8)) (4, 14) to (4, 15) ->
      (via (4, 23) to (4, 24)) (3, 5) to (3, 6) ->
      (via (3, 16) to (3, 17)) (2, 4) to (2, 5)) |}]

let%expect_test "declare_class2" =
  print_order_test {|
declare class C<S> {
  foo<T>(x: T): P;
}
  |};
  [%expect {|
    (2, 16) to (2, 17) =>
    (3, 6) to (3, 7) =>
    (2, 14) to (2, 15) |}]

let%expect_test "opaque" =
  print_order_test {|
type Y<W> = T<W>
type S<T> = T
opaque type T<X>: S<X> = Y<X>
  |};
  [%expect {|
    (2, 7) to (2, 8) =>
    (3, 7) to (3, 8) =>
    (3, 5) to (3, 6) =>
    (4, 14) to (4, 15) =>
    legal cycle: (((2, 5) to (2, 6)); ((4, 12) to (4, 13))) |}]

let%expect_test "catch" =
  print_init_test {|
var x;
try {} catch (e) { x = e }
  |};
  [%expect {|
    [
      (3, 14) to (3, 15) => catch;
      (3, 19) to (3, 20) => val (3, 23) to (3, 24)
    ] |}]

let%expect_test "declarepred" =
  print_init_test {|
declare function f(x: T): boolean %checks(x);
type T = number;
  |};
  [%expect {|
    [
      (2, 17) to (2, 18) => fun f;
      (2, 19) to (2, 20) => annot (2, 22) to (2, 23);
      (3, 5) to (3, 6) => alias (3, 9) to (3, 15)
    ] |}]

let%expect_test "for1" =
  print_init_test {|
for (var x = 0;;) { }
  |};
  [%expect {|
    [
      (2, 9) to (2, 10) => val (2, 13) to (2, 14)
    ] |}]

let%expect_test "for2" =
  print_init_test {|
var x;
for (x = 0;;) { }
  |};
  [%expect {|
    [
      (3, 5) to (3, 6) => val (3, 9) to (3, 10)
    ] |}]

let%expect_test "for3" =
  print_init_test {|
for (var [x,y] = [1,2];;) { }
  |};
  [%expect {|
    [
      (2, 10) to (2, 11) => (val (2, 17) to (2, 22))[0];
      (2, 12) to (2, 13) => (val (2, 17) to (2, 22))[1]
    ] |}]

let%expect_test "for4" =
  print_init_test {|
for (var [x,y]: T = [1,2];;) { }
  |};
  [%expect {|
    [
      (2, 10) to (2, 11) => (annot (2, 14) to (2, 17))[0];
      (2, 12) to (2, 13) => (annot (2, 14) to (2, 17))[1]
    ] |}]

let%expect_test "for_in1" =
  print_init_test {|
for (var x in foo) { }
  |};
  [%expect {|
    [
      (2, 9) to (2, 10) => for in (2, 14) to (2, 17)
    ] |}]

let%expect_test "for_in2" =
  print_init_test {|
var x;
for (x in foo) { }
  |};
  [%expect {|
    [
      (3, 5) to (3, 6) => for in (3, 10) to (3, 13)
    ] |}]

let%expect_test "for_in3" =
  print_init_test {|
for (var [x,y] in foo) { }
  |};
  [%expect {|
    [
      (2, 10) to (2, 11) => (for in (2, 18) to (2, 21))[0];
      (2, 12) to (2, 13) => (for in (2, 18) to (2, 21))[1]
    ] |}]

let%expect_test "for_in4" =
  print_init_test {|
for (var [x,y]: T in foo) { }
  |};
  [%expect {|
    [
      (2, 10) to (2, 11) => (annot (2, 14) to (2, 17))[0];
      (2, 12) to (2, 13) => (annot (2, 14) to (2, 17))[1]
    ] |}]

let%expect_test "for_of1" =
  print_init_test {|
for (var x of foo) { }
  |};
  [%expect {|
    [
      (2, 9) to (2, 10) => for of (2, 14) to (2, 17)
    ] |}]

let%expect_test "for_of2" =
  print_init_test {|
var x;
for (x of foo) { }
  |};
  [%expect {|
    [
      (3, 5) to (3, 6) => for of (3, 10) to (3, 13)
    ] |}]

let%expect_test "for_of3" =
  print_init_test {|
for (var [x,y] of foo) { }
  |};
  [%expect {|
    [
      (2, 10) to (2, 11) => (for of (2, 18) to (2, 21))[0];
      (2, 12) to (2, 13) => (for of (2, 18) to (2, 21))[1]
    ] |}]

let%expect_test "for_of4" =
  print_init_test {|
for (var [x,y]: T of foo) { }
  |};
  [%expect {|
    [
      (2, 10) to (2, 11) => (annot (2, 14) to (2, 17))[0];
      (2, 12) to (2, 13) => (annot (2, 14) to (2, 17))[1]
    ] |}]

let%expect_test "inc" =
  print_order_test {|
let x;
var y;
function f() {
  y = x;
}
x++;
  |};
  [%expect {|
    (7, 0) to (7, 1) =>
    (5, 2) to (5, 3) =>
    (4, 9) to (4, 10) |}]

let%expect_test "opassign" =
  print_order_test {|
let x;
var y;
function f() {
  y = x;
}
function h() {
  x += y;
}
  |};
  [%expect {|
    illegal cycle: ((5, 2) to (5, 3) ->
      (via (5, 6) to (5, 7)) (8, 2) to (8, 3) ->
      (via (8, 7) to (8, 8)) (5, 2) to (5, 3)) =>
    (4, 9) to (4, 10) =>
    (7, 9) to (7, 10) |}]
