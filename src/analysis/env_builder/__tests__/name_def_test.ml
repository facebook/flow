(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Test_utils
open Name_def.Print
open Name_def_ordering
module Ast = Flow_ast
module EnvMap = Env_api.EnvMap

let jsx_mode = ref Options.Jsx_react

let react_runtime = ref Options.ReactRuntimeClassic

module Context = struct
  type t = unit

  let enable_enums _cx = true

  let file _cx = File_key.SourceFile "test.js"

  let jsx _cx = !jsx_mode

  let react_runtime _cx = !react_runtime

  let lti _cx = true

  let enable_const_params _cx = false

  let add_literal_subtypes _ _ = ()

  let add_matching_props _ _ = ()

  let add_exhaustive_check _ _ _ = ()

  let exhaustive_check _ _ = ([], false)
end

module Name_resolver = Name_resolver.Make_Test_With_Cx (Context)
module Name_def_ordering = Name_def_ordering.Make_Test_With_Cx (Context)

let autocomplete_hooks =
  {
    Env_api.With_ALoc.id_hook = (fun _ _ -> false);
    literal_hook = (fun _ -> false);
    obj_prop_decl_hook = (fun _ _ -> false);
  }

let print_values values =
  let kvlist = EnvMap.bindings values in
  let strlist =
    Base.List.map
      ~f:(fun ((_, def_loc), (init, _, _, _)) ->
        Printf.sprintf "%s => %s" (ALoc.debug_to_string def_loc) (string_of_source init))
      kvlist
  in
  Printf.printf "[\n  %s\n]" (String.concat ";\n  " strlist)

let print_order lst =
  let msg_of_elt elt =
    match elt with
    | Normal (_, l)
    | Resolvable (_, l) ->
      ALoc.debug_to_string l
    | Illegal { payload = (_, loc); _ } ->
      Printf.sprintf "illegal self-cycle (%s)" (ALoc.debug_to_string loc)
  in
  let msg =
    let loc_of_elt elt =
      match elt with
      | Normal (_, l)
      | Resolvable (_, l)
      | Illegal { payload = (_, l); _ } ->
        l
    in
    let compare a b =
      let c = ALoc.compare (loc_of_elt a) (loc_of_elt b) in
      if c = 0 then
        Stdlib.compare a b
      else
        c
    in
    let compare_blame ({ payload = a; _ }, _) ({ payload = b; _ }, _) =
      let c = ALoc.compare (loc_of_elt a) (loc_of_elt b) in
      if c = 0 then
        Stdlib.compare a b
      else
        c
    in
    Base.List.map
      ~f:(function
        | Singleton elt -> msg_of_elt elt
        | IllegalSCC keys ->
          Printf.sprintf
            "illegal scc: ((%s))"
            (Nel.to_list keys
            |> Base.List.sort ~compare:compare_blame
            |> Base.List.map ~f:(fun ({ payload = elt; _ }, _) -> msg_of_elt elt)
            |> String.concat "); ("
            )
        | ResolvableSCC keys ->
          Printf.sprintf
            "legal scc: ((%s))"
            (Nel.to_list keys
            |> Base.List.sort ~compare
            |> Base.List.map ~f:msg_of_elt
            |> String.concat "); ("
            ))
      lst
    |> String.concat " => \n"
  in
  print_string msg

let print_init_test contents =
  let ast = parse_with_alocs contents in
  let (_, { Name_resolver.Env_api.env_entries; env_values; providers; _ }) =
    Name_resolver.program_with_scope () ast
  in
  let (inits, _) = Name_def.find_defs ~autocomplete_hooks env_entries env_values providers ast in
  print_values inits

let print_order_test ?(custom_jsx = None) ?(react_runtime_automatic = false) contents =
  if react_runtime_automatic then react_runtime := Options.ReactRuntimeAutomatic;
  (match custom_jsx with
  | None -> ()
  | Some str ->
    let (ast, _errors) = Parser_flow.jsx_pragma_expression str None in
    let aloc_ast = Ast_loc_utils.loc_to_aloc_mapper#expression ast in
    jsx_mode := Options.Jsx_pragma (str, aloc_ast));
  let ast = parse_with_alocs contents in
  let (_, ({ Name_resolver.Env_api.env_entries; env_values; providers; _ } as env)) =
    Name_resolver.program_with_scope () ast
  in
  let (inits, _) = Name_def.find_defs ~autocomplete_hooks env_entries env_values providers ast in
  let order = Name_def_ordering.build_ordering ~autocomplete_hooks () env inits in
  print_order order;
  react_runtime := Options.ReactRuntimeClassic;
  jsx_mode := Options.Jsx_react

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
      (2, 7) to (2, 8) => (val (2, 12) to (2, 14))[1];
      (2, 4) to (2, 9) => val (2, 12) to (2, 14)
    ] |}]

let%expect_test "elems_hole" =
  print_init_test {|
let [a,,b] = 42;
  |};
  [%expect {|
    [
      (2, 5) to (2, 6) => (val (2, 13) to (2, 15))[0];
      (2, 8) to (2, 9) => (val (2, 13) to (2, 15))[2];
      (2, 4) to (2, 10) => val (2, 13) to (2, 15)
    ] |}]

let%expect_test "elems_rest" =
  print_init_test {|
let [a,,b,...c] = 42;
  |};
  [%expect {|
    [
      (2, 5) to (2, 6) => (val (2, 18) to (2, 20))[0];
      (2, 8) to (2, 9) => (val (2, 18) to (2, 20))[2];
      (2, 13) to (2, 14) => (val (2, 18) to (2, 20))[...];
      (2, 4) to (2, 15) => val (2, 18) to (2, 20)
    ] |}]

let%expect_test "elems_def" =
  print_init_test {|
let [a=42] = 42;
  |};
  [%expect {|
    [
      (2, 5) to (2, 6) => (val (2, 13) to (2, 15))[0];
      (2, 4) to (2, 10) => val (2, 13) to (2, 15)
    ] |}]

let%expect_test "props" =
  print_init_test {|
let {a, b} = 42;
  |};
  [%expect {|
    [
      (2, 5) to (2, 6) => (val (2, 13) to (2, 15)).a;
      (2, 8) to (2, 9) => (val (2, 13) to (2, 15)).b;
      (2, 4) to (2, 10) => val (2, 13) to (2, 15)
    ] |}]

let%expect_test "props_lit" =
  print_init_test {|
let {a, '42':b} = 42;
  |};
  [%expect {|
    [
      (2, 5) to (2, 6) => (val (2, 18) to (2, 20)).a;
      (2, 13) to (2, 14) => (val (2, 18) to (2, 20)).42;
      (2, 4) to (2, 15) => val (2, 18) to (2, 20)
    ] |}]

let%expect_test "props_comp_rest" =
  print_init_test {|
let {a, [foo()]: b, ...c} = 42;
  |};
  [%expect {|
    [
      (2, 5) to (2, 6) => (val (2, 28) to (2, 30)).a;
      (2, 17) to (2, 18) => (val (2, 28) to (2, 30)).[computed];
      (2, 23) to (2, 24) => (val (2, 28) to (2, 30)){ ... };
      (2, 4) to (2, 25) => val (2, 28) to (2, 30)
    ] |}]

let%expect_test "props_comp_rest2" =
  print_order_test {|
let x: { [number]: number => number } = { [42]: v => v };
  |};
  [%expect {|
    (2, 4) to (2, 5) =>
    (2, 43) to (2, 45) =>
    (2, 48) to (2, 49) |}]

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
      (2, 4) to (2, 5) => function val (2, 8) to (4, 1);
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
      (2, 4) to (2, 5) => function val (2, 8) to (2, 36);
      (2, 8) to (2, 36) => fun <anonymous>;
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
  return 42;
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
    illegal self-cycle ((2, 9) to (2, 10)) |}]

let%expect_test "recur_func_anno" =
  print_order_test {|
function f(): void {
  return f();
}
  |};
  [%expect {|
    (2, 9) to (2, 10) |}]

let%expect_test "recur_fun_typeof" =
  print_order_test {|
var x = f ();
function f(): typeof x {
}
  |};
  [%expect {|
    illegal scc: (((2, 4) to (2, 5)); ((3, 9) to (3, 10))) |}]

let%expect_test "function_param_contextual" =
  print_order_test {|
var x = 1;
x = (a) => a;
  |};
  [%expect {|
    (2, 4) to (2, 5) =>
    (3, 5) to (3, 6) =>
    (3, 0) to (3, 1) |}]

let%expect_test "deps1" =
  print_order_test {|
type T = number;
let x: T;
function f() {
  x = x;
  return 42;
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
  return 42;
}
  |};
  [%expect {|
    (2, 5) to (2, 6) =>
    legal scc: (((3, 5) to (3, 6)); ((4, 5) to (4, 6))) =>
    illegal scc: (((7, 2) to (7, 3)); ((8, 2) to (8, 3)); ((9, 2) to (9, 3))) =>
    (6, 9) to (6, 15) |}]

let%expect_test "deps2a" =
  print_order_test {|
let x = f();
function f() {
  return x;
}
  |};
  [%expect {|
    illegal scc: (((2, 4) to (2, 5)); ((3, 9) to (3, 10))) |}]

let%expect_test "deps3" =
  print_order_test {|
function invalidate_x() {
  x = null; return x;
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
    (2, 9) to (2, 21) =>
    (8, 10) to (8, 31) |}]

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
var x = (42: T);
type T = typeof x;
  |};
  [%expect {|
    illegal scc: (((2, 4) to (2, 5)); ((3, 5) to (3, 6))) |}]

let%expect_test "func_exp" =
  print_order_test {|
var y = function f(): number {
  return 42;
}
  |};
  [%expect {|
    (2, 4) to (2, 5) =>
    (2, 17) to (2, 18) |}]

let%expect_test "class_def" =
  print_init_test {|
let x;
class C {
  foo() { x = 42; return 42 }
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
  foo() { x = 42; return 42; }
}
x = 10;
  |};
  [%expect {|
    (3, 6) to (3, 7) =>
    (6, 0) to (6, 1) =>
    (4, 10) to (4, 11) |}]

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
    legal scc: (((2, 6) to (2, 7)); ((5, 6) to (5, 7))) |}]

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
    legal scc: (((2, 6) to (2, 7)); ((5, 6) to (5, 7))) |}]

let%expect_test "enum" =
  print_order_test {|
function havoced() {
  var x: E = E.Foo; return 42;
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
    legal scc: (((2, 10) to (2, 11)); ((3, 10) to (3, 11)); ((4, 6) to (4, 7))) |}]

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
  foo() { return y; }
}
declare var x: mixed;
var y;

if (x instanceof C) {
  y = x;
}
  |};
  [%expect {|
    (2, 6) to (2, 7) =>
    (5, 12) to (5, 13) =>
    (8, 17) to (8, 18) =>
    (9, 2) to (9, 3) |}]

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
    illegal scc: (((2, 9) to (2, 10)); ((7, 2) to (7, 3))) =>
    (6, 6) to (6, 7) |}]

let%expect_test "refi_sentinel" =
  print_order_test {|
declare var x: mixed;
let y;

if (x.type === 1) {
  y = x;
}
  |};
  [%expect {|
    (2, 12) to (2, 13) =>
    (5, 4) to (5, 10) =>
    (5, 15) to (5, 16) =>
    (6, 2) to (6, 3) |}]

let%expect_test "declare_class" =
  print_order_test {|
declare class C mixins S { }
var f = new C();
type S = typeof f;
  |};
  [%expect {|
    illegal scc: (((2, 14) to (2, 15)); ((3, 4) to (3, 5)); ((4, 5) to (4, 6))) |}]

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
    legal scc: (((2, 5) to (2, 6)); ((4, 12) to (4, 13))) |}]

let%expect_test "unannotated catch" =
  print_init_test {|
var x;
try {} catch (e) { x = e }
  |};
  [%expect {|
    [
      (3, 14) to (3, 15) => unannotated catch param;
      (3, 19) to (3, 20) => val (3, 23) to (3, 24)
    ] |}]

let%expect_test "catch with mixed" =
  print_init_test {|
var x;
try {} catch (e: mixed) { x = e }
  |};
  [%expect {|
    [
      (3, 14) to (3, 15) => annot (3, 15) to (3, 22);
      (3, 26) to (3, 27) => val (3, 30) to (3, 31)
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
      (2, 12) to (2, 13) => (val (2, 17) to (2, 22))[1];
      (2, 9) to (2, 14) => val (2, 17) to (2, 22)
    ] |}]

let%expect_test "for4" =
  print_init_test {|
for (var [x,y]: T = [1,2];;) { }
  |};
  [%expect {|
    [
      (2, 10) to (2, 11) => (annot (2, 14) to (2, 17))[0];
      (2, 12) to (2, 13) => (annot (2, 14) to (2, 17))[1];
      (2, 9) to (2, 17) => annot (2, 14) to (2, 17)
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
      (2, 12) to (2, 13) => (for in (2, 18) to (2, 21))[1];
      (2, 9) to (2, 14) => for in (2, 18) to (2, 21)
    ] |}]

let%expect_test "for_in4" =
  print_init_test {|
for (var [x,y]: T in foo) { }
  |};
  [%expect {|
    [
      (2, 10) to (2, 11) => (annot (2, 14) to (2, 17))[0];
      (2, 12) to (2, 13) => (annot (2, 14) to (2, 17))[1];
      (2, 9) to (2, 17) => annot (2, 14) to (2, 17)
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
      (2, 12) to (2, 13) => (for of (2, 18) to (2, 21))[1];
      (2, 9) to (2, 14) => for of (2, 18) to (2, 21)
    ] |}]

let%expect_test "for_of4" =
  print_init_test {|
for (var [x,y]: T of foo) { }
  |};
  [%expect {|
    [
      (2, 10) to (2, 11) => (annot (2, 14) to (2, 17))[0];
      (2, 12) to (2, 13) => (annot (2, 14) to (2, 17))[1];
      (2, 9) to (2, 17) => annot (2, 14) to (2, 17)
    ] |}]

let%expect_test "heap_init" =
  print_init_test {|
if (x.y) { x.y }
  |};
  [%expect {|
    [
      (2, 4) to (2, 7) => exp (2, 4) to (2, 7) (hint = [])
    ] |}]

let%expect_test "destructuring init" =
  print_init_test {|
const {foo: [bar, baz = 3], hello: {world: flow}} = global;
  |};
  [%expect {|
    [
      (2, 13) to (2, 16) => ((val (2, 52) to (2, 58)).foo)[0];
      (2, 18) to (2, 21) => ((val (2, 52) to (2, 58)).foo)[1];
      (2, 43) to (2, 47) => ((val (2, 52) to (2, 58)).hello).world;
      (2, 6) to (2, 49) => val (2, 52) to (2, 58);
      (2, 12) to (2, 26) => (val (2, 52) to (2, 58)).foo;
      (2, 35) to (2, 48) => (val (2, 52) to (2, 58)).hello
    ] |}]

let%expect_test "destructuring order" =
  print_order_test {|
const {foo: [bar, baz = 3], hello: {world: flow}} = global;
  |};
  [%expect {|
    (2, 6) to (2, 49) =>
    (2, 12) to (2, 26) =>
    (2, 13) to (2, 16) =>
    (2, 18) to (2, 21) =>
    (2, 35) to (2, 48) =>
    (2, 43) to (2, 47) |}]

let%expect_test "refi_recorded_read" =
  print_order_test {|
if (x.prop) {
  const y = x.prop;
}
  |};
  [%expect {|
    (2, 4) to (2, 10) =>
    (3, 8) to (3, 9) |}]

let%expect_test "sentinel_refi" =
  print_order_test {|
if (obj.d.type === "a") {
  let {d: {payload, type}} = obj;
}
  |};
  [%expect {|
    (2, 4) to (2, 9) =>
    (2, 4) to (2, 14) =>
    (3, 6) to (3, 26) =>
    (2, 19) to (2, 22) =>
    (3, 10) to (3, 25) =>
    (3, 11) to (3, 18) =>
    (3, 20) to (3, 24) |}]

let%expect_test "inc" =
  print_order_test {|
let x;
var y;
function f() {
  y = x; return 42;
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
  y = x; return 42;
}
function h() {
  x += y; return 42;
}
  |};
  [%expect {|
    illegal scc: (((5, 2) to (5, 3)); (illegal self-cycle ((8, 2) to (8, 3)))) =>
    (4, 9) to (4, 10) =>
    (7, 9) to (7, 10) |}]

let%expect_test "type_alias" =
  print_order_test {|
type $unwrap = <T>(l: JSResourceReference<T>) => T;

class JSResourceReference<+T> {
  static loadAll<I: Array<JSResourceReference<mixed>>>(
    loaders: I,
    callback: I,
  ): void {
    // ...load the modules and then pass them to the callback
  }
}

  |};
  [%expect {|
    (2, 16) to (2, 17) =>
    (4, 27) to (4, 28) =>
    legal scc: (((4, 6) to (4, 25)); ((5, 17) to (5, 18)); ((6, 4) to (6, 11)); ((7, 4) to (7, 12))) =>
    (2, 5) to (2, 12) |}]

let%expect_test "type_params_dep" =
  print_order_test {|
function foo<A, B: A, C = A>() {}
  |};
  [%expect {|
    (2, 13) to (2, 14) =>
    (2, 16) to (2, 17) =>
    (2, 22) to (2, 23) =>
    (2, 9) to (2, 12)
   |}]

let%expect_test "refi" =
  print_order_test {|
function havoc(x) {
  let y;
  if (x instanceof R.Y) {
    y = x;
  }
}
import * as R from 'foo';
  |};
  [%expect {|
    (2, 15) to (2, 16) =>
    (2, 9) to (2, 14) =>
    (8, 12) to (8, 13) =>
    (4, 19) to (4, 22) =>
    (5, 4) to (5, 5) |}]

let%expect_test "object_prop_assign" =
  print_order_test {|
function test(obj) {
  obj.prop = 1;
}
  |};
  [%expect {|
    (2, 14) to (2, 17) =>
    (2, 9) to (2, 13) =>
    (3, 2) to (3, 10) |}]

let%expect_test "array_index_assign" =
  print_order_test {|
function test(arr, i) {
  arr[i++] = 1;
}
  |};
  [%expect {|
    (2, 14) to (2, 17) =>
    (2, 19) to (2, 20) =>
    (2, 9) to (2, 13) =>
    (3, 6) to (3, 7) |}]

let%expect_test "this" =
  print_order_test {|
class C {
  f() { return this.g() }
  g() { return 42; }
}
  |};
  [%expect {|
    (2, 6) to (2, 7) |}]

let%expect_test "class_question" =
  print_order_test {|
class C {
  f(x: C): boolean { }
}
  |};
  [%expect {|
    legal scc: (((2, 6) to (2, 7)); ((3, 4) to (3, 5))) |}]

let%expect_test "arr" =
  print_order_test {|
var x = []
function w() {
  var z = x; return 42;
}
function g() {
  x = 42; return 42;
}
x.push(42);
  |};
  [%expect {|
    (9, 7) to (9, 9) =>
    (2, 4) to (2, 5) =>
    (4, 6) to (4, 7) =>
    (3, 9) to (3, 10) =>
    (7, 2) to (7, 3) =>
    (6, 9) to (6, 10) |}]

let%expect_test "declare module" =
  print_order_test {|
declare module 'a' {
  declare type T = S;
}
type S = number;
declare module B {
  declare function f(S): S;
}
  |};
  [%expect {|
    (5, 5) to (5, 6) =>
    (3, 15) to (3, 16) =>
    (2, 15) to (2, 18) =>
    (7, 19) to (7, 20) =>
    (6, 15) to (6, 16) |}]

let%expect_test "declare module crash regression" =
  print_order_test {|
declare module 'a' {
  declare type T = number;
  declare type T = number;
}
  |};
  [%expect {|
    (3, 15) to (3, 16) =>
    (2, 15) to (2, 18) |}]

let%expect_test "empty arr" =
  print_order_test {|
var x = [];
x.push(42);
  |};
  [%expect {|
    (3, 7) to (3, 9) =>
    (2, 4) to (2, 5) |}]

let%expect_test "left to right deps" =
  print_order_test {|
pipe(
  "",
  s => 1,
  n => "",
);
  |};
  [%expect {|
    (3, 2) to (3, 4) =>
    (4, 2) to (4, 3) =>
    (4, 2) to (4, 8) =>
    (5, 2) to (5, 3) =>
    (5, 2) to (5, 9) |}]

let%expect_test "left to right cycles" =
  print_order_test {|
let x;
pipe(
  "",
  s => x,
  n => x = n,
);
  |};
  [%expect {|
    (4, 2) to (4, 4) =>
    (5, 2) to (5, 3) =>
    illegal scc: (((5, 2) to (5, 8)); ((6, 2) to (6, 3)); ((6, 7) to (6, 8))) =>
    (6, 2) to (6, 12) |}]

let%expect_test "inner-outer-class" =
  print_order_test {|
class Outer {
 constructor() {
   var Inner = class {
      constructor() {
        var x = new Outer();
      }
   };
 }
}
  |};
  [%expect {|
    (2, 6) to (2, 11) =>
    (4, 15) to (8, 4) =>
    (6, 12) to (6, 13) =>
    (4, 7) to (4, 12) |}]

let%expect_test "arr cycle" =
  print_order_test {|
function foo(arr: $ReadOnlyArray<Object>) {
    return arr.map(foo)
        .reduce((acc, item) => acc.concat(item), [])
  }
  |};
  [%expect {|
    (2, 13) to (2, 16) =>
    illegal scc: ((illegal self-cycle ((2, 9) to (2, 12))); ((3, 19) to (3, 22)); ((4, 16) to (4, 47)); ((4, 17) to (4, 20)); ((4, 22) to (4, 26)); ((4, 42) to (4, 46)); ((4, 49) to (4, 51))) |}]

let%expect_test "new cycle" =
  print_order_test {|
function foo(arr: $ReadOnlyArray<Object>) {
    return arr.map(foo)
        .reduce((acc, item) => acc.concat(item), new Set())
  }
  |};
  [%expect {|
    (2, 13) to (2, 16) =>
    illegal scc: ((illegal self-cycle ((2, 9) to (2, 12))); ((3, 19) to (3, 22)); ((4, 16) to (4, 47)); ((4, 17) to (4, 20)); ((4, 22) to (4, 26)); ((4, 42) to (4, 46)); ((4, 49) to (4, 58))) |}]

let%expect_test "lit cycle" =
  print_order_test {|
function foo(arr: $ReadOnlyArray<Object>) {
    return arr.map(foo)
        .reduce((acc, item) => acc.concat(item), 42)
  }
  |};
  [%expect {|
    (2, 13) to (2, 16) =>
    (4, 49) to (4, 51) =>
    illegal scc: ((illegal self-cycle ((2, 9) to (2, 12))); ((3, 19) to (3, 22)); ((4, 16) to (4, 47)); ((4, 17) to (4, 20)); ((4, 22) to (4, 26)); ((4, 42) to (4, 46))) |}]

let%expect_test "react_fwd" =
  print_order_test {|
import * as React from 'react';

const RC = <T />

function T(): void {}
|};
    [%expect {|
      (2, 12) to (2, 17) =>
      (6, 9) to (6, 10) =>
      (4, 6) to (4, 8) |}]

let%expect_test "std_jsx" =
  print_order_test {|
<FirstElement />
function time_to_create_some_elements_bro() {
  return <SecondElement />
}
function createMikesCoolElement() { return 42 };
import * as React from 'react';
|};
    [%expect {|
      (7, 12) to (7, 17) =>
      (3, 9) to (3, 41) =>
      (6, 9) to (6, 31) |}]

let%expect_test "custom_jsx_pragma" =
  print_order_test ~custom_jsx:(Some "createMikesCoolElement") {|
<FirstElement />
function time_to_create_some_elements_bro() {
  return <SecondElement />
}
function createMikesCoolElement() { return 42 };
import * as React from 'react';
|};
    [%expect {|
      (6, 9) to (6, 31) =>
      (3, 9) to (3, 41) =>
      (7, 12) to (7, 17) |}]

let%expect_test "jsx_pragma_member_expr" =
  print_order_test ~custom_jsx:(Some "Test.f") {|
var x = <Component />
function Component() {}
import * as React from 'react';
|};
    [%expect {|
      (3, 9) to (3, 18) =>
      (2, 4) to (2, 5) =>
      (4, 12) to (4, 17)
       |}]

let%expect_test "automatic_react_runtime" =
  print_order_test ~react_runtime_automatic:true {|
<FirstElement />
function time_to_create_some_elements_bro() {
  return <SecondElement />
}
function createMikesCoolElement() { return 42 };
import * as React from 'react';
|};
    [%expect {|
      (3, 9) to (3, 41) =>
      (6, 9) to (6, 31) =>
      (7, 12) to (7, 17) |}]

let%expect_test "fully_annotated_function_expr_and_arrow_self_recursive" =
  print_order_test {|
const foo = (): number => foo();
const bar = function (): number { return bar() };
const baz = function _(): number { return baz() };
|};
    [%expect {|
      (2, 6) to (2, 9) =>
      (3, 6) to (3, 9) =>
      (3, 12) to (3, 48) =>
      (4, 6) to (4, 9) =>
      (4, 21) to (4, 22) |}]

let%expect_test "return_annot" =
  print_order_test {|
const S = ()=> {
  const x = useStyle();
};

type Styles = 1

const useStyle = (): Styles => { return 1; }
|};
    [%expect {|
      (2, 6) to (2, 7) =>
      (6, 5) to (6, 11) =>
      (8, 6) to (8, 14) =>
      (3, 8) to (3, 9) |}]

let%expect_test "provider_refi" =
  print_order_test {|
declare var rule: {title_label: string};
const titlesAdlabels = [];
rule.title_label = "a";
titlesAdlabels.push(rule.title_label);
|};
    [%expect {|
      (2, 12) to (2, 16) =>
      (4, 0) to (4, 16) =>
      (5, 20) to (5, 36) =>
      (3, 6) to (3, 20) |}]

let%expect_test "logic_op_assign_repeat" =
  print_order_test {|
function member_op_assignment_refinement_ok(o: {p: ?number}) {
  o.p &&= 3;
  o.p &&= 3;
  o.p &&= 3;
}
|};
    [%expect {|
      (2, 44) to (2, 45) =>
      (2, 9) to (2, 43) =>
      (3, 2) to (3, 5) =>
      (4, 2) to (4, 5) =>
      (5, 2) to (5, 5)
        |}]

let%expect_test "logic_op_assign_repeat" =
  print_order_test {|
declare var values: mixed;

if (values.b === values.a) {
}
|};
    [%expect {|
      (2, 12) to (2, 18) =>
      (4, 17) to (4, 25) |}]

let%expect_test "def_class" =
  print_order_test {|
class A {
  B(defaultValue: boolean = false): void {}
}
|};
    [%expect {| legal scc: (((2, 6) to (2, 7)); ((3, 4) to (3, 16))) |}]

let%expect_test "def_class_fn_def" =
  print_order_test {|
class A {
  B<T>(
    localize: mixed = (name: T): string => f(name),
   ) { }
}
declare var f: any;
|};
    [%expect {|
      legal scc: (((2, 6) to (2, 7)); ((3, 4) to (3, 5)); ((4, 4) to (4, 12))) =>
      (4, 23) to (4, 27) =>
      (7, 12) to (7, 13) =>
      (4, 45) to (4, 49) |}]

let%expect_test "pred" =
  print_order_test {|
function isStack(maybeStack: mixed): boolean %checks { return maybeStack instanceof Stack; }
declare class Stack {
  static isStack: typeof isStack;
}

|};
    [%expect {|
      (2, 17) to (2, 27) =>
      legal scc: (((2, 9) to (2, 16)); ((2, 84) to (2, 89)); ((3, 14) to (3, 19))) |}]

let%expect_test "statics cycle" =
  print_order_test {|
function Dialog(): void { };

Dialog.Prop = function(): void {
    return (Dialog: any)
}
|};
    [%expect {|
      (4, 14) to (6, 1) =>
      (2, 9) to (2, 15) =>
      (4, 0) to (4, 11) |}]

let%expect_test "callee hint cycle" =
  print_order_test {|
g(function h() { return f() })
|};
    [%expect {|
      (2, 11) to (2, 12) =>
      (2, 2) to (2, 29) |}]

let%expect_test "callee hint cycle" =
  print_order_test {|
g(function () { return f() })
|};
    [%expect {|
      (2, 2) to (2, 28) =>
      (2, 2) to (2, 28) |}]

let%expect_test "instantiate hint cycle" =
  print_order_test {|
import * as React from 'react';
<F attr={function () { return f() }} />
|};
    [%expect {|
      (2, 12) to (2, 17) =>
      (3, 9) to (3, 35) |}]

let%expect_test "fwd ref provider" =
  print_order_test {|
let cc;
function foo() {
    cc = 1;
    return 42;
}
cc = null;
|};
    [%expect {|
      (4, 4) to (4, 6) =>
      (7, 0) to (7, 2) =>
      (3, 9) to (3, 12) |}]

let%expect_test "declare function overload read by typeof" =
  print_order_test {|
type T = typeof foo;
declare function foo(): void;
declare function foo(): void;
|};
    [%expect {|
      (3, 17) to (3, 20) =>
      (4, 17) to (4, 20) =>
      (2, 5) to (2, 6) |}]
