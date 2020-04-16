(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

module Translate =
  Estree_translator.Translate
    (Json_of_estree)
    (struct
      (* TODO: make these configurable via CLI flags *)
      let include_interned_comments = false

      let include_comments = true

      let include_locs = true
    end)

let pretty_print program =
  Source.contents
  @@ Pretty_printer.print ~source_maps:None ~skip_endline:true
  @@ Js_layout_generator.program_simple program

let print_ast program = Hh_json.json_to_string ~pretty:true @@ Translate.program None program

let verify_and_generate
    ?prevent_munge ?facebook_fbt ?ignore_static_propTypes ?facebook_keyMirror contents =
  let contents = String.concat "\n" contents in
  let ast = Signature_verifier_test.parse contents in
  let exports_info = File_sig.With_Loc.program_with_exports_info ~ast ~module_ref_prefix:None in
  let signature =
    match exports_info with
    | Ok exports_info -> Signature_builder.program ast ~exports_info
    | Error _ -> failwith "Signature builder failure!"
  in
  Signature_builder.Signature.verify_and_generate
    ?prevent_munge
    ?facebook_fbt
    ?ignore_static_propTypes
    ?facebook_keyMirror
    signature
    ast

let mk_signature_generator_test
    ?prevent_munge
    ?facebook_fbt
    ?ignore_static_propTypes
    ?facebook_keyMirror
    contents
    expected_msgs
    ctxt =
  let msgs =
    let (_errors, _env, program) =
      verify_and_generate
        ?prevent_munge
        ?facebook_fbt
        ?ignore_static_propTypes
        ?facebook_keyMirror
        contents
    in
    String.split_on_char '\n' @@ pretty_print program
  in
  let printer v = "\n" ^ String.concat "\n" v in
  assert_equal
    ~ctxt
    ~cmp:(Signature_verifier_test.eq printer)
    ~printer
    ~msg:"Results don't match!"
    expected_msgs
    msgs

let mk_generated_signature_file_sig_test
    ?prevent_munge
    ?facebook_fbt
    ?ignore_static_propTypes
    ?facebook_keyMirror
    contents
    expected_msgs
    ctxt =
  let msgs =
    let (_errors, _env, program) =
      verify_and_generate
        ?prevent_munge
        ?facebook_fbt
        ?ignore_static_propTypes
        ?facebook_keyMirror
        contents
    in
    match File_sig.With_Loc.program ~ast:program ~module_ref_prefix:None with
    | Ok fs -> File_sig.With_Loc.to_string fs |> String.split_on_char '\n'
    | Error _ -> []
  in
  let printer v = "\n" ^ String.concat "\n" v in
  assert_equal
    ~ctxt
    ~cmp:(Signature_verifier_test.eq printer)
    ~printer
    ~msg:"Results don't match!"
    expected_msgs
    msgs

let mk_verified_signature_generator_test
    ?prevent_munge ?facebook_fbt ?ignore_static_propTypes ?facebook_keyMirror contents ctxt =
  let msgs =
    let (_errors, _env, _program) =
      verify_and_generate
        ?prevent_munge
        ?facebook_fbt
        ?ignore_static_propTypes
        ?facebook_keyMirror
        contents
    in
    []
  in
  let printer v = String.concat "\n" v in
  assert_equal
    ~ctxt
    ~cmp:(Signature_verifier_test.eq printer)
    ~printer
    ~msg:"Results don't match!"
    []
    msgs

let verified_signature_generator_tests =
  List.fold_left
    (fun acc
         ( (prevent_munge, facebook_fbt, ignore_static_propTypes, facebook_keyMirror, name),
           contents,
           error_msgs,
           _other_msgs ) ->
      if error_msgs = [] then
        let name = "verified_" ^ name in
        ( name
        >:: mk_verified_signature_generator_test
              ?prevent_munge
              ?facebook_fbt
              ?ignore_static_propTypes
              ?facebook_keyMirror
              contents )
        :: acc
      else
        acc)
    []
    Signature_verifier_test.tests_data

let generated_signature_file_sig_tests =
  [
    "multiple_bindings_destructured_require"
    >:: mk_generated_signature_file_sig_test
          [
            "const {";
            "  foo: foo2,";
            "  bar: { barX },";
            "} = require('./something');";
            "module.exports = { foo2, barX }";
          ]
          [
            "{";
            "  module_sig: {";
            "    requires: [";
            "      Require (./something, Some (BindIdent: $4));";
            "      Require (./something, Some (BindIdent: $1));";
            "    ];";
            "    module_kind: CommonJS;";
            "    type_exports_named: {";
            "    };";
            "    type_exports_star: [";
            "    ];";
            "  };";
            "}";
          ];
  ]

let tests =
  "signature_generator"
  >::: [
         "dead_type" >:: mk_signature_generator_test ["type U = number"] [];
         "dead_declare_type" >:: mk_signature_generator_test ["declare type U = number"] [];
         "dead_types_transitive"
         >:: mk_signature_generator_test ["type U = number"; "declare type T = U"] [];
         "export_type_alias"
         >:: mk_signature_generator_test
               ["type U = number"; "export type T = U"]
               ["type U = number;"; "type T = U;"; "export type {T};"];
         (* TODO: change of spaces *)
         "export_type_specifier"
         >:: mk_signature_generator_test
               ["type U = number"; "export type { U }"]
               ["type U = number;"; "export type {U};"];
         "export_type_specifier_local"
         >:: mk_signature_generator_test
               ["type U = number"; "export type { U as U2 }"]
               ["type U = number;"; "export type {U as U2};"];
         "export_type_specifier_remote"
         >:: mk_signature_generator_test
               ["export type { K } from './foo'"]
               ["export type {K} from \"./foo\";"];
         (* TODO: change of quotes *)
         "export_type_specifier_remote_local1"
         >:: mk_signature_generator_test
               ["export type { K as K2 } from './foo'"]
               ["export type {K as K2} from \"./foo\";"];
         "export_type_specifier_remote_local2"
         >:: mk_signature_generator_test
               ["type K = number"; "export type { K as K2 } from './foo'"]
               ["export type {K as K2} from \"./foo\";"];
         "export_type_specifier_remote_local3"
         >:: mk_signature_generator_test
               ["export type K = number"; "export type { K as K2 } from './foo'"]
               ["type K = number;"; "export type {K};"; "export type {K as K2} from \"./foo\";"];
         "export_type_batch"
         >:: mk_signature_generator_test
               ["export type * from './foo'"]
               ["export type * from \"./foo\";"];
         "dead_var" >:: mk_signature_generator_test ["var x: number = 0"] [];
         "dead_declare_var" >:: mk_signature_generator_test ["declare var x: number"] [];
         "dead_transitive" >:: mk_signature_generator_test ["class C { }"; "var x: C = new C"] [];
         "module_exports_function_expression"
         >:: mk_signature_generator_test
               ["module.exports = function() { }"]
               ["declare module.exports: () => void;"];
         "module_exports_literal"
         >:: mk_signature_generator_test
               ["module.exports = 'hello'"]
               ["declare module.exports: $TEMPORARY$string<'hello'>;"];
         "module_exports_object"
         >:: mk_signature_generator_test
               ["module.exports = { x: 'hello' }"]
               ["declare module.exports: $TEMPORARY$object<{|x: $TEMPORARY$string<'hello'>|}>;"];
         "module_exports_object_with_spread"
         >:: mk_signature_generator_test
               ["const y = { y: 'world'};"; "module.exports = { x: 'hello', ...y }"]
               [
                 "declare var y: $TEMPORARY$object<{|y: $TEMPORARY$string<'world'>|}>;";
                 "declare module.exports: $TEMPORARY$object<";
                 "  {|x: $TEMPORARY$string<'hello'>, ...typeof y|},";
                 ">;";
               ];
         "module_exports_array_one"
         >:: mk_signature_generator_test
               ["module.exports = ['hello']"]
               ["declare module.exports: $TEMPORARY$array<$TEMPORARY$string<'hello'>>;"];
         "module_exports_array_many"
         >:: mk_signature_generator_test
               ["module.exports = ['hello', 42]"]
               [
                 "declare module.exports: $TEMPORARY$array<";
                 "  $TEMPORARY$string<'hello'> | $TEMPORARY$number<42>,";
                 ">;";
               ];
         "module_exports_class_expression"
         >:: mk_signature_generator_test
               ["module.exports = class { m(x: number): number { return x; } }"]
               ["declare class $1 {m(x: number): number}"; "declare module.exports: typeof $1;"];
         (* outlining *)
         "module_exports_named_class_expression"
         >:: mk_signature_generator_test
               ["module.exports = class C { m(x: C): C { return x; } }"]
               ["declare class C {m(x: C): C}"; "declare module.exports: typeof C;"];
         (* outlining *)
         "module_exports_require"
         >:: mk_signature_generator_test
               ["module.exports = require('./foo')"]
               ["const $1 = require(\"./foo\");"; "declare module.exports: typeof $1;"];
         (* outlining *)
         "module_exports_import"
         >:: mk_signature_generator_test
               ["module.exports = import('./foo')"]
               ["import * as $1 from \"./foo\";"; "declare module.exports: typeof $1;"];
         (* outlining *)
         "module_exports_bindings"
         >:: mk_signature_generator_test
               [
                 "function foo() { }";
                 "class C { }";
                 "const x: number = 0";
                 "const o = { p: x };";
                 "module.exports = { foo, C, x, p: o.p }";
               ]
               [
                 "declare function foo(): void;";
                 "declare class C {}";
                 "declare var x: number;";
                 "declare var o: $TEMPORARY$object<{|p: typeof x|}>;";
                 "declare module.exports: $TEMPORARY$object<";
                 "  {|foo: typeof foo, C: typeof C, x: typeof x, p: typeof o.p|},";
                 ">;";
               ];
         "declare_module_exports"
         >:: mk_signature_generator_test
               ["declare module.exports: () => void"]
               ["declare module.exports: () => void;"];
         "export_default_expression"
         >:: mk_signature_generator_test
               ["export default function(x: number): number { return x; }"]
               ["declare export default (x: number) => number;"];
         "declare_export_default_type"
         >:: mk_signature_generator_test
               ["declare export default (number) => number"]
               ["declare export default (number) => number;"];
         "export_default_function_declaration"
         >:: mk_signature_generator_test
               ["export default function foo(): void { }"]
               ["declare function foo(): void;"; "export {foo as default};"];
         "export_default_class_declaration"
         >:: mk_signature_generator_test
               ["export default class C { x: number = 0; }"]
               ["declare class C {x: number}"; "export {C as default};"];
         "export_default_class_declaration_with_private_fields"
         >:: mk_signature_generator_test
               ["export default class C { #x: number = 0; }"]
               ["declare class C {}"; "export {C as default};"];
         "declare_export_default_function_declaration"
         >:: mk_signature_generator_test
               ["declare export default function foo(): void;"]
               ["declare function foo(): void;"; "export {foo as default};"];
         "declare_export_default_class_declaration"
         >:: mk_signature_generator_test
               ["declare export default class C { x: number; }"]
               ["declare class C {x: number}"; "export {C as default};"];
         "export_function_declaration"
         >:: mk_signature_generator_test
               ["export function foo(): void { }"]
               ["declare function foo(): void;"; "export {foo};"];
         "export_class_declaration"
         >:: mk_signature_generator_test
               ["export class C { x: number = 0; }"]
               ["declare class C {x: number}"; "export {C};"];
         "declare_export_function_declaration"
         >:: mk_signature_generator_test
               ["declare export function foo(): void;"]
               ["declare function foo(): void;"; "export {foo};"];
         "declare_export_class_declaration"
         >:: mk_signature_generator_test
               ["declare export class C { x: number; }"]
               ["declare class C {x: number}"; "export {C};"];
         "export_specifier"
         >:: mk_signature_generator_test
               ["var x: number = 0"; "export { x }"]
               ["declare var x: number;"; "export {x};"];
         "export_specifier_local"
         >:: mk_signature_generator_test
               ["var x: number = 0"; "export { x as x2 }"]
               ["declare var x: number;"; "export {x as x2};"];
         "export_specifier_remote"
         >:: mk_signature_generator_test
               ["export { k } from './foo'"]
               ["export {k} from \"./foo\";"];
         "export_specifier_remote_local1"
         >:: mk_signature_generator_test
               ["export { k as k2 } from './foo'"]
               ["export {k as k2} from \"./foo\";"];
         "export_specifier_remote_local2"
         >:: mk_signature_generator_test
               ["function k() { }"; "export { k as k2 } from './foo'"]
               ["export {k as k2} from \"./foo\";"];
         "export_specifier_remote_local3"
         >:: mk_signature_generator_test
               ["export function k() { }"; "export { k as k2 } from './foo'"]
               ["declare function k(): void;"; "export {k};"; "export {k as k2} from \"./foo\";"];
         "export_batch"
         >:: mk_signature_generator_test ["export * from './foo'"] ["export * from \"./foo\";"];
         "export_batch_local"
         >:: mk_signature_generator_test
               ["export * as Foo from './foo'"]
               ["export * as Foo from \"./foo\";"];
         "import_default"
         >:: mk_signature_generator_test
               ["import C from './foo'"; "declare module.exports: C"]
               ["import C from \"./foo\";"; "declare module.exports: C;"];
         "import_specifier"
         >:: mk_signature_generator_test
               ["import { C } from './foo'"; "declare module.exports: C"]
               ["import {C} from \"./foo\";"; "declare module.exports: C;"];
         "import_specifier_local"
         >:: mk_signature_generator_test
               ["import { C as C2 } from './foo'"; "declare module.exports: C2"]
               ["import {C as C2} from \"./foo\";"; "declare module.exports: C2;"];
         "import_specifier_local_dead"
         >:: mk_signature_generator_test
               ["import { C as C2 } from './foo'"; "declare module.exports: C"]
               ["declare module.exports: C;"];
         "import_batch"
         >:: mk_signature_generator_test
               ["import * as Foo from './foo'"; "declare module.exports: Foo.C"]
               ["import * as Foo from \"./foo\";"; "declare module.exports: Foo.C;"];
         "import_type_default"
         >:: mk_signature_generator_test
               ["import type C from './foo'"; "declare module.exports: C"]
               ["import type C from \"./foo\";"; "declare module.exports: C;"];
         "import_type_specifier"
         >:: mk_signature_generator_test
               ["import type { T } from './foo'"; "declare module.exports: T"]
               ["import type {T} from \"./foo\";"; "declare module.exports: T;"];
         "import_type_specifier2"
         >:: mk_signature_generator_test
               ["import { type T } from './foo'"; "declare module.exports: T"]
               [
                 "import type {T} from \"./foo\";";
                 (* TODO: change of specifier kind *)
                 "declare module.exports: T;";
               ];
         "import_type_specifier_local"
         >:: mk_signature_generator_test
               ["import type { T as T2 } from './foo'"; "declare module.exports: T2"]
               ["import type {T as T2} from \"./foo\";"; "declare module.exports: T2;"];
         "import_type_specifier_local2"
         >:: mk_signature_generator_test
               ["import { type T as T2 } from './foo'"; "declare module.exports: T2"]
               ["import type {T as T2} from \"./foo\";"; "declare module.exports: T2;"];
         "import_type_specifier_local_dead"
         >:: mk_signature_generator_test
               ["import type { T as T2 } from './foo'"; "declare module.exports: T"]
               ["declare module.exports: T;"];
         "import_typeof_specifier"
         >:: mk_signature_generator_test
               ["import { typeof x as T2 } from './foo'"; "declare module.exports: T2"]
               ["import typeof {x as T2} from \"./foo\";"; "declare module.exports: T2;"];
         "import_dynamic" >:: mk_signature_generator_test ["import './foo'"] [];
         "require"
         >:: mk_signature_generator_test
               ["const Foo = require('./foo')"; "declare module.exports: Foo.C"]
               [
                 "declare var Foo: typeof $1;";
                 "const $1 = require(\"./foo\");";
                 "declare module.exports: Foo.C;";
               ];
         "require_destructured"
         >:: mk_signature_generator_test
               ["const { C } = require('./foo')"; "declare module.exports: C"]
               [
                 "declare var C: typeof $2.C;";
                 "declare var $2: typeof $1;";
                 "const $1 = require(\"./foo\");";
                 "declare module.exports: C;";
               ];
         "require_destructured_local"
         >:: mk_signature_generator_test
               ["const { C: C2 } = require('./foo')"; "declare module.exports: C2"]
               [
                 "declare var C2: typeof $2.C;";
                 "declare var $2: typeof $1;";
                 "const $1 = require(\"./foo\");";
                 "declare module.exports: C2;";
               ];
         "require_destructured_deep"
         >:: mk_signature_generator_test
               [
                 "const { C: C2, D: { E: E2 } } = require('./foo')";
                 "declare module.exports: [ C2, E2 ]";
               ]
               [
                 "declare var C2: typeof $2.C;";
                 "declare var E2: typeof $5.E;";
                 "declare var $5: typeof $4.D;";
                 "declare var $2: typeof $1;";
                 "declare var $4: typeof $3;";
                 "const $1 = require(\"./foo\");";
                 "const $3 = require(\"./foo\");";
                 "declare module.exports: [C2, E2];";
               ];
         "require_destructured_local_dead"
         >:: mk_signature_generator_test
               ["const { C: C2 } = require('./foo')"; "declare module.exports: C"]
               ["declare module.exports: C;"];
         "composite"
         >:: mk_signature_generator_test
               [
                 "export type T = number";
                 "type U = T";
                 (* reachable *)
                 "import { type V } from './foo'";
                 (* dead *)
                 "type W = [U, V]";
                 (* dead *)
                 "function foo() { return [0, 0]; }";
                 (* dead *)
                 "class B { +x: T = 0; m() { (foo(): W); } }";
                 (* reachable, but as declaration *)
                 "export interface A { +x: U; }";
                 "module.exports = function(x: B): A { return x; }";
               ]
               [
                 "type T = number;";
                 "type U = T;";
                 "";
                 (* TODO: pretty printing adds newlines for dead stuff *)
                 "declare class B {+x: T, m(): void}";
                 "interface A {+x: U}";
                 "export type {T};";
                 "";
                 "export type {A};";
                 "declare module.exports: (x: B) => A;";
               ];
         "class_statics"
         >:: mk_signature_generator_test
               ["export class C {"; "  static x: number = 0;"; "  static foo(): void { }"; "}"]
               [
                 "declare class C {";
                 "  static x: number,";
                 "  static foo(): void,";
                 "}";
                 "export {C};";
               ];
         "class_statics2"
         >:: mk_signature_generator_test
               ["export class C {"; "  foo: () => void;"; "  static foo(): void { }"; "}"]
               [
                 "declare class C {";
                 "  foo: () => void,";
                 "  static foo(): void,";
                 "}";
                 "export {C};";
               ];
         "class_implements"
         >:: mk_signature_generator_test
               [
                 "interface I {";
                 "  foo(x?: string): void;";
                 "}";
                 "export class C implements I {";
                 "  foo(x?: string): void { }";
                 "}";
               ]
               [
                 "interface I {";
                 "  foo(x?: string): void,";
                 "}";
                 "declare class C";
                 "  implements I {";
                 "  foo(x?: string): void,";
                 "}";
                 "export {C};";
               ];
         "class_extends_error"
         >:: mk_signature_generator_test
               ["export class C extends (undefined: any) { }"]
               ["declare class C extends $TEMPORARY$Super$FlowFixMe {}"; "export {C};"];
         "function_overloading"
         >:: mk_signature_generator_test
               [
                 "declare function foo<T>(x: T): void;";
                 "declare function foo<T,S>(x: T): void;";
                 "export function foo<T,S,R>(x: T): void { }";
               ]
               [
                 "declare function foo<T>(x: T): void;";
                 "declare function foo<T, S>(x: T): void;";
                 "export {foo};";
               ];
         "function_overloading2"
         >:: mk_signature_generator_test
               [
                 "declare export function foo<A>(x?: null, y?: null): void;";
                 "declare export function foo<A,B>(x: null, y?: null): void;";
               ]
               [
                 "declare function foo<A>(x?: null, y?: null): void;";
                 "declare function foo<A, B>(x: null, y?: null): void;";
                 "export {foo};";
               ];
         "opaque_type"
         >:: mk_signature_generator_test
               [
                 "declare export opaque type T1";
                 "declare export opaque type T2: number";
                 "opaque type T3 = number";
                 (* dead *)
                 "export opaque type T4: number = T3";
                 "opaque type T5 = number";
                 "export opaque type T6: T5 = number";
                 "export opaque type T7 = number;";
               ]
               [
                 "opaque type T1;";
                 "opaque type T2: number;";
                 "opaque type T3 = number;";
                 "opaque type T4: number = T3;";
                 "opaque type T5 = number;";
                 "opaque type T6: T5 = number;";
                 "opaque type T7 = number;";
                 "export type {T1};";
                 "export type {T2};";
                 "";
                 "export type {T4};";
                 "";
                 "export type {T6};";
                 "export type {T7};";
               ];
         "import_then_destructure"
         >:: mk_signature_generator_test
               ["import Foo from 'foo';"; "const { Bar } = Foo;"; "module.exports = Bar;"]
               [
                 "import Foo from \"foo\";";
                 "declare var Bar: typeof Foo.Bar;";
                 "declare module.exports: typeof Bar;";
               ];
         "import_then_destructure2"
         >:: mk_signature_generator_test
               ["import Foo from 'foo';"; "const { Foo: Bar } = { Foo };"; "module.exports = Bar;"]
               [
                 "import Foo from \"foo\";";
                 "declare var Bar: typeof $1.Foo;";
                 "declare var $1: $TEMPORARY$object<{|Foo: typeof Foo|}>;";
                 "declare module.exports: typeof Bar;";
               ];
         "optional_param"
         >:: mk_signature_generator_test
               ["module.exports = function(x?: number) { }"]
               ["declare module.exports: (x?: number) => void;"];
         "optional_param_default"
         >:: mk_signature_generator_test
               ["module.exports = function(x: number = 0) { }"]
               ["declare module.exports: (x?: number) => void;"];
         "optional_destructured_param_default"
         >:: mk_signature_generator_test
               ["module.exports = function({ x }: { x: number } = { x: 0 }) { }"]
               ["declare module.exports: (_?: {x: number}) => void;"];
         "array_summary_number"
         >:: mk_signature_generator_test
               ["module.exports = [1, 2, 3]"]
               [
                 "declare module.exports: $TEMPORARY$array<";
                 "  $TEMPORARY$number<1> | $TEMPORARY$number<2> | $TEMPORARY$number<3>,";
                 ">;";
               ];
         "array_summary_array"
         >:: mk_signature_generator_test
               ["module.exports = [[1, 2], [3]]"]
               [
                 "declare module.exports: $TEMPORARY$array<";
                 "  ";
                 "    | $TEMPORARY$array<$TEMPORARY$number<1> | $TEMPORARY$number<2>>";
                 "    | $TEMPORARY$array<$TEMPORARY$number<3>>,";
                 ">;";
               ];
         "array_summary_object"
         >:: mk_signature_generator_test
               ["module.exports = [{ x: 1 }, { x: 2 }]"]
               [
                 "declare module.exports: $TEMPORARY$array<";
                 "  ";
                 "    | $TEMPORARY$object<{|x: $TEMPORARY$number<1>|}>";
                 "    | $TEMPORARY$object<{|x: $TEMPORARY$number<2>|}>,";
                 ">;";
               ];
         "array_summary_object_array"
         >:: mk_signature_generator_test
               ["module.exports = [{ x: [1, 2] }, { x: [3] }]"]
               [
                 "declare module.exports: $TEMPORARY$array<";
                 "  ";
                 "    | $TEMPORARY$object<";
                 "      {|x: $TEMPORARY$array<$TEMPORARY$number<1> | $TEMPORARY$number<2>>|},";
                 "    >";
                 "    | $TEMPORARY$object<{|x: $TEMPORARY$array<$TEMPORARY$number<3>>|}>,";
                 ">;";
               ];
         "frozen_object"
         >:: mk_signature_generator_test
               ["module.exports = Object.freeze({ foo: 42, bar: 'hello' })"]
               [
                 "declare module.exports: $TEMPORARY$Object$freeze<";
                 "  {|foo: $TEMPORARY$number<42>, bar: $TEMPORARY$string<'hello'>|},";
                 ">;";
               ];
         "fbt_empty_open_close"
         >:: mk_signature_generator_test
               ~facebook_fbt:(Some "FbtElement")
               ["module.exports = <fbt></fbt>"]
               ["declare module.exports: FbtElement;"];
         "fbt_empty_open"
         >:: mk_signature_generator_test
               ~facebook_fbt:(Some "FbtElement")
               ["module.exports = <fbt/>"]
               ["declare module.exports: FbtElement;"];
         "fbt_with_child"
         >:: mk_signature_generator_test
               ~facebook_fbt:(Some "FbtElement")
               ["function foo(){}"; "module.exports = <fbt desc={foo()}></fbt>"]
               ["declare module.exports: FbtElement;"];
         "keyMirror"
         >:: mk_signature_generator_test
               ~facebook_keyMirror:true
               ["module.exports = keyMirror({"; "  a: null,"; "  b: null,"; "})"]
               [
                 "declare module.exports: $TEMPORARY$object<";
                 "  {|";
                 "    a: 'a',";
                 "    b: 'b',";
                 "  |},";
                 ">;";
               ];
         "unusual_cjs_exports1"
         >:: mk_signature_generator_test
               ["exports.wut = 'dead';"; "module.exports = { x: 42 };"]
               ["declare module.exports: $TEMPORARY$object<{|x: $TEMPORARY$number<42>|}>;"];
         "unusual_cjs_exports2"
         >:: mk_signature_generator_test
               ["module.exports = { x: 42 };"; "module.exports.wut = 'wut';"]
               [
                 "declare module.exports: $TEMPORARY$module$exports$assign<";
                 "  $TEMPORARY$object<{|x: $TEMPORARY$number<42>|}>,";
                 "  {";
                 "    wut: $TEMPORARY$string<'wut'>,";
                 "    ...,";
                 "  },";
                 ">;";
               ];
         "unusual_cjs_exports3"
         >:: mk_signature_generator_test
               [
                 "module.exports = { x: 0xdead };";
                 "module.exports.wut = 'dead';";
                 "module.exports = { x: 42 };";
                 "module.exports.wut = 'wut';";
               ]
               [
                 "declare module.exports: $TEMPORARY$module$exports$assign<";
                 "  $TEMPORARY$object<{|x: $TEMPORARY$number<42>|}>,";
                 "  {";
                 "    wut: $TEMPORARY$string<'wut'>,";
                 "    ...,";
                 "  },";
                 ">;";
               ];
         "function_statics"
         >:: mk_signature_generator_test
               ["function bar(): void { };"; "const x = 42;"; "bar.x = x;"; "module.exports = bar;"]
               [
                 "declare var bar: $TEMPORARY$function<";
                 "  () => void,";
                 "  {";
                 "    x: typeof x,";
                 "    ...,";
                 "  },";
                 ">;";
                 "declare var x: $TEMPORARY$number<42>;";
                 "";
                 "declare module.exports: typeof bar;";
               ];
         "function_predicates1"
         >:: mk_signature_generator_test
               [
                 "function foo(str: ?string): boolean %checks {";
                 "return str == null || str === '';";
                 "}";
                 "module.exports = foo;";
               ]
               [
                 "declare function foo(str: ?string): boolean %checks(str == null || str === \"\");";
                 "declare module.exports: typeof foo;";
               ];
         "function_predicates2"
         >:: mk_signature_generator_test
               [
                 "declare function foo(str: ?string): boolean %checks(str == null || str === '');";
                 "module.exports = foo;";
               ]
               [
                 "declare function foo(str: ?string): boolean %checks(str == null || str === \"\");";
                 "declare module.exports: typeof foo;";
               ];
         "function_predicates2"
         >:: mk_signature_generator_test
               [
                 "function foo1(x: ?string): boolean %checks { return x == null || x === ''; };";
                 "function foo2(x: ?string): boolean %checks { return foo1(x); }";
                 "module.exports = foo2;";
               ]
               [
                 "declare function foo1(x: ?string): boolean %checks(x == null || x === \"\");";
                 "declare function foo2(x: ?string): boolean %checks(foo1(x));";
                 "declare module.exports: typeof foo2;";
               ];
         "function_predicates3"
         >:: mk_signature_generator_test
               [
                 "class A {};";
                 "function foo(x: mixed): boolean %checks { return x instanceof A; };";
                 "module.exports = foo;";
               ]
               [
                 "declare class A {}";
                 "declare function foo(x: mixed): boolean %checks(x instanceof A);";
                 "declare module.exports: typeof foo;";
               ];
         "function_predicates4"
         >:: mk_signature_generator_test
               [
                 "function foo(x: mixed): boolean %checks { return typeof x === \"number\"; };";
                 "const obj = { foo };";
                 "function bar(x: mixed): boolean %checks { return obj.foo(x); };";
                 "module.exports = bar;";
               ]
               [
                 "declare function foo(x: mixed): boolean %checks(typeof x === \"number\");";
                 "declare var obj: $TEMPORARY$object<{|foo: typeof foo|}>;";
                 "declare function bar(x: mixed): boolean %checks(obj.foo(x));";
                 "declare module.exports: typeof bar;";
               ];
         "function_predicates5"
         >:: mk_signature_generator_test
               [
                 "export function foo(...x: Array<mixed>): boolean %checks { return typeof x === \"number\"; };";
               ]
               [
                 "declare function foo(...x: Array<mixed>): boolean %checks(typeof x === \"number\");";
                 "export {foo};";
               ];
         "destructure_annot"
         >:: mk_signature_generator_test
               ["var { a }: { a: number } = { a: 0 };"; "module.exports = a"]
               [
                 "declare var a: typeof $1.a;";
                 "declare var $1: {a: number};";
                 "declare module.exports: typeof a;";
               ];
         "destructure_annot2"
         >:: mk_signature_generator_test
               ["var { a: x }: { a: number } = { a: 0 };"; "module.exports = x"]
               [
                 "declare var x: typeof $1.a;";
                 "declare var $1: {a: number};";
                 "declare module.exports: typeof x;";
               ];
         "async_function_1"
         >:: mk_signature_generator_test
               ["async function foo() {};"; "module.exports = foo"]
               ["declare function foo(): Promise<void>;"; "declare module.exports: typeof foo;"];
         "async_function_2"
         >:: mk_signature_generator_test
               ["module.exports = async () => {}"]
               ["declare module.exports: () => Promise<void>;"];
         "async_method"
         >:: mk_signature_generator_test
               ["class C { async m() {} };"; "module.exports = C"]
               ["declare class C {m(): Promise<void>}"; "declare module.exports: typeof C;"];
         "getter"
         >:: mk_signature_generator_test
               ["class C { get x(): string { return \"\" } };"; "module.exports = C"]
               ["declare class C {get x(): string}"; "declare module.exports: typeof C;"];
         "declarations_with_implementation"
         >:: mk_signature_generator_test
               [
                 "declare function foo(x: string): void;";
                 "declare function foo(x: number): void;";
                 "function foo(x: any): any {};";
                 "module.exports = foo";
               ]
               [
                 "declare function foo(x: string): void;";
                 "declare function foo(x: number): void;";
                 "";
                 "declare module.exports: typeof foo;";
               ];
         "declarations_with_exported_implementation"
         >:: mk_signature_generator_test
               ["declare function foo(x: string): void;"; "export function foo(x: any): any {};"]
               ["declare function foo(x: string): void;"; "export {foo};"];
         "static_propTypes"
         >:: mk_signature_generator_test
               ["class C {static propTypes = {foo: 42}}"; "module.exports = C"]
               [
                 "declare class C {static propTypes: $FlowFixMe}";
                 "declare module.exports: typeof C;";
               ];
       ]
       @ verified_signature_generator_tests
       @ generated_signature_file_sig_tests
