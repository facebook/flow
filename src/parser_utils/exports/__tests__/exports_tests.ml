(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let indent_len str =
  let len = String.length str in
  let i = ref 0 in
  while !i < len && str.[!i] = ' ' do
    incr i
  done;
  !i

let dedent_trim str =
  let lines = String.split_on_char '\n' str in
  let lines = List.filter (fun line -> String.trim line <> "") lines in
  let min_indent = List.fold_left (fun acc line -> min acc (indent_len line)) max_int lines in
  let lines =
    List.map
      (fun line ->
        let len = String.length line in
        String.sub line min_indent (len - min_indent))
      lines
  in
  String.concat "\n" lines

let parse_options =
  let open Parser_env in
  Some { default_parse_options with enums = true; components = true }

let parse_and_pack_module ~strict sig_opts contents =
  let (ast, _errors) = Parser_flow.program ~parse_options contents in
  Type_sig_utils.parse_and_pack_module ~strict sig_opts None ast

let parse_and_pack_builtins sig_opts contents =
  let (ast, _errors) = Parser_flow.program ~parse_options contents in
  Type_sig_utils.(pack_builtins (parse_libs sig_opts [ast]))

let make_test_formatter () =
  let open Format in
  let fmt = formatter_of_out_channel stdout in
  (* Output is indented 4 spaces. 96+4 = 100 chars line length. *)
  pp_set_margin fmt 96;
  (* Nice round number, seems to work well by trial-and-error. *)
  pp_set_max_indent fmt 32;
  fmt

let sig_opts =
  {
    Type_sig_options.suppress_types = SSet.empty;
    munge = false;
    facebook_keyMirror = false;
    facebook_fbt = None;
    max_literal_len = 100;
    exact_by_default = true;
    enable_enums = true;
    enable_component_syntax = true;
    enable_ts_syntax = true;
    hooklike_functions = true;
    enable_relay_integration = false;
    casting_syntax = Options.CastingSyntax.Colon;
    relay_integration_module_prefix = None;
    for_builtins = false;
    locs_to_dirtify = [];
  }

let print_index exports =
  let fmt = make_test_formatter () in
  Format.pp_open_box fmt 0;
  Exports.pp fmt exports;
  Format.pp_close_box fmt ();
  Format.pp_print_newline fmt ()

let print_module contents_indent =
  let contents = dedent_trim contents_indent in
  let (_, _, packed_sig) =
    parse_and_pack_module ~strict:true ~platform_availability_set:None sig_opts contents
  in
  let exports = Exports.of_module packed_sig in
  print_index exports

let print_builtins contents_indent =
  let contents = dedent_trim contents_indent in
  let (_, _, packed_sig) = parse_and_pack_builtins sig_opts contents in
  let exports = Exports.of_builtins packed_sig in
  print_index exports

(* TODO: ocamlformat mangles the ppx syntax. *)
[@@@ocamlformat "disable=true"]

let%expect_test "es6_named_const" =
  print_module {|
    export const x : string = "foo"
  |};
  [%expect {|
    [(Named "x")]
  |}]

let%expect_test "cjs_named_const" =
  print_module {|
    const x : string = "foo";
    exports.x = x;
  |};
  [%expect {|
    [(Default None); (Named "x")]
  |}]

let%expect_test "es6_default_string_literal" =
  print_module {|
    export default "foo";
  |};
  [%expect {|
    [(Default None)]
  |}]

let%expect_test "cjs_default_string_literal" =
  print_module {|
    module.exports = "foo";
  |};
  [%expect {|
    [(Default None)]
  |}]

let%expect_test "es6_default_number_literal" =
  print_module {|
    export default 0;
  |};
  [%expect {|
    [(Default None)]
  |}]

let%expect_test "cjs_default_number_literal" =
  print_module {|
    module.exports = 0;
  |};
  [%expect {|
    [(Default None)]
  |}]

let%expect_test "es6_named_type_ref" =
  print_module {|
    type T = string;
    export type U = T;
    export default 0; // need an exported value to force ES6 modules
  |};
  [%expect {|
    [(NamedType "U"); (Default None)]
  |}]

let%expect_test "cjs_named_type_ref" =
  print_module {|
    type T = string;
    export type U = T;
  |};
  [%expect {|
    [(NamedType "U")]
  |}]

let%expect_test "es6_named_type_binding" =
  print_module {|
    export type T = string;
    export default 0; // need an exported value to force ES6 modules
  |};
  [%expect {|
    [(NamedType "T"); (Default None)]
  |}]

let%expect_test "cjs_named_type_binding" =
  print_module {|
    export type T = string;
  |};
  [%expect {|
    [(NamedType "T")]
  |}]

let%expect_test "es6_named_opaque_type_binding" =
  print_module {|
    export opaque type T = string;
    export default 0; // need an exported value to force ES6 modules
  |};
  [%expect {|
    [(NamedType "T"); (Default None)]
  |}]

let%expect_test "cjs_named_opaque_type_binding" =
  print_module {|
    export opaque type T = string;
  |};
  [%expect {|
    [(NamedType "T")]
  |}]

let%expect_test "es6_default_class" =
  print_module {|
    export default class Foo {
      static foo(): void {}
      bar(): void {}
    };
  |};
  [%expect {|
    [(Default (Some "Foo")); (DefaultType (Some "Foo"))]
  |}]

let%expect_test "cjs_default_class_ref" =
  print_module {|
    class Foo {
      static foo(): void {}
      bar(): void {}
    };
    module.exports = Foo;
  |};
  (* TODO: also DefaultType *)
  [%expect {|
    [(Default None)]
  |}]

let%expect_test "cjs_default_class_expr" =
  print_module {|
    module.exports = class Foo {
      static foo(): void {}
      bar(): void {}
    };
  |};
  (* TODO: also DefaultType *)
  [%expect {|
    [(Default None)]
  |}]

let%expect_test "es6_enum" =
  print_module {|
    export enum E {
      FOO,
      BAR,
    };
  |};
  [%expect {|
    [(Named "E"); (NamedType "E")]
  |}]

let%expect_test "cjs_enum" =
  print_module {|
    enum E {
      FOO,
      BAR,
    };
    exports.E = E;
  |};
  [%expect {|
    [(Default None); (Named "E"); (NamedType "E")]
  |}]

let%expect_test "es6_type_enum" =
  print_module {|
    enum EImpl {}
    export type E = EImpl;
    export default 0; // need an exported value to force ES6 modules
  |};
  [%expect {|
    [(NamedType "E"); (Default None)]
  |}]

let%expect_test "cjs_type_enum" =
  print_module {|
    enum EImpl {}
    export type E = EImpl;
  |};
  [%expect {|
    [(NamedType "E")]
  |}]

let%expect_test "es6_default_enum" =
  print_module {|
    export default enum E {
      FOO,
      BAR,
    };
  |};
  [%expect {|
    [(Default (Some "E")); (DefaultType (Some "E"))]
  |}]

let%expect_test "cjs_default_enum" =
  print_module {|
    enum E {
      FOO,
      BAR,
    };
    module.exports = E;
  |};
  (* TODO: also DefaultType *)
  [%expect {|
    [(Default None)]
  |}]

let%expect_test "cjs_default_named_enum" =
  print_module {|
    enum E {
      FOO,
      BAR,
    };
    module.exports = { E };
  |};
  [%expect {|
    [(Default None); (Named "E"); (NamedType "E")]
  |}]

let%expect_test "es6_named_class_ref" =
  print_module {|
    class Foo {}
    export { Foo };
  |};
  [%expect {|
    [(Named "Foo"); (NamedType "Foo")]
  |}]

let%expect_test "cjs_named_class_ref" =
  print_module {|
    class Foo {}
    exports.Foo = Foo;
  |};
  [%expect {|
    [(Default None); (Named "Foo"); (NamedType "Foo")]
  |}]

let%expect_test "es6_named_class_ref_ref" =
  print_module {|
    class Foo {}
    const Bar = Foo;
    export { Bar };
  |};
  [%expect {|
    [(Named "Bar"); (NamedType "Bar")]
  |}]

let%expect_test "cjs_named_class_ref_inline" =
  print_module {|
    exports.Foo = class Foo {};
  |};
  [%expect {|
    [(Default None); (Named "Foo"); (NamedType "Foo")]
  |}]

let%expect_test "es6_named_class_binding" =
  print_module {|
    export class Foo {};
  |};
  [%expect {|
    [(Named "Foo"); (NamedType "Foo")]
  |}]

let%expect_test "es6_default_obj" =
  print_module {|
    export default {
      foo: 123,
      bar: "bar",
      baz(): void {},
    };
  |};
  [%expect {|
    [(Default None)]
  |}]

let%expect_test "es6_default_variable" =
  print_module {|
    const foo = 3;
    export default foo;
  |};
  [%expect {|
    [(Default (Some "foo"))]
  |}]

let%expect_test "es6_declare_export_default_typeof_variable" =
  print_module {|
    const foo = 3;
    declare export default typeof foo;
  |};
  [%expect {|
    [(Default (Some "foo"))]
  |}]

let%expect_test "cjs_default_obj" =
  print_module {|
    module.exports = {
      foo: 123,
      bar: "bar",
      baz(): void {},
    };
  |};
  [%expect {|
    [(Default None); (Named "foo"); (Named "baz"); (Named "bar")]
  |}]

let%expect_test "cjs_default_obj_ref" =
  print_module {|
    const O = {
      foo: 123,
      bar: "bar",
    };
    module.exports = O;
  |};
  [%expect {|
    [(Default None); (Named "foo"); (Named "bar")]
  |}]

let%expect_test "cjs_default_obj_ref_annot" =
  print_module {|
    var O: {bar: string, foo: number,...} = {
      foo: 123,
      bar: "bar",
    };
    module.exports = O;
  |};
  [%expect {|
    [(Default None); (Named "foo"); (Named "bar")]
  |}]

let%expect_test "cjs_default_obj_ref_annot_ref" =
  print_module {|
    type T = { bar: string, foo: number };
    var O: T = { foo: 123, bar: "bar" };
    module.exports = O;
  |};
  [%expect {|
    [(Default None); (Named "foo"); (Named "bar")]
  |}]

let%expect_test "es6_default_obj_type" =
  print_module {|
    export default ({
      foo: 123,
      bar: "bar",
    }: { foo: number, bar: string });
  |};
  [%expect {|
    [(Default None)]
  |}]

let%expect_test "cjs_default_obj_type" =
  print_module {|
    module.exports = ({
      foo: 123,
      bar: "bar",
    }: { foo: number, bar: string });
  |};
  [%expect {|
    [(Default None); (Named "foo"); (Named "bar")]
  |}]

let%expect_test "cjs_default_obj_type_app" =
  print_module {|
    type O<T> = { foo: T }
    module.exports = ({ foo: 123 }: O<number>)
  |};
  [%expect {|
    [(Default None); (Named "foo")]
  |}]

let%expect_test "cjs_default_obj_type_ref" =
  print_module {|
    type O = { foo: number, bar: string }
    module.exports = ({ foo: 123, bar: "bar"} : O);
  |};
  [%expect {|
    [(Default None); (Named "foo"); (Named "bar")]
  |}]

let%expect_test "es6_default_obj_type_opaque" =
  print_module {|
    opaque type T = { foo: number, bar: string };
    export default ({
      foo: 123,
      bar: "bar",
    }: T);
  |};
  [%expect {|
    [(Default None)]
  |}]

let%expect_test "cjs_default_obj_type_opaque" =
  print_module {|
    opaque type T = { foo: number, bar: string };
    module.exports = ({
      foo: 123,
      bar: "bar",
    }: T);
  |};
  [%expect {|
    [(Default None)]
  |}]

let%expect_test "es6_default_obj_with_class_expr" =
  (* can't destructure the default export, so don't include its props *)
  print_module {|
    export default {
      Foo: class {}
    };
  |};
  [%expect {|
    [(Default None)]
  |}]

let%expect_test "cjs_default_obj_with_class_expr" =
  print_module {|
    module.exports = {
      Foo: class {}
    };
  |};
  [%expect {|
    [(Default None); (Named "Foo"); (NamedType "Foo")]
  |}]

let%expect_test "cjs_default_obj_with_class_ref" =
  print_module {|
    class Foo {}
    module.exports = { Foo };
  |};
  [%expect {|
    [(Default None); (Named "Foo"); (NamedType "Foo")]
  |}]

let%expect_test "cjs_default_obj_with_class_ref_inline" =
  print_module {|
    module.exports = {
      Foo: class Foo {}
    };
  |};
  [%expect {|
    [(Default None); (Named "Foo"); (NamedType "Foo")]
  |}]

let%expect_test "cjs_default_obj_with_string_literal" =
  (* string literals currently can't be imported via destructuring *)
  print_module {|
    module.exports = { 'Foo bar': 123 }
  |};
  [%expect {|
    [(Default None)]
  |}]

let%expect_test "cjs_default_pattern_ref" =
  print_module {|
    const {Foo} = {
      Foo: { foo: 123 }
    };
    module.exports = Foo;
  |};
  [%expect {|
    [(Default None); (Named "foo")]
  |}]

let%expect_test "cjs_default_pattern_ref_of_ref" =
  print_module {|
    const T = {
      Foo: { foo: 123 }
    };
    const {Foo} = T;
    module.exports = Foo;
  |};
  [%expect {|
    [(Default None); (Named "foo")]
  |}]

let%expect_test "cjs_default_pattern_tyref" =
  print_module {|
    const {Foo} = {
      Foo: class {}
    };
    module.exports = (new Foo(): Foo);
  |};
  (* TODO: also DefaultType *)
  [%expect {|
    [(Default None)]
  |}]

let%expect_test "cjs_default_eval_getprop" =
  print_module {|
    const O = { prop: { foo: 123 } };
    module.exports = O.prop;
  |};
  [%expect {|
    [(Default None); (Named "foo")]
  |}]

let%expect_test "cjs_default_eval_getprop_method" =
  print_module {|
    const O = { prop(): void {} };
    module.exports = O.prop;
  |};
  [%expect {|
    [(Default None)]
  |}]

let%expect_test "cjs_default_eval_getprop_of_variable" =
  print_module {|
    const O = { prop: { foo: 123 } };
    const T = O.prop;
    module.exports = T;
  |};
  [%expect {|
    [(Default None); (Named "foo")]
  |}]

let%expect_test "cjs_default_nested_typealias" =
  print_module {|
    type T<U> = { foo: U, bar: string };
    type O = T<number>;
    const X : O = { foo: 123, bar: "bar" };
    module.exports = X;
  |};
  [%expect {|
    [(Default None); (Named "foo"); (Named "bar")]
  |}]

let%expect_test "cjs_default_nested_ref" =
  print_module {|
    const O = { foo: 123, bar: "bar" };
    const X = O;
    const Y = X;
    module.exports = Y;
  |};
  [%expect {|
    [(Default None); (Named "foo"); (Named "bar")]
  |}]

let%expect_test "es6_default_function" =
  print_module {|
    export default function() {}
  |};
  [%expect {|
    [(Default None)]
  |}]

let%expect_test "cjs_default_function" =
  print_module {|
    module.exports = function foo(): void {}
  |};
  [%expect {|
    [(Default None)]
  |}]

let%expect_test "cjs_default_function_ref" =
  print_module {|
    function foo(): void {}
    foo.bar = 123;
    module.exports = foo;
  |};
  [%expect {|
    [(Default None)]
  |}]

let%expect_test "es6_named_function" =
  print_module {|
    export function foo(): string {}
  |};
  [%expect {|
    [(Named "foo")]
  |}]

let%expect_test "es6_named_alias" =
  print_module {|
    const foo = 123;
    export {foo as bar};
  |};
  [%expect {|
    [(Named "bar")]
  |}]

let%expect_test "cjs_default_remote_ref" =
  print_module {|
    import type {T} from './foo';
    module.exports = ({ foo: 123, bar: "bar" }: T);
  |};
  [%expect {|
    [(Default None)]
  |}]

let%expect_test "cjs_recursive_type" =
  print_module {|
    export type T = T;
  |};
  [%expect {|
    [(NamedType "T")]
  |}]

let%expect_test "cjs_recursive_ref" =
  print_module {|
    const T : T = { foo: 123, bar: "bar" };
    module.exports = T;
  |};
  [%expect {|
    [(Default None)]
  |}]

let%expect_test "es6_declare_function" =
  print_module {|
    declare function foo(): string;
    export {foo}
  |};
  [%expect {|
    [(Named "foo")]
  |}]

let%expect_test "es6_declare_class" =
  print_module {|
    declare class Foo {};
    export {Foo}
  |};
  [%expect {|
    [(Named "Foo"); (NamedType "Foo")]
  |}]

let%expect_test "es6_interface" =
  print_module {|
    export interface Foo { bar: string }
    export default 0; // need an exported value to force ES6 modules
  |};
  [%expect {|
    [(NamedType "Foo"); (Default None)]
  |}]

let%expect_test "cjs_interface" =
  print_module {|
    export interface Foo { bar: string }
  |};
  [%expect {|
    [(NamedType "Foo")]
  |}]

let%expect_test "lib_class" =
  print_builtins {|
    declare class Foo {}
  |};
  [%expect {|
    [(Named "Foo"); (NamedType "Foo")]
  |}]

let%expect_test "lib_type" =
  print_builtins {|
    declare type T = string;
  |};
  [%expect {|
    [(NamedType "T")]
  |}]

let%expect_test "lib_value" =
  print_builtins {|
    declare var foo : string;
  |};
  [%expect {|
    [(Named "foo")]
  |}]

let%expect_test "react_dollar_ac" =
  print_module {|
    const x: React$AbstractComponent<{}> = null;
    export default x;
  |};
  [%expect{| [(Default (Some "x")); (DefaultType (Some "x"))] |}]

let%expect_test "react_dot_ac" =
  print_module {|
    import * as React from 'react';
    const x: React.AbstractComponent<{}> = null;
    export default x;
  |};
  [%expect{| [(Default (Some "x")); (DefaultType (Some "x"))] |}]
