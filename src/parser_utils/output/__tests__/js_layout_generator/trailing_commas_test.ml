(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Layout_test_utils
open Layout_generator_test_utils
module I = Ast_builder.Identifiers
module E = Ast_builder.Expressions
module S = Ast_builder.Statements
module P = Ast_builder.Patterns
module F = Ast_builder.Functions
module L = Layout_builder
module C = Js_layout_generator.Trailing_commas

let spf = Printf.sprintf

let tests =
  [
    ( "array"
    >:::
    let a80 = String.make 80 'a' in
    let array_layout trailing_commas =
      Js_layout_generator.expression
        ~opts:Js_layout_generator.{ default_opts with trailing_commas }
        (E.array [E.array_expression (E.identifier a80); E.array_expression (E.identifier a80)])
    in
    [
      ( "all" >:: fun ctxt ->
        let layout = array_layout C.All in
        assert_output ~ctxt (spf "[%s,%s]" a80 a80) layout;
        assert_output ~ctxt ~pretty:true (spf "[\n  %s,\n  %s,\n]" a80 a80) layout );
      ( "es5" >:: fun ctxt ->
        let layout = array_layout C.ES5 in
        assert_output ~ctxt (spf "[%s,%s]" a80 a80) layout;
        assert_output ~ctxt ~pretty:true (spf "[\n  %s,\n  %s,\n]" a80 a80) layout );
      ( "off" >:: fun ctxt ->
        let layout = array_layout C.Off in
        assert_output ~ctxt (spf "[%s,%s]" a80 a80) layout;
        assert_output ~ctxt ~pretty:true (spf "[\n  %s,\n  %s\n]" a80 a80) layout );
    ] );
    ( "object"
    >:::
    let x40 = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" in
    let obj_layout trailing_commas =
      let prop1 = E.object_property (E.object_property_key "foo") (E.identifier x40) in
      let prop2 = E.object_property (E.object_property_key "bar") (E.identifier x40) in
      Js_layout_generator.expression
        ~opts:Js_layout_generator.{ default_opts with trailing_commas }
        (E.object_ [prop1; prop2])
    in
    [
      ( "all" >:: fun ctxt ->
        let layout = obj_layout C.All in
        assert_output ~ctxt (spf "{foo:%s,bar:%s}" x40 x40) layout;
        assert_output ~ctxt ~pretty:true (spf "{\n  foo: %s,\n  bar: %s,\n}" x40 x40) layout );
      ( "es5" >:: fun ctxt ->
        let layout = obj_layout C.ES5 in
        assert_output ~ctxt (spf "{foo:%s,bar:%s}" x40 x40) layout;
        assert_output ~ctxt ~pretty:true (spf "{\n  foo: %s,\n  bar: %s,\n}" x40 x40) layout );
      ( "off" >:: fun ctxt ->
        let layout = obj_layout C.Off in
        assert_output ~ctxt (spf "{foo:%s,bar:%s}" x40 x40) layout;
        assert_output ~ctxt ~pretty:true (spf "{\n  foo: %s,\n  bar: %s\n}" x40 x40) layout );
    ] );
    ( "call"
    >:::
    let a80 = String.make 80 'a' in
    let layout trailing_commas =
      Js_layout_generator.expression
        ~opts:Js_layout_generator.{ default_opts with trailing_commas }
        (E.call ~args:(E.arg_list [E.expression (E.identifier a80)]) (E.identifier "x"))
    in
    [
      ( "all" >:: fun ctxt ->
        let layout = layout C.All in
        assert_output ~ctxt (spf "x(%s)" a80) layout;
        assert_output ~ctxt ~pretty:true (spf "x(\n  %s,\n)" a80) layout );
      ( "es5" >:: fun ctxt ->
        let layout = layout C.ES5 in
        assert_output ~ctxt (spf "x(%s)" a80) layout;
        assert_output ~ctxt ~pretty:true (spf "x(\n  %s,\n)" a80) layout );
      ( "off" >:: fun ctxt ->
        let layout = layout C.Off in
        assert_output ~ctxt (spf "x(%s)" a80) layout;
        assert_output ~ctxt ~pretty:true (spf "x(\n  %s\n)" a80) layout );
    ] );
    ( "function_params"
    >:::
    let a80 = String.make 80 'a' in
    let layout trailing_commas =
      let params = F.params [F.param (P.identifier a80)] in
      Js_layout_generator.statement
        ~opts:Js_layout_generator.{ default_opts with trailing_commas }
        (S.function_declaration ~params (I.identifier "x"))
    in
    [
      ( "all" >:: fun ctxt ->
        let layout = layout C.All in
        assert_output ~ctxt (spf "function x(%s){}" a80) layout;
        assert_output ~ctxt ~pretty:true (spf "function x(\n  %s,\n) {}" a80) layout );
      ( "es5" >:: fun ctxt ->
        let layout = layout C.ES5 in
        assert_output ~ctxt (spf "function x(%s){}" a80) layout;
        assert_output ~ctxt ~pretty:true (spf "function x(\n  %s\n) {}" a80) layout );
      ( "off" >:: fun ctxt ->
        let layout = layout C.Off in
        assert_output ~ctxt (spf "function x(%s){}" a80) layout;
        assert_output ~ctxt ~pretty:true (spf "function x(\n  %s\n) {}" a80) layout );
    ] );
    ( "function_params_with_rest"
    >:::
    let a80 = String.make 80 'a' in
    let layout trailing_commas =
      let rest = F.rest_param (P.identifier "rest") in
      let params = F.params ~rest [F.param (P.identifier a80)] in
      Js_layout_generator.statement
        ~opts:Js_layout_generator.{ default_opts with trailing_commas }
        (S.function_declaration ~params (I.identifier "x"))
    in
    [
      ( "all" >:: fun ctxt ->
        let layout = layout C.All in
        assert_output ~ctxt (spf "function x(%s,...rest){}" a80) layout;
        assert_output ~ctxt ~pretty:true (spf "function x(\n  %s,\n  ...rest\n) {}" a80) layout );
      ( "es5" >:: fun ctxt ->
        let layout = layout C.ES5 in
        assert_output ~ctxt (spf "function x(%s,...rest){}" a80) layout;
        assert_output ~ctxt ~pretty:true (spf "function x(\n  %s,\n  ...rest\n) {}" a80) layout );
      ( "off" >:: fun ctxt ->
        let layout = layout C.Off in
        assert_output ~ctxt (spf "function x(%s,...rest){}" a80) layout;
        assert_output ~ctxt ~pretty:true (spf "function x(\n  %s,\n  ...rest\n) {}" a80) layout );
    ] );
  ]
