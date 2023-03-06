(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Ast_builder

let tests =
  [
    ( "import_named_specifier" >:: fun ctxt ->
      let open Statements in
      let ast =
        let import_kind = Flow_ast.Statement.ImportDeclaration.ImportValue in
        let source = (Loc.none, string_literal "foo") in
        let specifiers =
          [
            named_import_specifier ?local:None (Identifiers.identifier "foo");
            named_import_specifier
              ~local:(Identifiers.identifier "baz")
              (Identifiers.identifier "bar");
          ]
        in
        named_import_declaration import_kind source specifiers
      in
      let ast' = (new Flow_ast_mapper.mapper)#statement ast in
      assert_equal ~ctxt ~cmp:( == ) ast ast'
    );
  ]

let tests = "flow_ast_mapper" >::: tests
