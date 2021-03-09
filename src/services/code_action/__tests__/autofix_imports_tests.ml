(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let indent_len str =
  let len = String.length str in
  let i = ref 0 in
  while !i < len && str.[!i] = ' ' do
    incr i
  done;
  !i

let dedent_trim str =
  let lines = String.split_on_char '\n' str in
  let non_empty_lines = List.filter (fun line -> String.trim line <> "") lines in
  let min_indent =
    List.fold_left (fun acc line -> min acc (indent_len line)) max_int non_empty_lines
  in
  let lines =
    List.map
      (fun line ->
        let len = String.length line in
        if len < min_indent then
          line
        else
          String.sub line min_indent (len - min_indent))
      lines
  in
  String.trim (String.concat "\n" lines) ^ "\n"

let parse contents =
  let parse_options =
    Some
      Parser_env.
        {
          default_parse_options with
          enums = true;
          esproposal_class_instance_fields = true;
          esproposal_class_static_fields = true;
          esproposal_export_star_as = true;
        }
  in
  let (ast, _errors) = Parser_flow.program ~parse_options contents in
  ast

let offset_of_position str { Loc.line; column } =
  let rec helper init line column =
    if line = 1 then
      init + column
    else
      let eol = String.index_from str init '\n' in
      helper (eol + 1) (line - 1) column
  in
  helper 0 line column

let apply_patch contents patch =
  patch
  |> List.rev
  |> List.fold_left
       (fun contents (loc, str) ->
         let start_offset = offset_of_position contents loc.Loc.start in
         let end_offset = offset_of_position contents loc.Loc._end in
         Printf.eprintf "%d %d" start_offset end_offset;
         String.sub contents 0 start_offset
         ^ str
         ^ String.sub contents end_offset (String.length contents - end_offset))
       contents

let assert_patch ~ctxt ?(single_quotes = false) expected binding from contents =
  let expected = dedent_trim expected in
  let contents = dedent_trim contents in
  let options = Js_layout_generator.{ default_opts with single_quotes } in
  let patch = Autofix_imports.add_import ~options ~binding ~from (parse contents) in
  let patched = apply_patch contents patch in
  assert_equal ~ctxt ~printer:(fun x -> x) expected patched

(* TODO: ocamlformat mangles indentation. *)
[@@@ocamlformat "disable=true"]

let add_import_tests =
  [
    ( "import_named_no_existing" >:: fun ctxt ->
      let binding = (Export_index.Named, "foo") in
      let from = "./foo" in
      let contents = {|
        foo
      |} in
      let expected = {|
        import {foo} from "./foo";

        foo
      |} in
      assert_patch ~ctxt expected binding from contents;

      let expected = Str.global_replace (Str.regexp_string "\"") "'" expected in
      assert_patch ~ctxt ~single_quotes:true expected binding from contents );

    ( "import_named_above_existing" >:: fun ctxt ->
      let binding = (Export_index.Named, "foo") in
      let from = "./foo" in
      let contents = {|
        import {zzz} from "./zzz";

        foo
      |} in
      let expected = {|
        import {foo} from "./foo";
        import {zzz} from "./zzz";

        foo
      |} in
      assert_patch ~ctxt expected binding from contents );

    ( "import_named_below_existing" >:: fun ctxt ->
      let binding = (Export_index.Named, "foo") in
      let from = "./foo" in
      let contents = {|
        import {bar} from "./bar";

        foo
      |} in
      let expected = {|
        import {bar} from "./bar";
        import {foo} from "./foo";

        foo
      |} in
      assert_patch ~ctxt expected binding from contents );

    ( "import_named_sorted_existing" >:: fun ctxt ->
      let binding = (Export_index.Named, "baz") in
      let from = "./baz" in
      let contents = {|
        import {bar} from "./bar";
        import {foo} from "./foo";

        foo
      |} in
      let expected = {|
        import {bar} from "./bar";
        import {baz} from "./baz";
        import {foo} from "./foo";

        foo
      |} in
      assert_patch ~ctxt expected binding from contents );

    ( "import_named_unsorted_existing" >:: fun ctxt ->
      let binding = (Export_index.Named, "baz") in
      let from = "./baz" in
      let contents = {|
        import {foo} from "./foo";
        import {bar} from "./bar";

        foo
      |} in
      let expected = {|
        import {foo} from "./foo";
        import {bar} from "./bar";
        import {baz} from "./baz";

        foo
      |} in
      assert_patch ~ctxt expected binding from contents );

    ( "import_named_in_existing" >:: fun ctxt ->
      let binding = (Export_index.Named, "foo") in
      let from = "./foo" in
      let contents = {|
        import {bar} from "./foo";

        foo
      |} in
      let expected = {|
        import {bar, foo} from "./foo";

        foo
      |} in
      assert_patch ~ctxt expected binding from contents;

      (* not only inserts `{foo}` specifier, but changes `"./foo"` to `'./foo'` *)
      let expected = Str.global_replace (Str.regexp_string "\"") "'" expected in
      assert_patch ~ctxt ~single_quotes:true expected binding from contents );

    ( "import_named_below_existing_default" >:: fun ctxt ->
      let binding = (Export_index.Named, "foo") in
      let from = "./foo" in
      let contents = {|
        import Foo from "./foo";

        foo
      |} in
      let expected = {|
        import Foo from "./foo";
        import {foo} from "./foo";

        foo
      |} in
      assert_patch ~ctxt expected binding from contents );

    ( "import_named_below_existing_type" >:: fun ctxt ->
      let binding = (Export_index.Named, "foo") in
      let from = "./foo" in
      let contents = {|
        import type {IFoo} from "./foo";

        foo
      |} in
      let expected = {|
        import type {IFoo} from "./foo";

        import {foo} from "./foo";

        foo
      |} in
      assert_patch ~ctxt expected binding from contents );

    ( "import_default_no_existing" >:: fun ctxt ->
      let binding = (Export_index.Default, "foo") in
      let from = "./foo" in
      let contents = {|
        foo
      |} in
      let expected = {|
        import foo from "./foo";

        foo
      |} in
      assert_patch ~ctxt expected binding from contents );

    ( "import_default_duplicate" >:: fun ctxt ->
      let binding = (Export_index.Default, "Foo") in
      let from = "./foo" in
      let contents = {|
        import Bar from "./foo";

        foo
      |} in
      let expected = {|
        import Bar from "./foo";
        import Foo from "./foo";

        foo
      |} in
      assert_patch ~ctxt expected binding from contents );

    ( "import_type_no_existing" >:: fun ctxt ->
      let binding = (Export_index.NamedType, "IFoo") in
      let from = "./foo" in
      let contents = {|
        foo
      |} in
      let expected = {|
        import type {IFoo} from "./foo";

        foo
      |} in
      assert_patch ~ctxt expected binding from contents );

    ( "import_type_in_existing_type" >:: fun ctxt ->
      let binding = (Export_index.NamedType, "IBar") in
      let from = "./foo" in
      let contents = {|
        import type {IFoo} from "./foo";
      |} in
      let expected = {|
        import type {IBar, IFoo} from "./foo";
      |} in
      assert_patch ~ctxt expected binding from contents );

    ( "import_type_in_existing_type_unsorted" >:: fun ctxt ->
      let binding = (Export_index.NamedType, "IBar") in
      let from = "./foo" in
      let contents = {|
        import type {IFoo, IBaz} from "./foo";
      |} in
      let expected = {|
        import type {IFoo, IBaz, IBar} from "./foo";
      |} in
      assert_patch ~ctxt expected binding from contents );

    ( "import_type_above_existing_named" >:: fun ctxt ->
      let binding = (Export_index.NamedType, "IFoo") in
      let from = "./foo" in
      let contents = {|
        import {foo} from "./foo";

        foo
      |} in
      let expected = {|
        import type {IFoo} from "./foo";

        import {foo} from "./foo";

        foo
      |} in
      assert_patch ~ctxt expected binding from contents );

    ( "import_type_above_existing_default" >:: fun ctxt ->
      let binding = (Export_index.NamedType, "IFoo") in
      let from = "./foo" in
      let contents = {|
        import foo from "./foo";

        foo
      |} in
      let expected = {|
        import type {IFoo} from "./foo";

        import foo from "./foo";

        foo
      |} in
      assert_patch ~ctxt expected binding from contents );

    ( "import_type_unsorted_existing" >:: fun ctxt ->
      let binding = (Export_index.NamedType, "IBaz") in
      let from = "./baz" in
      let contents = {|
        import {foo} from "./foo";
        import {bar} from "./bar";

        foo
      |} in
      let expected = {|
        import {foo} from "./foo";
        import {bar} from "./bar";
        import type {IBaz} from "./baz";

        foo
      |} in
      assert_patch ~ctxt expected binding from contents );

    ( "import_namespace_no_existing" >:: fun ctxt ->
      let binding = (Export_index.Namespace, "React") in
      let from = "react" in
      let contents = {|
        foo
      |} in
      let expected = {|
        import * as React from "react";

        foo
      |} in
      assert_patch ~ctxt expected binding from contents );

    ( "import_namespace_below_existing_named_from_same_module" >:: fun ctxt ->
      let binding = (Export_index.Namespace, "React") in
      let from = "react" in
      let contents = {|
        import {foo} from "react";

        foo
      |} in
      let expected = {|
        import {foo} from "react";
        import * as React from "react";

        foo
      |} in
      assert_patch ~ctxt expected binding from contents );

    ( "import_namespace_above_existing_named" >:: fun ctxt ->
      let binding = (Export_index.Namespace, "React") in
      let from = "react" in
      let contents = {|
        import type {IFoo} from "./foo";

        import {foo} from "./foo";

        foo
      |} in
      let expected = {|
        import type {IFoo} from "./foo";

        import {foo} from "./foo";
        import * as React from "react";

        foo
      |} in
      assert_patch ~ctxt expected binding from contents );

    ( "import_namespace_duplicate" >:: fun ctxt ->
      let binding = (Export_index.Namespace, "Foo") in
      let from = "./foo" in
      let contents = {|
        import * as Bar from "./foo";

        foo
      |} in
      let expected = {|
        import * as Bar from "./foo";
        import * as Foo from "./foo";

        foo
      |} in
      assert_patch ~ctxt expected binding from contents );

    ( "insert_after_flow_comment" >:: fun ctxt ->
      let binding = (Export_index.Named, "foo") in
      let from = "./foo" in
      let contents = {|
        // @flow

        foo
      |} in
      let expected = {|
        // @flow

        import {foo} from "./foo";

        foo
      |} in
      assert_patch ~ctxt expected binding from contents );

    ( "insert_after_directives" >:: fun ctxt ->
      let binding = (Export_index.Named, "foo") in
      let from = "./foo" in
      let contents = {|
        // @flow

        "use strict";

        foo
      |} in
      let expected = {|
        // @flow

        "use strict";

        import {foo} from "./foo";

        foo
      |} in
      assert_patch ~ctxt expected binding from contents );
  ]

let tests = "autofix_imports" >::: ["add_import" >::: add_import_tests]
