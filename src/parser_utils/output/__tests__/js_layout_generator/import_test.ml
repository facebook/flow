(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Layout_generator_test_utils

let no_bracket_spacing opts = Js_layout_generator.{ opts with bracket_spacing = false }

let tests =
  [
    ( "basic" >:: fun ctxt ->
      assert_statement_string ~ctxt {|import"a";|};
      assert_statement_string ~ctxt {|import a from"a";|};
      assert_statement_string ~ctxt {|import type a from"a";|};
      assert_statement_string ~ctxt {|import typeof a from"a";|};
      assert_statement_string ~ctxt {|import a,*as b from"a";|};
      assert_statement_string ~ctxt {|import a,{b}from"a";|};
      assert_statement_string ~ctxt {|import{a,type b}from"a";|};
      assert_statement_string ~ctxt {|import{a,typeof b}from"a";|};
      assert_statement_string ~ctxt {|import{a,type b as c}from"a";|};
      assert_statement_string ~ctxt {|import{a as b}from"a";|};
      assert_statement_string ~ctxt {|import type{a}from"a";|};
      assert_statement_string ~ctxt {|import{a,b}from"a";|};
      assert_statement_string ~ctxt {|import type{}from"a";|};
      assert_statement_string ~ctxt {|import typeof{}from"a";|};
      assert_statement_string ~ctxt ~pretty:true {|import { a, b } from "a";|};
      assert_statement_string
        ~ctxt
        ~pretty:true
        ~opts:(no_bracket_spacing opts)
        {|import {a, b} from "a";|};
      assert_statement_string ~ctxt ~pretty:true {|import type { a, b } from "a";|};
      assert_statement_string
        ~ctxt
        ~pretty:true
        ~opts:(no_bracket_spacing opts)
        {|import type {a, b} from "a";|};
      assert_statement_string
        ~ctxt
        ~pretty:true
        ("import {\n  a,\n  " ^ String.make 80 'b' ^ ",\n} from \"a\";");
      assert_statement_string ~ctxt ~pretty:true {|import a, * as b from "a";|};
      assert_statement_string
        ~ctxt
        ~pretty:true
        ("import a, * as " ^ String.make 80 'b' ^ " from \"a\";");
      assert_statement_string ~ctxt ~pretty:true {|import a, { b } from "a";|};
      assert_statement_string
        ~ctxt
        ~pretty:true
        ~opts:(no_bracket_spacing opts)
        {|import a, {b} from "a";|}
    );
    ( "wrap_specifiers" >:: fun ctxt ->
      assert_statement_string
        ~ctxt
        ~pretty:true
        (Printf.sprintf "import a, {\n  %s,\n} from \"a\";" (String.make 80 'b'));
      assert_statement_string
        ~ctxt
        ~pretty:true
        (Printf.sprintf
           "import {\n  %s,\n  %s,\n} from \"a\";"
           (String.make 80 'a')
           (String.make 80 'b')
        );
      assert_statement_string
        ~ctxt
        ~pretty:true
        (Printf.sprintf
           "import {\n  %s,\n  %s,\n} from \"%s\";"
           (String.make 20 'a')
           (String.make 20 'b')
           (String.make 40 'c')
        );
      (* don't wrap a single specifier even if it's too long *)
      assert_statement_string
        ~ctxt
        ~pretty:true
        (Printf.sprintf "import { %s } from \"%s\";" (String.make 40 'a') (String.make 40 'b'));
      assert_statement_string
        ~ctxt
        ~pretty:true
        ~opts:(no_bracket_spacing opts)
        (Printf.sprintf "import {%s} from \"%s\";" (String.make 40 'a') (String.make 40 'b'));
      (* do wrap a single specifier if there's a default *)
      assert_statement_string
        ~ctxt
        ~pretty:true
        (Printf.sprintf
           "import x, {\n  %s,\n} from \"%s\";"
           (String.make 40 'a')
           (String.make 40 'b')
        )
    );
  ]
