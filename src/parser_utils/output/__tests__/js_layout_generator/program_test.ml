(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Layout_test_utils
open Layout_generator_test_utils

module S = Ast_builder.Statements
module E = Ast_builder.Expressions
module L = Layout_builder

let tests = [
  "blank_lines_if_in_original" >:: begin fun ctxt ->
    let ast = Ast_builder.program_of_string "var x = 1;\n\n\nvar y = 2;" in
    let layout = Js_layout_generator.program ~checksum:None ~preserve_docblock:false ast in
    assert_layout ~ctxt
      L.(program (sequence ~break:Layout.Break_if_pretty ~inline:(true, true) ~indent:0 [
        loc ~loc:{Loc.none with Loc.start={Loc.line=1; column=0; offset=0}; _end={Loc.line=1; column=10; offset=10}} (fused [
          loc ~loc:{Loc.none with Loc.start={Loc.line=1; column=0; offset=0}; _end={Loc.line=1; column=10; offset=10}} (fused [
            atom "var";
            space;
            loc ~loc:{Loc.none with Loc.start={Loc.line=1; column=4; offset=4}; _end={Loc.line=1; column=9; offset=9}} (fused [
              loc ~loc:{Loc.none with Loc.start={Loc.line=1; column=4; offset=4}; _end={Loc.line=1; column=5; offset=5}} (id ~loc:{Loc.none with Loc.start={Loc.line=1; column=4; offset=4}; _end={Loc.line=1; column=5; offset=5}} "x");
              pretty_space;
              atom "=";
              pretty_space;
              loc ~loc:{Loc.none with Loc.start={Loc.line=1; column=8; offset=8}; _end={Loc.line=1; column=9; offset=9}} (atom "1");
            ]);
          ]);
          atom ";";
        ]);
        fused [
          pretty_hardline;
          loc ~loc:{Loc.none with Loc.start={Loc.line=4; column=0; offset=13}; _end={Loc.line=4; column=10; offset=23}} (fused [
            loc ~loc:{Loc.none with Loc.start={Loc.line=4; column=0; offset=13}; _end={Loc.line=4; column=10; offset=23}} (fused [
              atom "var";
              atom " ";
              loc ~loc:{Loc.none with Loc.start={Loc.line=4; column=4; offset=17}; _end={Loc.line=4; column=9; offset=22}} (fused [
                loc ~loc:{Loc.none with Loc.start={Loc.line=4; column=4; offset=17}; _end={Loc.line=4; column=5; offset=18}} (id ~loc:{Loc.none with Loc.start={Loc.line=4; column=4; offset=17}; _end={Loc.line=4; column=5; offset=18}} "y");
                pretty_space;
                atom "=";
                pretty_space;
                loc ~loc:{Loc.none with Loc.start={Loc.line=4; column=8; offset=21}; _end={Loc.line=4; column=9; offset=22}} (atom "2");
              ]);
            ]);
            atom ";";
          ]);
        ];
      ]))
      layout;
    assert_output ~ctxt "var x=1;var y=2;" layout;
    assert_output ~ctxt ~pretty:true "var x = 1;\n\nvar y = 2;" layout;
  end;

  "program_artifact_newline" >::
    begin fun ctxt ->
      let ast = Ast_builder.mk_program [
        S.expression (E.identifier "x");
      ] in
      let layout = Js_layout_generator.program
        ~preserve_docblock:false
        ~checksum:(Some "@artifact abc123")
        ast
      in
      assert_layout ~ctxt
        L.(program (fused [
          sequence ~break:Layout.Break_if_pretty ~inline:(true, true) ~indent:0 [
            loc (fused [
              loc (id "x");
              atom ";";
            ]);
          ];
          hardline;
          atom "/* @artifact abc123 */";
        ]))
        layout;
      assert_output ~ctxt "x;\n/* @artifact abc123 */" layout;
      assert_output ~ctxt ~pretty:true "x;\n/* @artifact abc123 */" layout;
    end;

  "program_trailing_semicolon" >::
    begin fun ctxt ->
      let ast = Ast_builder.mk_program [
        S.expression (E.identifier "x");
        S.expression (E.identifier "y");
      ] in
      let layout = Js_layout_generator.program
        ~preserve_docblock:false
        ~checksum:None
        ast
      in
      assert_layout ~ctxt
        L.(program (sequence ~break:Layout.Break_if_pretty ~inline:(true, true) ~indent:0 [
          loc (fused [
            loc (id "x");
            atom ";";
          ]);
          loc (fused [
            loc (id "y");
            atom ";";
          ]);
        ]))
        layout;
      assert_output ~ctxt "x;y;" layout;
      assert_output ~ctxt ~pretty:true "x;\ny;" layout;
    end;
]
