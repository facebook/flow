(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast_builder
open Layout_test_utils

module S = Ast_builder.Statements
module E = Ast_builder.Expressions
module L = Layout_builder

let test ctxt =
  let rhs = E.sequence [E.identifier "y"; E.identifier "z"] in
  let ast = E.assignment (Patterns.identifier "x") rhs in
  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      loc (id "x");
      pretty_space; atom "="; pretty_space;
      wrap_in_parens (expression rhs);
    ]))
    ast;

  let rhs = E.assignment (Patterns.identifier "y") (E.identifier "z") in
  let ast = E.assignment (Patterns.identifier "x") rhs in
  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      loc (id "x"); pretty_space; atom "="; pretty_space; expression rhs;
    ]))
    ast;

  let rhs = E.function_ () in
  let ast = E.assignment (Patterns.identifier "x") rhs in
  let expected =
    L.(loc (fused [
      loc (id "x"); pretty_space; atom "="; pretty_space; expression rhs;
    ]))
  in
  assert_layout_of_expression ~ctxt expected ast;
  assert_layout_of_statement ~ctxt
    L.(loc (fused [expected; atom ";"]))
    (S.expression ast);

  assert_layout_of_statement_string ~ctxt
    L.(loc ~loc:{Loc.none with Loc.start={Loc.line=1; column=0; offset=0}; _end={Loc.line=1; column=8; offset=8}} (fused [
      fused [
        atom "(";
        sequence ~break:Layout.Break_if_needed [
          loc ~loc:{Loc.none with Loc.start={Loc.line=1; column=1; offset=1}; _end={Loc.line=1; column=6; offset=6}} (fused [
            loc ~loc:{Loc.none with Loc.start={Loc.line=1; column=1; offset=1}; _end={Loc.line=1; column=4; offset=4}} (sequence ~break:Layout.Break_if_needed ~inline:(true, true) ~indent:0 [
              fused [
                atom "{";
                sequence ~break:Layout.Break_if_needed [
                  fused [
                    loc ~loc:{Loc.none with Loc.start={Loc.line=1; column=2; offset=2}; _end={Loc.line=1; column=3; offset=3}} (id ~loc:{Loc.none with Loc.start={Loc.line=1; column=2; offset=2}; _end={Loc.line=1; column=3; offset=3}} "a");
                    Layout.IfBreak ((Layout.IfPretty ((atom ","), empty)), empty);
                  ];
                ];
                atom "}";
              ];
            ]);
            pretty_space; atom "="; pretty_space;
            loc ~loc:{Loc.none with Loc.start={Loc.line=1; column=5; offset=5}; _end={Loc.line=1; column=6; offset=6}} (id ~loc:{Loc.none with Loc.start={Loc.line=1; column=5; offset=5}; _end={Loc.line=1; column=6; offset=6}} "b");
          ]);
        ];
        atom ")";
      ];
      atom ";";
    ]))
    "({a}=b);";
