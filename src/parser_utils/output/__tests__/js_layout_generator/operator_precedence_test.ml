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
  let x, y, z = E.identifier "x", E.identifier "y", E.identifier "z" in
  let str = E.literal (Literals.string "a") in
  let (&&) a b = E.logical_and a b in
  let (||) a b = E.logical_or a b in
  let (+) a b = E.binary ~op:Flow_ast.Expression.Binary.Plus a b in
  let (-) a b = E.binary ~op:Flow_ast.Expression.Binary.Minus a b in

  (* nested logical expressions *)

  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      expression (x && y); pretty_space; atom "&&";
      sequence ~break:Layout.Break_if_needed ~inline:(false, true) [
        fused [flat_pretty_space; expression z];
      ];
    ]))
    ((x && y) && z);

  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      expression x; pretty_space; atom "&&";
      sequence ~break:Layout.Break_if_needed ~inline:(false, true) [
        fused [flat_pretty_space; wrap_in_parens (expression (y && z))];
      ];
    ]))
    (x && (y && z));

  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      expression (x && y); pretty_space; atom "||";
      sequence ~break:Layout.Break_if_needed ~inline:(false, true) [
        fused [flat_pretty_space; expression z];
      ];
    ]))
    ((x && y) || z);

  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      expression x; pretty_space; atom "&&";
      sequence ~break:Layout.Break_if_needed ~inline:(false, true) [
        fused [flat_pretty_space; wrap_in_parens (expression (y || z))];
      ];
    ]))
    (x && (y || z));

  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      expression (x || y); pretty_space; atom "||";
      sequence ~break:Layout.Break_if_needed ~inline:(false, true) [
        fused [flat_pretty_space; expression z]
      ];
    ]))
    ((x || y) || z);

  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      expression x; pretty_space; atom "||";
      sequence ~break:Layout.Break_if_needed ~inline:(false, true) [
        fused [flat_pretty_space; wrap_in_parens (expression (y || z))];
      ];
    ]))
    (x || (y || z));

  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      wrap_in_parens (expression (x || y));
      pretty_space; atom "&&";
      sequence ~break:Layout.Break_if_needed ~inline:(false, true) [
        fused [flat_pretty_space; expression z];
      ];
    ]))
    ((x || y) && z);

  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      expression x; pretty_space; atom "||";
      sequence ~break:Layout.Break_if_needed ~inline:(false, true) [
        fused [flat_pretty_space; expression (y && z)];
      ];
    ]))
    (x || (y && z));

  (* nested binary expressions *)

  assert_layout_of_expression ~ctxt
    L.(loc (fused [expression (x + y); pretty_space; atom "+"; pretty_space; expression z]))
    ((x + y) + z);

  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      expression x; pretty_space; atom "+"; pretty_space;
      wrap_in_parens (expression (y + z));
    ]))
    (x + (y + z));

  assert_layout_of_expression ~ctxt
    L.(loc (fused [expression (x + y); pretty_space; atom "-"; pretty_space; expression z]))
    ((x + y) - z);

  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      expression x; pretty_space; atom "+"; pretty_space;
      wrap_in_parens (expression (y - z));
    ]))
    (x + (y - z));

  (* logical expressions and binary expressions *)

  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      expression (x + y); pretty_space; atom "&&";
      sequence ~break:Layout.Break_if_needed ~inline:(false, true) [
        fused [flat_pretty_space; expression z];
      ];
    ]))
    ((x + y) && z);

  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      expression x; pretty_space; atom "+"; pretty_space;
      wrap_in_parens (expression (y && z));
    ]))
    (x + (y && z));

  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      wrap_in_parens (expression (x && y));
      pretty_space; atom "+"; pretty_space; expression z;
    ]))
    ((x && y) + z);

  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      expression x; pretty_space; atom "&&";
      sequence ~break:Layout.Break_if_needed ~inline:(false, true) [
        fused [flat_pretty_space; expression (y + z)]
      ];
    ]))
    (x && (y + z));

  (* literals *)

  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      expression str; pretty_space; atom "&&";
      sequence ~break:Layout.Break_if_needed ~inline:(false, true) [
        fused [flat_pretty_space; expression x];
      ];
    ]))
    (str && x);

  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      expression x; pretty_space; atom "&&";
      sequence ~break:Layout.Break_if_needed ~inline:(false, true) [
        fused [flat_pretty_space; expression str];
      ];
    ]))
    (x && str);

  (* function *)

  let fn = (Loc.none, Flow_ast.Expression.Function (
    Functions.make ~id:None ~expression:true ~params:[] ())
  ) in
  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      expression fn; pretty_space; atom "&&";
      sequence ~break:Layout.Break_if_needed ~inline:(false, true) [
        fused [flat_pretty_space; expression x];
      ];
    ]))
    (fn && x);

  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      expression x; pretty_space; atom "&&";
      sequence ~break:Layout.Break_if_needed ~inline:(false, true) [
        fused [flat_pretty_space; expression fn];
      ];
    ]))
    (x && fn);

  (* sequence *)

  let seq = E.sequence [x; y] in
  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      wrap_in_parens (expression seq); pretty_space; atom "&&";
      sequence ~break:Layout.Break_if_needed ~inline:(false, true) [
        fused [flat_pretty_space; expression z];
      ];
    ]))
    (seq && z);

  assert_layout_of_expression ~ctxt
    L.(loc (fused [
      expression z; pretty_space; atom "&&";
      sequence ~break:Layout.Break_if_needed ~inline:(false, true) [
        fused [flat_pretty_space; wrap_in_parens (expression seq)];
      ];
    ]))
    (z && seq);

  assert_layout_of_expression ~ctxt
    L.(loc (sequence ~break:Layout.Break_if_needed ~inline:(true, true) ~indent:0 [
      sequence ~break:Layout.Break_if_needed ~inline:(true, true) ~indent:0 [
        fused [
          expression z;
          Layout.IfBreak ((atom ","), (fused [atom ","; pretty_space]));
        ];
        wrap_in_parens (expression seq);
      ];
    ]))
    (E.sequence [z; seq]);

  assert_layout_of_expression ~ctxt
    L.(loc (sequence ~break:Layout.Break_if_needed ~inline:(true, true) ~indent:0 [
      sequence ~break:Layout.Break_if_needed ~inline:(true, true) ~indent:0 [
        fused [
          wrap_in_parens (expression seq);
          Layout.IfBreak ((atom ","), (fused [atom ","; pretty_space]));
        ];
        expression z;
      ];
    ]))
    (E.sequence [seq; z]);
