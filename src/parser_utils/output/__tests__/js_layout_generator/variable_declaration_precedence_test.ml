(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
  let seq = E.sequence [E.identifier "y"; E.identifier "z"] in
  let ast = S.variable_declaration [S.variable_declarator "x" ~init:seq] in
  assert_layout_of_statement
    ~ctxt
    L.(
      loc
        (loc
           (fused
              [
                atom "var";
                atom " ";
                loc
                  (fused
                     [
                       loc (id "x");
                       pretty_space;
                       atom "=";
                       pretty_space;
                       wrap_in_parens (expression seq);
                     ]
                  );
                atom ";";
              ]
           )
        )
    )
    ast;

  let ast =
    let init = E.assignment (Patterns.identifier "y") (E.identifier "z") in
    S.variable_declaration [S.variable_declarator "x" ~init]
  in
  assert_layout_of_statement
    ~ctxt
    L.(
      loc
        (loc
           (fused
              [
                atom "var";
                atom " ";
                loc
                  (fused
                     [
                       loc (id "x");
                       pretty_space;
                       atom "=";
                       pretty_space;
                       loc (fused [loc (id "y"); pretty_space; atom "="; pretty_space; loc (id "z")]);
                     ]
                  );
                atom ";";
              ]
           )
        )
    )
    ast;

  let fn_ast = E.function_ () in
  let ast = S.variable_declaration [S.variable_declarator "x" ~init:fn_ast] in
  assert_layout_of_statement
    ~ctxt
    L.(
      loc
        (loc
           (fused
              [
                atom "var";
                atom " ";
                loc (fused [loc (id "x"); pretty_space; atom "="; pretty_space; expression fn_ast]);
                atom ";";
              ]
           )
        )
    )
    ast
