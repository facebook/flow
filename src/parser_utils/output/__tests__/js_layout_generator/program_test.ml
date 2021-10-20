(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

let tests =
  [
    ( "blank_lines_if_in_original" >:: fun ctxt ->
      let ast = Ast_builder.program_of_string "var x = 1;\n\n\nvar y = 2;" in
      let layout = Js_layout_generator.program ~checksum:None ~preserve_docblock:false ast in
      assert_layout
        ~ctxt
        L.(
          program
            (group
               [
                 loc
                   ~loc:
                     {
                       Loc.none with
                       Loc.start = { Loc.line = 1; column = 0 };
                       _end = { Loc.line = 1; column = 10 };
                     }
                   (loc
                      ~loc:
                        {
                          Loc.none with
                          Loc.start = { Loc.line = 1; column = 0 };
                          _end = { Loc.line = 1; column = 10 };
                        }
                      (fused
                         [
                           atom "var";
                           space;
                           loc
                             ~loc:
                               {
                                 Loc.none with
                                 Loc.start = { Loc.line = 1; column = 4 };
                                 _end = { Loc.line = 1; column = 9 };
                               }
                             (fused
                                [
                                  loc
                                    ~loc:
                                      {
                                        Loc.none with
                                        Loc.start = { Loc.line = 1; column = 4 };
                                        _end = { Loc.line = 1; column = 5 };
                                      }
                                    (id
                                       ~loc:
                                         {
                                           Loc.none with
                                           Loc.start = { Loc.line = 1; column = 4 };
                                           _end = { Loc.line = 1; column = 5 };
                                         }
                                       "x"
                                    );
                                  pretty_space;
                                  atom "=";
                                  pretty_space;
                                  loc
                                    ~loc:
                                      {
                                        Loc.none with
                                        Loc.start = { Loc.line = 1; column = 8 };
                                        _end = { Loc.line = 1; column = 9 };
                                      }
                                    (atom "1");
                                ]
                             );
                           atom ";";
                         ]
                      )
                   );
                 pretty_hardline;
                 pretty_hardline;
                 loc
                   ~loc:
                     {
                       Loc.none with
                       Loc.start = { Loc.line = 4; column = 0 };
                       _end = { Loc.line = 4; column = 10 };
                     }
                   (loc
                      ~loc:
                        {
                          Loc.none with
                          Loc.start = { Loc.line = 4; column = 0 };
                          _end = { Loc.line = 4; column = 10 };
                        }
                      (fused
                         [
                           atom "var";
                           space;
                           loc
                             ~loc:
                               {
                                 Loc.none with
                                 Loc.start = { Loc.line = 4; column = 4 };
                                 _end = { Loc.line = 4; column = 9 };
                               }
                             (fused
                                [
                                  loc
                                    ~loc:
                                      {
                                        Loc.none with
                                        Loc.start = { Loc.line = 4; column = 4 };
                                        _end = { Loc.line = 4; column = 5 };
                                      }
                                    (id
                                       ~loc:
                                         {
                                           Loc.none with
                                           Loc.start = { Loc.line = 4; column = 4 };
                                           _end = { Loc.line = 4; column = 5 };
                                         }
                                       "y"
                                    );
                                  pretty_space;
                                  atom "=";
                                  pretty_space;
                                  loc
                                    ~loc:
                                      {
                                        Loc.none with
                                        Loc.start = { Loc.line = 4; column = 8 };
                                        _end = { Loc.line = 4; column = 9 };
                                      }
                                    (atom "2");
                                ]
                             );
                           atom ";";
                         ]
                      )
                   );
               ]
            )
        )
        layout;
      assert_output ~ctxt "var x=1;var y=2;" layout;
      assert_output ~ctxt ~pretty:true "var x = 1;\n\nvar y = 2;" layout
    );
    ( "program_artifact_newline" >:: fun ctxt ->
      let ast = Ast_builder.mk_program [S.expression (E.identifier "x")] in
      let layout =
        Js_layout_generator.program ~preserve_docblock:false ~checksum:(Some "@artifact abc123") ast
      in
      assert_layout
        ~ctxt
        L.(
          program
            (fused
               [
                 group [loc (fused [loc (id "x"); atom ";"])];
                 hardline;
                 atom "/* @artifact abc123 */";
               ]
            )
        )
        layout;
      assert_output ~ctxt "x;\n/* @artifact abc123 */" layout;
      assert_output ~ctxt ~pretty:true "x;\n/* @artifact abc123 */" layout
    );
    ( "program_trailing_semicolon" >:: fun ctxt ->
      let ast =
        Ast_builder.mk_program [S.expression (E.identifier "x"); S.expression (E.identifier "y")]
      in
      let layout = Js_layout_generator.program ~preserve_docblock:false ~checksum:None ast in
      assert_layout
        ~ctxt
        L.(
          program
            (group
               [
                 loc (fused [loc (id "x"); atom ";"]);
                 pretty_hardline;
                 loc (fused [loc (id "y"); atom ";"]);
               ]
            )
        )
        layout;
      assert_output ~ctxt "x;y;" layout;
      assert_output ~ctxt ~pretty:true "x;\ny;" layout
    );
    ( "preserve_docblock" >:: fun ctxt ->
      let c_loc = Loc.{ none with start = { line = 1; column = 1 } } in
      let s_loc = Loc.{ none with start = { line = 2; column = 1 } } in
      let ast =
        let all_comments = [Ast_builder.Comments.line ~loc:c_loc " hello world"] in
        let statements = [S.expression ~loc:s_loc (E.identifier "x")] in
        Ast_builder.mk_program ~all_comments statements
      in
      begin
        let layout = Js_layout_generator.program ~preserve_docblock:true ~checksum:None ast in
        assert_layout
          ~ctxt
          L.(
            program
              (group
                 [
                   loc ~loc:c_loc (fused [atom "//"; atom " hello world"; hardline]);
                   pretty_hardline;
                   loc ~loc:s_loc (fused [loc (id "x"); atom ";"]);
                 ]
              )
          )
          layout;
        assert_output ~ctxt "// hello world\nx;" layout;

        (* TODO: inserts an extra line between line comments *)
        assert_output ~ctxt ~pretty:true "// hello world\n\nx;" layout
      end;

      let layout = Js_layout_generator.program ~preserve_docblock:false ~checksum:None ast in
      assert_layout
        ~ctxt
        L.(program (group [loc ~loc:s_loc (fused [loc (id "x"); atom ";"])]))
        layout;
      assert_output ~ctxt "x;" layout;
      assert_output ~ctxt ~pretty:true "x;" layout
    );
  ]
