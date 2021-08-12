(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Ast_builder
open Layout_test_utils
open Layout_generator_test_utils
module I = Ast_builder.Identifiers
module S = Ast_builder.Statements
module E = Ast_builder.Expressions
module F = Ast_builder.Functions
module J = Ast_builder.JSXs
module L = Layout_builder

let tests =
  "js_layout_generator"
  >::: [
         "operator_precedence" >::: Operator_precedence_test.tests;
         "assignment_precedence" >:: Assignment_precedence_test.test;
         "variable_declaration_precedence" >:: Variable_declaration_precedence_test.test;
         "objects" >::: Object_test.tests;
         "comment" >::: Comment_test.tests;
         "pattern" >::: Pattern_test.tests;
         "program" >::: Program_test.tests;
         "jsx" >::: Jsx_test.tests;
         "trailing_commas" >::: Trailing_commas_test.tests;
         "imports" >::: Import_test.tests;
         ( "unary_plus_binary" >:: fun ctxt ->
           let x = E.identifier "x" in
           let y = E.identifier "y" in
           let plus_y = E.unary_plus y in
           let minus_y = E.unary_minus y in
           let ast = E.plus x plus_y in
           assert_expression ~ctxt "x+ +y" ast;

           let ast = E.plus plus_y x in
           assert_expression ~ctxt "+y+x" ast;

           let ast = E.minus x minus_y in
           assert_expression ~ctxt "x- -y" ast;

           let ast = E.plus x minus_y in
           assert_expression ~ctxt "x+-y" ast;

           let ast = E.minus x plus_y in
           assert_expression ~ctxt "x-+y" ast;

           let ast = E.plus x (E.conditional plus_y y y) in
           assert_expression ~ctxt "x+(+y?y:y)" ast;

           let ast = E.plus x (E.plus plus_y y) in
           assert_expression ~ctxt "x+(+y+y)" ast;

           (* `*` is higher precedence than `+`, so would not normally need parens if
              not for the `+y` *)
           let ast = E.plus x (E.mult plus_y y) in
           assert_expression ~ctxt "x+(+y)*y" ast;

           (* parens are necessary around the inner `+y+y`, but would be reundant
              around the multiplication. that is, we don't need `x+((+y+y)*y)`. *)
           let ast = E.plus x (E.mult (E.plus plus_y y) y) in
           assert_expression ~ctxt "x+(+y+y)*y" ast );
         ( "update_plus_binary" >:: fun ctxt ->
           let x = E.identifier "x" in
           let y = E.identifier "y" in
           let x_incr = E.increment ~prefix:false x in
           let x_decr = E.decrement ~prefix:false x in
           let incr_y = E.increment ~prefix:true y in
           let decr_y = E.decrement ~prefix:true y in
           begin
             let ast = E.plus x incr_y in
             let layout = Js_layout_generator.expression ~opts ast in
             assert_layout
               ~ctxt
               L.(
                 loc
                   (fused
                      [
                        loc (id "x");
                        pretty_space;
                        atom "+";
                        pretty_space;
                        ugly_space;
                        loc (fused [atom "++"; loc (id "y")]);
                      ]))
               layout;
             assert_output ~ctxt "x+ ++y" layout;
             assert_output ~ctxt ~pretty:true "x + ++y" layout
           end;

           let ast = E.minus x incr_y in
           assert_expression ~ctxt "x-++y" ast;

           let ast = E.minus x decr_y in
           assert_expression ~ctxt "x- --y" ast;

           let ast = E.plus x decr_y in
           assert_expression ~ctxt "x+--y" ast;

           let ast = E.plus x_incr y in
           assert_expression ~ctxt "x+++y" ast;

           let ast = E.minus x_decr y in
           assert_expression ~ctxt "x---y" ast;

           let ast = E.plus x_incr incr_y in
           assert_expression ~ctxt "x+++ ++y" ast;

           let ast = E.minus x_decr decr_y in
           assert_expression ~ctxt "x--- --y" ast );
         ( "do_while_semicolon" >:: fun ctxt ->
           (* do { x } while (y) *)
           let layout =
             Js_layout_generator.statement
               ~opts
               (let body = S.block [S.expression (E.identifier "x")] in
                let test = E.identifier "y" in
                S.do_while body test)
           in
           assert_output ~ctxt "do{x}while(y);" layout;
           assert_output ~ctxt ~pretty:true ("do {\n" ^ "  x;\n" ^ "} while (y);") layout );
         ( "do_while_long" >:: fun ctxt ->
           (* do { xxxx... } while (yyyy...) *)
           let x80 = String.make 80 'x' in
           let y80 = String.make 80 'y' in
           let layout =
             Js_layout_generator.statement
               ~opts
               (let body = S.block [S.expression (E.identifier x80)] in
                let test = E.identifier y80 in
                S.do_while body test)
           in
           assert_output ~ctxt ("do{" ^ x80 ^ "}while(" ^ y80 ^ ");") layout;
           assert_output
             ~ctxt
             ~pretty:true
             ("do {\n" ^ "  " ^ x80 ^ ";\n" ^ "} while (\n" ^ "  " ^ y80 ^ "\n" ^ ");")
             layout );
         ( "do_while_single_statement" >:: fun ctxt ->
           (* do x; while (y) *)
           let layout =
             Js_layout_generator.statement
               ~opts
               (let body = S.expression (E.identifier "x") in
                let test = E.identifier "y" in
                S.do_while body test)
           in
           assert_output ~ctxt "do x;while(y);" layout;
           assert_output ~ctxt ~pretty:true "do x; while (y);" layout );
         ( "do_while_single_statement_long" >:: fun ctxt ->
           (* do xxxx...; while (yyyy...) *)
           let x80 = String.make 80 'x' in
           let y80 = String.make 80 'y' in
           let layout =
             Js_layout_generator.statement
               ~opts
               (let body = S.expression (E.identifier x80) in
                let test = E.identifier y80 in
                S.do_while body test)
           in
           assert_output ~ctxt ("do " ^ x80 ^ ";while(" ^ y80 ^ ");") layout;
           assert_output
             ~ctxt
             ~pretty:true
             ("do " ^ x80 ^ "; while (\n" ^ "  " ^ y80 ^ "\n" ^ ");")
             layout );
         ( "do_while_empty_statement" >:: fun ctxt ->
           (* do ; while (y) *)
           let layout =
             Js_layout_generator.statement
               ~opts
               (let body = S.empty () in
                let test = E.identifier "y" in
                S.do_while body test)
           in
           assert_output ~ctxt "do;while(y);" layout;
           assert_output ~ctxt ~pretty:true "do ; while (y);" layout )
         (* TODO: remove space after do *);
         ( "conditionals" >:: fun ctxt ->
           let layout =
             Js_layout_generator.expression
               ~opts
               (E.conditional (E.identifier "a") (E.identifier "b") (E.identifier "c"))
           in
           assert_layout
             ~ctxt
             L.(
               loc
                 (group
                    [
                      loc (id "a");
                      indent
                        (fused
                           [
                             pretty_line;
                             atom "?";
                             pretty_space;
                             loc (id "b");
                             pretty_line;
                             atom ":";
                             pretty_space;
                             loc (id "c");
                           ]);
                    ]))
             layout;
           assert_output ~ctxt "a?b:c" layout;
           assert_output ~ctxt ~pretty:true "a ? b : c" layout;

           let a80 = String.make 80 'a' in
           let layout =
             Js_layout_generator.expression
               ~opts
               (E.conditional (E.identifier a80) (E.identifier "b") (E.identifier "c"))
           in
           assert_output ~ctxt (a80 ^ "?b:c") layout;
           assert_output ~ctxt ~pretty:true (a80 ^ "\n" ^ "  ? b\n" ^ "  : c") layout;

           let b80 = String.make 80 'b' in
           let layout =
             Js_layout_generator.expression
               ~opts
               (E.conditional (E.identifier "a") (E.identifier b80) (E.identifier "c"))
           in
           assert_output ~ctxt ("a?" ^ b80 ^ ":c") layout;
           assert_output ~ctxt ~pretty:true ("a\n" ^ "  ? " ^ b80 ^ "\n" ^ "  : c") layout );
         ( "conditional_expression_parens" >:: fun ctxt ->
           let (a, b, c, d, e) =
             ( E.identifier "a",
               E.identifier "b",
               E.identifier "c",
               E.identifier "d",
               E.identifier "e" )
           in
           (* a ? b++ : c-- *)
           let update =
             E.conditional a (E.increment ~prefix:false b) (E.decrement ~prefix:false c)
           in
           assert_expression ~ctxt "a?b++:c--" update;

           (* a ? +b : -c *)
           let unary = E.conditional a (E.unary_plus b) (E.unary_minus c) in
           assert_expression ~ctxt "a?+b:-c" unary;

           (* (a || b) ? c : d *)
           let logical_test = E.conditional (E.logical_or a b) c d in
           assert_expression ~ctxt "a||b?c:d" logical_test;

           (* (a ? b : c) ? d : e *)
           let nested_in_test = E.conditional (E.conditional a b c) d e in
           assert_expression ~ctxt "(a?b:c)?d:e" nested_in_test;

           (* a ? (b ? c : d) : e *)
           let nested_in_consequent = E.conditional a (E.conditional b c d) e in
           assert_expression ~ctxt "a?b?c:d:e" nested_in_consequent;

           (* a ? b : (c ? d : e) *)
           let nested_in_alternate = E.conditional a b (E.conditional c d e) in
           assert_expression ~ctxt "a?b:c?d:e" nested_in_alternate;

           let assignment =
             E.conditional
               a
               (E.assignment (Patterns.identifier "x") b)
               (E.assignment (Patterns.identifier "y") c)
           in
           assert_expression ~ctxt "a?x=b:y=c" assignment;

           let sequence = E.conditional a (E.sequence [b; c]) (E.sequence [d; e]) in
           assert_expression ~ctxt "a?(b,c):(d,e)" sequence );
         ( "call_expression_parens" >:: fun ctxt ->
           let x = E.identifier "x" in
           (* `(x++)()` *)
           let update = E.call (E.increment ~prefix:false x) in
           assert_expression ~ctxt "(x++)()" update;

           (* `x.y()` *)
           let member = E.call (E.member_expression (E.member x ~property:"y")) in
           assert_expression ~ctxt "x.y()" member;

           (* `x.y.z()` *)
           let two_members =
             E.call
               (E.member_expression
                  (E.member (E.member_expression (E.member x ~property:"y")) ~property:"z"))
           in
           assert_expression ~ctxt "x.y.z()" two_members;

           (* `x()()` *)
           let call = E.call (E.call x) in
           assert_expression ~ctxt "x()()" call;

           (* `new x()()` *)
           let new_ = E.call (E.new_ x ~args:(E.arg_list [])) in
           assert_expression ~ctxt "new x()()" new_;

           (* `(new x)()` *)
           let new_ = E.call (E.new_ x) in
           assert_expression ~ctxt "(new x)()" new_;

           (* `function() {}()` *)
           let func = E.call (E.function_ ()) in
           assert_expression ~ctxt "function(){}()" func;

           (* `(function() {}.foo)()` *)
           let func = E.call (E.member_expression (E.member (E.function_ ()) ~property:"foo")) in
           assert_expression ~ctxt "function(){}.foo()" func;

           (* `(() => {})()` *)
           let arrow = E.call (E.arrow_function ()) in
           assert_expression ~ctxt "(()=>{})()" arrow;

           (* `(foo, bar)()` *)
           let seq = E.call (E.sequence [x; E.identifier "y"]) in
           assert_expression ~ctxt "(x,y)()" seq;

           (* `__d("a", [], (function() {}), 1)` *)
           let underscore_d =
             E.call
               ~args:
                 (E.arg_list
                    [
                      E.expression (E.literal (Literals.string "a"));
                      E.expression (E.literal (Literals.string "b"));
                      E.expression (E.sequence [E.function_ ()]);
                      E.expression (E.literal (Literals.number 1. "1"));
                    ])
               (E.identifier "__d")
           in
           assert_expression ~ctxt "__d(\"a\",\"b\",(function(){}),1)" underscore_d );
         ( "member_expression_parens" >:: fun ctxt ->
           let x = E.identifier "x" in
           (* `(x++).y` *)
           let update =
             E.member_expression (E.member (E.increment ~prefix:false x) ~property:"y")
           in
           assert_expression ~ctxt "(x++).y" update;

           (* `x.y.z` *)
           let member =
             E.member_expression
               (E.member (E.member_expression (E.member x ~property:"y")) ~property:"z")
           in
           assert_expression ~ctxt "x.y.z" member;

           (* x().y *)
           let call = E.member_expression (E.member (E.call x) ~property:"y") in
           assert_expression ~ctxt "x().y" call;

           (* x()[y] *)
           let computed =
             E.member_expression (E.member_computed (E.call x) ~property:(E.identifier "y"))
           in
           assert_expression ~ctxt "x()[y]" computed;

           (* `(function() {}).x` *)
           let func = E.member_expression (E.member (E.function_ ()) ~property:"x") in
           assert_expression ~ctxt "function(){}.x" func;

           (* `(() => {}).x` *)
           let func = E.member_expression (E.member (E.arrow_function ()) ~property:"x") in
           assert_expression ~ctxt "(()=>{}).x" func;

           (* `(x, y).z` *)
           let seq =
             E.member_expression (E.member (E.sequence [x; E.identifier "y"]) ~property:"z")
           in
           assert_expression ~ctxt "(x,y).z" seq;

           let num =
             E.member_expression (E.member (E.literal (Literals.number 1.0 "1")) ~property:"z")
           in
           assert_expression ~ctxt "1..z" num;
           let num =
             E.member_expression (E.member (E.literal (Literals.number 1.1 "1.1")) ~property:"z")
           in
           assert_expression ~ctxt "1.1.z" num;
           let num =
             E.member_expression
               (E.member (E.literal (Literals.number 0.0000001 "0.0000001")) ~property:"z")
           in
           assert_expression ~ctxt "1e-7.z" num );
         ( "new_expression_empty_params" >:: fun ctxt ->
           (* `new xxxxxxx....()` *)
           let x80 = String.make 80 'x' in
           let layout =
             Js_layout_generator.expression ~opts (E.new_ ~args:(E.arg_list []) (E.identifier x80))
           in
           assert_layout
             ~ctxt
             L.(loc (group [atom "new"; space; loc (id x80); loc (group [atom "("; atom ")"])]))
             layout;
           assert_output ~ctxt ("new " ^ x80 ^ "()") layout;
           assert_output ~ctxt ~pretty:true ("new " ^ x80 ^ "()") layout );
         ( "new_expression_params" >:: fun ctxt ->
           (* `new Foo(x, y)` *)
           let layout =
             Js_layout_generator.expression
               ~opts
               (E.new_
                  (E.identifier "Foo")
                  ~args:
                    (E.arg_list [E.expression (E.identifier "x"); E.expression (E.identifier "y")]))
           in
           assert_layout
             ~ctxt
             L.(
               loc
                 (group
                    [
                      atom "new";
                      space;
                      loc (id "Foo");
                      loc
                        (group
                           [
                             atom "(";
                             indent
                               (fused
                                  [
                                    softline;
                                    loc (id "x");
                                    atom ",";
                                    pretty_line;
                                    loc (id "y");
                                    Layout.IfBreak (atom ",", empty);
                                  ]);
                             softline;
                             atom ")";
                           ]);
                    ]))
             layout;
           assert_output ~ctxt "new Foo(x,y)" layout;
           assert_output ~ctxt ~pretty:true "new Foo(x, y)" layout );
         ( "new_expression_params_long" >:: fun ctxt ->
           (* `new Foo(xxxxxxx....)` *)
           let x80 = String.make 80 'x' in
           let layout =
             Js_layout_generator.expression
               ~opts
               (E.new_ (E.identifier "Foo") ~args:(E.arg_list [E.expression (E.identifier x80)]))
           in
           assert_layout
             ~ctxt
             L.(
               loc
                 (group
                    [
                      atom "new";
                      space;
                      loc (id "Foo");
                      loc
                        (group
                           [
                             atom "(";
                             indent
                               (fused [softline; loc (id x80); Layout.IfBreak (atom ",", empty)]);
                             softline;
                             atom ")";
                           ]);
                    ]))
             layout;
           assert_output ~ctxt ("new Foo(" ^ x80 ^ ")") layout;
           assert_output ~ctxt ~pretty:true ("new Foo(\n" ^ "  " ^ x80 ^ ",\n" ^ ")") layout );
         ( "new_expression_parens" >:: fun ctxt ->
           let x80 = String.make 80 'x' in
           let (x, y, z, id80) =
             (E.identifier "x", E.identifier "y", E.identifier "z", E.identifier x80)
           in
           (* `new (x++)()` *)
           begin
             let layout =
               Js_layout_generator.expression
                 ~opts
                 (E.new_ (E.increment ~prefix:false x) ~args:(E.arg_list []))
             in
             assert_layout
               ~ctxt
               L.(
                 loc
                   (group
                      [
                        atom "new";
                        pretty_space;
                        wrap_in_parens (loc (fused [loc (id "x"); atom "++"]));
                        loc (group [atom "("; atom ")"]);
                      ]))
               layout;
             assert_output ~ctxt "new(x++)()" layout;
             assert_output ~ctxt ~pretty:true "new (x++)()" layout;

             let update = E.new_ (E.increment ~prefix:false id80) ~args:(E.arg_list []) in
             assert_expression ~ctxt ("new(" ^ x80 ^ "++)()") update;
             assert_expression ~ctxt ~pretty:true ("new (" ^ x80 ^ "++)()") update
           end;

           (* `new (x())()` *)
           let call = E.new_ (E.call x) ~args:(E.arg_list []) in
           assert_expression ~ctxt "new(x())()" call;

           (* `new x.y()` *)
           let member =
             E.new_ (E.member_expression (E.member x ~property:"y")) ~args:(E.arg_list [])
           in
           assert_expression ~ctxt "new x.y()" member;

           (* `new (x.y())()` *)
           let member_call =
             E.new_ (E.call (E.member_expression (E.member x ~property:"y"))) ~args:(E.arg_list [])
           in
           assert_expression ~ctxt "new(x.y())()" member_call;

           (* `new (x().y)()` *)
           let call_member =
             E.new_ (E.member_expression (E.member (E.call x) ~property:"y")) ~args:(E.arg_list [])
           in
           assert_expression ~ctxt "new(x().y)()" call_member;

           (* `new (x ? y : z)()` *)
           let cond = E.new_ (E.conditional x y z) ~args:(E.arg_list []) in
           assert_expression ~ctxt "new(x?y:z)()" cond );
         ( "unary_expression_parens" >:: fun ctxt ->
           (* `+(+x)` *)
           let plus = E.unary_plus (E.unary_plus (E.identifier "x")) in
           assert_expression ~ctxt "+(+x)" plus;

           (* `+-x` *)
           let minus = E.unary_plus (E.unary_minus (E.identifier "x")) in
           assert_expression ~ctxt "+-x" minus;

           (* `+(++x)` *)
           let prefix_incr = E.unary_plus (E.increment ~prefix:true (E.identifier "x")) in
           assert_expression ~ctxt "+(++x)" prefix_incr;

           (* `+--x` *)
           let prefix_decr = E.unary_plus (E.decrement ~prefix:true (E.identifier "x")) in
           assert_expression ~ctxt "+--x" prefix_decr;

           (* `+x++` *)
           let suffix_incr = E.unary_plus (E.increment ~prefix:false (E.identifier "x")) in
           assert_expression ~ctxt "+x++" suffix_incr;

           (* `+x--` *)
           let suffix_decr = E.unary_plus (E.decrement ~prefix:false (E.identifier "x")) in
           assert_expression ~ctxt "+x--" suffix_decr;

           (* `+x()` *)
           let call = E.unary_plus (E.call (E.identifier "x")) in
           assert_expression ~ctxt "+x()" call;

           (* `+new x()` *)
           let new_ = E.unary_plus (E.new_ ~args:(E.arg_list []) (E.identifier "x")) in
           assert_expression ~ctxt "+new x()" new_ );
         ( "expression_statement_parens" >:: fun ctxt ->
           let obj = S.expression (E.object_ []) in
           assert_statement ~ctxt "({});" obj;

           let func = S.expression (E.function_ ()) in
           assert_statement ~ctxt "(function(){});" func;

           let arrow = S.expression (E.arrow_function ()) in
           assert_statement ~ctxt "()=>{};" arrow;

           let klass = S.expression (E.class_ []) in
           assert_statement ~ctxt "(class{});" klass;

           let func_call = S.expression (E.call (E.function_ ())) in
           assert_statement ~ctxt "(function(){})();" func_call;

           let func_member =
             S.expression (E.member_expression (E.member (E.function_ ()) ~property:"foo"))
           in
           assert_statement ~ctxt "(function(){}).foo;" func_member;

           let class_member =
             S.expression (E.member_expression (E.member (E.class_ []) ~property:"foo"))
           in
           assert_statement ~ctxt "(class{}).foo;" class_member;

           let func_member_call =
             S.expression (E.call (E.member_expression (E.member (E.function_ ()) ~property:"foo")))
           in
           assert_statement ~ctxt "(function(){}).foo();" func_member_call;

           let func_call_member =
             S.expression (E.member_expression (E.member (E.call (E.function_ ())) ~property:"foo"))
           in
           assert_statement ~ctxt "(function(){})().foo;" func_call_member;

           let func_sequence = S.expression (E.sequence [E.function_ (); E.identifier "x"]) in
           assert_statement ~ctxt "(function(){}),x;" func_sequence );
         ( "arrow_body_parens" >:: fun ctxt ->
           let (x, y, z) = (E.identifier "x", E.identifier "y", E.identifier "z") in
           let arrow =
             let body = Functions.body_expression (E.sequence [x; y]) in
             E.arrow_function ~body ()
           in
           assert_expression ~ctxt "()=>(x,y)" arrow;

           let arrow =
             let body = Functions.body_expression (E.conditional x y z) in
             E.arrow_function ~body ()
           in
           assert_expression ~ctxt "()=>x?y:z" arrow;

           let arrow =
             let body = Functions.body_expression x in
             E.arrow_function ~body ()
           in
           assert_expression ~ctxt "(()=>x)?y:z" (E.conditional arrow y z);

           let arrow =
             let arrow = E.arrow_function () in
             let body = Functions.body_expression arrow in
             E.arrow_function ~body ()
           in
           assert_expression ~ctxt "()=>()=>{}" arrow );
         ( "argument_parens" >:: fun ctxt ->
           let f = E.identifier "f" in
           let (x, y, z) = (E.identifier "x", E.identifier "y", E.identifier "z") in
           let args = E.arg_list [] in
           let call = E.call ~args f in
           assert_expression ~ctxt "f()" call;

           let args =
             let seq = E.sequence [x; y] in
             E.arg_list [E.expression seq]
           in
           let call = E.call ~args f in
           assert_expression ~ctxt ~msg:"sequence should be parenthesized" "f((x,y))" call;

           let args = E.arg_list [E.spread (E.sequence [x; y])] in
           let call = E.call ~args f in
           assert_expression ~ctxt ~msg:"sequence should be parenthesized" "f(...(x,y))" call;

           let args = E.arg_list [E.expression (E.conditional x y z)] in
           let call = E.call ~args f in
           assert_expression ~ctxt ~msg:"higher-precedence ops don't need parens" "f(x?y:z)" call;

           let call =
             let arrow = E.arrow_function () in
             let args = E.arg_list [E.expression arrow] in
             E.call ~args f
           in
           assert_expression ~ctxt ~msg:"higher-precedence ops don't need parens" "f(()=>{})" call;

           let args =
             let seq = E.sequence [x; y] in
             let logical = E.logical_or seq z in
             E.arg_list [E.expression logical]
           in
           let call = E.call ~args f in
           assert_expression ~ctxt ~msg:"nested sequence has parens" "f((x,y)||z)" call );
         ( "binary_in_space" >:: fun ctxt ->
           let ast = statement_of_string {|if("foo" in {"foo": bar}){}|} in
           assert_statement ~ctxt {|if("foo"in{"foo":bar}){}|} ast;

           let ast = statement_of_string {|if("foo" in bar){}|} in
           assert_statement ~ctxt {|if("foo"in bar){}|} ast;

           let ast = statement_of_string {|if(foo in {"foo":bar}){}|} in
           assert_statement ~ctxt {|if(foo in{"foo":bar}){}|} ast );
         ( "binary_instanceof_space" >:: fun ctxt ->
           begin
             let ast = E.instanceof (E.literal (Literals.string "foo")) (E.object_ []) in
             let layout = Js_layout_generator.expression ~opts ast in
             assert_layout
               ~ctxt
               L.(
                 loc
                   (fused
                      [
                        loc (fused [atom "\""; atom "foo"; atom "\""]);
                        pretty_space;
                        atom "instanceof";
                        pretty_space;
                        loc (group [atom "{"; atom "}"]);
                      ]))
               layout;
             assert_output ~ctxt {|"foo"instanceof{}|} layout;
             assert_output ~ctxt ~pretty:true {|"foo" instanceof {}|} layout
           end;

           begin
             let ast = E.instanceof (E.literal (Literals.string "foo")) (E.identifier "bar") in
             let layout = Js_layout_generator.expression ~opts ast in
             assert_layout
               ~ctxt
               L.(
                 loc
                   (fused
                      [
                        loc (fused [atom "\""; atom "foo"; atom "\""]);
                        pretty_space;
                        atom "instanceof";
                        space;
                        loc (id "bar");
                      ]))
               layout;
             assert_output ~ctxt {|"foo"instanceof bar|} layout;
             assert_output ~ctxt ~pretty:true {|"foo" instanceof bar|} layout
           end;

           let ast = E.instanceof (E.identifier "foo") (E.object_ []) in
           let layout = Js_layout_generator.expression ~opts ast in
           assert_layout
             ~ctxt
             L.(
               loc
                 (fused
                    [
                      loc (id "foo");
                      space;
                      atom "instanceof";
                      pretty_space;
                      loc (group [atom "{"; atom "}"]);
                    ]))
             layout;
           assert_output ~ctxt {|foo instanceof{}|} layout;
           assert_output ~ctxt ~pretty:true {|foo instanceof {}|} layout );
         ( "logical_wrapping" >:: fun ctxt ->
           let x40 = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" in
           let ast = E.logical_and (E.identifier x40) (E.identifier x40) in
           let layout = Js_layout_generator.expression ~opts ast in
           assert_layout
             ~ctxt
             L.(
               loc
                 (group
                    [
                      loc (id "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
                      pretty_space;
                      atom "&&";
                      indent
                        (fused
                           [
                             Layout.IfBreak (hardline, pretty_space);
                             loc (id "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
                           ]);
                    ]))
             layout;
           assert_output ~ctxt (x40 ^ "&&" ^ x40) layout;
           assert_output ~ctxt ~pretty:true (x40 ^ " &&\n  " ^ x40) layout );
         ( "return_statement_parens" >:: fun ctxt ->
           let ret = S.return None in
           assert_statement ~ctxt "return;" ret;

           let x = E.identifier "x" in
           let y = E.identifier "y" in
           let seq = E.sequence [x; y] in
           let ret = S.return (Some seq) in
           assert_statement ~ctxt "return x,y;" ret;
           assert_statement ~ctxt ~pretty:true "return x, y;" ret;

           (* sequences get split across lines and wrapped in parens *)
           let x40 = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" in
           let y40 = "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy" in
           let func =
             S.function_declaration
               (I.identifier "f")
               ~body:(F.body [S.return (Some (E.sequence [E.identifier x40; E.identifier y40]))])
           in
           assert_layout_result
             ~ctxt
             L.(
               loc
                 (fused
                    [
                      atom "return";
                      space;
                      group
                        [
                          Layout.IfBreak (atom "(", empty);
                          indent
                            (fused
                               [
                                 softline;
                                 loc
                                   (group
                                      [
                                        loc (id "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
                                        atom ",";
                                        pretty_line;
                                        loc (id "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy");
                                      ]);
                               ]);
                          softline;
                          Layout.IfBreak (atom ")", empty);
                        ];
                      Layout.IfPretty (atom ";", empty);
                    ]))
             Layout_matcher.(body_of_function_declaration func >>= nth_fused 0);
           assert_statement ~ctxt ("function f(){return " ^ x40 ^ "," ^ y40 ^ "}") func;
           assert_statement
             ~ctxt
             ~pretty:true
             ("function f() {\n  return (\n    " ^ x40 ^ ",\n    " ^ y40 ^ "\n  );\n}")
             func;

           (* logicals get split *)
           let logical = E.logical_and (E.identifier x40) (E.identifier y40) in
           let func =
             S.function_declaration (I.identifier "f") ~body:(F.body [S.return (Some logical)])
           in
           assert_layout_result
             ~ctxt
             L.(
               loc
                 (fused
                    [
                      atom "return";
                      space;
                      group
                        [
                          Layout.IfBreak (atom "(", empty);
                          indent (fused [softline; expression logical]);
                          softline;
                          Layout.IfBreak (atom ")", empty);
                        ];
                      Layout.IfPretty (atom ";", empty);
                    ]))
             Layout_matcher.(body_of_function_declaration func >>= nth_fused 0);
           assert_statement
             ~ctxt
             ~pretty:true
             ("function f() {\n  return (\n    " ^ x40 ^ " &&\n      " ^ y40 ^ "\n  );\n}")
             func;

           (* binary expressions get split *)
           let plus = E.plus (E.identifier x40) (E.identifier y40) in
           let func =
             S.function_declaration (I.identifier "f") ~body:(F.body [S.return (Some plus)])
           in
           assert_layout_result
             ~ctxt
             L.(
               loc
                 (fused
                    [
                      atom "return";
                      space;
                      group
                        [
                          Layout.IfBreak (atom "(", empty);
                          indent (fused [softline; expression plus]);
                          softline;
                          Layout.IfBreak (atom ")", empty);
                        ];
                      Layout.IfPretty (atom ";", empty);
                    ]))
             Layout_matcher.(body_of_function_declaration func >>= nth_fused 0);
           assert_statement
             ~ctxt
             ~pretty:true
             ("function f() {\n  return (\n    " ^ x40 ^ " + " ^ y40 ^ "\n  );\n}")
             func;

           (* jsx gets split *)
           let long_name = String.make 80 'A' in
           let jsx = E.jsx_element (J.element (J.identifier long_name)) in
           let func =
             S.function_declaration (I.identifier "f") ~body:(F.body [S.return (Some jsx)])
           in
           assert_layout_result
             ~ctxt
             L.(
               loc
                 (fused
                    [
                      atom "return";
                      pretty_space;
                      group
                        [
                          Layout.IfBreak (atom "(", empty);
                          indent (fused [softline; expression jsx]);
                          softline;
                          Layout.IfBreak (atom ")", empty);
                        ];
                      Layout.IfPretty (atom ";", empty);
                    ]))
             Layout_matcher.(body_of_function_declaration func >>= nth_fused 0);
           assert_statement
             ~ctxt
             ~pretty:true
             ("function f() {\n  return (\n    <" ^ long_name ^ "></" ^ long_name ^ ">\n  );\n}")
             func;

           (* a string doesn't get split *)
           let x80 = x40 ^ x40 in
           let func =
             S.function_declaration
               (I.identifier "f")
               ~body:(F.body [S.return (Some (E.identifier x80))])
           in
           assert_layout_result
             ~ctxt
             L.(
               loc
                 (fused
                    [
                      atom "return";
                      atom " ";
                      loc
                        (id
                           "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
                      Layout.IfPretty (atom ";", empty);
                    ]))
             Layout_matcher.(body_of_function_declaration func >>= nth_fused 0);
           assert_statement ~ctxt ("function f(){return " ^ x80 ^ "}") func;
           assert_statement ~ctxt ~pretty:true ("function f() {\n  return " ^ x80 ^ ";\n}") func );
         ( "return_statement_space" >:: fun ctxt ->
           let assert_no_space ~ctxt expr =
             let ret = statement_of_string ("return " ^ expr ^ ";") in
             assert_statement ~ctxt ("return" ^ expr ^ ";") ret
           in
           assert_no_space ~ctxt {|"foo"|};
           assert_no_space ~ctxt {|{foo:"bar"}|};
           assert_no_space ~ctxt {|[foo]|};
           assert_no_space ~ctxt {|!foo|};
           assert_no_space ~ctxt {|+foo|};
           assert_no_space ~ctxt {|-foo|};
           assert_no_space ~ctxt {|~foo|};

           let ret = statement_of_string {|return (foo);|} in
           assert_statement ~ctxt {|return foo;|} ret;

           let ret = statement_of_string {|return 123;|} in
           assert_statement ~ctxt {|return 123;|} ret );
         ( "for_loop" >:: fun ctxt ->
           let x80 = String.make 80 'x' in
           let layout =
             Js_layout_generator.statement ~opts (S.for_ (E.identifier x80) None None (S.empty ()))
           in
           assert_layout
             ~ctxt
             L.(
               loc
                 (fused
                    [
                      group
                        [
                          atom "for";
                          pretty_space;
                          atom "(";
                          indent
                            (fused
                               [
                                 softline; loc (id x80); atom ";"; pretty_line; atom ";"; pretty_line;
                               ]);
                          softline;
                          atom ")";
                        ];
                      loc (atom ";");
                    ]))
             layout;
           assert_output ~ctxt ("for(" ^ x80 ^ ";;);") layout;
           assert_output
             ~ctxt
             ~pretty:true
             ("for (\n"
             ^ "  "
             ^ x80
             ^ ";\n"
             ^ "  ;\n"
             ^ "  \n"
             (* TODO: remove trailing whitespace *)
             ^ ");")
             layout;

           (* should wrap because `for (xxxxx...xxx; true; true) {` does't fit on one line *)
           let len = 80 - String.length "for (; true; true) {" + 1 in
           assert_statement_string
             ~ctxt
             ~pretty:true
             (Printf.sprintf "for (\n  %s;\n  true;\n  true\n) {}" (String.make len 'x')) );
         ( "binary_in_in_for_loops" >:: fun ctxt ->
           let ast =
             let (x, y) = (E.identifier "x", E.identifier "y") in
             let init = E.in_ x y in
             let body = S.empty () in
             S.for_ init None None body
           in
           assert_statement ~ctxt ~msg:"binary `in` expressions need parens" "for((x in y);;);" ast;

           let ast =
             let (y, z) = (E.identifier "y", E.identifier "z") in
             let true_ = Expressions.true_ () in
             let in_expr = E.in_ y z in
             let eq_expr = E.equal true_ in_expr in
             let init = E.assignment (Patterns.identifier "x") eq_expr in
             let body = S.empty () in
             S.for_ init None None body
           in
           assert_statement
             ~ctxt
             ~msg:"binary `in` expressions need parens"
             "for(x=true==(y in z);;);"
             ast );
         ( "for_in_space" >:: fun ctxt ->
           let ast = statement_of_string {|for(var x in {"foo": bar}){}|} in
           assert_statement ~ctxt {|for(var x in{"foo":bar}){}|} ast;

           let ast = statement_of_string {|for(var x in bar){}|} in
           assert_statement ~ctxt {|for(var x in bar){}|} ast );
         ( "for_statement_without_block" >:: fun ctxt ->
           assert_statement_string ~ctxt "for(;;)x;";
           assert_statement_string ~ctxt "{for(;;)x}" );
         ( "if_statement_with_labeled_consequent" >:: fun ctxt ->
           let ast =
             S.if_
               (E.identifier "x")
               (S.labeled (I.identifier "y") (S.expression (E.identifier "z")))
               (Some (S.if_alternate (S.expression (E.identifier "z"))))
           in
           assert_statement ~ctxt "if(x)y:z;else z;" ast;
           assert_statement ~ctxt ~pretty:true "if (x) y: z; else z;" ast );
         ( "if_statement_without_block" >:: fun ctxt ->
           let if_stmt = S.if_ (E.identifier "x") (S.expression (E.identifier "y")) None in
           assert_statement ~ctxt "if(x)y;" if_stmt;
           assert_statement ~ctxt ~pretty:true "if (x) y;" if_stmt;

           let ast = S.block [if_stmt; S.expression (E.identifier "z")] in
           assert_statement ~ctxt "{if(x)y;z}" ast;
           assert_statement ~ctxt ~pretty:true ("{\n" ^ "  if (x) y;\n" ^ "  z;\n" ^ "}") ast );
         ( "if_statement_with_empty_consequent" >:: fun ctxt ->
           let layout =
             Js_layout_generator.statement ~opts (S.if_ (E.identifier "x") (S.empty ()) None)
           in
           assert_output ~ctxt "if(x);" layout;
           assert_output ~ctxt ~pretty:true "if (x);" layout );
         ( "if_else_statement_without_block" >:: fun ctxt ->
           let if_else_stmt =
             S.if_
               (E.identifier "x")
               (S.expression (E.identifier "y"))
               (Some (S.if_alternate (S.expression (E.identifier "z"))))
           in
           assert_statement ~ctxt "if(x)y;else z;" if_else_stmt;
           assert_statement ~ctxt ~pretty:true "if (x) y; else z;" if_else_stmt;

           let ast = S.block [if_else_stmt] in
           assert_statement ~ctxt "{if(x)y;else z}" ast;
           assert_statement ~ctxt ~pretty:true ("{\n" ^ "  if (x) y; else z;\n" ^ "}") ast;

           let ast =
             S.if_
               (E.identifier "x")
               (S.expression (E.identifier "y"))
               (Some (S.if_alternate (S.expression (E.increment ~prefix:true (E.identifier "z")))))
           in
           assert_statement ~ctxt "if(x)y;else++z;" ast;
           assert_statement ~ctxt ~pretty:true "if (x) y; else ++z;" ast );
         ( "if_statement_without_block_long" >:: fun ctxt ->
           let a80 = String.make 80 'A' in
           let if_stmt = S.if_ (E.identifier a80) (S.expression (E.identifier "y")) None in
           assert_statement ~ctxt ("if(" ^ a80 ^ ")y;") if_stmt;
           assert_statement
             ~ctxt
             ~pretty:true
             ("if (\n" ^ "  " ^ a80 ^ "\n" ^ ")\n" ^ "  y;")
             if_stmt;

           let ast = S.block [if_stmt; S.expression (E.identifier "z")] in
           assert_statement ~ctxt ("{if(" ^ a80 ^ ")y;z}") ast;
           assert_statement
             ~ctxt
             ~pretty:true
             ("{\n" ^ "  if (\n" ^ "    " ^ a80 ^ "\n" ^ "  )\n" ^ "    y;\n" ^ "  z;\n" ^ "}")
             ast );
         ( "if_else_statement_with_empty_consequent" >:: fun ctxt ->
           let layout =
             Js_layout_generator.statement
               ~opts
               (S.if_
                  (E.identifier "x")
                  (S.empty ())
                  (Some (S.if_alternate (S.expression (E.identifier "y")))))
           in
           assert_output ~ctxt "if(x);else y;" layout;
           assert_output ~ctxt ~pretty:true "if (x); else y;" layout );
         ( "if_else_statement_with_empty_alternate" >:: fun ctxt ->
           let layout =
             Js_layout_generator.statement
               ~opts
               (S.if_
                  (E.identifier "x")
                  (S.expression (E.identifier "y"))
                  (Some (S.if_alternate (S.empty ()))))
           in
           assert_output ~ctxt "if(x)y;else;" layout;
           assert_output ~ctxt ~pretty:true "if (x) y; else ;" layout )
         (* TODO: remove extra space *);
         ( "if_else_statement_with_empty_consequent_and_alternate" >:: fun ctxt ->
           let layout =
             Js_layout_generator.statement
               ~opts
               (S.if_ (E.identifier "x") (S.empty ()) (Some (S.if_alternate (S.empty ()))))
           in
           assert_output ~ctxt "if(x);else;" layout;
           assert_output ~ctxt ~pretty:true "if (x); else ;" layout )
         (* TODO: remove extra space *);
         ( "while_statement_without_block" >:: fun ctxt ->
           let while_stmt = S.while_ (E.identifier "x") (S.expression (E.identifier "y")) in
           assert_statement ~ctxt "while(x)y;" while_stmt;

           let ast = S.block [while_stmt] in
           assert_statement ~ctxt "{while(x)y}" ast;

           let ast = S.while_ (E.identifier "x") (S.empty ()) in
           assert_statement ~ctxt "while(x);" ast;
           assert_statement ~ctxt ~pretty:true "while (x);" ast );
         ( "do_while_statements" >:: fun ctxt ->
           let ast =
             S.do_while
               (S.labeled (I.identifier "x") (S.expression (E.identifier "z")))
               (E.identifier "y")
           in
           assert_statement ~ctxt "do x:z;while(y);" ast;
           assert_statement ~ctxt ~pretty:true "do x: z; while (y);" ast;

           let ast =
             S.do_while
               (S.expression (E.increment ~prefix:true (E.identifier "x")))
               (E.identifier "y")
           in
           assert_statement ~ctxt "do++x;while(y);" ast );
         ( "labeled_empty_statement" >:: fun ctxt ->
           let layout =
             Js_layout_generator.statement ~opts (S.labeled (I.identifier "x") (S.empty ()))
           in
           assert_output ~ctxt "x:;" layout;
           assert_output ~ctxt ~pretty:true "x: ;" layout );
         ( "array_expressions" >:: fun ctxt ->
           assert_expression_string ~ctxt "[]";
           assert_expression_string ~ctxt "[a]";
           assert_expression_string ~ctxt "[a,b]";
           assert_expression_string ~ctxt "[a,,b]";
           assert_expression_string ~ctxt "[a,b,,]";
           assert_expression_string ~ctxt ~pretty:true "[a]";
           assert_expression_string ~ctxt ~pretty:true "[a, b]";
           assert_expression_string ~ctxt ~pretty:true "[a, b, ,]";
           assert_expression_string
             ~ctxt
             ~pretty:true
             ("[\n  a,\n  " ^ String.make 80 'b' ^ ",\n  ,\n]") );
         ( "array_with_trailing_hole" >:: fun ctxt ->
           let layout =
             Js_layout_generator.expression
               ~opts
               (E.array [E.array_expression (E.identifier "a"); E.array_hole ()])
           in
           assert_layout
             ~ctxt
             L.(
               loc
                 (group
                    [
                      atom "[";
                      indent (fused [softline; loc (id "a"); atom ","; pretty_line; atom ","]);
                      softline;
                      atom "]";
                    ]))
             layout;
           assert_output ~ctxt "[a,,]" layout;
           assert_output ~ctxt ~pretty:true "[a, ,]" layout;

           let a80 = String.make 80 'a' in
           let layout =
             Js_layout_generator.expression
               ~opts
               (E.array [E.array_expression (E.identifier a80); E.array_hole ()])
           in
           assert_output ~ctxt ("[" ^ a80 ^ ",,]") layout;
           assert_output ~ctxt ~pretty:true ("[\n" ^ "  " ^ a80 ^ ",\n" ^ "  ,\n" ^ "]") layout );
         ( "function_statements" >:: fun ctxt ->
           assert_statement_string ~ctxt "function a(){}";
           assert_statement_string ~ctxt "async function a(){}";
           assert_statement_string ~ctxt "function* a(){}";
           assert_statement_string ~ctxt "function a(a){}";
           assert_statement_string ~ctxt "function a(a,b){}";
           assert_statement_string ~ctxt "function a(a:b){}";
           assert_statement_string ~ctxt "function a(a:b,c:d){}";
           assert_statement_string ~ctxt "function a(a:?b=b){}";
           assert_statement_string ~ctxt "function a():a{}";
           assert_statement_string ~ctxt "function a<b>(){}";
           assert_statement_string ~ctxt "function a():%checks{}";
           assert_statement_string ~ctxt "function a():a%checks{}";
           assert_statement_string ~ctxt "function a():a%checks(a){}";
           assert_statement_string ~ctxt ~pretty:true "function a(): a %checks(a) {}";
           assert_statement_string ~ctxt ~pretty:true "function a(a: a, b: b): a {}";
           assert_statement_string
             ~ctxt
             ~pretty:true
             ("function a(\n  a: a,\n  b: " ^ String.make 80 'b' ^ ",\n): a {}");
           assert_statement_string ~ctxt ~pretty:true "function a() {\n  a;\n}" );
         ( "function_expressions" >:: fun ctxt ->
           assert_expression_string ~ctxt "function(){}";
           assert_expression_string ~ctxt "function a(){}";
           assert_expression_string ~ctxt "async function(){}";
           assert_expression_string ~ctxt "function*(){}";
           assert_expression_string ~ctxt "function(a){}";
           assert_expression_string ~ctxt "function(a,b){}";
           assert_expression_string ~ctxt "function(a:a,b:b):c{}";
           assert_expression_string ~ctxt "function<a>(){}";
           assert_expression_string ~ctxt ~pretty:true "function(a: a, b: b): c {}";
           assert_expression_string ~ctxt "()=>a";
           assert_expression_string ~ctxt "()=>{}";
           assert_expression_string ~ctxt "():* =>{}";
           assert_expression_string ~ctxt "async()=>{}";
           assert_expression_string ~ctxt "a=>{}";
           assert_expression_string ~ctxt "async a=>{}";
           assert_expression_string ~ctxt "<a>(a)=>{}";
           assert_expression_string ~ctxt "(a,b)=>{}";
           assert_expression_string ~ctxt "(a):%checks=>{}";
           assert_expression_string ~ctxt "({a})=>a";
           assert_expression_string ~ctxt "({a})=>({a:b})";
           assert_expression_string ~ctxt "({a})=>[]";
           assert_expression_string ~ctxt "({a})=>i++";
           assert_expression_string ~ctxt "({a})=>a()";
           assert_expression_string ~ctxt "(a:b)=>{}";
           assert_expression_string ~ctxt "(a?:b)=>{}";
           assert_expression_string ~ctxt "(a):b=>{}";
           assert_expression_string ~ctxt "():c=>{}";
           assert_expression_string ~ctxt "(a):c=>{}";
           assert_expression_string ~ctxt "(a:a,b:b):c=>{}";
           assert_expression_string ~ctxt ~pretty:true "(a: a, b: b): c => {}" );
         ( "function_parameters" >:: fun ctxt ->
           let long_a = String.make 80 'a' in
           let long_b = String.make 80 'b' in
           assert_expression_string
             ~ctxt
             ~pretty:true
             ("function f(\n  " ^ long_a ^ ",\n  " ^ long_b ^ ",\n) {}");
           assert_expression_string
             ~ctxt
             ~pretty:true
             ("function f(\n  " ^ long_a ^ ",\n  ..." ^ long_b ^ "\n) {}");
           assert_expression_string
             ~ctxt
             ~pretty:true
             ("(\n  " ^ long_a ^ ",\n  " ^ long_b ^ ",\n) => {}");
           assert_expression_string
             ~ctxt
             ~pretty:true
             ("(\n  " ^ long_a ^ ",\n  ..." ^ long_b ^ "\n) => {}") );
         ( "class_statements" >:: fun ctxt ->
           let long_a = String.make 80 'a' in
           let long_b = String.make 80 'b' in
           assert_statement_string ~ctxt "class a{}";
           assert_statement_string ~ctxt "class a extends b{}";
           assert_statement_string ~ctxt "class a<a> extends b{}";
           assert_statement_string ~ctxt "class a extends b<b>{}";
           assert_statement_string ~ctxt ~pretty:true ("class " ^ long_a ^ " {}");
           assert_statement_string ~ctxt ~pretty:true ("class a\n  extends " ^ long_b ^ " {}");
           assert_statement_string ~ctxt "@a class a extends b{}";
           assert_statement_string ~ctxt "@a@b class a extends b{}";
           assert_statement_string ~ctxt "@a()@b class a extends b{}";
           assert_statement_string ~ctxt "@(++a)@b class a extends b{}";
           assert_statement_string ~ctxt "@(a&&b)@b class a extends b{}";
           assert_statement_string ~ctxt "@(()=>{})@b class a extends b{}";
           assert_statement_string ~ctxt ~pretty:true "@a\nclass a extends b {}";
           assert_statement_string ~ctxt ~pretty:true "@a\n@b\nclass a extends b {}";
           assert_statement_string ~ctxt "class a implements b{}";
           assert_statement_string ~ctxt "class a implements b<b>{}";
           assert_statement_string ~ctxt "class a implements b,c{}";
           assert_statement_string ~ctxt "class a implements b<b>,c<c>{}";

           begin
             let ast =
               S.class_declaration
                 ~id:(I.identifier "a")
                 ~super:(E.identifier "b")
                 ~implements:[Ast_builder.Classes.implements (I.identifier "c")]
                 []
             in
             let layout = Js_layout_generator.statement ~opts ast in
             assert_layout
               ~ctxt
               L.(
                 loc
                   (group
                      [
                        loc
                          (group
                             [
                               atom "class";
                               space;
                               id "a";
                               indent
                                 (fused
                                    [
                                      line;
                                      loc (fused [atom "extends"; space; loc (loc (id "b"))]);
                                      line;
                                      loc (fused [atom "implements"; space; loc (id "c")]);
                                    ]);
                               pretty_space;
                               loc (atom "{}");
                             ]);
                      ]))
               layout;
             assert_output ~ctxt "class a extends b implements c{}" layout;
             assert_output ~ctxt ~pretty:true "class a extends b implements c {}" layout
           end;

           begin
             let x35 = String.make 35 'x' in
             let y29 = String.make 29 'y' in
             let c2 = S.class_declaration ~id:(I.identifier x35) ~super:(E.identifier y29) [] in
             let ast = S.block [c2] in
             let layout = Js_layout_generator.statement ~opts ast in
             assert_layout
               ~ctxt
               L.(
                 loc
                   (loc
                      (group
                         [
                           atom "{";
                           indent
                             (fused
                                [
                                  pretty_hardline;
                                  loc
                                    (group
                                       [
                                         loc
                                           (group
                                              [
                                                atom "class";
                                                space;
                                                id "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
                                                indent
                                                  (fused
                                                     [
                                                       line;
                                                       loc
                                                         (fused
                                                            [
                                                              atom "extends";
                                                              space;
                                                              loc
                                                                (loc
                                                                   (id
                                                                      "yyyyyyyyyyyyyyyyyyyyyyyyyyyyy"));
                                                            ]);
                                                     ]);
                                                pretty_space;
                                                loc (atom "{}");
                                              ]);
                                       ]);
                                ]);
                           pretty_hardline;
                           atom "}";
                         ])))
               layout;
             assert_output ~ctxt ("{class " ^ x35 ^ " extends " ^ y29 ^ "{}}") layout;
             assert_output
               ~ctxt
               ~pretty:true
               ("{\n  class " ^ x35 ^ "\n    extends " ^ y29 ^ " {}" ^ "\n}")
               layout
           end;

           assert_statement_string
             ~ctxt
             ~pretty:true
             ("class a\n  extends " ^ long_b ^ "\n  implements c {}");
           assert_statement_string
             ~ctxt
             ~pretty:true
             ("class a\n  extends " ^ long_b ^ "\n  implements " ^ long_b ^ " {}");

           (* TODO: this seems wrong, `c {` should break onto a new line *)
           assert_statement_string
             ~ctxt
             ~pretty:true
             ("class a\n  extends " ^ long_b ^ "\n  implements " ^ long_b ^ ", c {}") );
         ( "class_expressions" >:: fun ctxt ->
           assert_expression_string ~ctxt "class{}";
           assert_expression_string ~ctxt "class a{}";
           assert_expression_string ~ctxt "class a extends b{}" );
         ( "class_methods" >:: fun ctxt ->
           assert_statement_string ~ctxt "class a{b(){}}";
           assert_statement_string ~ctxt ~pretty:true "class a {\n  b() {}\n  static b() {}\n}";
           assert_statement_string
             ~ctxt
             ~pretty:true
             "class a {\n  async a() {}\n  static async a() {}\n}";
           assert_statement_string
             ~ctxt
             ~pretty:true
             "class a {\n  get a() {}\n  set a() {}\n  static get a() {}\n}";
           assert_statement_string ~ctxt ~pretty:true "class a {\n  constructor() {}\n}";
           assert_statement_string ~ctxt "class a{@a a(){}}";
           assert_statement_string ~ctxt "class a{@(()=>{}) a(){}}";
           assert_statement_string ~ctxt "class a{@a@b a(){}}";
           assert_statement_string ~ctxt ~pretty:true "class a {\n  @a\n  a() {}\n}";
           assert_statement_string ~ctxt ~pretty:true "class a {\n  @a\n  @b\n  a() {}\n}";
           assert_statement_string ~ctxt "class a{*b(){}}" );
         ( "class_properties" >:: fun ctxt ->
           assert_statement_string ~ctxt "class a{a;}";
           assert_statement_string ~ctxt "class a{a:a;}";
           assert_statement_string ~ctxt "class a{a;b=c;}";
           assert_statement_string ~ctxt "class a{a;b:b=c;}";
           assert_statement_string ~ctxt "class a{+a;}";
           assert_statement_string ~ctxt "class a{+a:a=a;}";
           assert_statement_string ~ctxt "class a{static a;}";
           assert_statement_string ~ctxt "class a{static +a:a=a;}";
           assert_statement_string
             ~ctxt
             ~pretty:true
             "class a {\n  a;\n  b = c;\n  static b = c;\n}";
           assert_statement_string ~ctxt ~pretty:true "class a {\n  +a: a;\n  b: b = c;\n}" );
         ( "class_private_properties" >:: fun ctxt ->
           assert_statement_string ~ctxt "class a{#a;}";
           assert_statement_string ~ctxt "class a{#a:a;}";
           assert_statement_string ~ctxt "class a{#a;#b=c;}";
           assert_statement_string ~ctxt "class a{#a;#b:b=c;}";
           assert_statement_string ~ctxt "class a{+#a;}";
           assert_statement_string ~ctxt "class a{+#a:a=a;}";
           assert_statement_string ~ctxt "class a{static #a;}";
           assert_statement_string ~ctxt "class a{static +#a:a=a;}";
           assert_statement_string
             ~ctxt
             ~pretty:true
             "class a {\n  #a;\n  #b = c;\n  static #b = c;\n}";
           assert_statement_string ~ctxt ~pretty:true "class a {\n  +#a: a;\n  #b: b = c;\n}" );
         ( "class_declared_fields" >:: fun ctxt ->
           assert_statement_string ~ctxt "class a{declare x;}";
           assert_statement_string ~ctxt "class a{declare x:string;}" );
         ( "forin_statement_declaration" >:: fun ctxt ->
           let mk_layout a b =
             Js_layout_generator.statement
               ~opts
               (S.for_in
                  (S.for_in_declarator [S.variable_declarator a])
                  (E.identifier b)
                  (S.block [S.expression (E.identifier a)]))
           in
           begin
             let layout = mk_layout "a" "b" in
             assert_layout
               ~ctxt
               L.(
                 loc
                   (fused
                      [
                        atom "for";
                        pretty_space;
                        group
                          [
                            atom "(";
                            loc (fused [atom "var"; space; loc (loc (id "a"))]);
                            space;
                            atom "in";
                            space;
                            loc (id "b");
                            atom ")";
                          ];
                        pretty_space;
                        loc
                          (loc
                             (group
                                [
                                  atom "{";
                                  indent
                                    (fused
                                       [
                                         pretty_hardline;
                                         loc
                                           (fused [loc (id "a"); Layout.IfPretty (atom ";", empty)]);
                                       ]);
                                  pretty_hardline;
                                  atom "}";
                                ]));
                      ]))
               layout;
             assert_output ~ctxt "for(var a in b){a}" layout;
             assert_output ~ctxt ~pretty:true ("for (var a in b) {\n" ^ "  a;\n" ^ "}") layout
           end;

           let a80 = String.make 80 'a' in
           let layout = mk_layout a80 "b" in
           assert_output ~ctxt ("for(var " ^ a80 ^ " in b){" ^ a80 ^ "}") layout;
           assert_output
             ~ctxt
             ~pretty:true
             ("for (var " ^ a80 ^ " in b) {\n" ^ "  " ^ a80 ^ ";\n" ^ "}")
             layout );
         ( "forin_statement_pattern_identifier" >:: fun ctxt ->
           let mk_layout a b =
             Js_layout_generator.statement
               ~opts
               (S.for_in (S.for_in_pattern (Patterns.identifier a)) (E.identifier b) (S.block []))
           in
           begin
             let layout = mk_layout "a" "b" in
             assert_layout
               ~ctxt
               L.(
                 loc
                   (fused
                      [
                        atom "for";
                        pretty_space;
                        group
                          [atom "("; loc (id "a"); space; atom "in"; space; loc (id "b"); atom ")"];
                        pretty_space;
                        loc (loc (atom "{}"));
                      ]))
               layout;
             assert_output ~ctxt "for(a in b){}" layout;
             assert_output ~ctxt ~pretty:true "for (a in b) {}" layout
           end;

           let a80 = String.make 80 'a' in
           let layout = mk_layout a80 "b" in
           assert_output ~ctxt ("for(" ^ a80 ^ " in b){}") layout;
           assert_output ~ctxt ~pretty:true ("for (" ^ a80 ^ " in b) {}") layout );
         ( "forin_statement_without_block" >:: fun ctxt ->
           assert_statement_string ~ctxt "for(a in b)x;";
           assert_statement_string ~ctxt "{for(a in b)x}" );
         ( "forin_empty_body" >:: fun ctxt ->
           let layout =
             Js_layout_generator.statement
               ~opts
               (S.for_in
                  (S.for_in_pattern (Patterns.identifier "a"))
                  (E.identifier "b")
                  (S.empty ()))
           in
           assert_output ~ctxt "for(a in b);" layout;
           assert_output ~ctxt ~pretty:true "for (a in b);" layout );
         ( "forof_statement_declaration" >:: fun ctxt ->
           let mk_layout a b =
             Js_layout_generator.statement
               ~opts
               (S.for_of
                  (S.for_of_declarator [S.variable_declarator a])
                  (E.identifier b)
                  (S.block [S.expression (E.identifier a)]))
           in
           begin
             let layout = mk_layout "a" "b" in
             assert_layout
               ~ctxt
               L.(
                 loc
                   (fused
                      [
                        atom "for";
                        pretty_space;
                        group
                          [
                            atom "(";
                            loc (fused [atom "var"; space; loc (loc (id "a"))]);
                            space;
                            atom "of";
                            space;
                            loc (id "b");
                            atom ")";
                          ];
                        pretty_space;
                        loc
                          (loc
                             (group
                                [
                                  atom "{";
                                  indent
                                    (fused
                                       [
                                         pretty_hardline;
                                         loc
                                           (fused [loc (id "a"); Layout.IfPretty (atom ";", empty)]);
                                       ]);
                                  pretty_hardline;
                                  atom "}";
                                ]));
                      ]))
               layout;
             assert_output ~ctxt "for(var a of b){a}" layout;
             assert_output ~ctxt ~pretty:true ("for (var a of b) {\n" ^ "  a;\n" ^ "}") layout
           end;

           let a80 = String.make 80 'a' in
           let layout = mk_layout a80 "b" in
           assert_output ~ctxt ("for(var " ^ a80 ^ " of b){" ^ a80 ^ "}") layout;
           assert_output
             ~ctxt
             ~pretty:true
             ("for (var " ^ a80 ^ " of b) {\n" ^ "  " ^ a80 ^ ";\n" ^ "}")
             layout );
         ( "forof_statement_pattern_identifier" >:: fun ctxt ->
           let mk_layout a b =
             Js_layout_generator.statement
               ~opts
               (S.for_of (S.for_of_pattern (Patterns.identifier a)) (E.identifier b) (S.block []))
           in
           begin
             let layout = mk_layout "a" "b" in
             assert_layout
               ~ctxt
               L.(
                 loc
                   (fused
                      [
                        atom "for";
                        pretty_space;
                        group
                          [atom "("; loc (id "a"); space; atom "of"; space; loc (id "b"); atom ")"];
                        pretty_space;
                        loc (loc (atom "{}"));
                      ]))
               layout;
             assert_output ~ctxt "for(a of b){}" layout;
             assert_output ~ctxt ~pretty:true "for (a of b) {}" layout
           end;

           let a80 = String.make 80 'a' in
           let layout = mk_layout a80 "b" in
           assert_output ~ctxt ("for(" ^ a80 ^ " of b){}") layout;
           assert_output ~ctxt ~pretty:true ("for (" ^ a80 ^ " of b) {}") layout );
         ( "forof_statement_async" >:: fun ctxt ->
           assert_statement_string ~ctxt "async function f(){for await(let x of y){}}" );
         ( "forof_statement_without_block" >:: fun ctxt ->
           assert_statement_string ~ctxt "for(a of b)x;";
           assert_statement_string ~ctxt "{for(a of b)x}" );
         ( "forof_empty_body" >:: fun ctxt ->
           let layout =
             Js_layout_generator.statement
               ~opts
               (S.for_of
                  (S.for_of_pattern (Patterns.identifier "a"))
                  (E.identifier "b")
                  (S.empty ()))
           in
           assert_output ~ctxt "for(a of b);" layout;
           assert_output ~ctxt ~pretty:true "for (a of b);" layout );
         ( "yield_expressions" >:: fun ctxt ->
           assert_expression_string ~ctxt "function* f(){yield}";
           assert_expression_string ~ctxt "function* f(){yield a}";
           assert_expression_string ~ctxt "function* f(){yield* a}" );
         ( "meta_property_expressions" >:: fun ctxt ->
           assert_statement_string ~ctxt "function F(){new.target}";
           assert_statement_string ~ctxt "function F(){new.target.name}" );
         ( "tagged_template_expressions" >:: fun ctxt ->
           assert_expression_string ~ctxt "a``";
           assert_expression_string ~ctxt "b.c``";
           assert_expression_string ~ctxt "(()=>{})``";
           assert_expression_string ~ctxt "(b=c)``";
           assert_expression_string ~ctxt "(b+c)``";
           assert_expression_string ~ctxt "b()``";
           assert_expression_string ~ctxt "(class{})``";
           assert_expression_string ~ctxt "(b?c:d)``";
           assert_expression_string ~ctxt "(function(){})``";
           assert_expression_string ~ctxt "(b||c)``";
           assert_expression_string ~ctxt "(new B())``";
           assert_expression_string ~ctxt "({})``";
           assert_expression_string ~ctxt "(b,c)``";
           assert_expression_string ~ctxt "````";
           assert_expression_string ~ctxt "(void b)``";
           assert_expression_string ~ctxt "(++b)``" );
         ( "template_expressions" >:: fun ctxt ->
           assert_expression_string ~ctxt "``";
           assert_expression_string ~ctxt "`${a}`";
           assert_expression_string ~ctxt "`a${b}c`";
           assert_expression_string ~ctxt "`a${b}c${d}e`";
           assert_expression_string ~ctxt "`\\``" );
         ( "import_expressions" >:: fun ctxt ->
           assert_expression_string ~ctxt {|import("a")|};
           assert_expression_string ~ctxt "import(a)" );
         ( "export_declaration_statement" >:: fun ctxt ->
           assert_statement_string ~ctxt "export{};";
           assert_statement_string ~ctxt "export{}from\"a\";";
           assert_statement_string ~ctxt "export{a}from\"a\";";
           assert_statement_string ~ctxt "export{a,b as c};";
           assert_statement_string ~ctxt "export*from\"a\";";
           assert_statement_string ~ctxt "export*as a from\"a\";";
           assert_statement_string ~ctxt "export type{};";
           assert_statement_string ~ctxt "export type{a};";
           assert_statement_string ~ctxt "export type a=b;";
           assert_statement_string ~ctxt "export let a;";
           assert_statement_string ~ctxt "export const a=b;";
           assert_statement_string ~ctxt "export interface a{a():b}";
           assert_statement_string ~ctxt ~pretty:true "export {};";
           assert_statement_string ~ctxt ~pretty:true "export { a } from \"a\";";
           assert_statement_string
             ~ctxt
             ~pretty:true
             ~opts:(no_bracket_spacing opts)
             "export {a} from \"a\";";
           assert_statement_string ~ctxt ~pretty:true "export * from \"a\";";
           assert_statement_string ~ctxt ~pretty:true "export * as a from \"a\";";
           assert_statement_string ~ctxt ~pretty:true "export type { a };";
           assert_statement_string
             ~ctxt
             ~pretty:true
             ~opts:(no_bracket_spacing opts)
             "export type {a};";
           assert_statement_string
             ~ctxt
             ~pretty:true
             ("export {\n  a,\n  b as " ^ String.make 80 'c' ^ ",\n} from \"a\";");
           assert_statement_string
             ~ctxt
             ~pretty:true
             ("export * as " ^ String.make 80 'a' ^ " from \"a\";");
           assert_statement_string ~ctxt ~pretty:true "export opaque type a = b;" )
         (* TODO: Flow does not parse this but should
            assert_statement_string ~ctxt "export a,{b}from'a';";
            assert_statement_string ~ctxt "export*as foo,{bar}from'a';"; *);
         ( "default_export_declaration_statement" >:: fun ctxt ->
           assert_statement_string ~ctxt "export default a;";
           assert_statement_string ~ctxt "export default a=b;";
           assert_statement_string ~ctxt "export default function(){}";
           assert_statement_string ~ctxt "export default class{}" );
         ( "type_alias_statement" >:: fun ctxt ->
           assert_statement_string ~ctxt "type a=a;";
           assert_statement_string ~ctxt "type a<a>=a;";
           assert_statement_string ~ctxt ~pretty:true "type a = a;" );
         ( "opaque_type_alias_statement" >:: fun ctxt ->
           assert_statement_string ~ctxt "opaque type a=a;";
           assert_statement_string ~ctxt "opaque type a:b=a;";
           assert_statement_string ~ctxt "opaque type a<a>=a;";
           assert_statement_string ~ctxt "opaque type a<a>:b<a>=a;";
           assert_statement_string ~ctxt "opaque type a<a>:b<a>=c<a>;";
           assert_statement_string ~ctxt ~pretty:true "opaque type a = a;";
           assert_statement_string ~ctxt ~pretty:true "opaque type a: b = a;" );
         ( "declare_opaque_type_alias_statement" >:: fun ctxt ->
           assert_statement_string ~ctxt "declare opaque type a;";
           assert_statement_string ~ctxt "declare opaque type a:b;";
           assert_statement_string ~ctxt ~pretty:true "declare opaque type a: b;";
           assert_statement_string ~ctxt "declare export opaque type a;";
           assert_statement_string ~ctxt "declare export opaque type a:b;";
           assert_statement_string ~ctxt ~pretty:true "declare export opaque type a: b;" );
         ( "type_cast_expression" >:: fun ctxt ->
           let layout =
             Js_layout_generator.expression ~opts (E.typecast (E.identifier "a") Types.mixed)
           in
           assert_layout
             ~ctxt
             L.(
               loc
                 (group
                    [
                      atom "(";
                      loc (id "a");
                      loc (fused [atom ":"; pretty_space; loc (atom "mixed")]);
                      atom ")";
                    ]))
             layout;
           assert_output ~ctxt "(a:mixed)" layout;
           assert_output ~ctxt ~pretty:true "(a: mixed)" layout;

           let a80 = String.make 80 'a' in
           let layout =
             Js_layout_generator.expression ~opts (E.typecast (E.identifier a80) Types.mixed)
           in
           assert_output ~ctxt ("(" ^ a80 ^ ":mixed)") layout;
           assert_output ~ctxt ~pretty:true ("(" ^ a80 ^ ": mixed)") layout;
           assert_statement_string ~ctxt ~pretty:true "var a = (b: mixed);";
           (* Arrow function with type params is wrapped in parens *)
           assert_expression_string ~ctxt "((<A>()=>B):C)" );
         ( "type_parameter" >:: fun ctxt ->
           assert_statement_string ~ctxt "type a<a>=a;";
           assert_statement_string ~ctxt "type a<a,b>=a;";
           assert_statement_string ~ctxt "type a<+a>=a;";
           assert_statement_string ~ctxt "type a<c=a>=a;";
           assert_statement_string ~ctxt "type a<a:b>=a;";
           assert_statement_string ~ctxt "type a<a:b=c>=a;";
           assert_statement_string ~ctxt "type a<a,+a:b=c>=a;";
           assert_statement_string ~ctxt ~pretty:true "type a<a, +a: b = c> = a;";
           assert_statement_string
             ~ctxt
             ~pretty:true
             ("type a<\n  a,\n  +a: b = " ^ String.make 80 'c' ^ ",\n> = a;");
           assert_statement_string
             ~ctxt
             ~pretty:true
             ("type a<\n  a,\n  b,\n> = " ^ String.make 80 'a' ^ ";") );
         ( "type" >:: fun ctxt ->
           assert_statement_string ~ctxt "type a=any;";
           assert_statement_string ~ctxt "type a=mixed;";
           assert_statement_string ~ctxt "type a=empty;";
           assert_statement_string ~ctxt "type a=void;";
           assert_statement_string ~ctxt "type a=null;";
           assert_statement_string ~ctxt "type a=number;";
           assert_statement_string ~ctxt "type a=string;";
           assert_statement_string ~ctxt "type a=boolean;";
           assert_statement_string ~ctxt "type a=a;";
           assert_statement_string ~ctxt "type a=?a;";
           assert_statement_string ~ctxt ~pretty:true "type a = ?a;";
           assert_statement_string ~ctxt "type a=Array<a>;";
           assert_statement_string ~ctxt "type a=a.b;";
           assert_statement_string ~ctxt "type a=a.b.c;";
           assert_statement_string ~ctxt "type a=a<b>;";
           assert_statement_string ~ctxt "type a=a.b<c,d>;";
           assert_statement_string ~ctxt ~pretty:true "type a = a.b<c, d>;";
           assert_statement_string
             ~ctxt
             ~pretty:true
             ("type a = a.b<\n  c,\n  " ^ String.make 80 'd' ^ ",\n>;");
           assert_statement_string ~ctxt "type a=typeof a;";
           assert_statement_string ~ctxt "type a=[a,b];";
           assert_statement_string ~ctxt ~pretty:true "type a = [a, b];";
           assert_statement_string
             ~ctxt
             ~pretty:true
             ("type a = [\n  a,\n  " ^ String.make 80 'b' ^ ",\n];");
           assert_statement_string ~ctxt "type a=*;";
           assert_statement_string ~ctxt "type a='';";
           assert_statement_string ~ctxt "type a=1;";
           assert_statement_string ~ctxt "type a=true;" );
         ( "type_function" >:: fun ctxt ->
           assert_statement_string ~ctxt "type a=()=>c;";
           assert_statement_string ~ctxt "type a=(a:b)=>c;";
           assert_statement_string ~ctxt "type a=(a:b,c:d)=>c;";
           assert_statement_string ~ctxt "type a=(a:b,c?:d)=>c;";
           assert_statement_string ~ctxt "type a=(a,b)=>c;";
           assert_statement_string ~ctxt "type a=<a>()=>c;";
           assert_statement_string ~ctxt "type a=(...a)=>c;";
           assert_statement_string ~ctxt ~pretty:true "type a = () => c;";
           assert_statement_string ~ctxt ~pretty:true "type a = (a) => c;";
           assert_statement_string ~ctxt ~pretty:true "type a = (a: b) => c;";
           assert_statement_string ~ctxt ~pretty:true "type a = (a?: b) => c;";
           assert_statement_string ~ctxt ~pretty:true "type a = (a?: b, c) => c;";
           assert_statement_string ~ctxt ~pretty:true "type a = <a>(a?: b, c) => c;";
           assert_statement_string
             ~ctxt
             ~pretty:true
             ("type a = <a>(\n  a?: b,\n  " ^ String.make 80 'c' ^ "\n) => c;");
           let a30 = String.make 30 'a' in
           let b30 = String.make 30 'b' in
           assert_expression_string
             ~ctxt
             ("(" ^ a30 ^ ":" ^ a30 ^ ",..." ^ b30 ^ ":" ^ b30 ^ "):c=>{}") );
         ( "type_object" >:: fun ctxt ->
           assert_statement_string ~ctxt "type a={};";
           assert_statement_string ~ctxt "type a={...};";
           assert_statement_string ~ctxt "type a={||};";
           assert_statement_string ~ctxt "type a={a:b};";
           assert_statement_string ~ctxt "type a={|a:b|};";
           assert_statement_string ~ctxt "type a={a:b,...};";
           assert_statement_string ~ctxt "type a={+a:b};";
           assert_statement_string ~ctxt "type a={a?:b};";
           assert_statement_string ~ctxt "type a={a:?b};";
           assert_statement_string ~ctxt "type a={a?:?b};";
           assert_statement_string ~ctxt "type a={\"a\":b};";
           assert_statement_string ~ctxt "type a={a:b};";
           assert_statement_string ~ctxt "type a={a:b,c:d};";
           assert_statement_string ~ctxt "type a={...a};";
           assert_statement_string ~ctxt "type a={a:b,...a};";
           assert_statement_string ~ctxt ~pretty:true "type a = { ... };";
           assert_statement_string ~ctxt ~pretty:true "type a = { a: b };";
           assert_statement_string ~ctxt ~pretty:true "type a = { a: b, ... };";
           assert_statement_string
             ~ctxt
             ~pretty:true
             ~opts:(no_bracket_spacing opts)
             "type a = {a: b};";
           assert_statement_string ~ctxt ~pretty:true "type a = { a: b, c: d };";
           assert_statement_string
             ~ctxt
             ~pretty:true
             ~opts:(no_bracket_spacing opts)
             "type a = {a: b, c: d};";
           assert_statement_string
             ~ctxt
             ~pretty:true
             ("type a = {\n  a: b,\n  c: " ^ String.make 80 'd' ^ ",\n};");
           assert_statement_string ~ctxt "type a={a():b};";
           assert_statement_string ~ctxt "type a={get a():b};";
           assert_statement_string ~ctxt "type a={set a():b};";
           assert_statement_string ~ctxt ~pretty:true "type a = { set a(): b };";
           assert_statement_string ~ctxt "type a={a?:()=>a};";
           assert_statement_string ~ctxt "type a={+a:()=>a};";
           assert_statement_string ~ctxt "type a={():a};";
           assert_statement_string ~ctxt "type a={[b]:a};";
           assert_statement_string ~ctxt "type a={[a:b]:a};";
           assert_statement_string ~ctxt "type a={+[a:b]:a};";
           assert_statement_string ~ctxt ~pretty:true "type a = { +[a: b]: a };";
           assert_statement_string ~ctxt "type a={a:b,+[a:b]:a,():a,c():b};";
           (* TODO: the RHS should be indented *)
           assert_statement_string ~ctxt ~pretty:true "type T =\n/* foo */\n{ ... };";
           (* TODO: the RHS should be indented *)
           assert_statement_string ~ctxt ~pretty:true "type T =\n/* foo */\n{\n  // bar\n  ...\n};";
           assert_statement_string ~ctxt ~pretty:true "type T = {\n  // foo\n  ...\n};";
           assert_statement_string ~ctxt ~pretty:true "type T = {\n  /* foo */\n  ...\n};" );
         ( "type_union_or_intersection" >:: fun ctxt ->
           assert_statement_string ~ctxt "type a=a|b;";
           assert_statement_string ~ctxt "type a=a|b|c;";
           assert_statement_string ~ctxt "type a=?(a|b);";
           assert_statement_string ~ctxt "type a=a&b;";
           assert_statement_string ~ctxt "type a=a&b&c;";
           assert_statement_string ~ctxt "type a=?(a&b);";
           assert_statement_string ~ctxt "type a=a|(b&c)|d;";
           assert_statement_string ~ctxt "type a=(a|b)&c;";
           assert_statement_string ~ctxt "type a=(a&b)|c;";
           assert_statement_string ~ctxt "type a=a|(b|c);";
           assert_statement_string ~ctxt "type a=(a&b)|c;";
           assert_statement_string ~ctxt "type a=a|(()=>b)|c;";
           assert_statement_string ~ctxt ~pretty:true "type a = a | b;";
           assert_statement_string ~ctxt ~pretty:true "type a = a | b | c;";
           assert_statement_string ~ctxt ~pretty:true "type a = a & b & c;";
           assert_statement_string
             ~ctxt
             ~pretty:true
             ("type a = \n  | a\n  | b\n  | " ^ String.make 80 'c' ^ ";") );
         ( "interface_declaration_statements" >:: fun ctxt ->
           assert_statement_string ~ctxt "interface a{}";
           assert_statement_string ~ctxt "interface a extends b{}";
           assert_statement_string ~ctxt "interface a<a> extends b{}";
           assert_statement_string ~ctxt "interface a extends b,c{}";
           assert_statement_string ~ctxt ~pretty:true "interface a {}";
           assert_statement_string ~ctxt ~pretty:true "interface a extends b, c {}";
           assert_statement_string
             ~ctxt
             ~pretty:true
             ("interface a {\n  a: b,\n  d(): " ^ String.make 80 'c' ^ ",\n}") );
         ( "declare_class_statements" >:: fun ctxt ->
           assert_statement_string ~ctxt "declare class a{}";
           assert_statement_string ~ctxt "declare class a extends b{}";
           assert_statement_string ~ctxt "declare class a implements b{}";
           assert_statement_string ~ctxt "declare class a extends b mixins c implements d{}";
           assert_statement_string ~ctxt "declare class a extends b implements c{}";
           assert_statement_string
             ~ctxt
             ~pretty:true
             ("declare class a {\n  static a: b,\n  static d(): " ^ String.make 80 'c' ^ ",\n}") );
         ( "declare_function_statements" >:: fun ctxt ->
           assert_statement_string ~ctxt "declare function a():b;";
           assert_statement_string ~ctxt ~pretty:true "declare function a(): b;";
           assert_statement_string ~ctxt "declare function f():a%checks;";
           assert_statement_string ~ctxt "declare function f(a:b):a%checks(!a);";
           assert_statement_string ~ctxt ~pretty:true "declare function f(a: b): a %checks(!a);" );
         ( "declare_var_statements" >:: fun ctxt ->
           assert_statement_string ~ctxt "declare var a;";
           assert_statement_string ~ctxt "declare var a:b;" );
         ( "declare_module_exports_statements" >:: fun ctxt ->
           assert_statement_string ~ctxt "declare module.exports:a;" );
         ( "declare_module_statements" >:: fun ctxt ->
           assert_statement_string ~ctxt "declare module a{}";
           assert_statement_string ~ctxt "declare module \"a\"{}";
           assert_statement_string ~ctxt ~pretty:true "declare module \"a\" {}" );
         ( "declare_export_declaration_statements" >:: fun ctxt ->
           assert_statement_string ~ctxt "declare export default a;";
           assert_statement_string ~ctxt "declare export var a;";
           assert_statement_string ~ctxt "declare export function a():a;";
           assert_statement_string ~ctxt "declare export default function a():a;";
           assert_statement_string ~ctxt "declare export class a{}";
           assert_statement_string ~ctxt "declare export default class a{}";
           assert_statement_string ~ctxt "declare export{}";
           assert_statement_string ~ctxt "declare export{a,b}";
           assert_statement_string ~ctxt "declare export{a,b}from\"a\"";
           assert_statement_string ~ctxt "declare export*from\"a\"" );
         ( "regexp" >:: fun ctxt ->
           (* flags should be sorted *)
           let regexp = expression_of_string "/foo/ymg" in
           assert_expression ~ctxt "/foo/gmy" regexp );
         ( "string_literal_quotes" >:: fun ctxt ->
           assert_expression ~ctxt {|"'''"|} (expression_of_string {|"'''"|});
           assert_expression ~ctxt {|'"'|} (expression_of_string {|"\""|});
           assert_expression ~ctxt {|"''"|} (expression_of_string {|'\'\''|});
           assert_expression ~ctxt {|"''\""|} (expression_of_string {|"''\""|});
           assert_expression ~ctxt {|'""\''|} (expression_of_string {|'""\''|}) );
         ( "switch" >:: fun ctxt ->
           let case1_loc =
             Loc.{ none with start = { line = 1; column = 1 }; _end = { line = 2; column = 3 } }
           in
           let case2_loc =
             Loc.{ none with start = { line = 4; column = 1 }; _end = { line = 5; column = 3 } }
           in
           let layout =
             Js_layout_generator.statement
               ~opts
               (S.switch
                  (E.identifier "x")
                  [
                    S.switch_case
                      ~loc:case1_loc
                      ~test:(E.literal (Literals.string "a"))
                      [S.expression (E.increment ~prefix:false (E.identifier "x")); S.break ()];
                    S.switch_case
                      ~loc:case2_loc
                      ~test:(E.literal (Literals.string "b"))
                      [S.expression (E.increment ~prefix:false (E.identifier "x")); S.break ()];
                  ])
           in
           assert_layout
             ~ctxt
             L.(
               loc
                 (fused
                    [
                      group
                        [
                          atom "switch";
                          pretty_space;
                          atom "(";
                          indent (fused [softline; loc (id "x")]);
                          softline;
                          atom ")";
                        ];
                      pretty_space;
                      atom "{";
                      indent
                        (fused
                           [
                             pretty_hardline;
                             loc
                               ~loc:case1_loc
                               (fused
                                  [
                                    atom "case";
                                    pretty_space;
                                    loc (fused [atom "\""; atom "a"; atom "\""]);
                                    atom ":";
                                    indent
                                      (fused
                                         [
                                           pretty_hardline;
                                           loc
                                             (fused
                                                [loc (fused [loc (id "x"); atom "++"]); atom ";"]);
                                           pretty_hardline;
                                           loc (fused [atom "break"; atom ";"]);
                                         ]);
                                  ]);
                             pretty_hardline;
                             pretty_hardline;
                             loc
                               ~loc:case2_loc
                               (fused
                                  [
                                    atom "case";
                                    pretty_space;
                                    loc (fused [atom "\""; atom "b"; atom "\""]);
                                    atom ":";
                                    indent
                                      (fused
                                         [
                                           pretty_hardline;
                                           loc
                                             (fused
                                                [loc (fused [loc (id "x"); atom "++"]); atom ";"]);
                                           pretty_hardline;
                                           loc
                                             (fused
                                                [atom "break"; Layout.IfPretty (atom ";", empty)]);
                                         ]);
                                  ]);
                           ]);
                      pretty_hardline;
                      atom "}";
                    ]))
             layout;
           assert_output ~ctxt "switch(x){case\"a\":x++;break;case\"b\":x++;break}" layout;
           assert_output
             ~ctxt
             ~pretty:true
             ("switch (x) {\n"
             ^ "  case \"a\":\n"
             ^ "    x++;\n"
             ^ "    break;\n"
             ^ "  \n"
             (* TODO: fix trailing whitespace *)
             ^ "  case \"b\":\n"
             ^ "    x++;\n"
             ^ "    break;\n"
             ^ "}")
             layout );
         ( "switch_conditional_wrap" >:: fun ctxt ->
           (* the conditional should wrap, because `switch (xxxxx...xxx) {` does't fit on one line *)
           let len = 80 - String.length "switch () {" + 1 in
           assert_statement_string
             ~ctxt
             ~pretty:true
             (* TODO: fix trailing whitespace *)
             (Printf.sprintf "switch (\n  %s\n) {\n  \n}" (String.make len 'x')) );
         ( "switch_case_space" >:: fun ctxt ->
           let assert_no_space ~ctxt expr =
             let ret = statement_of_string ("switch(x){case " ^ expr ^ ":break}") in
             assert_statement ~ctxt ("switch(x){case" ^ expr ^ ":break}") ret
           in
           assert_no_space ~ctxt {|"foo"|};
           assert_no_space ~ctxt {|{foo:"bar"}|};
           assert_no_space ~ctxt {|[foo]|};
           assert_no_space ~ctxt {|!foo|};
           assert_no_space ~ctxt {|+foo|};
           assert_no_space ~ctxt {|-foo|};
           assert_no_space ~ctxt {|~foo|};

           let ret = statement_of_string "switch(x){case (foo):break}" in
           assert_statement ~ctxt "switch(x){case foo:break}" ret;

           let ret = statement_of_string "switch(x){case 123:break}" in
           assert_statement ~ctxt "switch(x){case 123:break}" ret );
         ( "switch_case_empty" >:: fun ctxt ->
           let layout =
             Js_layout_generator.statement
               ~opts
               (S.switch
                  (E.identifier "x")
                  [S.switch_case ~test:(E.literal (Literals.string "a")) [S.empty ()]])
           in
           assert_output ~ctxt "switch(x){case\"a\":;}" layout;
           assert_output
             ~ctxt
             ~pretty:true
             ("switch (x) {\n" ^ "  case \"a\":\n" ^ "    ;\n" ^ "}")
             layout );
         ( "throw_space" >:: fun ctxt ->
           let assert_no_space ~ctxt expr =
             let ret = statement_of_string ("throw " ^ expr ^ ";") in
             assert_statement ~ctxt ("throw" ^ expr ^ ";") ret
           in
           assert_no_space ~ctxt {|"foo"|};
           assert_no_space ~ctxt {|{foo:"bar"}|};
           assert_no_space ~ctxt {|[foo]|};
           assert_no_space ~ctxt {|!foo|};
           assert_no_space ~ctxt {|+foo|};
           assert_no_space ~ctxt {|-foo|};
           assert_no_space ~ctxt {|~foo|};

           assert_statement_string ~ctxt "throw foo;";
           assert_statement ~ctxt "throw foo;" (statement_of_string "throw (foo);");
           assert_statement_string ~ctxt "throw new Error();" );
         ( "string_literal" >:: fun ctxt ->
           let ast = E.literal (Literals.string "a") in
           let layout = Js_layout_generator.expression ~opts ast in
           assert_layout ~ctxt L.(loc (fused [atom "\""; atom "a"; atom "\""])) layout;
           assert_output ~ctxt {|"a"|} layout;
           assert_output ~ctxt ~pretty:true {|"a"|} layout );
         ( "unicode_string_literal" >:: fun ctxt ->
           (* escaped using Unicode codepoint *)
           let ast = expression_of_string {|"\u{1F4A9}"|} in
           assert_expression ~ctxt {|"\ud83d\udca9"|} ast;

           (* not escaped when formatting is preserved *)
           assert_expression ~ctxt ~opts:preserve_formatting_opts {|"\u{1F4A9}"|} ast;

           (* escaped using UTF-16 (hex get lowercased) *)
           let ast = expression_of_string {|"\uD83D\uDCA9"|} in
           assert_expression ~ctxt {|"\ud83d\udca9"|} ast;

           (* literal emoji *)
           let ast = expression_of_string "\"\xF0\x9F\x92\xA9\"" in
           assert_expression ~ctxt {|"\ud83d\udca9"|} ast;

           (* zero followed by ASCII number *)
           let ast = expression_of_string "\"\x00\x31\"" in
           assert_expression ~ctxt {|"\x001"|} ast;

           (* not `\01`! *)
           let ast = expression_of_string "\"\x00\x39\"" in
           assert_expression ~ctxt {|"\x009"|} ast;

           (* not `\09`! *)

           (* unprintable ascii, escaped *)
           let ast = expression_of_string {|"\x07"|} in
           assert_expression ~ctxt {|"\x07"|} ast;
           let ast = expression_of_string {|"\x11"|} in
           assert_expression ~ctxt {|"\x11"|} ast;

           (* unprintable ascii, literal *)
           let ast = expression_of_string "\"\x11\"" in
           assert_expression ~ctxt {|"\x11"|} ast;

           (* special escapes *)
           let ast = expression_of_string {|"\x09"|} in
           assert_expression ~ctxt {|"\t"|} ast;
           let ast = expression_of_string {|"\\"|} in
           assert_expression ~ctxt {|"\\"|} ast );
         ( "numbers" >:: fun ctxt ->
           assert_expression ~ctxt "100" (expression_of_string "1e2");
           assert_expression ~ctxt "1e3" (expression_of_string "1000");
           assert_expression ~ctxt "2592e6" (expression_of_string "2.592e+09") );
         ( "sequence_long" >:: fun ctxt ->
           let x80 = String.make 80 'x' in
           let layout =
             Js_layout_generator.expression ~opts (E.sequence [E.identifier x80; E.identifier x80])
           in
           assert_output ~ctxt (x80 ^ "," ^ x80) layout;
           assert_output ~ctxt ~pretty:true (x80 ^ ",\n" ^ x80) layout );
         ( "with_statement_with_empty_body" >:: fun ctxt ->
           let layout =
             Js_layout_generator.statement ~opts (S.with_ (E.identifier "x") (S.empty ()))
           in
           assert_output ~ctxt "with(x);" layout;
           assert_output ~ctxt ~pretty:true "with (x);" layout );
         ( "indexed_access" >:: fun ctxt ->
           let open Flow_ast.Type in
           let layout =
             Js_layout_generator.type_
               ~opts
               ( Loc.none,
                 IndexedAccess
                   {
                     IndexedAccess._object = Ast_builder.Types.unqualified_generic "T";
                     index = Ast_builder.Types.unqualified_generic "K";
                     comments = None;
                   } )
           in
           assert_output ~ctxt "T[K]" layout );
         ( "optional_indexed_access" >:: fun ctxt ->
           let open Flow_ast.Type in
           let layout =
             Js_layout_generator.type_
               ~opts
               ( Loc.none,
                 OptionalIndexedAccess
                   {
                     OptionalIndexedAccess.indexed_access =
                       {
                         IndexedAccess._object = Ast_builder.Types.unqualified_generic "T";
                         index = Ast_builder.Types.unqualified_generic "K";
                         comments = None;
                       };
                     optional = true;
                   } )
           in
           assert_output ~ctxt "T?.[K]" layout );
         ( "enum_of_boolean" >:: fun ctxt ->
           S.EnumDeclarations.(
             let layout ~explicit_type =
               Js_layout_generator.statement ~opts
               @@ S.enum_declaration
                    (I.identifier "E")
                    (boolean_body
                       ~explicit_type
                       [
                         initialized_member (I.identifier "A") (boolean_literal true);
                         initialized_member (I.identifier "B") (boolean_literal false);
                       ])
             in
             assert_output ~ctxt "enum E{A=true,B=false,}" (layout ~explicit_type:false);
             let pretty_output = "enum E {\n" ^ "  A = true,\n" ^ "  B = false,\n" ^ "}" in
             assert_output ~ctxt ~pretty:true pretty_output (layout ~explicit_type:false);

             assert_output ~ctxt "enum E of boolean{A=true,B=false,}" (layout ~explicit_type:true);
             let explicit_type_pretty_output =
               "enum E of boolean {\n" ^ "  A = true,\n" ^ "  B = false,\n" ^ "}"
             in
             assert_output
               ~ctxt
               ~pretty:true
               explicit_type_pretty_output
               (layout ~explicit_type:true)) );
         ( "enum_of_number" >:: fun ctxt ->
           S.EnumDeclarations.(
             let layout ~explicit_type =
               Js_layout_generator.statement ~opts
               @@ S.enum_declaration
                    (I.identifier "E")
                    (number_body
                       ~explicit_type
                       [
                         initialized_member (I.identifier "A") (number_literal 1.0 "1");
                         initialized_member (I.identifier "B") (number_literal 2.0 "2");
                       ])
             in
             assert_output ~ctxt "enum E{A=1,B=2,}" (layout ~explicit_type:false);
             let pretty_output = "enum E {\n" ^ "  A = 1,\n" ^ "  B = 2,\n" ^ "}" in
             assert_output ~ctxt ~pretty:true pretty_output (layout ~explicit_type:false);

             assert_output ~ctxt "enum E of number{A=1,B=2,}" (layout ~explicit_type:true);
             let explicit_type_pretty_output =
               "enum E of number {\n" ^ "  A = 1,\n" ^ "  B = 2,\n" ^ "}"
             in
             assert_output
               ~ctxt
               ~pretty:true
               explicit_type_pretty_output
               (layout ~explicit_type:true)) );
         ( "enum_of_string_initialized" >:: fun ctxt ->
           S.EnumDeclarations.(
             let layout ~explicit_type =
               Js_layout_generator.statement ~opts
               @@ S.enum_declaration
                    (I.identifier "E")
                    (string_initialized_body
                       ~explicit_type
                       [
                         initialized_member (I.identifier "A") (string_literal "a");
                         initialized_member (I.identifier "B") (string_literal "b");
                       ])
             in
             assert_output ~ctxt "enum E{A=\"a\",B=\"b\",}" (layout ~explicit_type:false);
             let pretty_output = "enum E {\n" ^ "  A = \"a\",\n" ^ "  B = \"b\",\n" ^ "}" in
             assert_output ~ctxt ~pretty:true pretty_output (layout ~explicit_type:false);

             assert_output ~ctxt "enum E of string{A=\"a\",B=\"b\",}" (layout ~explicit_type:true);
             let explicit_type_pretty_output =
               "enum E of string {\n" ^ "  A = \"a\",\n" ^ "  B = \"b\",\n" ^ "}"
             in
             assert_output
               ~ctxt
               ~pretty:true
               explicit_type_pretty_output
               (layout ~explicit_type:true)) );
         ( "enum_of_string_defaulted" >:: fun ctxt ->
           S.EnumDeclarations.(
             let layout ~explicit_type =
               Js_layout_generator.statement ~opts
               @@ S.enum_declaration
                    (I.identifier "E")
                    (string_defaulted_body
                       ~explicit_type
                       [defaulted_member (I.identifier "A"); defaulted_member (I.identifier "B")])
             in
             assert_output ~ctxt "enum E{A,B,}" (layout ~explicit_type:false);
             let pretty_output = "enum E {\n" ^ "  A,\n" ^ "  B,\n" ^ "}" in
             assert_output ~ctxt ~pretty:true pretty_output (layout ~explicit_type:false);

             assert_output ~ctxt "enum E of string{A,B,}" (layout ~explicit_type:true);
             let explicit_type_pretty_output = "enum E of string {\n" ^ "  A,\n" ^ "  B,\n" ^ "}" in
             assert_output
               ~ctxt
               ~pretty:true
               explicit_type_pretty_output
               (layout ~explicit_type:true)) );
         ( "enum_of_symbol" >:: fun ctxt ->
           S.EnumDeclarations.(
             let layout =
               Js_layout_generator.statement ~opts
               @@ S.enum_declaration
                    (I.identifier "E")
                    (symbol_body
                       [defaulted_member (I.identifier "A"); defaulted_member (I.identifier "B")])
             in
             assert_output ~ctxt "enum E of symbol{A,B,}" layout;
             let pretty_output = "enum E of symbol {\n" ^ "  A,\n" ^ "  B,\n" ^ "}" in
             assert_output ~ctxt ~pretty:true pretty_output layout) );
         ( "enum_with_unknown_members" >:: fun ctxt ->
           S.EnumDeclarations.(
             let layout =
               Js_layout_generator.statement ~opts
               @@ S.enum_declaration
                    (I.identifier "E")
                    (symbol_body
                       ~has_unknown_members:true
                       [defaulted_member (I.identifier "A"); defaulted_member (I.identifier "B")])
             in
             assert_output ~ctxt "enum E of symbol{A,B,...}" layout;
             let pretty_output = "enum E of symbol {\n" ^ "  A,\n" ^ "  B,\n" ^ "  ...\n" ^ "}" in
             assert_output ~ctxt ~pretty:true pretty_output layout) );
         ( "enum_with_internal_comment" >:: fun ctxt ->
           S.EnumDeclarations.(
             let layout =
               Js_layout_generator.statement ~opts
               @@ S.enum_declaration
                    (I.identifier "E")
                    (symbol_body
                       ~comments:
                         Flow_ast.Syntax.
                           {
                             leading = [];
                             trailing = [];
                             internal =
                               [Ast_builder.Comments.line ~on_newline:true " internal comment"];
                           }
                       [defaulted_member (I.identifier "A"); defaulted_member (I.identifier "B")])
             in
             assert_output ~ctxt "enum E of symbol{A,B,// internal comment\n}" layout;
             let pretty_output = "enum E of symbol {\n  A,\n  B,\n  // internal comment\n  \n}" in
             assert_output ~ctxt ~pretty:true pretty_output layout) );
         ( "arrow_function_with_function_return_type" >:: fun ctxt ->
           assert_expression_string ~ctxt "():((x)=>y)=>{}";
           assert_expression_string ~ctxt "():((x)=>y)%checks=>{}";
           assert_expression_string ~ctxt "():()=>x=>{}";
           assert_expression_string ~ctxt "():()=>x%checks=>{}";
           assert_expression_string ~ctxt "():(...x)=>y=>{}";
           assert_expression_string ~ctxt "():(...x)=>y%checks=>{}" );
         ( "nullish_coalesce_with_logical" >:: fun ctxt ->
           assert_expression_string ~ctxt "(a&&b)??c";
           assert_expression_string ~ctxt "a??(b&&c)";
           assert_expression_string ~ctxt "a&&(b??c)";
           assert_expression_string ~ctxt "(a??b)&&c";

           assert_expression_string ~ctxt "(a||b)??c";
           assert_expression_string ~ctxt "a??(b||c)";
           assert_expression_string ~ctxt "a||(b??c)";
           assert_expression_string ~ctxt "(a??b)||c" );
         ("optional_call" >:: fun ctxt -> assert_expression_string ~ctxt "foo?.()");
         ( "class_preserve_blank_lines_between_elements" >:: fun ctxt ->
           (* Single blank line is preserved *)
           assert_expression_string ~ctxt ~pretty:true "class C {\n  a;\n  \n  b;\n}";
           (* Multiple blank lines are condensed to a single blank line *)
           assert_expression
             ~ctxt
             ~pretty:true
             "class C {\n  a;\n  \n  b;\n}"
             (expression_of_string "class C {\n  a;\n  \n  \n b;\n}");
           (* Comments are not treated as blank lines *)
           assert_expression_string ~ctxt ~pretty:true "class C {\n  a;\n  //L\n  b;\n}" );
         ( "object_type_preserve_blank_lines_between_properties" >:: fun ctxt ->
           (* Single blank line is preserved *)
           assert_statement_string ~ctxt ~pretty:true "type T = {\n  a: 1,\n  \n  b: 2,\n};";
           (* Multiple blank lines are condensed to a single blank line *)
           assert_statement
             ~ctxt
             ~pretty:true
             "type T = {\n  a: 1,\n  \n  b: 2,\n};"
             (statement_of_string "type T = {\n  a: 1,\n  \n  \n b: 2,\n};");
           (* Comments are not treated as blank lines *)
           assert_statement_string ~ctxt ~pretty:true "type T = {\n  a: 1,\n  //L\n  b: 2,\n};" );
         ( "object_pattern_preserve_blank_lines_between_properties" >:: fun ctxt ->
           (* Single blank line is preserved *)
           assert_statement_string ~ctxt ~pretty:true "var {\n  a,\n  \n  b\n};";
           (* Multiple blank lines are condensed to a single blank line *)
           assert_statement
             ~ctxt
             ~pretty:true
             "var {\n  a,\n  \n  b\n};"
             (statement_of_string "var {\n  a,\n  \n  \n b\n};");
           (* Comments are not treated as blank lines *)
           assert_statement_string ~ctxt ~pretty:true "var {\n  a,\n  //L\n  b\n};" );
         ( "switch_preserve_blank_lines_between_cases" >:: fun ctxt ->
           (* Single blank line is preserved *)
           assert_statement_string
             ~ctxt
             ~pretty:true
             "switch (true) {\n  case a:\n    break;\n  \n  case b:\n    break;\n}";
           (* Multiple blank lines are condensed to a single blank line *)
           assert_statement
             ~ctxt
             ~pretty:true
             "switch (true) {\n  case a:\n    break;\n  \n  case b:\n    break;\n}"
             (statement_of_string
                "switch (true) {\n  case a:\n    break;\n  \n  \n  case b:\n    break;\n}");
           (* Comments are not treated as blank lines *)
           assert_statement_string
             ~ctxt
             ~pretty:true
             "switch (true) {\n  case a:\n    break;\n  //L\n  case b:\n    break;\n}" );
         ( "object_type_preserve_wrapping" >:: fun ctxt ->
           (* Object type that fits on single line with no wrapping is printed on single line *)
           assert_statement_string ~ctxt ~pretty:true "type T = { a: 1 };";
           (* Object type that fits on single line but wraps is printed as wrapping *)
           assert_statement_string ~ctxt ~pretty:true "type T = {\n  a: 1,\n};" );
         ( "function_params_preserve_blank_lines_between_params" >:: fun ctxt ->
           (* Single blank line is preserved *)
           assert_expression_string ~ctxt ~pretty:true "(\n  a,\n  \n  b,\n) => {}";
           (* Multiple blank lines are condensed to a single blank line *)
           assert_expression
             ~ctxt
             ~pretty:true
             "(\n  a,\n  \n  b,\n) => {}"
             (expression_of_string "(\n  a,\n  \n  \n  b,\n) => {}");
           (* Comments are not treated as blank lines *)
           assert_expression_string ~ctxt ~pretty:true "(\n  a,\n  //L\n  b,\n) => {}" );
         ( "function_type_params_preserve_blank_lines_between_params" >:: fun ctxt ->
           (* Single blank line is preserved *)
           assert_statement_string ~ctxt ~pretty:true "type T = (\n  a,\n  \n  b\n) => c;";
           (* Multiple blank lines are condensed to a single blank line *)
           assert_statement
             ~ctxt
             ~pretty:true
             "type T = (\n  a,\n  \n  b\n) => c;"
             (statement_of_string "type T = (\n  a,\n  \n  \n  b\n) => c;");
           (* Comments are not treated as blank lines *)
           assert_statement_string ~ctxt ~pretty:true "type T = (\n  a,\n  //L\n  b\n) => c;" );
         ( "array_preserve_blank_lines_between_elements" >:: fun ctxt ->
           (* Single blank line is preserved *)
           assert_expression_string ~ctxt ~pretty:true "[\n  a,\n  \n  b,\n]";
           (* Multiple blank lines are condensed to a single blank line *)
           assert_expression
             ~ctxt
             ~pretty:true
             "[\n  a,\n  \n  b,\n]"
             (expression_of_string "[\n  a,\n  \n  \n  b,\n]");
           (* Comments are not treated as blank lines *)
           assert_expression_string ~ctxt ~pretty:true "[\n  a,\n  //L\n  b,\n]";
           (* Blank lines between holes are preserved *)
           assert_expression_string ~ctxt ~pretty:true "[\n  ,\n  \n  ,\n]";
           assert_expression_string ~ctxt ~pretty:true "[\n  a,\n  \n  ,\n]";
           assert_expression_string ~ctxt ~pretty:true "[\n  ,\n  \n  a,\n]" );
         ( "array_pattern_preserve_blank_lines_between_elements" >:: fun ctxt ->
           (* Single blank line is preserved *)
           assert_statement_string ~ctxt ~pretty:true "var [\n  a,\n  \n  b\n];";
           (* Multiple blank lines are condensed to a single blank line *)
           assert_statement
             ~ctxt
             ~pretty:true
             "var [\n  a,\n  \n  b\n];"
             (statement_of_string "var [\n  a,\n  \n  \n  b\n];");
           (* Comments are not treated as blank lines *)
           assert_statement_string ~ctxt ~pretty:true "var [\n  a,\n  //L\n  b\n];";
           (* Blank lines between holes are preserved *)
           assert_statement_string ~ctxt ~pretty:true "var [\n  ,\n  \n  ,\n  a\n];";
           assert_statement_string ~ctxt ~pretty:true "var [\n  a,\n  \n  ,\n  \n  b\n];" );
         ( "call_preserve_blank_lines_between_args" >:: fun ctxt ->
           (* Single blank line is preserved *)
           assert_expression_string ~ctxt ~pretty:true "foo(\n  a,\n  \n  b,\n)";
           (* Multiple blank lines are condensed to a single blank line *)
           assert_expression
             ~ctxt
             ~pretty:true
             "foo(\n  a,\n  \n  b,\n)"
             (expression_of_string "foo(\n  a,\n  \n  \n  b,\n)");
           (* Comments are not treated as blank lines *)
           assert_expression_string ~ctxt ~pretty:true "foo(\n  a,\n  //L\n  b,\n)" );
         ( "new_preserve_blank_lines_between_args" >:: fun ctxt ->
           (* Single blank line is preserved *)
           assert_expression_string ~ctxt ~pretty:true "new Foo(\n  a,\n  \n  b,\n)";
           (* Multiple blank lines are condensed to a single blank line *)
           assert_expression
             ~ctxt
             ~pretty:true
             "new Foo(\n  a,\n  \n  b,\n)"
             (expression_of_string "new Foo(\n  a,\n  \n  \n  b,\n)");
           (* Comments are not treated as blank lines *)
           assert_expression_string ~ctxt ~pretty:true "new Foo(\n  a,\n  //L\n  b,\n)" );
         ( "type_args_preserve_blank_lines_between_args" >:: fun ctxt ->
           (* Single blank line is preserved *)
           assert_statement_string ~ctxt ~pretty:true "type Foo = Bar<\n  a,\n  \n  b,\n>;";
           (* Multiple blank lines are condensed to a single blank line *)
           assert_statement
             ~ctxt
             ~pretty:true
             "type Foo = Bar<\n  a,\n  \n  b,\n>;"
             (statement_of_string "type Foo = Bar<\n  a,\n  \n  \n  b,\n>;");
           (* Comments are not treated as blank lines *)
           assert_statement_string ~ctxt ~pretty:true "type Foo = Bar<\n  a,\n  //L\n  b,\n>;" );
         ( "call_type_args_preserve_blank_lines_between_args" >:: fun ctxt ->
           (* Single blank line is preserved *)
           assert_expression_string ~ctxt ~pretty:true "foo<\n  a,\n  \n  b,\n>()";
           (* Multiple blank lines are condensed to a single blank line *)
           assert_expression
             ~ctxt
             ~pretty:true
             "foo<\n  a,\n  \n  b,\n>()"
             (expression_of_string "foo<\n  a,\n  \n  \n  b,\n>()");
           (* Comments are not treated as blank lines *)
           assert_expression_string ~ctxt ~pretty:true "foo<\n  a,\n  //L\n  b,\n>()" );
         ( "call_template_wrapping" >:: fun ctxt ->
           assert_expression_string ~ctxt ~pretty:true "foo(`a\nb`)";
           assert_expression_string ~ctxt ~pretty:true "foo(\n  `a\nb`,\n)" );
         ( "type_params_preserve_blank_lines_between_params" >:: fun ctxt ->
           (* Single blank line is preserved *)
           assert_expression_string ~ctxt ~pretty:true "<\n  a,\n  \n  b,\n>() => {}";
           (* Multiple blank lines are condensed to a single blank line *)
           assert_expression
             ~ctxt
             ~pretty:true
             "<\n  a,\n  \n  b,\n>() => {}"
             (expression_of_string "<\n  a,\n  \n  \n  b,\n>() => {}");
           (* Comments are not treated as blank lines *)
           assert_expression_string ~ctxt ~pretty:true "<\n  a,\n  //L\n  b,\n>() => {}" );
         ( "function_params_break_before_return_type" >:: fun ctxt ->
           (* If both function params and return type can break, params should break first *)
           let a20 = String.make 20 'a' in
           let b20 = String.make 20 'b' in
           assert_statement
             ~ctxt
             ~pretty:true
             ("function f(\n  "
             ^ a20
             ^ ",\n  "
             ^ b20
             ^ ",\n): { "
             ^ a20
             ^ ": t, "
             ^ b20
             ^ ": t } {}")
             (statement_of_string
                ("function f(" ^ a20 ^ ", " ^ b20 ^ "): {" ^ a20 ^ ": t, " ^ b20 ^ ": t} {}")) );
         ( "function_type_params_break_before_return_type" >:: fun ctxt ->
           (* If both function param types and return type can break, params should break first *)
           let a20 = String.make 20 'a' in
           let b20 = String.make 20 'b' in
           assert_statement
             ~ctxt
             ~pretty:true
             ("type T = (\n  " ^ a20 ^ ",\n  " ^ b20 ^ "\n) => { " ^ a20 ^ ": t, " ^ b20 ^ ": t };")
             (statement_of_string
                ("type T = (" ^ a20 ^ ", " ^ b20 ^ ") => {" ^ a20 ^ ": t, " ^ b20 ^ ": t};")) );
         ( "jsx_in_new_expression" >:: fun ctxt ->
           (* Verify that JSX wrapped in parens is not reprinted with extra blank lines before and after *)
           let a80 = String.make 80 'a' in
           assert_expression
             ~ctxt
             ~pretty:true
             ("new Foo(\n"
             ^ "  a,\n"
             ^ "  <jsx>\n"
             ^ ("    " ^ a80 ^ "\n")
             ^ "  </jsx>,\n"
             ^ "  b,\n"
             ^ ")")
             (expression_of_string
                ("new Foo(\n"
                ^ "  a,\n"
                ^ "  (\n"
                ^ "    <jsx>\n"
                ^ ("  " ^ a80 ^ "\n")
                ^ "    </jsx>\n"
                ^ "  ),\n"
                ^ "  b,\n"
                ^ ")")) );
         ( "assignment_arrow_function_rhs" >:: fun ctxt ->
           assert_statement_string ~ctxt ~pretty:true "const x = () => {};";
           assert_statement_string ~ctxt ~pretty:true "const x = () => y = 123;" );
       ]
