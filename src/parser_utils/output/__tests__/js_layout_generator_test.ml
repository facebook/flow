(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)


open OUnit2
open Ast_builder
open Layout_test_utils

module S = Ast_builder.Statements
module E = Ast_builder.Expressions
module J = Ast_builder.JSXs
module L = Layout_builder

let assert_output ~ctxt ?msg ?(pretty=false) expected_str layout =
  let print =
    if pretty then Pretty_printer.print ~source_maps:None
    else Compact_printer.print ~source_maps:None
  in
  let out = String.trim (print layout |> Source.contents) in
  assert_equal ~ctxt ?msg ~printer:(fun x -> x) expected_str out

let assert_expression
    ~ctxt ?msg ?pretty ?(expr_ctxt=Js_layout_generator.normal_context)
    expected_str ast =
  let layout = Js_layout_generator.expression ~ctxt:expr_ctxt ast in
  assert_output ~ctxt ?msg ?pretty expected_str layout

let assert_expression_string ~ctxt ?msg ?pretty ?expr_ctxt str =
  let ast = expression_of_string str in
  assert_expression ~ctxt ?msg ?pretty ?expr_ctxt str ast

let assert_statement ~ctxt ?msg ?pretty expected_str ast =
  let layout = Js_layout_generator.statement ast in
  assert_output ~ctxt ?msg ?pretty expected_str layout

let assert_statement_string ~ctxt ?msg ?pretty str =
  let ast = statement_of_string str in
  let layout = Js_layout_generator.statement ast in
  assert_output ~ctxt ?msg ?pretty str layout

let tests = "js_layout_generator" >::: [
  "operator_precedence" >:: Operator_precedence_test.test;
  "assignment_precedence" >:: Assignment_precedence_test.test;
  "variable_declaration_precedence" >:: Variable_declaration_precedence_test.test;

  "unary_plus_binary" >::
    begin fun ctxt ->
      let module U = Ast.Expression.Unary in
      let module B = Ast.Expression.Binary in

      let x = E.identifier "x" in
      let y = E.identifier "y" in
      let plus_y = E.unary ~op:U.Plus y in
      let minus_y = E.unary ~op:U.Minus y in

      let ast = E.binary ~op:B.Plus x plus_y in
      assert_expression ~ctxt "x+ +y" ast;

      let ast = E.binary ~op:B.Plus plus_y x in
      assert_expression ~ctxt "+y+x" ast;

      let ast = E.binary ~op:B.Minus x minus_y in
      assert_expression ~ctxt "x- -y" ast;

      let ast = E.binary ~op:B.Plus x minus_y in
      assert_expression ~ctxt "x+-y" ast;

      let ast = E.binary ~op:B.Minus x plus_y in
      assert_expression ~ctxt "x-+y" ast;

      let ast = E.binary ~op:B.Plus x (E.conditional plus_y y y) in
      assert_expression ~ctxt "x+(+y?y:y)" ast;

      let ast = E.binary ~op:B.Plus x (E.binary plus_y ~op:B.Plus y) in
      assert_expression ~ctxt "x+(+y+y)" ast;

      (* `*` is higher precedence than `+`, so would not normally need parens if
         not for the `+y` *)
      let ast = E.binary ~op:B.Plus x (E.binary plus_y ~op:B.Mult y) in
      assert_expression ~ctxt "x+(+y)*y" ast;

      (* parens are necessary around the inner `+y+y`, but would be reundant
         around the multiplication. that is, we don't need `x+((+y+y)*y)`. *)
      let ast = E.binary
        ~op:B.Plus x (E.binary ~op:B.Mult (E.binary plus_y ~op:B.Plus y) y) in
      assert_expression ~ctxt "x+(+y+y)*y" ast;
    end;

  "update_plus_binary" >::
    begin fun ctxt ->
      let x = E.identifier "x" in
      let y = E.identifier "y" in
      let x_incr = (Loc.none, Ast.Expression.Update { Ast.Expression.Update.
        operator = Ast.Expression.Update.Increment;
        prefix = false;
        argument = x;
      }) in
      let x_decr = (Loc.none, Ast.Expression.Update { Ast.Expression.Update.
        operator = Ast.Expression.Update.Decrement;
        prefix = false;
        argument = x;
      }) in
      let incr_y = (Loc.none, Ast.Expression.Update { Ast.Expression.Update.
        operator = Ast.Expression.Update.Increment;
        prefix = true;
        argument = y;
      }) in
      let decr_y = (Loc.none, Ast.Expression.Update { Ast.Expression.Update.
        operator = Ast.Expression.Update.Decrement;
        prefix = true;
        argument = y;
      }) in

      let ast = E.binary ~op:Ast.Expression.Binary.Plus x incr_y in
      assert_expression ~ctxt "x+ ++y" ast;

      let ast = E.binary ~op:Ast.Expression.Binary.Minus x incr_y in
      assert_expression ~ctxt "x- ++y" ast;

      let ast = E.binary ~op:Ast.Expression.Binary.Minus x decr_y in
      assert_expression ~ctxt "x- --y" ast;

      let ast = E.binary ~op:Ast.Expression.Binary.Plus x decr_y in
      assert_expression ~ctxt "x+ --y" ast;

      let ast = E.binary ~op:Ast.Expression.Binary.Plus x_incr y in
      assert_expression ~ctxt "x+++y" ast;

      let ast = E.binary ~op:Ast.Expression.Binary.Minus x_decr y in
      assert_expression ~ctxt "x---y" ast;

      let ast = E.binary ~op:Ast.Expression.Binary.Plus x_incr incr_y in
      assert_expression ~ctxt "x+++ ++y" ast;

      let ast = E.binary ~op:Ast.Expression.Binary.Minus x_decr decr_y in
      assert_expression ~ctxt "x--- --y" ast;
    end;

  "object_property_is_function" >::
    begin fun ctxt ->
      let ast = E.object_ [
        E.object_property
          (E.object_property_key "foo")
          (E.function_ ());
      ] in
      assert_expression ~ctxt "{foo:function(){}}" ast
    end;

  "object_property_is_method" >::
    begin fun ctxt ->
      let ast = E.object_ [
        E.object_method (E.object_property_key "foo");
      ] in
      assert_expression ~ctxt "{foo(){}}" ast
    end;

  "object_property_is_generator_method" >::
    begin fun ctxt ->
      let ast = E.object_ [
        E.object_method ~generator:true (E.object_property_key "foo");
      ] in
      assert_expression ~ctxt "{*foo(){}}" ast
    end;

  "object_property_is_sequence" >::
    begin fun ctxt ->
      let ast = E.object_ [
        E.object_property
          (E.object_property_key "foo")
          (E.sequence [E.identifier "x"; E.identifier "y"]);
      ] in
      assert_expression ~ctxt "{foo:(x,y)}" ast
    end;

  "object_property_key_is_literal" >::
    begin fun ctxt ->
      let ast = E.object_ [
        E.object_property_with_literal
          (Literals.string "foo")
          (E.literal (Literals.string "bar"));
      ] in
      assert_expression ~ctxt ~msg:"string literal keys should be quoted"
        "{\"foo\":\"bar\"}" ast
    end;

  "do_while_semicolon" >::
    begin fun ctxt ->
      let module S = Ast.Statement in
      (* do { x } while (y) *)
      let ast = (Loc.none, S.DoWhile { S.DoWhile.
        body = (Loc.none, S.Block { S.Block.
          body = [
            Loc.none, S.Expression { S.Expression.
              expression = E.identifier "x";
              directive = None;
            };
          ];
        });
        test = E.identifier "y";
      }) in
      assert_statement ~ctxt "do{x}while(y);" ast;
    end;

  "do_while_single_statement" >::
    begin fun ctxt ->
      let module S = Ast.Statement in
      (* do x; while (y) *)
      let ast = (Loc.none, S.DoWhile { S.DoWhile.
        body = (Loc.none, S.Expression { S.Expression.
          expression = E.identifier "x";
          directive = None;
        });
        test = E.identifier "y";
      }) in
      assert_statement ~ctxt "do x;while(y);" ast;
    end;

  "conditional_expression_parens" >::
    begin fun ctxt ->
      let module Expr = Ast.Expression in

      let a, b, c, d, e =
        E.identifier "a", E.identifier "b", E.identifier "c",
        E.identifier "d", E.identifier "e" in

      (* a ? b++ : c-- *)
      let update = E.conditional a
        (E.update ~op:Expr.Update.Increment ~prefix:false b)
        (E.update ~op:Expr.Update.Decrement ~prefix:false c) in
      assert_expression ~ctxt "a?b++:c--" update;

      (* a ? +b : -c *)
      let unary = E.conditional a
        (E.unary ~op:Expr.Unary.Plus b)
        (E.unary ~op:Expr.Unary.Minus c) in
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

      let assignment = E.conditional
        a
        (E.assignment (Patterns.identifier "x") b)
        (E.assignment (Patterns.identifier "y") c)
      in
      assert_expression ~ctxt "a?x=b:y=c" assignment;

      let sequence = E.conditional a (E.sequence [b; c]) (E.sequence [d; e]) in
      assert_expression ~ctxt "a?(b,c):(d,e)" sequence;
    end;

  "call_expression_parens" >::
    begin fun ctxt ->
      let x = E.identifier "x" in

      (* `(x++)()` *)
      let update = E.call (E.update ~op:Ast.Expression.Update.Increment ~prefix:false x) in
      assert_expression ~ctxt "(x++)()" update;

      (* `x.y()` *)
      let member = E.call (E.member_expression (E.member x ~property:"y")) in
      assert_expression ~ctxt "x.y()" member;

      (* `x.y.z()` *)
      let two_members = E.call
        (E.member_expression (E.member
          (E.member_expression (E.member x ~property:"y"))
          ~property:"z")) in
      assert_expression ~ctxt "x.y.z()" two_members;

      (* `x()()` *)
      let call = E.call (E.call x) in
      assert_expression ~ctxt "x()()" call;

      (* `new x()()` *)
      let new_ = E.call (E.new_ x) in
      assert_expression ~ctxt "new x()()" new_;

      (* `function() {}()` *)
      let func = E.call (E.function_ ()) in
      assert_expression ~ctxt "function(){}()" func;

      (* `(function() {}.foo)()` *)
      let func = E.call (E.member_expression (E.member
          (E.function_ ()) ~property:"foo"
      )) in
      assert_expression ~ctxt "function(){}.foo()" func;

      (* `(() => {})()` *)
      let arrow = E.call (E.arrow_function ()) in
      assert_expression ~ctxt "(()=>{})()" arrow;

      (* `(foo, bar)()` *)
      let seq = E.call (E.sequence [x; E.identifier "y"]) in
      assert_expression ~ctxt "(x,y)()" seq;

      (* `__d("a", [], (function() {}), 1)` *)
      let underscore_d = E.call
        ~args:[
          Ast.Expression.Expression (E.literal (Literals.string "a"));
          Ast.Expression.Expression (E.literal (Literals.string "b"));
          Ast.Expression.Expression (E.function_ ());
          Ast.Expression.Expression (E.literal (Literals.number 1. "1"));
        ]
        (E.identifier "__d") in
      assert_expression ~ctxt "__d(\"a\",\"b\",(function(){}),1)" underscore_d;
    end;

  "member_expression_parens" >::
    begin fun ctxt ->
      let x = E.identifier "x" in

      (* `(x++).y` *)
      let update = E.member_expression (E.member
        (E.update ~op:Ast.Expression.Update.Increment ~prefix:false x)
        ~property:"y") in
      assert_expression ~ctxt "(x++).y" update;

      (* `x.y.z` *)
      let member = E.member_expression (E.member
        (E.member_expression (E.member x ~property:"y"))
        ~property:"z") in
      assert_expression ~ctxt "x.y.z" member;

      (* x().y *)
      let call = E.member_expression (E.member (E.call x) ~property:"y") in
      assert_expression ~ctxt "x().y" call;

      (* x()[y] *)
      let computed = E.member_expression (
        E.member_computed (E.call x) ~property:"y"
      ) in
      assert_expression ~ctxt "x()[y]" computed;

      (* `(function() {}).x` *)
      let func = E.member_expression (E.member
        (E.function_ ())
        ~property:"x"
      ) in
      assert_expression ~ctxt "function(){}.x" func;

      (* `(() => {}).x` *)
      let func = E.member_expression (E.member
        (E.arrow_function ())
        ~property:"x"
      ) in
      assert_expression ~ctxt "(()=>{}).x" func;

      (* `(x, y).z` *)
      let seq = E.member_expression (E.member
        (E.sequence [x; E.identifier "y"])
        ~property:"z"
      ) in
      assert_expression ~ctxt "(x,y).z" seq;

      let num = E.member_expression (E.member
        (E.literal (Literals.number 1.0 "1"))
        ~property:"z"
      ) in
      assert_expression ~ctxt "1..z" num;
      let num = E.member_expression (E.member
        (E.literal (Literals.number 1.1 "1.1"))
        ~property:"z"
      ) in
      assert_expression ~ctxt "1.1.z" num;
      let num = E.member_expression (E.member
        (E.literal (Literals.number 0.0000001 "0.0000001"))
        ~property:"z"
      ) in
      assert_expression ~ctxt "1e-7.z" num;

    end;

  "new_expression_parens" >::
    begin fun ctxt ->
      let x, y, z = E.identifier "x", E.identifier "y", E.identifier "z" in

      (* `new (x++)()` *)
      let update = E.new_ (
        E.update ~op:Ast.Expression.Update.Increment ~prefix:false x
      ) in
      assert_expression ~ctxt "new(x++)()" update;

      (* `new (x())()` *)
      let call = E.new_ (E.call x) in
      assert_expression ~ctxt "new(x())()" call;

      (* `new x.y()` *)
      let member = E.new_ (Loc.none, Ast.Expression.Member { Ast.Expression.Member.
        _object = x;
        property = Ast.Expression.Member.PropertyIdentifier (Loc.none, "y");
        computed = false;
      }) in
      assert_expression ~ctxt "new x.y()" member;

      (* `new (x.y())()` *)
      let member_call = E.new_ (E.call (
        E.member_expression (E.member x ~property:"y")
      )) in
      assert_expression ~ctxt "new(x.y())()" member_call;

      (* `new (x().y)()` *)
      let call_member = E.new_ (E.member_expression (
        E.member (E.call x) ~property:"y"
      )) in
      assert_expression ~ctxt "new(x().y)()" call_member;

      (* `new (x ? y : z)()` *)
      let cond = E.new_ (E.conditional x y z) in
      assert_expression ~ctxt "new(x?y:z)()" cond;
    end;

  "unary_expression_parens" >::
    begin fun ctxt ->
      let module Unary = Ast.Expression.Unary in
      let module Update = Ast.Expression.Update in

      (* `+(+x)` *)
      let plus = E.unary ~op:Unary.Plus (
        E.unary ~op:Unary.Plus (E.identifier "x")
      ) in
      assert_expression ~ctxt "+(+x)" plus;

      (* `+-x` *)
      let minus = E.unary ~op:Unary.Plus (
        E.unary ~op:Unary.Minus (E.identifier "x")
      ) in
      assert_expression ~ctxt "+-x" minus;

      (* `+(++x)` *)
      let prefix_incr = E.unary ~op:Unary.Plus (
        E.update ~op:Update.Increment ~prefix:true (E.identifier "x")
      ) in
      assert_expression ~ctxt "+(++x)" prefix_incr;

      (* `+--x` *)
      let prefix_decr = E.unary ~op:Unary.Plus (
        E.update ~op:Update.Decrement ~prefix:true (E.identifier "x")
      ) in
      assert_expression ~ctxt "+--x" prefix_decr;

      (* `+x++` *)
      let suffix_incr = E.unary ~op:Unary.Plus (
        E.update ~op:Update.Increment ~prefix:false (E.identifier "x")
      ) in
      assert_expression ~ctxt "+x++" suffix_incr;

      (* `+x--` *)
      let suffix_decr = E.unary ~op:Unary.Plus (
        E.update ~op:Update.Decrement ~prefix:false (E.identifier "x")
      ) in
      assert_expression ~ctxt "+x--" suffix_decr;

      (* `+x()` *)
      let call = E.unary ~op:Unary.Plus (E.call (E.identifier "x")) in
      assert_expression ~ctxt "+x()" call;

      (* `+new x()` *)
      let new_ = E.unary ~op:Unary.Plus (E.new_ (E.identifier "x")) in
      assert_expression ~ctxt "+new x()" new_;
    end;

  "expression_statement_parens" >::
    begin fun ctxt ->
      let obj = S.expression (E.object_ []) in
      assert_statement ~ctxt "({});" obj;

      let func = S.expression (E.function_ ()) in
      assert_statement ~ctxt "(function(){});" func;

      let arrow = S.expression (E.arrow_function ()) in
      assert_statement ~ctxt "()=>{};" arrow;

      let klass = S.expression (E.class_ []) in
      assert_statement ~ctxt "(class{});" klass;

      let func_call = S.expression (
        E.call (E.function_ ())
      ) in
      assert_statement ~ctxt "(function(){})();" func_call;

      let func_member = S.expression (E.member_expression (
        E.member (E.function_ ()) ~property:"foo"
      )) in
      assert_statement ~ctxt "(function(){}).foo;" func_member;

      let class_member = S.expression (E.member_expression (
        E.member (E.class_ []) ~property:"foo"
      )) in
      assert_statement ~ctxt "(class{}).foo;" class_member;

      let func_member_call = S.expression (
        E.call (E.member_expression (E.member
          (E.function_ ()) ~property:"foo"
        ))
      ) in
      assert_statement ~ctxt "(function(){}).foo();" func_member_call;

      let func_call_member = S.expression (
        E.member_expression (E.member
          (E.call (E.function_ ())) ~property:"foo"
        )
      ) in
      assert_statement ~ctxt "(function(){})().foo;" func_call_member;

      let func_sequence = S.expression (
        E.sequence [E.function_ (); E.identifier "x"]
      ) in
      assert_statement ~ctxt "(function(){}),x;" func_sequence;
    end;

  "arrow_body_parens" >::
    begin fun ctxt ->
      let x, y, z = E.identifier "x", E.identifier "y", E.identifier "z" in

      let arrow =
        let body = Functions.body_expression (E.sequence [x; y]) in
        E.arrow_function ~body () in
      assert_expression ~ctxt "()=>(x,y)" arrow;

      let arrow =
        let body = Functions.body_expression (E.conditional x y z) in
        E.arrow_function ~body () in
      assert_expression ~ctxt "()=>x?y:z" arrow;

      let arrow =
        let arrow = E.arrow_function () in
        let body = Functions.body_expression arrow in
        E.arrow_function ~body () in
      assert_expression ~ctxt "()=>()=>{}" arrow;
    end;

  "argument_parens" >::
    begin fun ctxt ->
      let f = E.identifier "f" in
      let x, y, z = E.identifier "x", E.identifier "y", E.identifier "z" in

      let args = [] in
      let call = E.call ~args f in
      assert_expression ~ctxt "f()" call;

      let args =
        let seq = E.sequence [x; y] in
        [Ast.Expression.Expression seq] in
      let call = E.call ~args f in
      assert_expression ~ctxt ~msg:"sequence should be parenthesized"
        "f((x,y))" call;

      let args = [E.spread (E.sequence [x; y])] in
      let call = E.call ~args f in
      assert_expression ~ctxt ~msg:"sequence should be parenthesized"
        "f(...(x,y))" call;

      let args = [Ast.Expression.Expression (E.conditional x y z)] in
      let call = E.call ~args f in
      assert_expression ~ctxt ~msg:"higher-precedence ops don't need parens"
        "f(x?y:z)" call;

      let call =
        let arrow = E.arrow_function () in
        let args = [Ast.Expression.Expression arrow] in
        E.call ~args f in
      assert_expression ~ctxt ~msg:"higher-precedence ops don't need parens"
        "f(()=>{})" call;

      let args =
        let seq = E.sequence [x; y] in
        let logical = E.logical_or seq z in
        [Ast.Expression.Expression logical] in
      let call = E.call ~args f in
      assert_expression ~ctxt ~msg:"nested sequence has parens"
        "f((x,y)||z)" call;
    end;

  "binary_in_space" >::
    begin fun ctxt ->
      let ast = statement_of_string {|if("foo" in {"foo": bar}){}|} in
      assert_statement ~ctxt {|if("foo"in{"foo":bar}){}|} ast;

      let ast = statement_of_string {|if("foo" in bar){}|} in
      assert_statement ~ctxt {|if("foo"in bar){}|} ast;

      let ast = statement_of_string {|if(foo in {"foo":bar}){}|} in
      assert_statement ~ctxt {|if(foo in{"foo":bar}){}|} ast;
    end;

  "binary_instanceof_space" >::
    begin fun ctxt ->
      let ast = statement_of_string {|if("foo" instanceof {"foo": bar}){}|} in
      assert_statement ~ctxt {|if("foo"instanceof{"foo":bar}){}|} ast;

      let ast = statement_of_string {|if("foo" instanceof bar){}|} in
      assert_statement ~ctxt {|if("foo"instanceof bar){}|} ast;

      let ast = statement_of_string {|if(foo instanceof {"foo":bar}){}|} in
      assert_statement ~ctxt {|if(foo instanceof{"foo":bar}){}|} ast;
    end;

  "logical_wrapping" >::
    begin fun ctxt ->
      let x40 = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" in
      let ast = E.logical_and (E.identifier x40) (E.identifier x40) in
      assert_layout_of_expression ~ctxt
        L.(loc (fused [
          loc (id "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
          pretty_space;
          atom "&&";
          sequence ~break:Layout.Break_if_needed ~inline:(false, true) [
            fused [
              flat_pretty_space;
              loc (id "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
            ];
          ];
        ]))
        ast;
    end;

  "return_statement_parens" >::
    begin fun ctxt ->
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
      let func = S.function_declaration (Loc.none, "f") ~body:[
        S.return (Some (E.sequence [E.identifier x40; E.identifier y40]));
      ] in
      assert_layout_result ~ctxt
        L.(loc (fused [
          fused [
            atom "return";
            atom " ";
            sequence ~break:Layout.Break_if_needed ~inline:(true, true) ~indent:0 [
              fused [
                Layout.IfBreak ((atom "("), empty);
                sequence ~break:Layout.Break_if_needed [
                  loc (sequence ~break:Layout.Break_if_needed ~inline:(true, true) ~indent:0 [
                    sequence ~break:Layout.Break_if_needed ~inline:(true, true) ~indent:0 [
                      fused [
                        loc (id "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
                        Layout.IfBreak ((atom ","), (fused [atom ","; pretty_space]));
                      ];
                      loc (id "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy");
                    ];
                  ]);
                ];
                Layout.IfBreak ((atom ")"), empty);
              ];
            ];
          ];
          Layout.IfPretty ((atom ";"), empty);
        ]))
        Layout_matcher.(body_of_function_declaration func >>= nth_sequence 0);
      assert_statement ~ctxt ("function f(){return "^x40^","^y40^"}") func;
      assert_statement ~ctxt ~pretty:true
        ("function f() {\n  return (\n    "^x40^",\n    "^y40^"\n  );\n}")
        func;

      (* logicals get split *)
      let logical = E.logical_and (E.identifier x40) (E.identifier y40) in
      let func = S.function_declaration (Loc.none, "f") ~body:[S.return (Some logical)] in
      assert_layout_result ~ctxt
        L.(loc (fused [
          fused [
            atom "return";
            atom " ";
            sequence ~break:Layout.Break_if_needed ~inline:(true, true) ~indent:0 [
              fused [
                Layout.IfBreak ((atom "("), empty);
                sequence ~break:Layout.Break_if_needed [expression logical];
                Layout.IfBreak ((atom ")"), empty);
              ];
            ];
          ];
          Layout.IfPretty ((atom ";"), empty);
        ]))
        Layout_matcher.(body_of_function_declaration func >>= nth_sequence 0);
      assert_statement ~ctxt ~pretty:true
        ("function f() {\n  return (\n    "^x40^" &&\n      " ^ y40 ^ "\n  );\n}")
        func;

      (* binary expressions get split *)
      let func = S.function_declaration (Loc.none, "f") ~body:[
        let op = Ast.Expression.Binary.Plus in
        S.return (Some (E.binary ~op (E.identifier x40) (E.identifier y40)))
      ] in
      assert_layout_result ~ctxt
        L.(loc (fused [
          fused [
            atom "return";
            atom " ";
            sequence ~break:Layout.Break_if_needed ~inline:(true, true) ~indent:0 [
              fused [
                Layout.IfBreak ((atom "("), empty);
                sequence ~break:Layout.Break_if_needed [
                  (* TODO: this is wrong, it should allow the + to break *)
                  loc (fused [
                    loc (id "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
                    pretty_space;
                    atom "+";
                    pretty_space;
                    loc (id "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy");
                  ]);
                ];
                Layout.IfBreak ((atom ")"), empty);
              ];
            ];
          ];
          Layout.IfPretty ((atom ";"), empty);
        ]))
        Layout_matcher.(body_of_function_declaration func >>= nth_sequence 0);
      assert_statement ~ctxt ~pretty:true
        ("function f() {\n  return (\n    "^x40^" + " ^ y40 ^ "\n  );\n}")
        func;

      (* jsx gets split *)
      let long_name = String.make 80 'A' in
      let jsx = E.jsx_element (J.element (J.identifier long_name)) in
      let func = S.function_declaration (Loc.none, "f") ~body:[S.return (Some jsx)] in
      assert_layout_result ~ctxt
        L.(loc (fused [
          fused [
            atom "return";
            pretty_space;
            sequence ~break:Layout.Break_if_needed ~inline:(true, true) ~indent:0 [
              fused [
                Layout.IfBreak ((atom "("), empty);
                sequence ~break:Layout.Break_if_needed [expression jsx];
                Layout.IfBreak ((atom ")"), empty);
              ];
            ];
          ];
          Layout.IfPretty ((atom ";"), empty);
        ]))
        Layout_matcher.(body_of_function_declaration func >>= nth_sequence 0);
      assert_statement ~ctxt ~pretty:true
        ("function f() {\n  return (\n    <"^long_name^"></"^long_name^">\n  );\n}")
        func;

      (* a string doesn't get split *)
      let x80 = x40 ^ x40 in
      let func = S.function_declaration (Loc.none, "f") ~body:[
        S.return (Some (E.identifier x80))
      ] in
      assert_layout_result ~ctxt
        L.(loc (fused [
          fused [
            atom "return";
            atom " ";
            loc (id "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
          ];
          Layout.IfPretty ((atom ";"), empty);
        ]))
        Layout_matcher.(body_of_function_declaration func >>= nth_sequence 0);
      assert_statement ~ctxt ("function f(){return "^x80^"}") func;
      assert_statement ~ctxt ~pretty:true ("function f() {\n  return "^x80^";\n}") func;
    end;

  "return_statement_space" >::
    begin fun ctxt ->
      let assert_no_space ~ctxt expr =
        let ret = statement_of_string ("return "^expr^";") in
        assert_statement ~ctxt ("return"^expr^";") ret
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
      assert_statement ~ctxt {|return 123;|} ret;
    end;

  "for_loops" >::
    begin fun ctxt ->
      let ast =
        let x, y = E.identifier "x", E.identifier "y" in
        let init = E.binary x ~op:Ast.Expression.Binary.In y in
        let body = S.empty () in
        S.for_ init None None body
      in
      assert_statement ~ctxt ~msg:"binary `in` expressions need parens"
        "for((x in y);;);" ast;

      let ast =
        let y, z = E.identifier "y", E.identifier "z" in
        let true_ = Expressions.true_ () in
        let in_expr = E.binary y ~op:Ast.Expression.Binary.In z in
        let eq_expr = E.binary true_ ~op:Ast.Expression.Binary.Equal in_expr in
        let init = E.assignment (Patterns.identifier "x") eq_expr in
        let body = S.empty () in
        S.for_ init None None body
      in
      assert_statement ~ctxt ~msg:"binary `in` expressions need parens"
        "for(x=true==(y in z);;);" ast;
    end;

  "for_in_space" >::
    begin fun ctxt ->
      let ast = statement_of_string {|for(var x in {"foo": bar}){}|} in
      assert_statement ~ctxt {|for(var x in{"foo":bar}){}|} ast;

      let ast = statement_of_string {|for(var x in bar){}|} in
      assert_statement ~ctxt {|for(var x in bar){}|} ast;
    end;

  "for_statement_without_block" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "for(;;)x;";
      assert_statement_string ~ctxt "{for(;;)x}";
    end;

  "if_statements" >::
    begin fun ctxt ->
      let ast = S.if_
        (E.identifier "x")
        (S.labeled (Loc.none, "y") (S.empty ()))
        (Some (S.empty ()))
      in
      assert_statement ~ctxt "if(x)y:;else;" ast;
    end;

  "if_statement_without_block" >::
    begin fun ctxt ->
      let if_stmt = S.if_
        (E.identifier "x")
        (S.expression (E.identifier "y"))
        (None)
      in
      let if_else_stmt = S.if_
        (E.identifier "x")
        (S.expression (E.identifier "y"))
        (Some (S.expression (E.identifier "z")))
      in

      assert_statement ~ctxt "if(x)y;" if_stmt;
      assert_statement ~ctxt "if(x)y;else z;" if_else_stmt;

      let ast = S.block [
        if_stmt;
        S.expression (E.identifier "z");
      ] in
      assert_statement ~ctxt "{if(x)y;z}" ast;

      let ast = S.block [
        if_else_stmt;
      ] in
      assert_statement ~ctxt "{if(x)y;else z}" ast;

      let ast = S.if_
        (E.identifier "x")
        (S.expression (E.identifier "y"))
        (Some (S.expression (
          E.update ~op:Ast.Expression.Update.Increment ~prefix:true (E.identifier "z")
        )))
      in
      assert_statement ~ctxt "if(x)y;else++z;" ast
    end;

  "while_statement_without_block" >::
    begin fun ctxt ->
      let while_stmt = S.while_
        (E.identifier "x")
        (S.expression (E.identifier "y"))
      in
      assert_statement ~ctxt "while(x)y;" while_stmt;

      let ast = S.block [while_stmt] in
      assert_statement ~ctxt "{while(x)y}" ast;
    end;

  "do_while_statements" >::
    begin fun ctxt ->
      let ast = S.do_while
        (S.labeled (Loc.none, "x") (S.empty ()))
        (E.identifier "y")
      in
      assert_statement ~ctxt "do x:;while(y);" ast;

      let ast = S.do_while
        (S.expression (
          E.update ~op:Ast.Expression.Update.Increment ~prefix:true (E.identifier "x")
        ))
        (E.identifier "y")
      in
      assert_statement ~ctxt "do++x;while(y);" ast;
    end;

  "array_expressions" >::
    begin fun ctxt ->
      assert_expression_string ~ctxt "[]";
      assert_expression_string ~ctxt "[a]";
      assert_expression_string ~ctxt "[a,b]";
      assert_expression_string ~ctxt "[a,,b]";
      assert_expression_string ~ctxt "[a,b,,]";
      assert_expression_string ~ctxt ~pretty:true "[a]";
      assert_expression_string ~ctxt ~pretty:true "[a, b]";
      assert_expression_string ~ctxt ~pretty:true "[a, b, ,]";
      assert_expression_string ~ctxt ~pretty:true (
        "[\n  a,\n  " ^ String.make 80 'b' ^ ",\n  ,\n]"
      );
    end;

  "function_statements" >::
    begin fun ctxt ->
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
      assert_statement_string ~ctxt ~pretty:true (
        "function a(): a %checks(a) {}"
      );
      assert_statement_string ~ctxt ~pretty:true (
        "function a(a: a, b: b): a {}"
      );
      assert_statement_string ~ctxt ~pretty:true (
        "function a(\n  a: a,\n  b: " ^ String.make 80 'b' ^ ",\n): a {}"
      );
      assert_statement_string ~ctxt ~pretty:true (
        "function a() {\n  a;\n}"
      );
    end;

  "function_expressions" >::
    begin fun ctxt ->
      assert_expression_string ~ctxt "function(){}";
      assert_expression_string ~ctxt "function a(){}";
      assert_expression_string ~ctxt "async function(){}";
      assert_expression_string ~ctxt "function*(){}";
      assert_expression_string ~ctxt "function(a){}";
      assert_expression_string ~ctxt "function(a,b){}";
      assert_expression_string ~ctxt "function(a:a,b:b):c{}";
      assert_expression_string ~ctxt "function<a>(){}";
      assert_expression_string ~ctxt ~pretty:true (
        "function(a: a, b: b): c {}"
      );
      assert_expression_string ~ctxt "()=>a";
      assert_expression_string ~ctxt "()=>{}";
      assert_expression_string ~ctxt "():* =>{}";
      assert_expression_string ~ctxt "async ()=>{}";
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
      assert_expression_string ~ctxt ~pretty:true (
        "(a: a, b: b): c => {}"
      );
    end;

  "class_statements" >::
    begin fun ctxt ->
      let long_a = String.make 80 'a' in
      let long_b = String.make 80 'b' in

      assert_statement_string ~ctxt "class a{}";
      assert_statement_string ~ctxt "class a extends b{}";
      assert_statement_string ~ctxt "class a<a> extends b{}";
      assert_statement_string ~ctxt "class a extends b<b>{}";
      assert_statement_string ~ctxt ~pretty:true (
        "class " ^ long_a ^ " {}"
      );
      assert_statement_string ~ctxt ~pretty:true (
        "class a\n  extends " ^ long_b ^ " {}"
      );
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
      assert_statement_string ~ctxt "class a extends b implements c{}";
      assert_statement_string ~ctxt ~pretty:true (
        "class a extends b implements c {}"
      );
      assert_statement_string ~ctxt ~pretty:true (
        "class a\n  extends " ^ long_b ^ "\n  implements c {}"
      );
      assert_statement_string ~ctxt ~pretty:true (
        "class a\n  extends " ^ long_b ^ "\n  implements " ^ long_b ^ " {}"
      );
      (* TODO: this seems wrong, `c {` should break onto a new line *)
      assert_statement_string ~ctxt ~pretty:true (
        "class a\n  extends " ^ long_b ^ "\n  implements " ^ long_b ^ ", c {}"
      );
    end;

  "class_expressions" >::
    begin fun ctxt ->
      assert_expression_string ~ctxt "class{}";
      assert_expression_string ~ctxt "class a{}";
      assert_expression_string ~ctxt "class a extends b{}";
    end;

  "class_methods" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "class a{b(){}}";
      assert_statement_string ~ctxt ~pretty:true (
        "class a {\n  b() {}\n  static b() {}\n}"
      );
      assert_statement_string ~ctxt ~pretty:true (
        "class a {\n  async a() {}\n  static async a() {}\n}"
      );
      assert_statement_string ~ctxt ~pretty:true (
        "class a {\n  get a() {}\n  set a() {}\n  static get a() {}\n}"
      );
      assert_statement_string ~ctxt ~pretty:true (
        "class a {\n  constructor() {}\n}"
      );
      assert_statement_string ~ctxt "class a{@a a(){}}";
      assert_statement_string ~ctxt "class a{@(()=>{}) a(){}}";
      assert_statement_string ~ctxt "class a{@a@b a(){}}";
      assert_statement_string ~ctxt ~pretty:true (
        "class a {\n  @a\n  a() {}\n}"
      );
      assert_statement_string ~ctxt ~pretty:true (
        "class a {\n  @a\n  @b\n  a() {}\n}"
      );
      assert_statement_string ~ctxt "class a{*b(){}}";
    end;

  "class_properties" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "class a{a;}";
      assert_statement_string ~ctxt "class a{a:a;}";
      assert_statement_string ~ctxt "class a{a;b=c;}";
      assert_statement_string ~ctxt "class a{a;b:b=c;}";
      assert_statement_string ~ctxt "class a{+a;}";
      assert_statement_string ~ctxt "class a{+a:a=a;}";
      assert_statement_string ~ctxt "class a{static a;}";
      assert_statement_string ~ctxt "class a{static +a:a=a;}";
      assert_statement_string ~ctxt ~pretty:true (
        "class a {\n  a;\n  b = c;\n  static b = c;\n}"
      );
      assert_statement_string ~ctxt ~pretty:true (
        "class a {\n  +a: a;\n  b: b = c;\n}"
      );
    end;

  "class_private_properties" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "class a{#a;}";
      assert_statement_string ~ctxt "class a{#a:a;}";
      assert_statement_string ~ctxt "class a{#a;#b=c;}";
      assert_statement_string ~ctxt "class a{#a;#b:b=c;}";
      assert_statement_string ~ctxt "class a{+#a;}";
      assert_statement_string ~ctxt "class a{+#a:a=a;}";
      assert_statement_string ~ctxt "class a{static #a;}";
      assert_statement_string ~ctxt "class a{static +#a:a=a;}";
      assert_statement_string ~ctxt ~pretty:true (
        "class a {\n  #a;\n  #b = c;\n  static #b = c;\n}"
      );
      assert_statement_string ~ctxt ~pretty:true (
        "class a {\n  +#a: a;\n  #b: b = c;\n}"
      );
    end;

  "forof_statements" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "for(let a of b){}";
      assert_statement_string ~ctxt "for(a of b){}";
      assert_statement_string ~ctxt ~pretty:true (
        "for (let a of b) {\n  a;\n}"
      );
      assert_statement_string ~ctxt (
        "async function f(){for await(let x of y){}}"
      );
    end;

  "forof_statement_without_block" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "for(a of b)x;";
      assert_statement_string ~ctxt "{for(a of b)x}";
    end;

  "forin_statement_without_block" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "for(a in b)x;";
      assert_statement_string ~ctxt "{for(a in b)x}";
    end;

  "yield_expressions" >::
    begin fun ctxt ->
      assert_expression_string ~ctxt "function* f(){yield}";
      assert_expression_string ~ctxt "function* f(){yield a}";
      assert_expression_string ~ctxt "function* f(){yield* a}";
    end;

  "meta_property_expressions" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "function F(){new.target}";
      assert_statement_string ~ctxt "function F(){new.target.name}";
    end;

  "tagged_template_expressions" >::
    begin fun ctxt ->
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
      assert_expression_string ~ctxt "(++b)``";
    end;

  "template_expressions" >::
    begin fun ctxt ->
      assert_expression_string ~ctxt "``";
      assert_expression_string ~ctxt "`${a}`";
      assert_expression_string ~ctxt "`a${b}c`";
      assert_expression_string ~ctxt "`a${b}c${d}e`";
      assert_expression_string ~ctxt "`\\``";
    end;

  "import_expressions" >::
    begin fun ctxt ->
      assert_expression_string ~ctxt {|import("a")|};
      assert_expression_string ~ctxt "import(a)";
    end;

  "jsx_element" >::
    begin fun ctxt ->
      assert_expression_string ~ctxt "<A/>";
      assert_expression_string ~ctxt "<A></A>";
      assert_expression_string ~ctxt "<A:a/>";
      assert_expression_string ~ctxt "<A.b/>";
      assert_expression_string ~ctxt "<A.b.c/>";
      assert_expression_string ~ctxt "<A><B/></A>";
      assert_expression_string ~ctxt ~pretty:true (
        "<A a=\"a\"><B /></A>"
      );

      begin
        let ast = E.jsx_element (
          J.element
            (J.identifier "A")
            ~attrs:[
              J.attr
                (J.attr_identifier "a")
                (Some (J.attr_literal (Literals.string (String.make 80 'a'))))
            ]
            ~children:[J.child_element (J.identifier "B") ~selfclosing:true]
        ) in
        let layout = L.(loc (fused [
          loc (fused [
            atom "<"; id "A";
            sequence ~break:Layout.Break_if_needed ~inline:(true, true) ~indent:0 [
              fused [
                Layout.IfBreak (empty, (atom " "));
                sequence ~break:Layout.Break_if_needed ~inline:(false, true) [
                  loc (fused [
                    id "a";
                    fused [
                      atom "=";
                      loc (fused [
                        atom "\"";
                        atom "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
                        atom "\"";
                      ]);
                    ];
                  ]);
                ];
              ];
            ];
            atom ">";
          ]);
          sequence ~break:Layout.Break_if_needed [
            loc (loc (fused [atom "<"; id "B"; pretty_space; atom "/>"]));
          ];
          loc (fused [atom "</"; id "A"; atom ">"]);
        ])) in
        assert_layout_of_expression ~ctxt layout ast;
        assert_expression ~ctxt ~pretty:true (
          "<A\n  a=\"" ^ String.make 80 'a' ^ "\">\n  <B />\n</A>"
        ) ast;
      end;

      assert_expression_string ~ctxt ~pretty:true (
        "<A a=\"a\">\n  " ^ String.make 80 'b' ^ "\n</A>"
      );
      assert_expression_string ~ctxt ~pretty:true (
        "<A><B /><C /></A>"
      );
      assert_expression_string ~ctxt ~pretty:true (
        "<A>\n  <" ^ String.make 80 'B' ^ " />\n  <C />\n</A>"
      );
      (* TODO: Utils_jsx.trim_jsx_text is overly aggressive for pretty
       *       printing, user supplied newlines between words should be
       *       maintained. The following test should pass:
       *
       *  assert_expression_string ~ctxt ~pretty:true (
       *    "<A>\n  " ^ String.make 80 'a' ^ "\n  " ^ String.make 80 'b' ^ "\n</A>"
       *  );
       *)
    end;

  "jsx_attribute" >::
    begin fun ctxt ->
      (* TODO: valueless attributes shouldnt print trailing spaces when last *)
      assert_expression_string ~ctxt "<A a />";
      assert_expression_string ~ctxt "<A a:a />";
      assert_expression_string ~ctxt "<A a={1}/>";
      assert_expression_string ~ctxt "<A a=\"\"/>";
      assert_expression_string ~ctxt "<A {...a}/>";
      assert_expression_string ~ctxt "<A a {...a}b />";
      assert_expression_string ~ctxt "<A a b />";
      assert_expression_string ~ctxt "<A a b=\"\"/>";
      assert_expression_string ~ctxt "<A a=\"a\"b={b}/>";
      assert_expression_string ~ctxt "<A b=\"\"a />";
      assert_expression_string ~ctxt "<A b={1}a />";
      assert_expression_string ~ctxt ~pretty:true (
        "<A a=\"a\" b />"
      );
      assert_expression_string ~ctxt ~pretty:true (
        "<A\n  a=\"" ^ String.make 80 'a' ^ "\"\n  b\n/>"
      );
      assert_expression_string ~ctxt ~pretty:true (
        "<A a=\"a\" b=\"b\" />"
      );
      assert_expression_string ~ctxt ~pretty:true (
        "<A\n  a=\"" ^ String.make 80 'a' ^ "\"\n  b=\"b\"\n/>"
      );
    end;

  "import_declaration_statement" >::
    begin fun ctxt ->
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
      assert_statement_string ~ctxt ~pretty:true (
        {|import {a, b} from "a";|}
      );
      assert_statement_string ~ctxt ~pretty:true (
        {|import type {a, b} from "a";|}
      );
      assert_statement_string ~ctxt ~pretty:true (
        "import {\n  a,\n  " ^ String.make 80 'b' ^ ",\n} from \"a\";"
      );
      assert_statement_string ~ctxt ~pretty:true (
        {|import a, * as b from "a";|}
      );
      assert_statement_string ~ctxt ~pretty:true (
        "import a, * as " ^ String.make 80 'b' ^ " from \"a\";"
      );
      assert_statement_string ~ctxt ~pretty:true (
        {|import a, {b} from "a";|}
      );
      assert_statement_string ~ctxt ~pretty:true (
        "import a, {\n  " ^ String.make 80 'b' ^ ",\n} from \"a\";"
      );
    end;

  "export_declaration_statement" >::
    begin fun ctxt ->
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
      assert_statement_string ~ctxt ~pretty:true "export {a} from \"a\";";
      assert_statement_string ~ctxt ~pretty:true "export * from \"a\";";
      assert_statement_string ~ctxt ~pretty:true "export * as a from \"a\";";
      assert_statement_string ~ctxt ~pretty:true "export type {a};";
      assert_statement_string ~ctxt ~pretty:true (
        "export {\n  a,\n  b as " ^ String.make 80 'c' ^ ",\n} from \"a\";"
      );
      assert_statement_string ~ctxt ~pretty:true (
        "export * as " ^ String.make 80 'a' ^ " from \"a\";"
      );
      assert_statement_string ~ctxt ~pretty:true "export opaque type a = b;";

      (* TODO: Flow does not parse this but should
      assert_statement_string ~ctxt "export a,{b}from'a';";
      assert_statement_string ~ctxt "export*as foo,{bar}from'a';"; *)
    end;

  "default_export_declaration_statement" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "export default a;";
      assert_statement_string ~ctxt "export default a=b;";
      assert_statement_string ~ctxt "export default function(){}";
      assert_statement_string ~ctxt "export default class{}";
    end;

  "type_alias_statement" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "type a=a;";
      assert_statement_string ~ctxt "type a<a>=a;";
      assert_statement_string ~ctxt ~pretty:true "type a = a;";
    end;

  "opaque_type_alias_statement" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "opaque type a=a;";
      assert_statement_string ~ctxt "opaque type a:b=a;";
      assert_statement_string ~ctxt "opaque type a<a>=a;";
      assert_statement_string ~ctxt "opaque type a<a>:b<a>=a;";
      assert_statement_string ~ctxt "opaque type a<a>:b<a>=c<a>;";
      assert_statement_string ~ctxt ~pretty:true "opaque type a = a;";
      assert_statement_string ~ctxt ~pretty:true "opaque type a: b = a;";
    end;

  "declare_opaque_type_alias_statement" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "declare opaque type a;";
      assert_statement_string ~ctxt "declare opaque type a:b;";
      assert_statement_string ~ctxt ~pretty:true "declare opaque type a: b;";
      assert_statement_string ~ctxt "declare export opaque type a;";
      assert_statement_string ~ctxt "declare export opaque type a:b;";
      assert_statement_string ~ctxt ~pretty:true "declare export opaque type a: b;";
    end;

  "type_cast_expression" >::
    begin fun ctxt ->
      assert_expression_string ~ctxt "(a:b)";
      assert_expression_string ~ctxt ~pretty:true "(a: b)";
    end;

  "type_parameter" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "type a<a>=a;";
      assert_statement_string ~ctxt "type a<a,b>=a;";
      assert_statement_string ~ctxt "type a<+a>=a;";
      assert_statement_string ~ctxt "type a<c=a>=a;";
      assert_statement_string ~ctxt "type a<a:b>=a;";
      assert_statement_string ~ctxt "type a<a:b=c>=a;";
      assert_statement_string ~ctxt "type a<a,+a:b=c>=a;";
      assert_statement_string ~ctxt ~pretty:true (
        "type a<a, +a: b = c> = a;"
      );
      assert_statement_string ~ctxt ~pretty:true (
        "type a<\n  a,\n  +a: b = " ^ String.make 80 'c' ^ ",\n> = a;"
      );
      assert_statement_string ~ctxt ~pretty:true (
        "type a<a, b> = " ^ String.make 80 'a' ^ ";"
      );
    end;

  "type" >::
    begin fun ctxt ->
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
      assert_statement_string ~ctxt "type a=a[];";
      assert_statement_string ~ctxt "type a=a.b;";
      assert_statement_string ~ctxt "type a=a.b.c;";
      assert_statement_string ~ctxt "type a=a<b>;";
      assert_statement_string ~ctxt "type a=a.b<c,d>;";
      assert_statement_string ~ctxt ~pretty:true (
        "type a = a.b<c, d>;"
      );
      assert_statement_string ~ctxt ~pretty:true (
        "type a = a.b<\n  c,\n  " ^ String.make 80 'd' ^ ",\n>;"
      );
      assert_statement_string ~ctxt "type a=typeof a;";
      assert_statement_string ~ctxt "type a=[a,b];";
      assert_statement_string ~ctxt ~pretty:true "type a = [a, b];";
      assert_statement_string ~ctxt ~pretty:true (
        "type a = [\n  a,\n  " ^ String.make 80 'b' ^ ",\n];"
      );
      assert_statement_string ~ctxt "type a=*;";
      assert_statement_string ~ctxt "type a='';";
      assert_statement_string ~ctxt "type a=1;";
      assert_statement_string ~ctxt "type a=true;";
    end;

  "type_function" >::
    begin fun ctxt ->
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
      assert_statement_string ~ctxt ~pretty:true (
        "type a = <a>(\n  a?: b,\n  " ^ String.make 80 'c' ^ ",\n) => c;"
      );
    end;

  "type_object" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "type a={};";
      assert_statement_string ~ctxt "type a={||};";
      assert_statement_string ~ctxt "type a={a:b};";
      assert_statement_string ~ctxt "type a={|a:b|};";
      assert_statement_string ~ctxt "type a={+a:b};";
      assert_statement_string ~ctxt "type a={a?:b};";
      assert_statement_string ~ctxt "type a={a:?b};";
      assert_statement_string ~ctxt "type a={a?:?b};";
      assert_statement_string ~ctxt "type a={\"a\":b};";
      assert_statement_string ~ctxt "type a={a:b};";
      assert_statement_string ~ctxt "type a={a:b,c:d};";
      assert_statement_string ~ctxt "type a={...a};";
      assert_statement_string ~ctxt "type a={a:b,...a};";
      assert_statement_string ~ctxt ~pretty:true "type a = {a: b};";
      assert_statement_string ~ctxt ~pretty:true "type a = {a: b, c: d};";
      assert_statement_string ~ctxt ~pretty:true (
        "type a = {\n  a: b,\n  c: " ^ String.make 80 'd' ^ ",\n};"
      );
      assert_statement_string ~ctxt "type a={a():b};";
      assert_statement_string ~ctxt "type a={get a():b};";
      assert_statement_string ~ctxt "type a={set a():b};";
      assert_statement_string ~ctxt ~pretty:true "type a = {set a(): b};";
      assert_statement_string ~ctxt "type a={a?:()=>a};";
      assert_statement_string ~ctxt "type a={+a:()=>a};";
      assert_statement_string ~ctxt "type a={():a};";
      assert_statement_string ~ctxt "type a={[b]:a};";
      assert_statement_string ~ctxt "type a={[a:b]:a};";
      assert_statement_string ~ctxt "type a={+[a:b]:a};";
      assert_statement_string ~ctxt ~pretty:true "type a = {+[a: b]: a};";
      assert_statement_string ~ctxt "type a={a:b,+[a:b]:a,():a,c():b};";
    end;

  "type_union_or_intersection" >::
    begin fun ctxt ->
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
      assert_statement_string ~ctxt ~pretty:true (
        "type a = \n  | a\n  | b\n  | " ^ String.make 80 'c' ^ ";"
      );
    end;

  "interface_declaration_statements" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "interface a{}";
      assert_statement_string ~ctxt "interface a extends b{}";
      assert_statement_string ~ctxt "interface a<a> extends b{}";
      assert_statement_string ~ctxt "interface a extends b,c{}";
      assert_statement_string ~ctxt ~pretty:true "interface a {}";
      assert_statement_string ~ctxt ~pretty:true "interface a extends b, c {}";
      assert_statement_string ~ctxt ~pretty:true (
        "interface a {\n  a: b,\n  d(): " ^
          String.make 80 'c' ^ ",\n}"
      );
    end;

  "declare_class_statements" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "declare class a{}";
      assert_statement_string ~ctxt "declare class a extends b{}";
      assert_statement_string ~ctxt ~pretty:true (
        "declare class a {\n  static a: b,\n  static d(): " ^
          String.make 80 'c' ^ ",\n}"
      );
    end;

  "declare_function_statements" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "declare function a():b;";
      assert_statement_string ~ctxt ~pretty:true (
        "declare function a(): b;"
      );
      assert_statement_string ~ctxt "declare function f():a%checks;";
      assert_statement_string ~ctxt "declare function f(a:b):a%checks(!a);";
      assert_statement_string ~ctxt ~pretty:true (
        "declare function f(a: b): a %checks(!a);"
      );
    end;

  "declare_var_statements" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "declare var a;";
      assert_statement_string ~ctxt "declare var a:b;";
    end;

  "declare_module_exports_statements" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "declare module.exports:a;";
    end;

  "declare_module_statements" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "declare module a{}";
      assert_statement_string ~ctxt "declare module \"a\"{}";
      assert_statement_string ~ctxt ~pretty:true "declare module \"a\" {}";
    end;

  "declare_export_declaration_statements" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "declare export default a;";
      assert_statement_string ~ctxt "declare export var a;";
      assert_statement_string ~ctxt "declare export function a():a;";
      assert_statement_string ~ctxt "declare export default function a():a;";
      assert_statement_string ~ctxt "declare export class a{}";
      assert_statement_string ~ctxt "declare export default class a{}";
      assert_statement_string ~ctxt "declare export{}";
      assert_statement_string ~ctxt "declare export{a,b}";
      assert_statement_string ~ctxt "declare export{a,b}from\"a\"";
      assert_statement_string ~ctxt "declare export*from\"a\"";
    end;

  "pattern" >::
    begin fun ctxt ->
      assert_statement_string ~ctxt "let a=a;";
      assert_statement_string ~ctxt "let a?=a;";
      assert_statement_string ~ctxt "let a:b=a;";
      assert_statement_string ~ctxt "let a?:b=a;";
      assert_statement_string ~ctxt "let {}=a;";
      assert_statement_string ~ctxt "let {}:b=a;";
      assert_statement_string ~ctxt "let {a}=a;";
      assert_statement_string ~ctxt "let {a:b}=a;";
      assert_statement_string ~ctxt "let {a:b}=a;";
      assert_statement_string ~ctxt "let {a,b}=a;";
      assert_statement_string ~ctxt "let {a,b:{c}}=a;";
      assert_statement_string ~ctxt "let {a=b}=a;";
      assert_statement_string ~ctxt "let {a:b=c}=a;";
      assert_statement_string ~ctxt "let {a=++b}=a;";
      assert_statement_string ~ctxt "let {...a}=a;";
      assert_statement_string ~ctxt "let {a,...b}=a;";
      assert_statement_string ~ctxt ~pretty:true "let {a} = a;";
      assert_statement_string ~ctxt ~pretty:true "let {a: b} = a;";
      assert_statement_string ~ctxt ~pretty:true "let {a: b, c} = a;";
      assert_statement_string ~ctxt ~pretty:true "let {a: b, c = d} = a;";
      assert_statement_string ~ctxt ~pretty:true "let {...a} = a;";
      assert_statement_string ~ctxt ~pretty:true (
        "let {\n  a: b,\n  c = " ^ String.make 80 'd' ^ ",\n} = a;"
      );
      assert_statement_string ~ctxt "let []=a;";
      assert_statement_string ~ctxt "let []:a=a;";
      assert_statement_string ~ctxt "let [a]=a;";
      assert_statement_string ~ctxt "let [a?]=a;";
      assert_statement_string ~ctxt "let [a:b]=a;";
      assert_statement_string ~ctxt "let [a?:b]=a;";
      assert_statement_string ~ctxt "let [a,b]=a;";
      assert_statement_string ~ctxt "let [,,a]=a;";
      assert_statement_string ~ctxt "let [[]]=a;";
      assert_statement_string ~ctxt "let [,,[a]]=a;";
      assert_statement_string ~ctxt "let [...a]=a;";
      assert_statement_string ~ctxt "let [a,...b]=a;";
      assert_statement_string ~ctxt ~pretty:true "let [a, b] = a;";
      assert_statement_string ~ctxt ~pretty:true "let [a, ...b] = a;";
      assert_statement_string ~ctxt ~pretty:true "let [a, , b] = a;";
      assert_statement_string ~ctxt ~pretty:true (
        "let [\n  a,\n  ,\n  " ^ String.make 80 'b' ^ ",\n] = a;"
      );
    end;

  "program_artifact_newline" >::
    begin fun ctxt ->
      let ast = mk_program [
        S.expression (E.identifier "x");
      ] in
      let layout = Js_layout_generator.program
        ~preserve_docblock:false
        ~checksum:(Some "@artifact abc123")
        ast
      in
      let expected =
        L.(program (
          sequence ~break:Layout.Break_always ~inline:(false, true) ~indent:0 [
            sequence ~break:Layout.Break_if_pretty ~inline:(true, true) ~indent:0 [
              loc (fused [loc (id "x"); atom ";"])
            ];
            atom "/* @artifact abc123 */"
          ]
        ))
      in
      assert_layout ~ctxt expected layout
    end;

  "program_trailing_semicolon" >::
    begin fun ctxt ->
      let ast = mk_program [
        S.expression (E.identifier "x");
        S.expression (E.identifier "y");
      ] in
      let layout = Js_layout_generator.program
        ~preserve_docblock:false
        ~checksum:None
        ast
      in
      let expected =
        L.(program (
          fused_vertically ~inline:(true, true) [
            loc (fused [loc (id "x"); atom ";"]);
            loc (fused [loc (id "y"); atom ";"]);
          ]
        ))
      in
      assert_layout ~ctxt expected layout
    end;

  "regexp" >::
    begin fun ctxt ->
      (* flags should be sorted *)
      let regexp = expression_of_string "/foo/ymg" in
      assert_expression ~ctxt "/foo/gmy" regexp
    end;

  "string_literal_quotes" >::
    begin fun ctxt ->
      assert_expression ~ctxt {|"'''"|} (expression_of_string {|"'''"|});
      assert_expression ~ctxt {|'"'|} (expression_of_string {|"\""|});
      assert_expression ~ctxt {|"''"|} (expression_of_string {|'\'\''|});
      assert_expression ~ctxt {|"''\""|} (expression_of_string {|"''\""|});
      assert_expression ~ctxt {|'""\''|} (expression_of_string {|'""\''|});
    end;

  "switch_case_space" >::
    begin fun ctxt ->
      let assert_no_space ~ctxt expr =
        let ret = statement_of_string ("switch(x){case "^expr^":break}") in
        assert_statement ~ctxt ("switch(x){case"^expr^":break}") ret
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
      assert_statement ~ctxt "switch(x){case 123:break}" ret;
    end;

  "throw_space" >::
    begin fun ctxt ->
      let assert_no_space ~ctxt expr =
        let ret = statement_of_string ("throw "^expr^";") in
        assert_statement ~ctxt ("throw"^expr^";") ret
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
      assert_statement_string ~ctxt "throw new Error();";
    end;

  "unicode_string_literal" >::
    begin fun ctxt ->
      (* escaped using Unicode codepoint *)
      let ast = expression_of_string {|"\u{1F4A9}"|} in
      assert_expression ~ctxt {|"\ud83d\udca9"|} ast;

      (* escaped using UTF-16 (hex get lowercased) *)
      let ast = expression_of_string {|"\uD83D\uDCA9"|} in
      assert_expression ~ctxt {|"\ud83d\udca9"|} ast;

      (* literal emoji *)
      let ast = expression_of_string "\"\xF0\x9F\x92\xA9\"" in
      assert_expression ~ctxt {|"\ud83d\udca9"|} ast;

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
      assert_expression ~ctxt {|"\\"|} ast;
    end;

    "numbers" >::
      begin fun ctxt ->
        assert_expression ~ctxt "100" (expression_of_string "1e2");
        assert_expression ~ctxt "1e3" (expression_of_string "1000");
        assert_expression ~ctxt "2592e6" (expression_of_string "2.592e+09");
      end
]
