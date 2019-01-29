(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

open OUnit2
open Test_utils

let mk_comments_test
  contents
  expected_attached_comments
  expected_unattached_comments =
  begin fun ctxt ->
    let info = Comment_attacher.program (parse contents) in
    let attached_comments = Utils_js.LocMap.fold
      (fun loc comments a -> a @ [
          (
            loc,
            Core_list.map ~f:(fun (_, (_, comment)) ->
              match comment with | Ast.Comment.Block s | Ast.Comment.Line s -> s
            ) comments
          )
        ]
      )
      info.Comment_attacher.attached_comments
      []
    in
    let unattached_comments = List.map
      (fun (_, comment) ->
        match comment with | Ast.Comment.Block s | Ast.Comment.Line s -> s
      )
      info.Comment_attacher.unattached_comments
    in
    let printer_attached = print_list (fun (loc, s) ->
      Loc.to_string loc ^ " ~ \"" ^ (String.concat "\", \"" s) ^ "\""
    ) in
    let printer_unattached = String.concat ", " in
    assert_equal ~ctxt
      ~cmp:(eq printer_attached)
      ~printer:printer_attached
      ~msg:"Defs of all uses don't match!"
      expected_attached_comments attached_comments;
    assert_equal ~ctxt
      ~cmp:(eq printer_unattached)
      ~printer:printer_unattached
      ~msg:"Defs of all uses don't match!"
      expected_unattached_comments unattached_comments
  end

let tests = "scope_builder" >::: [
  "simple_fn" >:: mk_comments_test
    "//1
    function foo() {}"
    [
      mk_loc (2, 4) (2, 21), ["1"];
    ]
    [];

  "multi_fn" >:: mk_comments_test
    "//1
    //2
    /*3*/
    function foo() {}"
    [
      mk_loc (4, 4) (4, 21), ["1"; "2"; "3"];
    ]
    [];

  "var_fn" >:: mk_comments_test
    "//1
    const A = function() {}"
    [
      mk_loc (2, 4) (2, 27), ["1"];
    ]
    [];

  "class_element" >:: mk_comments_test
    "class A {
      //1
      a() {
        //2
        a;
      }
    }"
    [
      mk_loc (3, 6) (6, 7), ["1"];
      mk_loc (5, 8) (5, 10), ["2"];
    ]
    [];

  "object_properties" >:: mk_comments_test
    "const A = {
      //1
      a() {
        //2
        a;
      }
    }"
    [
      mk_loc (3, 6) (6, 7), ["1"];
      mk_loc (5, 8) (5, 10), ["2"];
    ]
    [];
]
