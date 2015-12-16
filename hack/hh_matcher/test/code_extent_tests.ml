(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 * Driver for tests of the hack AST visitor and the Ast_code_extent module.
 *)
open Sys_utils

module Ace = Ast_code_extent

class code_extent_test_visitor
        (file : Relative_path.t)
        (content : string) =
object
  inherit [unit] AstVisitor.ast_visitor as super

  val file = file;
  val content = content;

  method! on_def acc d =
    begin
      print_endline "on_def:\n";
      let code_range = Ace.source_extent_def file content d in
      Hh_match_test_utils.pretty_print_test_output content code_range;
      super#on_def acc d;
    end
  method! on_catch acc c =
    begin
      print_endline "on_catch:\n";
      let code_range = Ace.source_extent_catch file content c in
      Hh_match_test_utils.pretty_print_test_output content code_range;
      super#on_catch acc c;
    end
  method! on_do acc b e =
    begin
      print_endline "on_do:\n";
      let code_range = Ace.source_extent_do file content b e in
      Hh_match_test_utils.pretty_print_test_output content code_range;
      super#on_do acc b e;
    end
  method! on_efun acc f ibl =
    begin
      print_endline "on_efun:\n";
      let code_range = Ace.source_extent_efun file content f ibl in
      Hh_match_test_utils.pretty_print_test_output content code_range;
      super#on_efun acc f ibl;
    end
  method! on_eif acc e1 eopt e2 =
    begin
      print_endline "on_eif:\n";
      let code_range = Ace.source_extent_eif file content e1 eopt e2 in
      Hh_match_test_utils.pretty_print_test_output content code_range;
      super#on_eif acc e1 eopt e2;
    end
  method! on_expr acc e =
    begin
      print_endline "on_expr:\n";
      let code_range = Ace.source_extent_expr file content e in
      Hh_match_test_utils.pretty_print_test_output content code_range;
      super#on_expr acc e;
    end
  method! on_for acc e1 e2 e3 b =
    begin
      print_endline "on_for:\n";
      let code_range = Ace.source_extent_for file content e1 e2 e3 b in
      Hh_match_test_utils.pretty_print_test_output content code_range;
      super#on_for acc e1 e2 e3 b;
    end
  method! on_foreach acc e popt ae b =
    begin
      print_endline "on_foreach:\n";
      let code_range = Ace.source_extent_foreach file content e popt ae b in
      Hh_match_test_utils.pretty_print_test_output content code_range;
      super#on_foreach acc e popt ae b;
    end
  method! on_hint acc h =
    begin
      print_endline "on_hint:\n";
      let code_range = Ace.source_extent_hint file content h in
      Hh_match_test_utils.pretty_print_test_output content code_range;
      super#on_hint acc h;
    end
  method! on_if acc e b1 b2 =
    begin
      print_endline "on_if:\n";
      let code_range = Ace.source_extent_if file content e b1 b2 in
      Hh_match_test_utils.pretty_print_test_output content code_range;
      super#on_if acc e b1 b2;
    end
  method! on_lfun acc f =
    begin
      print_endline "on_lfun:\n";
      let code_range = Ace.source_extent_lfun file content f in
      Hh_match_test_utils.pretty_print_test_output content code_range;
      super#on_lfun acc f;
    end
  method! on_stmt acc s =
    begin
      print_endline "on_stmt:\n";
      let code_range = Ace.source_extent_stmt file content s in
      Hh_match_test_utils.pretty_print_test_output content code_range;
      super#on_stmt acc s;
    end
  method! on_switch acc e cl =
    begin
      print_endline "on_switch:\n";
      let code_range = Ace.source_extent_switch file content e cl in
      Hh_match_test_utils.pretty_print_test_output content code_range;
      super#on_switch acc e cl;
    end
  method! on_throw acc e =
    begin
      print_endline "on_throw:\n";
      let code_range = Ace.source_extent_throw file content e in
      Hh_match_test_utils.pretty_print_test_output content code_range;
      super#on_throw acc e;
    end
  method! on_try acc b1 cl b2 =
    begin
      print_endline "on_try:\n";
      let code_range = Ace.source_extent_try file content b1 cl b2 in
      Hh_match_test_utils.pretty_print_test_output content code_range;
      super#on_try acc b1 cl b2;
    end
  method! on_while acc e b =
    begin
      print_endline "on_while:\n";
      let code_range = Ace.source_extent_while file content e b in
      Hh_match_test_utils.pretty_print_test_output content code_range;
      super#on_while acc e b;
    end
  method! on_class_ acc c =
    begin
      print_endline "on_class_:\n";
      let code_range = Ace.source_extent_class_ file content c in
      Hh_match_test_utils.pretty_print_test_output content code_range;
      super#on_class_ acc c;
    end
  method! on_fun_ acc f =
    begin
      print_endline "on_fun_:\n";
      let code_range = Ace.source_extent_fun_ file content f in
      Hh_match_test_utils.pretty_print_test_output content code_range;
      super#on_fun_ acc f;
    end
  method! on_method_ acc m =
    begin
      print_endline "on_method_:\n";
      let code_range = Ace.source_extent_method_ file content m in
      Hh_match_test_utils.pretty_print_test_output content code_range;
      super#on_method_ acc m;
    end
  method! on_program acc p =
    begin
      print_endline "on_program:\n";
      let code_range = Ace.source_extent_program file content p in
      Hh_match_test_utils.pretty_print_test_output content code_range;
      super#on_program acc p;
    end
end

let run_test file : unit =
  let parser_return = Ace.parse_file file in
  let abs_fn = Relative_path.to_absolute file in
  let content = cat abs_fn in
  let test_visitor = new code_extent_test_visitor file content in
  test_visitor#on_program () (parser_return.Parser_hack.ast)

let _ =
  begin
  let fname = Sys.argv.(1) in
  let _handle = SharedMem.init_default () in
  Hhi.set_hhi_root_for_unit_test (Path.make "/tmp/hhi");
  run_test (Relative_path.create Relative_path.Dummy fname);
  end
