(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 * Driver for unit tests of the hack_spatch module.
 *)
open Hh_match_utils

class replace_with_self_stmt_visitor (src:string) =
object inherit [patch list] AstVisitor.ast_visitor as super
  val src = src
  method! on_stmt acc s =
    begin
      let wrapped = Stmt s in
      let newpatch =
        Patcher.create_patch
          ~src ~src_node:wrapped ~tgt_node:wrapped ~adjust_ranges:true in
      let acc = match newpatch with
        | None -> acc
        | Some patch -> patch :: acc in
      super#on_stmt acc s;
    end
end

class replace_with_self_expr_visitor (src:string) =
object inherit [patch list] AstVisitor.ast_visitor as super
  val src = src
  method! on_expr acc e =
    begin
      let wrapped = Expr e in
      let newpatch =
        Patcher.create_patch
          ~src ~src_node:wrapped ~tgt_node:wrapped ~adjust_ranges:false in
      let acc = match newpatch with
        | None -> acc
        | Some patch ->
           (* so the reason for this is if we are just iterating over the AST
              indiscriminately transforming expressions we transform some things
              that should never be transformed (mainly a specific expr in for
              loops that has a bad position from the parser) *)
           let initialcode =
             String.sub src patch.start_loc (patch.end_loc - patch.start_loc) in
           if String.contains initialcode ';'
           then acc
           else
             patch :: acc in
      super#on_expr acc e;
    end
end

let run_patch_test file = begin
  let parsed_file = Ast_code_extent.parse_file file in
  let contents = Sys_utils.cat (Relative_path.to_absolute file) in
  let run_single_test prefix visitor = begin
    let patches = visitor#on_program [] parsed_file.Parser_hack.ast in
    print_endline prefix;
    print_endline
      (Patcher.apply_patches ~format_result:true ~src:contents ~patches);
    print_endline "\n\n=====\n\n"; end in
  (* try patching each type of construct individually *)
  run_single_test "Stmt test:" (new replace_with_self_stmt_visitor contents);
  run_single_test "Expr test:" (new replace_with_self_expr_visitor contents);
  end

let _ =
  begin
  let fname = Sys.argv.(1) in
  let _handle = SharedMem.init_default () in
  Hhi.set_hhi_root_for_unit_test (Path.make "/tmp/hhi");
  let fpath = (Relative_path.create Relative_path.Dummy fname) in
  run_patch_test fpath
  end
