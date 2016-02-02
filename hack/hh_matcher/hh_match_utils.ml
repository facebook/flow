(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
module Ace = Ast_code_extent
(* To allow keeping track of patches for applying them all at once
   because it's cheaper and won't result in an unparseable intermediate
   file. *)
type patch = {
    (* Character offsets for the substring in the source file to be replaced
       (can get these from Ast_code_extent) range is [start_loc, end_loc) *)
    start_loc : int;
    end_loc : int;
    (* String to replace [start_loc, end_loc) in the source file with *)
    result_str : string;
    (* How to adjust the range for the node if necessary (based off node type)
       for situations like expanding the extent of statements to include
       semicolons, newlines, leading spaces) *)
    range_adjustment_fn : string -> int -> int -> int * int;
  }

(* return value of patcher - contains associative lists for all the
   transformations specified by a pattern, target pair *)
type patch_maps = {
    stmt_delete_list : Ast.stmt list;
    expr_delete_list : Ast.expr list;
    (* assoc list, see explanation in Patcher.pat_acc *)
    stmt_transf_map : (Ast.stmt * Ast.stmt list) list;
    expr_transf_map : (Ast.expr * Ast.expr list) list;
  }

(* Wrapper for all the possible AST nodes where we can start making
   a match or start a patch *)
type ast_node =
  | Program of Ast.program
  | Def of Ast.def

  | Fun_ of Ast.fun_
  | Tparam of Ast.tparam
  | Fun_param of Ast.fun_param

  | Class_ of Ast.class_
  | Class_elt of Ast.class_elt

  | Method of Ast.method_

  | Stmt of Ast.stmt
  | Expr of Ast.expr
  | Hint of Ast.hint
  (* TODO Temporary solution until I figure out what nodes I need here *)
  | Nodes of ast_node list
  | DummyNode

module NastToAst = struct
    (* Tags that parallel all possible Nast.stmts *)
    type stmt_tag =
      | Expr
      | Break
      | Continue
      | Return
      | Throw | While | If
      | For | Switch | Foreach
      | Static_var
      | Do | Try
      | Noop | Fallthrough

    let nast_to_tag (stm : Nast.stmt) : stmt_tag =
      match stm with
      | Nast.Expr _ -> Expr
      | Nast.Break _ -> Break
      | Nast.Continue _ -> Continue
      | Nast.Return _ -> Return
      | Nast.Throw _ -> Throw
      | Nast.While _ -> While
      | Nast.If _ -> If
      | Nast.For _ -> For
      | Nast.Switch _ -> Switch
      | Nast.Foreach _ -> Foreach
      | Nast.Static_var _ -> Static_var
      | Nast.Do _ -> Do
      | Nast.Try _ -> Try
      | Nast.Noop -> Noop
      | Nast.Fallthrough -> Fallthrough

    (* Accumulator for finding the stmt:
       fst = has found the position we're looking for
       snd = has found the stmt type we're looking for
       3rd = actual stmt, will be set by the first stmt ancestor
             of the node that sets the snd flag
             (reason for not constructing the stmt when we find the node
              type is for shorter code, and the first stmt covering the stmt
              we match is garanteed to be that stmt) *)
    type transform_acc = bool * bool * Ast.stmt option

    (* For converting the Nast to the Ast *)
    (* takes a file and a target position (grabbed from the target Nast node
       using Ai_utils.pos_for_stm
       NOTE: works only on stmts *)
    class nast_ast_transformer
            (content : string)
            (target : Pos.t)
            (target_tag : stmt_tag) =
    object (this)
      inherit [transform_acc] Ast_visitor.ast_visitor as super

      val content = content;
      val tgt_tag = target_tag;
      val tgt_pos = target;

      (* For getting the stmt that corresponds to the Nast stmt*)
      method! on_stmt acc s =
        begin
          let acc = super#on_stmt acc s in
          let found_range, found_stm, stm_opt = acc in
          match stm_opt with
          | None when found_range && found_stm ->
             found_range, found_stm, Some s
          | _ -> acc
        end

      (* For getting source extents *)
      method! on_do acc b e =
        begin
          let found_range, found_stm, stm_opt = super#on_do acc b e in
          if found_range
          then found_range, found_stm || tgt_tag = Do, stm_opt
          else found_range, found_stm, stm_opt
        end
      method! on_for acc e1 e2 e3 b =
        begin
          let found_range, found_stm, stm_opt = super#on_for acc e1 e2 e3 b in
          if found_range
          then found_range, found_stm || tgt_tag = For, stm_opt
          else found_range, found_stm, stm_opt
        end
      method! on_foreach acc e popt ae b =
        begin
          let found_range, found_stm, stm_opt =
            super#on_foreach acc e popt ae b in
          if found_range
          then found_range, found_stm || tgt_tag = Foreach, stm_opt
          else found_range, found_stm, stm_opt
        end
      method! on_if acc e b1 b2 =
        begin
          let found_range, found_stm, stm_opt = super#on_if acc e b1 b2 in
          if found_range
          then found_range, found_stm || tgt_tag = If, stm_opt
          else found_range, found_stm, stm_opt
        end
      method! on_switch acc e cl =
        begin
          let found_range, found_stm, stm_opt = super#on_switch acc e cl in
          if found_range
          then found_range, found_stm || tgt_tag = Switch, stm_opt
          else found_range, found_stm, stm_opt
        end
      method! on_throw acc e =
        begin
          let found_range, found_stm, stm_opt = super#on_throw acc e in
          if found_range
          then found_range, found_stm || tgt_tag = Throw, stm_opt
          else found_range, found_stm, stm_opt
        end
      method! on_try acc b1 cl b2 =
        begin
          let found_range, found_stm, stm_opt = super#on_try acc b1 cl b2 in
          if found_range
          then found_range, found_stm || tgt_tag = Try, stm_opt
          else found_range, found_stm, stm_opt
        end
      method! on_while acc e b =
        begin
          let found_range, found_stm, stm_opt = super#on_while acc e b in
          if found_range
          then found_range, found_stm || tgt_tag = While, stm_opt
          else found_range, found_stm, stm_opt
        end

      (* For checking when we hit the pos we're looking for *)
      method private update_found_pos pos acc =
        begin
          let _, found_stm, stm_opt = acc in
          if pos = tgt_pos
          then true, found_stm, stm_opt
          else acc
        end

      (* statements with positions *)
      method! on_expr acc e =
        begin
          let found_range, found_stm, stm_opt =
            this#update_found_pos (fst e) (super#on_expr acc e) in
          if found_range
          then found_range, found_stm || tgt_tag = Expr, stm_opt
          else found_range, found_stm, stm_opt
        end
      method! on_break acc p =
        begin
          let found_range, found_stm, stm_opt =
            this#update_found_pos p (super#on_break acc p) in
          if found_range
          then found_range, found_stm || tgt_tag = Break, stm_opt
          else found_range, found_stm, stm_opt
        end
      method! on_continue acc p =
        begin
          let found_range, found_stm, stm_opt =
            this#update_found_pos p (super#on_continue acc p) in
          if found_range
          then found_range, found_stm || tgt_tag = Continue, stm_opt
          else found_range, found_stm, stm_opt
        end
      method! on_return acc p eopt =
        begin
          let found_range, found_stm, stm_opt =
            this#update_found_pos p (super#on_return acc p eopt) in
          if found_range
          then found_range, found_stm || tgt_tag = Return, stm_opt
          else found_range, found_stm, stm_opt
        end

      method! on_id acc id =
        begin
          this#update_found_pos (fst id) (super#on_id acc id)
        end
      method! on_pstring acc pstr =
        begin
          this#update_found_pos (fst pstr) (super#on_pstring acc pstr)
        end
      method! on_hint acc h =
        begin
          this#update_found_pos (fst h) (super#on_hint acc h)
        end
    end
  end

open NastToAst

(* Given a file, target_pos (position corresponding to a part of the stmt),
   target_stmt (the stmt from the Nast)
   Returns the code extent and corresponding Ast.stmt if they can be found *)
let nast_to_extent_stmt
      ~(file : Relative_path.t)
      ~(target_pos : Pos.t)
      ~(target_stmt : Nast.stmt) :
      Ast.stmt option =
  if target_pos = Pos.none
  then None
  else
  let abs_fn = Relative_path.to_absolute file in
  let content = Sys_utils.cat abs_fn in
  let visitor =
    new nast_ast_transformer content target_pos (nast_to_tag target_stmt) in
  let parse_errs, parse_res =
    Errors.do_ (fun () -> Parser_hack.program file content) in
  (* If we can't parse the file, return None *)
  match parse_errs with
  | [] ->
     let _, _, res =
       visitor#on_program (false, false, None) parse_res.Parser_hack.ast in
     res
  | _ -> None
