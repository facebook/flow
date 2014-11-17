(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module M = Map_ast
module CE = Common_exns
open Ast

(*
  we run a new mapper on a function body to check for the occurences of
  yield and yield_break. However, if there is a function defined inside
  the function body, we just spit it out without touching it. It'll get
  eventually picked up by the outer mapper.
  Example:
    old: function foo() {
          yield 5;
          yield break;
        }
    new: function foo() {
          yield 5;
          return;
        }
*)

let dummy_yield p =
  If ((p, False), [Expr (p, Yield (AFvalue (p, False)))], [Noop])

let inner_map b =
  let (has_yield, has_yield_break) = (ref false, ref false) in
  let inner_mapper = M.mk_mapper { M.default_mapper with
    M.k_expr_ = begin fun (k, _) expr_ -> match expr_ with
                                          | Lfun _ | Efun _ -> expr_
                                          | Yield _ ->
                                              has_yield := true;
                                              k expr_
                                          | _ -> k expr_
                                          end;
    M.k_stmt = begin fun (k, _) stmt -> match stmt with
                                        | Expr (p, Yield_break) ->
                                              has_yield_break := true;
                                              k (Return (p, None))
                                        | _ -> k stmt
                                        end;
  } in
  let b = match inner_mapper (`MBlock b) with
  | `MBlock b -> b
  | _ -> raise CE.Impossible in
  (!has_yield, !has_yield_break, b)

let strip_yield_break block (p, _) =
  let (has_yield, has_yield_break, block) = inner_map block in
  if has_yield_break && (not has_yield)
  then (dummy_yield p)::block
  else block

let map =
  M.mk_program_mapper { M.default_mapper with
    M.k_fun_ = (fun (k, _) ({f_body; f_name; _} as fun_) ->
      k {fun_ with f_body = (strip_yield_break f_body f_name)});
    M.k_method_ = (fun (k, _) _ ({m_body; m_name; _} as method_) ->
      k {method_ with m_body = (strip_yield_break m_body m_name)});
  }
