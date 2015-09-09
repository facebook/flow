(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

(* Given a Ast.program, give me the list of entities it defines *)
let get_defs ast =
  (* fold_right traverses the file from top to bottom, and as such gives nicer
   * error messages than fold_left. E.g. in the case where a function is
   * declared twice in the same file, the error will say that the declaration
   * with the larger line number is a duplicate. *)
  List.fold_right ast ~init:([], [], [], [])
    ~f:begin fun def (acc1, acc2, acc3, acc4 as acc) ->
      match def with
      | Ast.Fun f -> f.Ast.f_name :: acc1, acc2, acc3, acc4
      | Ast.Class c -> acc1, c.Ast.c_name :: acc2, acc3, acc4
      | Ast.Typedef t -> acc1, acc2, t.Ast.t_id :: acc3, acc4
      | Ast.Constant cst -> acc1, acc2, acc3, cst.Ast.cst_name :: acc4
      (* namespace nodes will never appear as long as elaborate_namespaces is
       * set to true when the parser is invoked *)
      | Ast.Namespace _
      | Ast.NamespaceUse _ -> assert false
       (* toplevel statements are ignored *)
      | Ast.Stmt _ -> acc
    end
