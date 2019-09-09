(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module S = Flow_ast.Statement
module E = Flow_ast.Expression
module T = Flow_ast.Type
module P = Flow_ast.Pattern
module Utils = Flowtestgen_utils

(* ESSENTIAL: Syntax type and related functions *)
module Syntax = Syntax_base
open Ruleset_base

class ruleset_rtest =
  object (self)
    inherit Ruleset_base.ruleset_base

    method! get_name () : string = "rtest"

    method! weak_assert b = if (not b) && Random.int 3 > 0 then raise Engine.Backtrack

    (* Rule for declaring a variable with init and type annotation *)
    method! rule_vardecl_with_type (env : env_t) : Syntax.t * env_t =
      (* require an expression from the environment *)
      let rhs = self#choose 0 (fun () -> self#require_expr env) in
      let (rhs_expr, rhs_type) =
        match rhs with
        | Expr (e, t) -> (e, t)
        | _ -> failwith "This has to be an expression"
      in
      (* require a type from the environment.*)
      let vtype = self#choose 1 (fun () -> self#require_type env) in
      let vtype =
        match vtype with
        | Type t -> t
        | _ -> failwith "This has to a type"
      in
      (* assert the subtyping relationhips between the rhs and lhs *)
      self#weak_assert (self#is_subtype rhs_type vtype);
      let vname = Utils.mk_var () in
      let var_decl = Syntax.mk_vardecl ~etype:vtype vname rhs_expr in
      let new_env =
        self#add_binding
          env
          (Expr (E.Identifier (Flow_ast_utils.ident_of_source (Loc.none, vname)), vtype))
      in
      let new_env = self#add_binding new_env (Type vtype) in
      (var_decl, new_env)

    method! get_all_rules () =
      [| self#rule_num_lit;
         self#rule_str_lit;
         self#rule_obj_lit 2 0;
         self#rule_vardecl_with_type;
         self#rule_vardecl_with_type;
         self#rule_runtime_check |]
  end
