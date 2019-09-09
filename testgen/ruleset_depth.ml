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

class ruleset_depth =
  object (self)
    inherit Ruleset_base.ruleset_base

    method! get_name () : string = "depth"

    method! weak_assert b = self#backtrack_on_false b

    method! is_subtype_obj (o1 : (Loc.t, Loc.t) T.Object.t) (o2 : (Loc.t, Loc.t) T.Object.t) =
      let get_prop_set (o : (Loc.t, Loc.t) T.Object.t) =
        let tbl = Hashtbl.create 1000 in
        T.Object.Property.(
          List.iter
            (fun p ->
              match p with
              | T.Object.Property
                  ( _,
                    {
                      key = E.Object.Property.Identifier (_, name);
                      value = Init (_, t);
                      optional = _;
                      static = _;
                      proto = _;
                      _method = _;
                      variance = _;
                    } ) ->
                Hashtbl.add tbl name t
              | _ -> ())
            T.Object.(o.properties);
          tbl)
      in
      let s1 = get_prop_set o1 in
      let s2 = get_prop_set o2 in
      let subtype = ref true in
      (* check non optional properties *)
      Hashtbl.iter
        (fun n t ->
          (* Shouldn't use call is_subtyping recursivingly. We should
           use equality to limit depth subtyping *)
          if (not (Hashtbl.mem s1 n)) || not (self#is_subtype (Hashtbl.find s1 n) t) then
            subtype := false)
        s2;
      !subtype

    (* A helper funtions for wrapping an expression and a type
     into an object for mutation and expose type errors. *)
    method wrap_in_obj (expr : (Loc.t, Loc.t) E.t') (etype : (Loc.t, Loc.t) T.t')
        : (Loc.t, Loc.t) E.t' * (Loc.t, Loc.t) T.t' =
      let pname = "p_0" in
      let obj_expr =
        let prop =
          E.Object.Property.(
            E.Object.Property
              ( Loc.none,
                Init
                  {
                    key = Identifier (Flow_ast_utils.ident_of_source (Loc.none, pname));
                    value = (Loc.none, expr);
                    shorthand = false;
                  } ))
        in
        let properties = [prop] in
        E.Object.(E.Object { properties; comments = Flow_ast_utils.mk_comments_opt () })
      in
      let obj_type =
        T.Object.Property.(
          let prop_type =
            T.Object.Property
              ( Loc.none,
                {
                  key =
                    E.Object.Property.Identifier (Flow_ast_utils.ident_of_source (Loc.none, pname));
                  value = Init (Loc.none, etype);
                  optional = false;
                  static = false;
                  proto = false;
                  _method = false;
                  variance = None;
                } )
          in
          T.Object.(T.Object { exact = false; properties = [prop_type]; inexact = true }))
      in
      (obj_expr, obj_type)

    (* property update rule *)
    method! rule_prop_update (env : env_t) : Syntax.t * env_t =
      (* get an object variable *)
      let obj = self#choose 0 (fun () -> self#require_expr env) in
      self#backtrack_on_false
        (match obj with
        | Expr (E.Identifier _, T.Object _) -> true
        | _ -> false);
      let (oexpr, otype) =
        match obj with
        | Expr (e, t) -> (e, t)
        | _ -> failwith "This has to be an expression"
      in
      let prop = self#choose 1 (fun () -> self#require_prop otype true) in
      let (pexpr, ptype) =
        match prop with
        | Expr (e, t) -> (e, t)
        | _ -> failwith "This has to be an expression"
      in
      self#backtrack_on_false
        (match ptype with
        | T.Object _ -> true
        | _ -> false);

      (* get the expression on the rhs of the update *)
      let rhs = self#choose 2 (fun () -> self#require_expr env) in
      let (rhs_expr, rhs_type) =
        match rhs with
        | Expr (e, t) -> (e, t)
        | _ -> failwith "This has to be an expression"
      in
      self#backtrack_on_false
        (match rhs_expr with
        | E.Object _ -> true
        | _ -> false);

      (* assert that type(rhs) <: type(prop) *)
      self#weak_assert (self#is_subtype rhs_type ptype);

      (* produce a write syntax *)
      let write =
        Syntax.mk_prop_write (Utils.string_of_expr oexpr) (Utils.string_of_expr pexpr) rhs_expr
      in
      (* update the type of the object *)
      let ret_type =
        let o_type =
          match otype with
          | T.Object o -> o
          | _ -> failwith "Has to be an object type"
        in
        T.Object o_type
      in
      let new_env = self#add_binding env (Expr (oexpr, ret_type)) in
      let new_env = self#add_binding new_env (Type ret_type) in
      (write, new_env)

    (* A rule for generating object literals *)
    method! rule_obj_lit (prop_num : int) (opt_num : int) (env : env_t) : Syntax.t * env_t =
      let (lit, lit_expr, ret_type) = self#gen_obj_lit prop_num opt_num env in
      let (wrap_expr, wrap_ret_type) = self#wrap_in_obj lit_expr ret_type in
      let new_env =
        self#add_binding
          (self#add_binding env (Expr (lit_expr, ret_type)))
          (Expr (wrap_expr, wrap_ret_type))
      in
      let new_env = self#add_binding new_env (Type ret_type) in
      let new_env = self#add_binding new_env (Type wrap_ret_type) in
      (lit, new_env)

    (* Rule for declaring a variable with init and type annotation *)
    method! rule_vardecl_with_type (env : env_t) : Syntax.t * env_t =
      (* require an expression from the environment *)
      let rhs = self#choose 0 (fun () -> self#require_expr env) in
      let (rhs_expr, rhs_type) =
        match rhs with
        | Expr (e, t) -> (e, t)
        | _ -> failwith "This has to be an expression"
      in
      (* For fast search on depth-subtyping *)
      self#backtrack_on_false
        (match (rhs_expr, rhs_type) with
        | (E.Identifier _, _) -> true
        | (_, T.Object _) -> true
        | _ -> false);

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
         self#rule_obj_lit 2 0;
         self#rule_vardecl_with_type;
         self#rule_obj_lit 1 0;
         self#rule_vardecl_with_type;
         self#rule_prop_update;
         self#rule_runtime_check |]
  end

class ruleset_random_depth =
  object
    inherit ruleset_depth

    method! weak_assert b = if (not b) && Random.int 3 > 0 then raise Engine.Backtrack
  end
