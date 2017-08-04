(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module S = Ast.Statement;;
module E = Ast.Expression;;
module T = Ast.Type;;
module P = Ast.Pattern;;
module Utils = Flowtestgen_utils;;
module FRandom = Utils.FRandom;;

(* ESSENTIAL: Syntax type and related functions *)
module Syntax = Syntax_base;;
open Ruleset_base;;

class ruleset_union (depth : int) = object(self)
  inherit Ruleset_base.ruleset_base depth

  method! weak_assert b = self#backtrack_on_false b

  (* A helper funtions for wrapping an expression and a type
     into an object for mutation and expose type errors. *)
  method wrap_in_obj (expr : E.t') (etype : T.t') : (E.t' * T.t') =
    let pname = "p_0" in
    let obj_expr =
      let prop =
        let open E.Object.Property in
        E.Object.Property (Loc.none, {key = Identifier (Loc.none, pname);
                    value = Init (Loc.none, expr);
                    _method = false;
                    shorthand = false}) in
      let properties = [prop] in
      E.Object.(E.Object {properties}) in
    let obj_type =
      let open T.Object.Property in
      let prop_type =
        T.Object.Property (Loc.none, {key = E.Object.Property.Identifier (Loc.none, pname);
                                      value = Init (Loc.none, etype);
                                      optional = false;
                                      static = false;
                                      _method = false;
                                      variance = None;}) in
      T.Object.(T.Object {exact = false; properties = [prop_type]}) in
      obj_expr, obj_type

  (* A rule for generating object literals *)
  method! rule_obj_lit (env : env_t) : (Syntax.t * env_t) =

    (* a helper function for generating expression for object
       properties *)
    let rec gen_expr_list
        (count : int)
        (limit : int)
        (result : (E.t' * T.t') list) : (E.t' * T.t') list =
      if count = limit then result
      else
        let expr = self#choose count (fun () -> self#require_expr env) in
        let ep = match expr with
          | Expr (e, t) -> (e, t)
          | _ -> failwith "This has to be an expression" in

        (* we don't want functions to be properties of an object *)
        self#backtrack_on_false (match snd ep with
            | T.Function _ -> false
            | _ -> true);
        gen_expr_list (count + 1) limit (ep :: result) in

    (* We are getting at most 2 properties *)
    let elist = gen_expr_list 0 ((FRandom.rint 2) + 1) [] in
    let props =
      let count = ref 0 in
      let mk_prop () =
        let r = "p_" ^ (string_of_int !count) in
        count := !count + 1;
        r in
      List.map (fun e -> mk_prop (), e) elist in

    (* get the literal syntax and its type *)
    let lit = Syntax.mk_obj_lit props in
    let lit_expr = (match lit with
         | Syntax.Expr e -> e
         | _ -> failwith "[rule_obj_lit] Literal has to be an expr") in
    let ret_type =
      let prop_types =
        List.map (fun e ->
            let open T.Object.Property in
            T.Object.Property (Loc.none, {key = E.Object.Property.Identifier (Loc.none, fst e);
                                          value = Init (Loc.none, snd (snd e));
                                          optional = false;
                                          static = false;
                                          _method = false;
                                          variance = None})) props in
      let open T.Object in
      T.Object {exact = false; properties = prop_types} in

    (* Randomly wrap them into an object *)
    let lit_expr, ret_type = match FRandom.rbool () with
      | true -> self#wrap_in_obj lit_expr ret_type
      | false -> lit_expr, ret_type in
    let new_env =
      self#add_binding
        env
        (Expr (lit_expr, ret_type)) in
    let new_env = self#add_binding new_env (Type ret_type) in
    lit, new_env

  (* Rule for declaring a variable with init and type annotation *)
  method! rule_vardecl_with_type (env : env_t) : (Syntax.t * env_t) =
    (* require an expression from the environment *)
    let rhs = self#choose 0 (fun () -> self#require_expr env) in
    let rhs_expr, rhs_type = match rhs with
        | Expr (e, t) -> e, t
        | _ -> failwith "This has to be an expression" in

    (* For fast search on depth-subtyping *)
    self#backtrack_on_false (match rhs_expr, rhs_type with
        | E.Identifier _, _ -> true
        | _, T.Object _ -> true
        | _ -> false);

    (* require a type from the environment.*)
    let vtype = self#choose 1 (fun () -> self#require_type env) in
    let vtype = match vtype with
      | Type t -> t
      | _ -> failwith "This has to a type" in

    (* assert the subtyping relationhips between the rhs and lhs *)
    self#weak_assert (self#is_subtype rhs_type vtype);
    let vname = Utils.mk_var () in
    let var_decl = Syntax.mk_vardecl ~etype:vtype vname rhs_expr in
    let new_env =
      self#add_binding
        env
        (Expr ((E.Identifier (Loc.none, vname)), vtype)) in
    let new_env = self#add_binding new_env (Type vtype) in
    var_decl, new_env

  method! get_all_rules () =
    [|self#rule_num_lit;
      self#rule_str_lit;
      self#rule_union_type;
      self#rule_obj_lit;
      self#rule_vardecl_with_type;
      self#rule_prop_update;
      self#rule_func_mutate;
      self#rule_func_call;
      self#rule_prop_read;|]
end

class ruleset_random_union (depth : int) = object
  inherit ruleset_union depth
  method! weak_assert b =
    if (not b) && ((FRandom.rint 20) > 0) then raise Engine.Fail
end
