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

class ruleset_depth = object(self)
  inherit Ruleset_base.ruleset_base

  method! weak_assert b = self#backtrack_on_false b

  method! is_subtype_obj (o1 : T.Object.t) (o2 : T.Object.t) =
    let get_prop_set (o : T.Object.t) =
      let tbl = Hashtbl.create 1000 in
      let open T.Object.Property in
      List.iter (fun p -> match p with
          | T.Object.Property (_, {key = E.Object.Property.Identifier (_, name);
                                   value = Init (_, t);
                                   optional = _;
                                   static = _;
                                   _method = _;
                                   variance = _;}) -> Hashtbl.add tbl name t
          | _ -> ()) T.Object.(o.properties);
      tbl in
    let s1 = get_prop_set o1 in
    let s2 = get_prop_set o2 in
    let subtype = ref true in
    (* check non optional properties *)
    Hashtbl.iter (fun n t ->
        (* Shouldn't use call is_subtyping recursivingly. We should
           use equality to limit depth subtyping *)
        if (not (Hashtbl.mem s1 n)) || (not (self#is_subtype (Hashtbl.find s1 n) t)) then
          subtype := false) s2;
    !subtype

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

  (* property update rule *)
  method! rule_prop_update (env : env_t) : (Syntax.t * env_t) =
    (* get an object variable *)
    let obj = self#choose 0 (fun () -> self#require_expr env) in
    self#backtrack_on_false (match obj with
        | Expr (E.Identifier _, T.Object _) -> true
        | _ -> false);
    let oexpr, otype = match obj with
      | Expr (e, t) -> e, t
      | _ -> failwith "This has to be an expression" in

    let prop = self#choose 1 (fun () -> self#require_prop otype true) in
    let pexpr, ptype = match prop with
        | Expr (e, t) -> e, t
        | _ -> failwith "This has to be an expression" in
    self#backtrack_on_false (match ptype with
        | T.Object _ -> true
        | _ -> false);

    (* get the expression on the rhs of the update *)
    let rhs = self#choose 2 (fun () -> self#require_expr env) in
    let rhs_expr, rhs_type = match rhs with
        | Expr (e, t) -> e, t
        | _ -> failwith "This has to be an expression" in
    self#backtrack_on_false (match rhs_expr with
        | E.Object _ -> true
        | _ -> false);

    (* assert that type(rhs) <: type(prop) *)
    self#weak_assert (self#is_subtype rhs_type ptype);

    (* produce a write syntax *)
    let write =
      Syntax.mk_prop_write
        (Utils.string_of_expr oexpr)
        (Utils.string_of_expr pexpr)
        rhs_expr in

    (* update the type of the object *)
    let ret_type =
      let o_type = match otype with
        | T.Object o -> o
        | _ -> failwith "Has to be an object type" in
      if pexpr = E.Identifier (Loc.none, "_number_prop_") then
        let new_prop = let open T.Object.Property in
          {key = E.Object.Property.Identifier (Loc.none, (Utils.string_of_expr pexpr));
           value = Init (Loc.none, T.Number);
           optional = false;
           static = false;
           _method = false;
           variance = None} in
        let open T.Object in
        T.Object {exact = o_type.exact;
                  properties = Property (Loc.none, new_prop) :: o_type.properties}
      else
        T.Object o_type in

    let new_env = self#add_binding env (Expr (oexpr, ret_type)) in
    let new_env = self#add_binding new_env (Type ret_type) in
    (write, new_env)

  (* A rule for generating object literals *)
  method! rule_obj_lit (env : env_t) : (Syntax.t * env_t) =

    (* a helper function for generating expression for object
       properties *)
    let rec gen_expr_list
        (depth : int)
        (count : int)
        (limit : int)
        (result : (E.t' * T.t') list) : (E.t' * T.t') list =
      if count = limit then result
      else
        let expr = self#choose (depth + count) (fun () -> self#require_expr env) in
        let ep = match expr with
          | Expr (e, t) -> (e, t)
          | _ -> failwith "This has to be an expression" in
        self#backtrack_on_false ((snd ep) = T.Number);
        gen_expr_list depth (count + 1) limit (ep :: result) in

    (* We are getting at most 2 properties *)
    let prop_num = match self#choose 0 (fun () -> [Int 1; Int 2]) with
      | Int i -> i
      | _ -> failwith "This has to be an integer." in
    let elist = gen_expr_list 1 0 prop_num [] in
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
    let wrap_expr, wrap_ret_type = self#wrap_in_obj lit_expr ret_type in
    let new_env =
      self#add_binding
        (self#add_binding
           env
           (Expr (lit_expr, ret_type)))
        (Expr (wrap_expr, wrap_ret_type)) in
    let new_env = self#add_binding new_env (Type ret_type) in
    let new_env = self#add_binding new_env (Type wrap_ret_type) in
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
      self#rule_obj_lit;
      self#rule_obj_lit;
      self#rule_vardecl_with_type;
      self#rule_vardecl_with_type;
      self#rule_prop_update;
      self#rule_runtime_check;
    |]
end

class ruleset_random_depth = object
  inherit ruleset_depth
  method! weak_assert b =
    if (not b) && ((FRandom.rint 20) > 0) then raise Engine.Fail
end
