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

class ruleset_optional (depth : int) = object(self)
  inherit Ruleset_base.ruleset_base depth

  method! weak_assert b = self#backtrack_on_false b

  method! is_subtype_obj (o1 : T.Object.t) (o2 : T.Object.t) =
    let get_prop_set (o : T.Object.t) =
      let tbl = Hashtbl.create 1000 in

      (* hash table for storing optional properties *)
      let opt_tbl = Hashtbl.create 1000 in
      let open T.Object.Property in
      List.iter (fun p -> match p with
          | T.Object.Property (_, {key = E.Object.Property.Identifier (_, name);
                                   value = Init (_, t);
                                   optional = o;
                                   static = _;
                                   _method = _;
                                   variance = _;}) ->
            if o then Hashtbl.add opt_tbl name t
            else Hashtbl.add tbl name t
          | _ -> ()) T.Object.(o.properties);
      tbl, opt_tbl in
    let s1, opt1 = get_prop_set o1 in
    let s2, opt2 = get_prop_set o2 in
    let subtype = ref true in
    (* check non optional properties *)
    Hashtbl.iter (fun n t ->
      if (not (Hashtbl.mem s1 n)) || (not ((Hashtbl.find s1 n) = t)) then
          subtype := false) s2;

    (* check optional properties *)
    (* This is bad subtyping *)
    Hashtbl.iter (fun n t ->
        if (((Hashtbl.mem s1 n) && ((Hashtbl.find s1 n) != t)) ||
            ((Hashtbl.mem opt1 n) && ((Hashtbl.find opt1 n) != t)))
        then subtype := false) opt2;
    !subtype

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
        self#backtrack_on_false (match snd ep with
            | T.Number | T.String -> true
            | _ -> false);
        gen_expr_list (count + 1) limit (ep :: result) in

    (* We are getting exactly 1 property *)
    let elist = gen_expr_list 0 1 [] in
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

    let new_env =
      self#add_binding
        env
        (Expr (lit_expr, ret_type)) in
    let new_env = self#add_binding new_env (Type ret_type) in
    lit, new_env

  (* A rule for adding primitive types *)
  method! rule_obj_type (env : env_t) : (Syntax.t * env_t) =
    (* a helper function for generating object property types *)
    let rec gen_type_list
        (count : int)
        (limit : int)
        (result : T.t' list) : T.t' list =
      if count = limit then result
      else
        let ptype = self#choose count (fun () -> self#require_type env) in
        let ptype = match ptype with
          | Type t -> t
          | _ -> failwith "This has to be a type" in
        self#backtrack_on_false (match ptype with
            | T.Number | T.String -> true
            | _ -> false);
        gen_type_list (count + 1) limit (ptype :: result) in

    let prop_types = gen_type_list 0 2 [] in
    let props =
      let count = ref 0 in
      let mk_prop () =
        let r = "p_" ^ (string_of_int !count) in
        count := !count + 1;
        r in
      List.map (fun t -> mk_prop (), t) prop_types in

    let ret_type =
      let prop_types =
        List.map (fun p ->
            let open T.Object.Property in
            T.Object.Property (Loc.none, {key = E.Object.Property.Identifier (Loc.none, fst p);
                                          value = Init (Loc.none, snd p);
                                          optional = FRandom.rbool ();
                                          static = false;
                                          _method = false;
                                          variance = None})) props in
      let open T.Object in
      T.Object {exact = false; properties = prop_types} in
    let new_env =
      self#add_binding env (Type ret_type) in
    Syntax.Empty, new_env

  (* rule for variable declaration with initialization *)
  method! rule_vardecl (env : env_t) : (Syntax.t * env_t) =
    (* get the init expression *)
    let init = self#choose 0 (fun () -> self#require_expr env) in
    let init_expr, init_type = match init with
        | Expr (e, t) -> e, t
        | _ -> failwith "This has to be an expression" in

    (* For fast search on depth-subtyping *)
    self#backtrack_on_false (match init_expr, init_type with
        | E.Object _, T.Object _ -> true
        | _ -> false);

    let vname = Utils.mk_var () in
    let var_decl = Syntax.mk_vardecl vname init_expr in
    let new_env =
      self#add_binding
        env
        (Expr ((E.Identifier (Loc.none, vname)), init_type)) in
    let new_env = self#add_binding new_env (Type init_type) in
    var_decl, new_env

  (* Rule for declaring a variable with init and type annotation *)
  method! rule_vardecl_with_type (env : env_t) : (Syntax.t * env_t) =
    (* require an expression from the environment *)
    let rhs = self#choose 0 (fun () -> self#require_expr env) in
    let rhs_expr, rhs_type = match rhs with
        | Expr (e, t) -> e, t
        | _ -> failwith "This has to be an expression" in

    (* For fast search *)
    self#backtrack_on_false (match rhs_expr, rhs_type with
        | E.Identifier _, _ -> true
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

    self#backtrack_on_false (match otype with
        | T.Object ot -> T.Object.(List.length ot.properties) = 2
        | _ -> true);

    let prop = self#choose 1 (fun () -> self#require_prop otype true) in
    let pexpr, ptype = match prop with
        | Expr (e, t) -> e, t
        | _ -> failwith "This has to be an expression" in

    (* get the expression on the rhs of the update *)
    let rhs = self#choose 2 (fun () -> self#require_expr env) in
    let rhs_expr, rhs_type = match rhs with
        | Expr (e, t) -> e, t
        | _ -> failwith "This has to be an expression" in
    self#backtrack_on_false (match rhs_expr with
        | E.Identifier _ | E.Member _ -> false
        | _ -> true);

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

  (* A rule for adding primitive types *)
  method rule_obj_type_copy (env : env_t) : (Syntax.t * env_t) =
    (* a helper function for generating object property types *)
    let get_prop (o : T.t') =
      let open T.Object.Property in
      let o = match o with
        | T.Object ot -> ot
        | _ -> failwith "Has to be an object" in
      List.map (fun p -> match p with
          | T.Object.Property (_, {key = _;
                                   value = Init (_, t);
                                   optional = o;
                                   static = _;
                                   _method = _;
                                   variance = _;}) -> t, o
          | _ -> failwith "Has to be a property" ) T.Object.(o.properties) in

    let obj_type = self#choose 0 (fun () -> self#require_type env) in
    let obj_type = match obj_type with
      | Type t -> t
      | _ -> failwith "has to be a type" in
    self#backtrack_on_false (match obj_type with
        | T.Object _ -> true
        | _ -> false);

    let prop_types = get_prop obj_type in
    let props =
      let count = ref 0 in
      let mk_prop () =
        let r = "p_" ^ (string_of_int !count) in
        count := !count + 1;
        r in
      List.map (fun (t, o) -> mk_prop (), t, o) prop_types in

    let ret_type =
      let prop_types =
        List.map (fun (name, t, o) ->
            let open T.Object.Property in
            T.Object.Property (Loc.none, {key = E.Object.Property.Identifier (Loc.none, name);
                                          value = Init (Loc.none, t);
                                          optional = o || (FRandom.rbool ());
                                          static = false;
                                          _method = false;
                                          variance = None})) props in
      let open T.Object in
      T.Object {exact = false; properties = prop_types} in
    let new_env =
      self#add_binding env (Type ret_type) in
    Syntax.Empty, new_env

  method! get_all_rules () =
    [|self#rule_num_lit;
      self#rule_str_lit;
      self#rule_obj_lit;
      self#rule_obj_type;
      self#rule_obj_type_copy;
      self#rule_obj_lit;
      self#rule_obj_type;
      self#rule_obj_type_copy;
      self#rule_vardecl;
      self#rule_vardecl_with_type;
      self#rule_prop_read;
      self#rule_prop_update;|]
end

class ruleset_random_optional (depth : int) = object
  inherit ruleset_optional depth
  method! weak_assert b =
    if (not b) && ((FRandom.rint 20) > 0) then raise Engine.Fail
end
