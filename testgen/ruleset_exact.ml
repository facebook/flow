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

(* Show how to use exact types. *)
open Ruleset_base;;

(* ESSENTIAL: Syntax type and related functions *)
module Syntax = Syntax_base;;

class ruleset_exact (depth : int) = object(self)
  inherit ruleset_base depth as super

  method! weak_assert b = self#backtrack_on_false b

  method! is_subtype_obj (o1 : T.Object.t) (o2 : T.Object.t) =
    let open T.Object in
    if (o1.exact && o2.exact) then
      o1 = o2
    else if (o1.exact || o2.exact) then
      false
    else
      super#is_subtype_obj o1 o2

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
      T.Object {exact = FRandom.rbool(); properties = prop_types} in  (* changed here from base *)

    let new_env =
      self#add_binding
        env
        (Expr (lit_expr, ret_type)) in
    let new_env = self#add_binding new_env (Type ret_type) in
    lit, new_env

  method! get_all_rules () =
    [|self#rule_num_lit;
      self#rule_obj_lit;
      self#rule_vardecl_with_type;
      self#rule_prop_read;
      self#rule_prop_update;|]
  end

  class ruleset_random_exact (depth : int) = object
    inherit ruleset_exact depth
    method! weak_assert b =
      if (not b) && ((FRandom.rint 20) > 0) then raise Engine.Fail
  end
