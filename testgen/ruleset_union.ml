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

class ruleset_union =
  object (self)
    inherit Ruleset_base.ruleset_base

    method! get_name () : string = "union"

    method! weak_assert b = self#backtrack_on_false b

    (* check t1 <: t2 *)
    method! is_subtype (t1 : (Loc.t, Loc.t) T.t') (t2 : (Loc.t, Loc.t) T.t') : bool =
      match (t1, t2) with
      | (t, T.Union ((_, tu1), (_, tu2), tlist)) ->
        (* t should be one of the branches of Union *)
        List.mem t (tu1 :: tu2 :: Core_list.map ~f:snd tlist)
      | (T.Object o1, T.Object o2) -> self#is_subtype_obj o1 o2
      | (T.Function f1, T.Function f2) -> self#is_subtype_func f1 f2
      | _ when t1 = t2 -> true
      | _ -> false

    (* Using a loose form of subtyping from ruleset_depth, so we can allow
    passing { p : 3 } to a function that expects { p : number | string }.
    In general this is unsound, and Flow allows this only in certain
    situations.
   *)
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

    method! rule_func_call (env : env_t) : Syntax.t * env_t =
      (* require a function from the environment.*)
      let func = self#choose 0 (fun () -> self#require_expr env) in
      let (func_expr, func_type) =
        match func with
        | Expr (e, t) -> (e, t)
        | _ -> failwith "This has to be an expression"
      in
      self#backtrack_on_false
        (match func_type with
        | T.Function _ -> true
        | _ -> false);

      (* get the type of the parameter assuming we only have one param *)
      let f_ptype =
        match func_type with
        | T.Function ft ->
          let ft_param = T.Function.(ft.params) |> snd in
          let params = T.Function.Params.(ft_param.params) |> List.hd |> snd in
          T.Function.Param.(params.annot)
        | _ -> failwith "This has to a function type"
      in
      (* parameter *)
      let param = self#choose 1 (fun () -> self#require_expr env) in
      let (param_expr, param_type) =
        match param with
        | Expr (e, t) -> (e, t)
        | _ -> failwith "This has to be an expression"
      in
      self#backtrack_on_false
        (match param_expr with
        | E.Identifier _ -> true
        | _ -> false);
      self#weak_assert (self#is_subtype param_type (snd f_ptype));

      let func_call = Syntax.mk_func_call func_expr param_expr in
      let ret_type =
        T.Function.(
          match func_type with
          | T.Function { params = _; return = (_, rt); tparams = _ } -> rt
          | _ -> failwith "This has to be a function type")
      in
      let new_env =
        self#add_binding
          env
          (Expr
             ( (match func_call with
               | Syntax.Expr e -> e
               | _ -> failwith "This has to be an expression"),
               ret_type ))
      in
      let new_env = self#add_binding new_env (Type ret_type) in
      (func_call, new_env)

    method! rule_obj_type (prop_num : int) (opt_num : int) (env : env_t) : Syntax.t * env_t =
      let ret_type =
        self#gen_obj_type prop_num opt_num env ~cons:(fun elt ->
            match elt with
            | Type (T.Union _) -> true
            | _ -> false)
      in
      let new_env = self#add_binding env (Type ret_type) in
      (Syntax.Empty, new_env)

    method! get_all_rules () =
      [|
        self#rule_num_lit;
        self#rule_str_lit;
        self#rule_obj_lit 1 0;
        self#rule_vardecl;
        self#rule_union_type 2;
        self#rule_obj_type 1 0;
        self#rule_func_mutate;
        self#rule_func_call;
        self#rule_runtime_check
        (*
      self#rule_vardecl_with_type; (*make it challenging*)
      self#rule_prop_update;
      self#rule_vardecl_with_type;
      self#rule_prop_update;
      self#rule_func_mutate;
      self#rule_func_call;
      self#rule_prop_read;
         *);
      |]
  end

class ruleset_random_union =
  object
    inherit ruleset_union

    method! weak_assert b = if (not b) && Random.int 5 > 0 then raise Engine.Backtrack
  end
