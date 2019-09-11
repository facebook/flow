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

class ruleset_func =
  object (self)
    inherit Ruleset_base.ruleset_base

    method! get_name () : string = "func"

    method! weak_assert b = self#backtrack_on_false b

    method! is_subtype_func (f1 : (Loc.t, Loc.t) T.Function.t) (f2 : (Loc.t, Loc.t) T.Function.t)
        : bool =
      T.Function.(
        let get_type_list (f : (Loc.t, Loc.t) T.Function.t) : (Loc.t, Loc.t) T.t' list =
          T.Function.Param.(
            let (_, { T.Function.Params.params; rest = _ }) = f.params in
            List.map (fun param -> (snd param).annot |> snd) params @ [f.return |> snd])
        in
        let rec func_subtype_helper l1 l2 =
          match (l1, l2) with
          | ([], []) -> true
          (* checking the return type *)
          | ([hd1], [hd2]) -> self#is_subtype hd1 hd2
          (* checking the param type *)
          | (hd1 :: tl1, hd2 :: tl2) ->
            (* BAD subtyping check. Please look at ruleset_base
           for the correct subtyping check *)
            (match (hd1, hd2) with
            | (T.Object _, T.Object _)
            | (T.Number, T.Number)
            | (T.String, T.String)
            | (T.Function _, T.Function _) ->
              func_subtype_helper tl1 tl2
            | _ -> false)
          | _ -> false
        in
        let p1_list = get_type_list f1 in
        let p2_list = get_type_list f2 in
        if not (List.length p1_list = List.length p2_list) then
          false
        else
          func_subtype_helper p1_list p2_list)

    (* A rule for generating function definitions *)
    method! rule_funcdef (env : env_t) : Syntax.t * env_t =
      let mk_func_type (ptype : (Loc.t, Loc.t) T.t') (rtype : (Loc.t, Loc.t) T.t') :
          (Loc.t, Loc.t) T.t' =
        let param_type =
          (Loc.none, T.Function.Param.{ name = None; annot = (Loc.none, ptype); optional = false })
        in
        let ret_type = (Loc.none, rtype) in
        T.Function.(
          T.Function
            {
              params = (Loc.none, { Params.params = [param_type]; rest = None });
              return = ret_type;
              tparams = None;
            })
      in
      (* parameter type *)
      let param_type =
        match self#choose 0 (fun () -> self#require_type env) with
        | Type t -> t
        | _ -> failwith "has to be a type"
      in
      self#backtrack_on_false
        (match param_type with
        | T.Object _
        | T.Function _ ->
          true
        | _ -> false);

      (* We are assuming we only have one parameter for now *)
      let pname = "param" in
      (* make a new environment to account for parameters

       TODO: This is a hacky way to account for parameters. The
       correct way to do this is to change every expression
       that has the variable occurrences whose type is the super
       type of the parameter *)
      let fenv =
        Expr (E.Identifier (Flow_ast_utils.ident_of_source (Loc.none, pname)), param_type)
        :: T.Function.(
             match param_type with
             (* If the parameter is a function, we create new function calls *)
             | T.Function { params = _; return = (_, rt); tparams = _ } ->
               E.Call.(
                 List.fold_right
                   (fun elt acc ->
                     match elt with
                     | Expr (E.Call { callee = (_, fid); targs; arguments = args }, _) ->
                       let ftype = self#get_type_from_expr fid env in
                       if self#is_subtype param_type ftype then
                         Expr
                           ( E.Call
                               {
                                 callee =
                                   ( Loc.none,
                                     E.Identifier
                                       (Flow_ast_utils.ident_of_source (Loc.none, pname)) );
                                 targs;
                                 arguments = args;
                               },
                             rt )
                         :: elt
                         :: acc
                       else
                         elt :: acc
                     | _ -> elt :: acc)
                   env
                   [])
             (* If the parameter is an object, we create new property read *)
             | T.Object _ ->
               E.Member.(
                 List.fold_right
                   (fun elt acc ->
                     match elt with
                     | Expr (E.Member { _object = (_, obj); property = prop }, t) ->
                       let otype = self#get_type_from_expr obj env in
                       if self#is_subtype param_type otype then
                         Expr
                           ( E.Member
                               {
                                 _object =
                                   ( Loc.none,
                                     E.Identifier
                                       (Flow_ast_utils.ident_of_source (Loc.none, pname)) );
                                 property = prop;
                               },
                             t )
                         :: elt
                         :: acc
                       else
                         elt :: acc
                     | _ -> elt :: acc)
                   env
                   [])
             | _ -> env)
      in
      (* return expression and its type *)
      let func_return_type =
        match self#choose 1 (fun () -> self#require_type fenv) with
        | Type t -> t
        | _ -> failwith "Has to be a type"
      in
      self#backtrack_on_false
        (match func_return_type with
        | T.Object _ -> true
        | _ -> false);

      let fname = Utils.mk_func () in
      (* return expression and its type *)
      let ret_expr = self#choose 2 (fun () -> self#require_expr fenv) in
      let (ret_expr_expr, ret_expr_type) =
        match ret_expr with
        | Expr (e, t) -> (e, t)
        | _ -> failwith "This has to be an expression"
      in
      self#backtrack_on_false (self#is_subtype ret_expr_type func_return_type);
      let ret_stmt = Syntax.mk_ret_stmt ret_expr_expr in
      let func_def = Syntax.mk_func_def fname pname param_type [ret_stmt] func_return_type in
      let ret_type = mk_func_type param_type func_return_type in
      let new_env =
        self#add_binding
          env
          (Expr (E.Identifier (Flow_ast_utils.ident_of_source (Loc.none, fname)), ret_type))
      in
      let new_env = self#add_binding new_env (Type ret_type) in
      (func_def, new_env)

    (* A rule for generating function calls *)
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
        T.Function.(
          match func_type with
          | T.Function
              { params = (_, { Params.params = plist; rest = _ }); return = _; tparams = _ } ->
            T.Function.Param.((plist |> List.hd |> snd).annot)
          | _ -> failwith "This has to a function type")
      in
      (* parameter *)
      let param = self#choose 1 (fun () -> self#require_expr env) in
      let (param_expr, param_type) =
        match param with
        | Expr (e, t) -> (e, t)
        | _ -> failwith "This has to be an expression"
      in
      self#backtrack_on_false
        (match param_type with
        | T.Function _ -> true
        | T.Object _ -> true
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

    method! get_all_rules () =
      [|
        self#rule_num_lit;
        self#rule_obj_lit 1 0;
        self#rule_obj_lit 2 0;
        self#rule_funcdef;
        self#rule_funcdef;
        self#rule_func_call;
        self#rule_funcdef;
        self#rule_func_call;
      |]
  end

class ruleset_random_func =
  object
    inherit ruleset_func

    method! weak_assert b = if (not b) && Random.int 3 > 0 then raise Engine.Backtrack
  end
