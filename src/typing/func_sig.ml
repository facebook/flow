(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Flow = Flow_js

open Reason
open Type

type kind =
  | Ordinary
  | Async
  | Generator
  | AsyncGenerator
  | FieldInit of (Loc.t, Loc.t) Ast.Expression.t
  | Predicate
  | Ctor

type t = {
  reason: reason;
  kind: kind;
  tparams: Type.typeparam list;
  tparams_map: Type.t SMap.t;
  fparams: Func_params.t;
  body: (Loc.t, Loc.t) Ast.Function.body option;
  return_t: Type.t;
}

let return_loc =
  let module F = Ast.Function in
  let open F in function
  | {return = Some (_, (loc, _)); _}
  | {F.body = BodyExpression (loc, _); _} -> loc
  | {F.body = BodyBlock (loc, _); _} -> Loc.char_before loc

let default_constructor reason = {
  reason;
  kind = Ctor;
  tparams = [];
  tparams_map = SMap.empty;
  fparams = Func_params.empty;
  body = None;
  return_t = VoidT.why reason;
}

let field_initializer tparams_map reason expr return_t = {
  reason;
  kind = FieldInit expr;
  tparams = [];
  tparams_map;
  fparams = Func_params.empty;
  body = None;
  return_t;
}

let subst cx map x =
  let {tparams; tparams_map; fparams; return_t; _} = x in
  (* Remove shadowed type params from `map`, but allow bounds/defaults to be
     substituted if they refer to a type param before it is shadowed. *)
  let tparams, map = tparams |> List.fold_left (fun (tparams, map) tp ->
    let bound = Flow.subst cx map tp.bound in
    let default = Option.map ~f:(Flow.subst cx map) tp.default in
    {tp with bound; default}::tparams,
    SMap.remove tp.name map
  ) ([], map) in
  let tparams = List.rev tparams in
  let tparams_map = SMap.map (Flow.subst cx map) tparams_map in
  let fparams = Func_params.subst cx map fparams in
  let return_t = Flow.subst cx map return_t in
  {x with tparams; tparams_map; fparams; return_t}

let generate_tests cx f x =
  let {tparams; tparams_map; fparams; return_t; _} = x in
  Flow.generate_tests cx tparams (fun map -> f {
    x with
    tparams_map = SMap.map (Flow.subst cx map) tparams_map;
    fparams = Func_params.subst cx map fparams;
    return_t = Flow.subst cx map return_t;
  })

let functiontype cx this_t {reason; kind; tparams; fparams; return_t; _} =
  let knot = Tvar.mk cx reason in
  let static =
    let proto = FunProtoT reason in
    Obj_type.mk_with_proto cx reason ~call:knot proto
  in
  let prototype =
    let reason = replace_reason_const RPrototype reason in
    Obj_type.mk cx reason
  in
  let funtype = { Type.
    this_t;
    params = Func_params.value fparams;
    rest_param = Func_params.rest fparams;
    return_t;
    is_predicate = kind = Predicate;
    closure_t = Env.peek_frame ();
    changeset = Env.retrieve_closure_changeset ();
    def_reason = reason;
  } in
  let t = DefT (reason, FunT (static, prototype, funtype)) in
  let t = poly_type (Context.make_nominal cx) tparams t in
  Flow.unify cx t knot;
  t

let methodtype cx {reason; tparams; fparams; return_t; _} =
  let params = Func_params.value fparams in
  let params_names, params_tlist = List.split params in
  let rest_param = Func_params.rest fparams in
  let def_reason = reason in
  let t = DefT (reason, FunT (
    dummy_static reason,
    dummy_prototype,
    mk_boundfunctiontype
      params_tlist ~rest_param ~def_reason ~params_names return_t
  )) in
  poly_type (Context.make_nominal cx) tparams t

let gettertype ({return_t; _}: t) = return_t

let settertype {fparams; _} =
  match Func_params.value fparams with
  | [(_, param_t)] -> param_t
  | _ -> failwith "Setter property with unexpected type"

let toplevels id cx this super ~decls ~stmts ~expr
  {reason=reason_fn; kind; tparams_map; fparams; body; return_t; _} =

  let loc = Ast.Function.(match body with
  | Some (BodyBlock (loc, _)) -> loc
  | Some (BodyExpression (loc, _)) -> loc
  | None -> Loc.none
  ) in
  let reason = mk_reason RFunctionBody loc in

  let env =  Env.peek_env () in
  let new_env = Env.clone_env env in

  Env.update_env cx loc new_env;
  Env.havoc_all();

  (* create and prepopulate function scope *)
  let function_scope =
    let var_scope_kind =
      match kind with
      | Ordinary
      | FieldInit _ -> Scope.Ordinary
      | Predicate -> Scope.Predicate
      | Async -> Scope.Async
      | Generator -> Scope.Generator
      | AsyncGenerator -> Scope.AsyncGenerator
      | Ctor -> Scope.Ctor
    in
    Scope.fresh ~var_scope_kind ()
  in

  (* push the scope early so default exprs can reference earlier params *)
  Env.push_var_scope cx function_scope;

  (* add `this` and `super` before looking at parameter bindings as when using
   * `this` in default parameter values it refers to the function scope and
   * `super` should resolve to the method's [[HomeObject]]
  *)
  Scope.add_entry (internal_name "this") this function_scope;
  Scope.add_entry (internal_name "super") super function_scope;

  (* bind type params *)
  SMap.iter (fun name t ->
    let r = reason_of_t t in
    let loc = loc_of_reason r in
    Env.bind_type cx name (DefT (r, TypeT (TypeParamKind, t))) loc
      ~state:Scope.State.Initialized
  ) tparams_map;

  (* Check the rest parameter annotation *)
  Option.iter
    ~f:(fun (_, loc, t) ->
      let rest_reason =
        mk_reason (RCustom "Rest params are always arrays") loc in
      Flow_js.flow cx (t, AssertRestParamT rest_reason)
    )
    (Func_params.rest fparams);

  (* add param bindings *)
  let const_params = Context.enable_const_params cx in
  fparams |> Func_params.iter Scope.(fun (name, loc, t, default) ->
    let reason = mk_reason (RParameter (Some name)) loc in
    (* add default value as lower bound, if provided *)
    Option.iter ~f:(fun default ->
      let default_t = Flow.mk_default cx reason default
        ~expr:(fun cx e -> snd (fst (expr cx e))) in
      Flow.flow_t cx (default_t, t)
    ) default;
    (* add to scope *)
    if const_params
    then Env.bind_implicit_const ~state:State.Initialized
      Entry.ConstParamBinding cx name t loc
    else
      let new_kind =
        if Env.promote_to_const_like cx loc then Entry.ConstlikeParamBinding
        else Entry.ParamBinding in
      Env.bind_implicit_let ~state:State.Initialized
      new_kind cx name t loc
  );

  (* early-add our own name binding for recursive calls *)
  Option.iter id ~f:(fun (loc, name) ->
    let entry = Scope.Entry.new_var ~loc (AnyT.at loc) in
    Scope.add_entry name entry function_scope
  );

  let yield_t, next_t =
    if kind = Generator || kind = AsyncGenerator then
      Tvar.mk cx (replace_reason_const (RCustom "yield") reason),
      Tvar.mk cx (replace_reason_const (RCustom "next") reason)
    else
      DefT (replace_reason_const (RCustom "no yield") reason, MixedT Mixed_everything),
      DefT (replace_reason_const (RCustom "no next") reason, MixedT Mixed_everything)
  in

  let yield, next, return = Scope.(
    let new_entry t = Entry.(
      let loc = loc_of_t t in
      let state = State.Initialized in
      new_const ~loc ~state t
    ) in
    new_entry yield_t, new_entry next_t, new_entry return_t
  ) in

  Scope.add_entry (internal_name "yield") yield function_scope;
  Scope.add_entry (internal_name "next") next function_scope;
  Scope.add_entry (internal_name "return") return function_scope;

  let statements, reconstruct_body = Ast.Statement.(
    match body with
    | None -> [], Fn.const None
    | Some (Ast.Function.BodyBlock (loc, { Block.body })) ->
      body, (fun body -> Some (Ast.Function.BodyBlock (loc, { Block.body })))
    | Some (Ast.Function.BodyExpression expr) ->
      [fst expr, Return {Return.argument = Some expr}],
      (function
      | [_, Return { Return.argument = Some expr }]
      | [_, Expression { Expression.expression = expr; _ }] ->
        Some (Ast.Function.BodyExpression expr)
      | _ -> failwith "expected return body")
  ) in

  (* NOTE: Predicate functions can currently only be of the form:
       function f(...) { return <exp>; }
  *)
  Ast.Statement.(
    match kind with
    | Predicate -> begin
        match statements with
        | [(_, Return { Return.argument = Some _})] -> ()
        | _ ->
          let loc = loc_of_reason reason in
          Flow_js.add_output cx
            Flow_error.(EUnsupportedSyntax (loc, PredicateInvalidBody))
      end
    | _ -> ()
  );

  (* decl/type visit pre-pass *)
  decls cx statements;

  (* statement visit pass *)
  let statements_ast, statements_abnormal =
    Abnormal.catch_stmts_control_flow_exception (fun () -> stmts cx statements) in
  let is_void = Abnormal.(
    match statements_abnormal with
    | Some Return -> false
    | Some Throw -> false (* NOTE *)
    | Some exn ->
        (* TODO: look into where this throws to. Is it ok that this throws? *)
        throw_stmt_control_flow_exception Typed_ast.Statement.error exn
    | None -> true
  ) in
  let body_ast = reconstruct_body statements_ast in

  (* build return type for void funcs *)
  let init_ast = if is_void then
    let loc = loc_of_t return_t in
    (* Some branches add an ImplicitTypeParam frame to force our flow_use_op
     * algorithm to pick use_ops outside the provided loc. *)
    let use_op, void_t, init_ast = match kind with
    | Ordinary
    | Ctor ->
      let t = VoidT.at loc in
      let use_op = Op (FunImplicitReturn {fn = reason_fn; upper = reason_of_t return_t}) in
      use_op, t, None
    | Async ->
      let reason = annot_reason (mk_reason (RType "Promise") loc) in
      let void_t = VoidT.at loc in
      let t = Flow.get_builtin_typeapp cx reason "Promise" [void_t] in
      let use_op = Op (FunImplicitReturn {fn = reason_fn; upper = reason_of_t return_t}) in
      let use_op = Frame (ImplicitTypeParam (loc_of_t return_t), use_op) in
      use_op, t, None
    | Generator ->
      let reason = annot_reason (mk_reason (RType "Generator") loc) in
      let void_t = VoidT.at loc in
      let t = Flow.get_builtin_typeapp cx reason "Generator" [yield_t; void_t; next_t] in
      let use_op = Op (FunImplicitReturn {fn = reason_fn; upper = reason_of_t return_t}) in
      let use_op = Frame (ImplicitTypeParam (loc_of_t return_t), use_op) in
      use_op, t, None
    | AsyncGenerator ->
      let reason = annot_reason (mk_reason (RType "AsyncGenerator") loc) in
      let void_t = VoidT.at loc in
      let t = Flow.get_builtin_typeapp cx reason "AsyncGenerator" [yield_t; void_t; next_t] in
      let use_op = Op (FunImplicitReturn {fn = reason_fn; upper = reason_of_t return_t}) in
      let use_op = Frame (ImplicitTypeParam (loc_of_t return_t), use_op) in
      use_op, t, None
    | FieldInit e ->
      let (_, t), _ as ast = expr cx e in
      unknown_use, t, Some ast
    | Predicate ->
      let loc = loc_of_reason reason in
      Flow_js.add_output cx
        Flow_error.(EUnsupportedSyntax (loc, PredicateVoidReturn));
      let t = VoidT.at loc in
      let use_op = Op (FunImplicitReturn {fn = reason_fn; upper = reason_of_t return_t}) in
      use_op, t, None
    in
    Flow.flow cx (void_t, UseT (use_op, return_t));
    init_ast
  else None in

  Env.pop_var_scope ();

  Env.update_env cx loc env;

  (*  return a tuple of (function body AST option, field initializer AST option).
      - the function body option is Some _ if the func sig's body was Some, and
        None if the func sig's body was None.
      - the field initializer is Some expr' if the func sig's kind was FieldInit expr,
        where expr' is the typed AST translation of expr.
  *)
  body_ast, init_ast

let to_ctor_sig f = { f with kind = Ctor }

let with_typeparams cx f x =
  Type_table.with_typeparams x.tparams (Context.type_table cx) f
