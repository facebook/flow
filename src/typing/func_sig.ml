(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Anno = Type_annotation
module Flow = Flow_js

open Reason
open Type

type kind =
  | Ordinary
  | Async
  | Generator
  | AsyncGenerator
  | FieldInit of Loc.t Ast.Expression.t
  | Predicate
  | Ctor

type t = {
  reason: reason;
  kind: kind;
  tparams: Type.typeparam list;
  tparams_map: Type.t SMap.t;
  fparams: Func_params.t;
  body: Loc.t Ast.Function.body;
  return_t: Type.t;
}

let return_loc =
  let module F = Ast.Function in
  let open F in function
  | {return = Some (_, (loc, _)); _}
  | {F.body = BodyExpression (loc, _); _} -> loc
  | {F.body = BodyBlock (loc, _); _} -> Loc.char_before loc

let function_kind {Ast.Function.async; generator; predicate; _ } =
  Ast.Type.Predicate.(match async, generator, predicate with
  | true, true, None -> AsyncGenerator
  | true, false, None -> Async
  | false, true, None -> Generator
  | false, false, None -> Ordinary
  | false, false, Some (_, Declared _) -> Predicate
  | false, false, Some (_ , Inferred) -> Predicate
  | _, _, _ -> Utils_js.assert_false "(async || generator) && pred")

let mk_params cx tparams_map ~expr func =
  let add_param_with_default default = function
    | loc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
        name = (_, name) as id;
        annot;
        optional;
      } ->
      let reason = mk_reason (RParameter (Some name)) loc in
      let t = Anno.mk_type_annotation cx tparams_map reason annot in
      Func_params.add_simple cx ~tparams_map ~optional ?default loc (Some id) t
    | loc, _ as patt ->
      let reason = mk_reason RDestructuring loc in
      let annot = Destructuring.type_of_pattern patt in
      let t = Anno.mk_type_annotation cx tparams_map reason annot in
      Func_params.add_complex cx ~tparams_map ~expr ?default patt t
  in
  let add_rest patt params =
    match patt with
    | loc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
        name = (_, name) as id;
        annot;
        _;
      } ->
      let reason = mk_reason (RRestParameter (Some name)) loc in
      let t = Anno.mk_type_annotation cx tparams_map reason annot in
      Func_params.add_rest cx ~tparams_map loc (Some id) t params
    | loc, _ ->
      Flow_js.add_output cx
        Flow_error.(EInternal (loc, RestParameterNotIdentifierPattern));
      params
  in
  let add_param = function
    | _, Ast.Pattern.Assignment { Ast.Pattern.Assignment.left; right; } ->
      add_param_with_default (Some right) left
    | patt ->
      add_param_with_default None patt
  in
  let {Ast.Function.params = (_, { Ast.Function.Params.params; rest }); _} = func in
  let params = List.fold_left (fun acc param ->
    add_param param acc
  ) Func_params.empty params in
  match rest with
  | Some (_, { Ast.Function.RestElement.argument }) -> add_rest argument params
  | None -> params

let mk cx tparams_map ~expr loc func =
  let {Ast.Function.tparams; return; body; predicate; _} = func in
  let reason = func_reason func loc in
  let kind = function_kind func in
  let tparams, tparams_map =
    Anno.mk_type_param_declarations cx ~tparams_map tparams
  in
  let fparams = mk_params cx tparams_map ~expr func in
  let ret_reason = mk_reason RReturn (return_loc func) in
  let return_t =
    Anno.mk_type_annotation cx tparams_map ret_reason return
  in
  let return_t = Ast.Type.Predicate.(match predicate with
    | None ->
        return_t
    | Some (_, Inferred) ->
        (* Restrict the fresh condition type by the declared return type *)
        let fresh_t = Anno.mk_type_annotation cx tparams_map ret_reason None in
        Flow.flow_t cx (fresh_t, return_t);
        fresh_t
    | Some (loc, Declared _) ->
        Flow_js.add_output cx Flow_error.(
          EUnsupportedSyntax (loc, PredicateDeclarationForImplementation)
        );
        Anno.mk_type_annotation cx tparams_map ret_reason None
  ) in
  {reason; kind; tparams; tparams_map; fparams; body; return_t}

let empty_body =
  let loc = Loc.none in
  let body = [] in
  Ast.Function.BodyBlock (loc, {Ast.Statement.Block.body})

let convert_params cx tparams_map func =
  let open Ast.Type.Function in
  let add_param (loc, {Param.name=id; annot; optional; _}) =
    let t = Anno.convert cx tparams_map annot in
    Func_params.add_simple cx ~tparams_map ~optional loc id t
  in
  let add_rest (loc, {Param.name=id; annot; _}) =
    let t = Anno.convert cx tparams_map annot in
    let () =
      let name = Option.map id ~f:snd in
      let reason = mk_reason (RRestParameter name) (loc_of_t t) in
      Flow.flow cx (t, AssertRestParamT reason)
    in
    Func_params.add_rest cx ~tparams_map loc id t
  in
  let (_, { Params.params; rest }) = func.Ast.Type.Function.params in
  let params = List.fold_left (fun acc param ->
    add_param param acc
  ) Func_params.empty params in
  match rest with
  | Some (_, { RestParam.argument }) -> add_rest argument params
  | None -> params

let convert cx tparams_map loc func =
  let {Ast.Type.Function.tparams; return; _} = func in
  let reason = mk_reason RFunctionType loc in
  let kind = Ordinary in
  let tparams, tparams_map =
    Anno.mk_type_param_declarations cx ~tparams_map tparams
  in
  let fparams = convert_params cx tparams_map func in
  let body = empty_body in
  let return_t = Anno.convert cx tparams_map return in

  {reason; kind; tparams; tparams_map; fparams; body; return_t}

let default_constructor reason = {
  reason;
  kind = Ctor;
  tparams = [];
  tparams_map = SMap.empty;
  fparams = Func_params.empty;
  body = empty_body;
  return_t = VoidT.why reason;
}

let field_initializer tparams_map reason expr return_t = {
  reason;
  kind = FieldInit expr;
  tparams = [];
  tparams_map;
  fparams = Func_params.empty;
  body = empty_body;
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
    let props = SMap.singleton "$call" (Method (None, knot)) in
    let proto = FunProtoT reason in
    Obj_type.mk_with_proto cx reason ~props proto
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

let toplevels id cx this super static ~decls ~stmts ~expr
  {reason=reason_fn; kind; tparams_map; fparams; body; return_t; _} =

  let loc, reason =
    let loc = Ast.Function.(match body with
      | BodyBlock (loc, _)
      | BodyExpression (loc, _) -> loc
    ) in
    loc, mk_reason RFunctionBody loc
  in

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
    Env.bind_type_param cx static name t
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
      let default_t = Flow.mk_default cx reason default ~expr in
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

  let statements = Ast.Statement.(
    match body with
    | Ast.Function.BodyBlock (_, {Block.body}) ->
      body
    | Ast.Function.BodyExpression expr ->
      [fst expr, Return {Return.argument = Some expr}]
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
  let is_void = Abnormal.(
    match catch_control_flow_exception (fun () -> stmts cx statements) with
    | Some Return -> false
    | Some Throw -> false (* NOTE *)
    | Some exn -> throw_control_flow_exception exn (* NOTE *)
    | None -> true
  ) in

  (* build return type for void funcs *)
  (if is_void then
    let loc = loc_of_t return_t in
    (* Some branches add an ImplicitTypeParam frame to force our flow_use_op
     * algorithm to pick use_ops outside the provided loc. *)
    let use_op, void_t = match kind with
    | Ordinary
    | Ctor ->
      let t = VoidT.at loc in
      let use_op = Op (FunImplicitReturn {fn = reason_fn; upper = reason_of_t return_t}) in
      use_op, t
    | Async ->
      let reason = annot_reason (mk_reason (RType "Promise") loc) in
      let void_t = VoidT.at loc in
      let t = Flow.get_builtin_typeapp cx reason "Promise" [void_t] in
      let use_op = Op (FunImplicitReturn {fn = reason_fn; upper = reason_of_t return_t}) in
      let use_op = Frame (ImplicitTypeParam (loc_of_t return_t), use_op) in
      use_op, t
    | Generator ->
      let reason = annot_reason (mk_reason (RType "Generator") loc) in
      let void_t = VoidT.at loc in
      let t = Flow.get_builtin_typeapp cx reason "Generator" [yield_t; void_t; next_t] in
      let use_op = Op (FunImplicitReturn {fn = reason_fn; upper = reason_of_t return_t}) in
      let use_op = Frame (ImplicitTypeParam (loc_of_t return_t), use_op) in
      use_op, t
    | AsyncGenerator ->
      let reason = annot_reason (mk_reason (RType "AsyncGenerator") loc) in
      let void_t = VoidT.at loc in
      let t = Flow.get_builtin_typeapp cx reason "AsyncGenerator" [yield_t; void_t; next_t] in
      let use_op = Op (FunImplicitReturn {fn = reason_fn; upper = reason_of_t return_t}) in
      let use_op = Frame (ImplicitTypeParam (loc_of_t return_t), use_op) in
      use_op, t
    | FieldInit e ->
      unknown_use, expr cx e
    | Predicate ->
      let loc = loc_of_reason reason in
      Flow_js.add_output cx
        Flow_error.(EUnsupportedSyntax (loc, PredicateVoidReturn));
      let t = VoidT.at loc in
      let use_op = Op (FunImplicitReturn {fn = reason_fn; upper = reason_of_t return_t}) in
      use_op, t
    in
    Flow.flow cx (void_t, UseT (use_op, return_t))
  );

  Env.pop_var_scope ();

  Env.update_env cx loc env

let to_ctor_sig f = { f with kind = Ctor }
