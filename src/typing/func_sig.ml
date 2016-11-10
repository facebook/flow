(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Ast = Spider_monkey_ast
module Anno = Type_annotation
module Flow = Flow_js

open Reason
open Type

type kind =
  | Ordinary
  | Async
  | Generator
  | AsyncGenerator
  | FieldInit of Ast.Expression.t
  | Predicate

type t = {
  reason: reason;
  kind: kind;
  tparams: Type.typeparam list;
  tparams_map: Type.t SMap.t;
  params: Func_params.t;
  body: Ast.Function.body;
  return_t: Type.t;
}

let return_loc =
  let module F = Ast.Function in
  let open F in function
  | {returnType = Some (_, (loc, _)); _}
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

let mk cx tparams_map ~expr reason func =
  let {Ast.Function.typeParameters; returnType; body; predicate; _} = func in
  let kind = function_kind func in
  let tparams, tparams_map =
    Anno.mk_type_param_declarations cx ~tparams_map typeParameters
  in
  let params = Func_params.mk cx tparams_map ~expr func in
  let return_t =
    let reason = mk_reason RReturn (return_loc func) in
    Anno.mk_type_annotation cx tparams_map reason returnType
  in
  let return_t = Ast.Type.Predicate.(match predicate with
    | None ->
        return_t
    | Some (_, Inferred) ->
        (* Restrict the fresh condition type by the declared return type *)
        let fresh_t = Anno.mk_type_annotation cx tparams_map reason None in
        Flow.flow_t cx (fresh_t, return_t);
        fresh_t
    | Some (loc, Declared _) ->
        Flow_error.(add_output cx
          (EUnsupportedSyntax (loc, PredicateDeclarationForImplementation)));
        Anno.mk_type_annotation cx tparams_map reason None
  ) in
  {reason; kind; tparams; tparams_map; params; body; return_t}

let empty_body =
  let loc = Loc.none in
  let body = [] in
  Ast.Function.BodyBlock (loc, {Ast.Statement.Block.body})

let convert cx tparams_map loc func =
  let {Ast.Type.Function.typeParameters; returnType; _} = func in
  let reason = mk_reason RFunctionType loc in
  let kind = Ordinary in
  let tparams, tparams_map =
    Anno.mk_type_param_declarations cx ~tparams_map typeParameters
  in
  let params = Func_params.convert cx tparams_map func in
  let body = empty_body in
  let return_t = Anno.convert cx tparams_map returnType in
  {reason; kind; tparams; tparams_map; params; body; return_t}

let default_constructor reason = {
  reason;
  kind = Ordinary;
  tparams = [];
  tparams_map = SMap.empty;
  params = Func_params.empty;
  body = empty_body;
  return_t = VoidT.t;
}

let field_initializer tparams_map reason expr return_t = {
  reason;
  kind = FieldInit expr;
  tparams = [];
  tparams_map;
  params = Func_params.empty;
  body = empty_body;
  return_t;
}

let subst cx map x =
  let {tparams; tparams_map; params; return_t; _} = x in
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
  let params = Func_params.subst cx map params in
  let return_t = Flow.subst cx map return_t in
  {x with tparams; tparams_map; params; return_t}

let generate_tests cx f x =
  let {reason; tparams; tparams_map; params; return_t; _} = x in
  Flow.generate_tests cx reason tparams (fun map -> f {
    x with
    tparams_map = SMap.map (Flow.subst cx map) tparams_map;
    params = Func_params.subst cx map params;
    return_t = Flow.subst cx map return_t;
  })

let functiontype cx this_t {reason; kind; tparams; params; return_t; _} =
  let static =
    let reason = replace_reason (fun desc -> RStatics desc) reason in
    let proto = FunProtoT reason in
    Flow.mk_object_with_proto cx reason proto
  in
  let prototype =
    let reason = replace_reason_const RPrototype reason in
    Flow.mk_object cx reason
  in
  let funtype = { Type.
    this_t;
    params_tlist = Func_params.tlist params;
    params_names = Some (Func_params.names params);
    return_t;
    is_predicate = kind = Predicate;
    closure_t = Env.peek_frame ();
    changeset = Env.retrieve_closure_changeset ()
  } in
  let t = FunT (reason, static, prototype, funtype) in
  if tparams = []
  then t
  else PolyT (tparams, t)

let methodtype {reason; tparams; params; return_t; _} =
  let params_tlist = Func_params.tlist params in
  let params_names = Func_params.names params in
  let t = FunT (
    reason,
    Flow.dummy_static reason,
    Flow.dummy_prototype,
    Flow.mk_boundfunctiontype params_tlist ~params_names return_t
  ) in
  if tparams = []
  then t
  else PolyT (tparams, t)

let methodtype_DEPRECATED {reason; params; return_t; _} =
  let params_tlist = Func_params.tlist params in
  let params_names = Func_params.names params in
  let frame = Env.peek_frame () in
  FunT (
    reason,
    Flow.dummy_static reason,
    Flow.dummy_prototype,
    Flow.mk_functiontype params_tlist ~params_names return_t ~frame
  )

let gettertype ({return_t; _}: t) = return_t

let settertype {params; _} =
  match Func_params.tlist params with
  | [param_t] -> param_t
  | _ -> failwith "Setter property with unexpected type"

let toplevels id cx this super ~decls ~stmts ~expr
  {kind; tparams_map; params; body; return_t; _} =

  let reason =
    let loc = Ast.Function.(match body with
      | BodyBlock (loc, _)
      | BodyExpression (loc, _) -> loc
    ) in
    mk_reason RFunctionBody loc
  in

  let env =  Env.peek_env () in
  let new_env = Env.clone_env env in

  Env.update_env cx reason new_env;
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
    in
    Scope.fresh ~var_scope_kind ()
  in

  (* push the scope early so default exprs can reference earlier params *)
  Env.push_var_scope cx function_scope;

  (* bind type params *)
  SMap.iter (fun name t ->
    let r = reason_of_t t in
    Env.bind_type cx name (TypeT (r, t)) r
      ~state:Scope.State.Initialized
  ) tparams_map;

  (* add param bindings *)
  let const_params = Context.enable_const_params cx in
  params |> Func_params.iter Scope.(fun (name, t, loc) ->
    let reason = mk_reason (RParameter name) loc in
    (* add default value as lower bound, if provided *)
    Func_params.with_default name (fun default ->
      let default_t = Flow.mk_default cx reason default ~expr in
      Flow.flow_t cx (default_t, t)
    ) params;
    (* add to scope *)
    if const_params
    then Env.bind_implicit_const ~state:State.Initialized
      Entry.ConstParamBinding cx name t reason
    else Env.bind_implicit_let ~state:State.Initialized
      Entry.ParamBinding cx name t reason
  );

  (* early-add our own name binding for recursive calls *)
  Option.iter id ~f:(fun (loc, name) ->
    let entry = Scope.Entry.new_var ~loc (AnyT.at loc) in
    Scope.add_entry name entry function_scope
  );

  let yield_t, next_t =
    if kind = Generator || kind = AsyncGenerator then
      Flow.mk_tvar cx (replace_reason_const (RCustom "yield") reason),
      Flow.mk_tvar cx (replace_reason_const (RCustom "next") reason)
    else
      MixedT (replace_reason_const (RCustom "no yield") reason, Mixed_everything),
      MixedT (replace_reason_const (RCustom "no next") reason, Mixed_everything)
  in

  let yield, next, return = Scope.(
    let new_entry t = Entry.(
      let loc = loc_of_t t in
      let state = State.Initialized in
      new_const ~loc ~state t
    ) in
    new_entry yield_t, new_entry next_t, new_entry return_t
  ) in

  Scope.add_entry (internal_name "this") this function_scope;
  Scope.add_entry (internal_name "super") super function_scope;
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
          Flow_error.(add_output cx
            (EUnsupportedSyntax (loc, PredicateInvalidBody)))
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
    let use_op, void_t = match kind with
    | Ordinary ->
      FunImplicitReturn, VoidT.at loc
    | Async ->
      let reason = mk_reason (RCustom "Promise<void>") loc in
      let promise = Flow.get_builtin cx "Promise" reason in
      FunImplicitReturn, TypeAppT (promise, [VoidT.at loc])
    | Generator ->
      let reason = mk_reason (RCustom "Generator<Yield,void,Next>") loc in
      let return_t = VoidT.at loc in
      FunImplicitReturn,
      Flow.get_builtin_typeapp cx reason "Generator" [yield_t; return_t; next_t]
    | AsyncGenerator ->
      let reason = mk_reason (RCustom "AsyncGenerator<Yield,void,Next>") loc in
      let return_t = VoidT.at loc in
      FunImplicitReturn,
      Flow.get_builtin_typeapp cx reason "AsyncGenerator" [yield_t; return_t; next_t]
    | FieldInit e ->
      let return_t = expr cx e in
      UnknownUse, return_t
    | Predicate ->
      let loc = loc_of_reason reason in
      Flow_error.(add_output cx
        (EUnsupportedSyntax (loc, PredicateVoidReturn)));
      FunImplicitReturn, VoidT.at loc
    in
    Flow.flow cx (void_t, UseT (use_op, return_t))
  );

  Env.pop_var_scope ();

  Env.update_env cx reason env
