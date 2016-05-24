module Env = Env_js
module Ast = Spider_monkey_ast
module Anno = Type_annotation
module Flow = Flow_js
module FlowError = Flow_error

open Reason_js
open Type

type kind =
  | Ordinary
  | Async
  | Generator
  | FieldInit of Ast.Expression.t

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

let function_kind {Ast.Function.async; generator; _ } =
  match async, generator with
  | true, true -> Utils_js.assert_false "async && generator"
  | true, false -> Async
  | false, true -> Generator
  | false, false -> Ordinary

let mk_fn cx tparams tparams_map reason func params =
  let {Ast.Function.returnType; body; _} = func in
  let kind = function_kind func in
  let return_t =
    let reason = mk_reason "return" (return_loc func) in
    Anno.mk_type_annotation cx tparams_map reason returnType
  in
  {reason; kind; tparams; tparams_map; params; body; return_t}

let mk_function cx tparams_map reason this func =
  let tparams, tparams_map = Anno.mk_type_param_declarations
                               cx tparams_map func.Ast.Function.typeParameters
  in
  let params = Func_params.mk_function cx tparams_map this func in
  mk_fn cx tparams tparams_map reason func params

let mk_method cx tparams_map reason implicit_this func =
  let tparams, tparams_map = Anno.mk_type_param_declarations
                               cx tparams_map func.Ast.Function.typeParameters
  in
  let params = Func_params.mk_method cx tparams_map implicit_this func in
  mk_fn cx tparams tparams_map reason func params

let empty_body =
  let loc = Loc.none in
  let body = [] in
  Ast.Function.BodyBlock (loc, {Ast.Statement.Block.body})

let convert_fn cx tparams tparams_map loc func params =
  let reason = mk_reason "function type" loc in
  let kind = Ordinary in
  let body = empty_body in
  let return_t = Ast.Type.Function.(
    Anno.convert cx tparams_map func.returnType
  ) in
  {reason; kind; tparams; tparams_map; params; body; return_t}

let convert_function cx tparams_map this loc func =
  let tparams, tparams_map = Ast.Type.Function.(
    Anno.mk_type_param_declarations cx tparams_map func.typeParameters
  ) in
  let params = Func_params.convert_function cx tparams_map this func in
  convert_fn cx tparams tparams_map loc func params

let convert_method cx tparams_map ?(static=false) loc func =
  let tparams, tparams_map = Ast.Type.Function.(
    Anno.mk_type_param_declarations cx tparams_map func.typeParameters
  ) in
  let params = Func_params.convert_method cx tparams_map ~static func in
  convert_fn cx tparams tparams_map loc func params

let default_constructor tparams_map reason = {
  reason;
  kind = Ordinary;
  tparams = [];
  tparams_map;
  params = Func_params.empty Flow.dummy_this;
  body = empty_body;
  return_t = VoidT.t;
}

let field_initializer tparams_map reason expr return_t = {
  reason;
  kind = FieldInit expr;
  tparams = [];
  tparams_map;
  params = Func_params.empty Flow.dummy_this; (*TJP: Find some test cases that activate this*)
  body = empty_body;
  return_t;
}

let this x = Func_params.this x.params

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

let functiontype cx {reason; tparams; params; return_t; _} =
  let static =
    let reason = prefix_reason "statics of " reason in
    let proto = FunProtoT reason in
    Flow.mk_object_with_proto cx reason proto
  in
  let prototype =
    let reason = replace_reason "prototype" reason in
    Flow.mk_object cx reason
  in
  let funtype = { Type.
    this_t = Func_params.this params;
    params_tlist = Func_params.tlist params;
    params_names = Some (Func_params.names params);
    return_t;
    closure_t = Env.peek_frame ();
    changeset = Env.retrieve_closure_changeset ()
  } in
  let t = FunT (reason, static, prototype, funtype) in
  if tparams = []
  then t
  else PolyT (tparams, t)

let methodtype {reason; tparams; params; return_t; _} =
  let this = Func_params.this params in
  let params_tlist = Func_params.tlist params in
  let params_names = Func_params.names params in
  let t = FunT (
    reason,
    Flow.dummy_static reason,
    Flow.dummy_prototype,
    Flow.mk_methodtype this params_tlist ~params_names return_t
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
    Flow.mk_functiontype2 params_tlist ~params_names return_t frame
  )

let gettertype x =
  match methodtype x with
  | FunT (_, _, _, { Type.return_t; _; }) -> return_t
  | _ -> failwith "Getter property with unexpected type"

let settertype x =
  match methodtype x with
  | FunT (_, _, _, { params_tlist = [param_t]; _; }) -> param_t
  | _ ->  failwith "Setter property with unexpected type"

let toplevels id cx this super ~decls ~stmts ~expr
  {kind; tparams_map; params; body; return_t; _} =

  let reason =
    let loc = Ast.Function.(match body with
      | BodyBlock (loc, _)
      | BodyExpression (loc, _) -> loc
    ) in
    mk_reason "function body" loc
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
      | Async -> Scope.Async
      | Generator -> Scope.Generator
    in
    Scope.fresh ~var_scope_kind ()
  in

  (* push the scope early so default exprs can reference earlier params *)
  Env.push_var_scope cx function_scope;

  (* add param bindings *)
  let const_params = Context.enable_const_params cx in
  params |> Func_params.iter Scope.(fun (name, t, loc) ->
    let reason = mk_reason (Utils_js.spf "param `%s`" name) loc in
    (* add default value as lower bound, if provided *)
    Func_params.with_default name (fun default ->
      let default_t = Flow.mk_default cx reason default
        ~expr:(expr cx tparams_map)
      in
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
  Option.iter id ~f:(fun (loc, {Ast.Identifier.name; _}) ->
    let entry = Scope.Entry.new_var ~loc (AnyT.at loc) in
    Scope.add_entry name entry function_scope
  );

  let yield_t, next_t =
    if kind = Generator then
      Flow.mk_tvar cx (prefix_reason "yield of " reason),
      Flow.mk_tvar cx (prefix_reason "next of " reason)
    else
      MixedT (replace_reason "no yield" reason, Mixed_everything),
      MixedT (replace_reason "no next" reason, Mixed_everything)
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

  (* decl/type visit pre-pass *)
  decls cx tparams_map statements;

  (* statement visit pass *)
  let is_void = Abnormal.(
    match catch_control_flow_exception (fun () ->
      stmts cx tparams_map statements
    ) with
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
      let reason = mk_reason "Promise<void>" loc in
      let promise = Flow.get_builtin cx "Promise" reason in
      FunImplicitReturn, TypeAppT (promise, [VoidT.at loc])
    | Generator ->
      let reason = mk_reason "return Generator<Yield,void,Next>" loc in
      let return_t = VoidT.at loc in
      FunImplicitReturn,
      Flow.get_builtin_typeapp cx reason "Generator" [yield_t; return_t; next_t]
    | FieldInit e ->
      let return_t = expr cx tparams_map e in
      UnknownUse, return_t
    in
    Flow.flow cx (void_t, UseT (use_op, return_t))
  );

  Env.pop_var_scope ();

  Env.update_env cx reason env
