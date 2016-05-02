module Env = Env_js
module Ast = Spider_monkey_ast
module Anno = Type_annotation
module Flow = Flow_js

open Reason_js
open Type

type t = {
  reason: reason;
  tparams: Type.typeparam list;
  tparams_map: Type.t SMap.t;
  params: Func_params.t;
  return_t: Type.t;
}

let return_loc = Ast.Function.(function
  | {returnType = Some (_, (loc, _)); _}
  | {body = BodyExpression (loc, _); _} -> loc
  | {body = BodyBlock (loc, _); _} -> Loc.char_before loc
)

let mk cx tparams_map reason func =
  let {Ast.Function.typeParameters; returnType; _} = func in
  let tparams, tparams_map =
    Anno.mk_type_param_declarations cx tparams_map typeParameters
  in
  let params = Func_params.mk cx tparams_map func in
  let return_t =
    let reason = mk_reason "return" (return_loc func) in
    Anno.mk_type_annotation cx tparams_map reason returnType
  in
  {reason; tparams; tparams_map; params; return_t}

let convert cx tparams_map loc func =
  let {Ast.Type.Function.typeParameters; returnType; _} = func in
  let reason = mk_reason "function type" loc in
  let tparams, tparams_map =
    Anno.mk_type_param_declarations cx tparams_map typeParameters
  in
  let params = Func_params.convert cx tparams_map func in
  let return_t = Anno.convert cx tparams_map returnType in
  {reason; tparams; tparams_map; params; return_t}

let empty reason = {
  reason;
  tparams = [];
  tparams_map = SMap.empty;
  params = Func_params.empty;
  return_t = VoidT.t;
}

let subst cx map x =
  let {tparams; tparams_map; params; return_t; _} = x in
  let tparams = List.map (fun tp ->
    {tp with bound = Flow.subst cx map tp.bound}
  ) tparams in
  let tparams_map = SMap.map (Flow.subst cx map) tparams_map in
  let params = Func_params.subst cx map params in
  let return_t = Flow.subst cx map return_t in
  {x with tparams; tparams_map; params; return_t}

let generate_tests cx f x =
  let {reason; tparams; _} = x in
  let f map = f (subst cx map x) in
  Flow.generate_tests cx reason tparams f

let functiontype cx this_t {reason; tparams; params; return_t; _} =
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
    this_t;
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
  let params_tlist = Func_params.tlist params in
  let params_names = Func_params.names params in
  let t = FunT (
    reason,
    Flow.dummy_static reason,
    Flow.dummy_prototype,
    Flow.mk_functiontype params_tlist ~params_names return_t
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
