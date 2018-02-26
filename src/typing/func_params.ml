module Anno = Type_annotation
module Flow = Flow_js

open Reason
open Type
open Destructuring

type binding = string option * Type.t * Loc.t
type param =
  | Simple of Type.t * binding
  | Complex of Type.t * binding list
type t = {
  list: param list;
  rest: binding option;
  defaults: Loc.t Ast.Expression.t Default.t SMap.t;
}

let empty = {
  list = [];
  rest = None;
  defaults = SMap.empty
}

(* Loc.t Ast.Function.t -> Func_params.t *)
let mk cx type_params_map ~expr func =
  let add_param_with_default params pattern default = Ast.Pattern.(
    match pattern with
    | loc, Identifier { Ast.Pattern.Identifier.
        name = (id_loc, name);
        typeAnnotation;
        optional;
      } ->
      let reason = mk_reason (RParameter (Some name)) loc in
      let t = Anno.mk_type_annotation cx type_params_map reason typeAnnotation
      in (match default with
      | None ->
        let t =
          if optional
          then Type.optional t
          else t
        in
        Type_table.set (Context.type_table cx) loc t;
        let id_info = name, t, Type_table.Other in
        Type_table.set_info (Context.type_table cx) id_loc id_info;
        let binding = Some name, t, loc in
        let list = Simple (t, binding) :: params.list in
        { params with list }
      | Some expr ->
        (* TODO: assert (not optional) *)
        let binding = Some name, t, loc in
        { list = Simple (Type.optional t, binding) :: params.list;
          defaults = SMap.add name (Default.Expr expr) params.defaults;
          rest = params.rest; })
    | loc, _ ->
      let reason = mk_reason RDestructuring loc in
      let typeAnnotation = type_of_pattern pattern in
      let t = typeAnnotation
        |> Anno.mk_type_annotation cx type_params_map reason in
      let default = Option.map default Default.expr in
      let rev_bindings = ref [] in
      let defaults = ref params.defaults in
      destructuring cx ~expr t None default pattern ~f:(fun ~use_op:_ loc name default t ->
        let t = match typeAnnotation with
        | None -> t
        | Some _ ->
          let reason = mk_reason (RIdentifier name) loc in
          EvalT (t, DestructuringT (reason, Become), mk_id())
        in
        Type_table.set (Context.type_table cx) loc t;
        rev_bindings := (Some name, t, loc) :: !rev_bindings;
        Option.iter default ~f:(fun default ->
          defaults := SMap.add name default !defaults
        )
      );
      let t = match default with
        | Some _ -> Type.optional t
        | None -> t (* TODO: assert (not optional) *)
      in
      let param = Complex (t, List.rev !rev_bindings) in
      {
        list = param :: params.list;
        defaults = !defaults;
        rest = params.rest;
      }
  ) in
  let add_rest params pattern =
    match pattern with
    | loc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
        name = (_, name);
        typeAnnotation;
        _;
      } ->
      let reason = mk_reason (RRestParameter (Some name)) loc in
      let t =
        Anno.mk_type_annotation cx type_params_map reason typeAnnotation
      in
      { params with rest = Some (Some name, t, loc) }
    | loc, _ ->
      Flow_js.add_output cx
        Flow_error.(EInternal (loc, RestParameterNotIdentifierPattern));
      params
  in
  let add_param params pattern =
    match pattern with
    | _, Ast.Pattern.Assignment { Ast.Pattern.Assignment.left; right; } ->
      add_param_with_default params left (Some right)
    | _ ->
      add_param_with_default params pattern None
  in
  let {Ast.Function.params = (_, { Ast.Function.Params.params; rest }); _} = func in
  let params = List.fold_left add_param empty params in
  let params = match rest with
    | Some (_, { Ast.Function.RestElement.argument }) ->
      add_rest params argument
    | None -> params
  in
  { params with list = List.rev params.list }

(* Ast.Type.Function.t -> Func_params.t *)
let convert cx type_params_map func = Ast.Type.Function.(
  let add_param params (loc, {Param.name=id; typeAnnotation; optional; _}) =
    let name = Option.map id ~f:(fun (_, x) -> x) in
    let t = Anno.convert cx type_params_map typeAnnotation in
    let t = if optional then Type.optional t else t in
    let binding = name, t, loc in
    Option.iter ~f:(fun (loc, name) ->
      let id_info = name, t, Type_table.Other in
      Type_table.set_info (Context.type_table cx) loc id_info
    ) id;
    { params with list = Simple (t, binding) :: params.list }
  in
  let add_rest params (loc, {Param.name; typeAnnotation; _}) =
    let name = match name with
    | None -> None
    | Some (_, name) -> Some name in
    let t = Anno.convert cx type_params_map typeAnnotation in
    let reason = mk_reason (RRestParameter name) (loc_of_t t) in
    Flow.flow cx (t, AssertRestParamT reason);
    { params with rest = Some (name, t, loc) }
  in
  let (_, { Params.params; rest }) = func.Ast.Type.Function.params in
  let params = List.fold_left add_param empty params in
  let params = match rest with
  | Some (_, { RestParam.argument }) -> add_rest params argument
  | None -> params
  in
  { params with list = List.rev params.list }
)

let value params =
  params.list |> List.map (function
    | Simple (t, (name, _, _)) -> name, t
    | Complex (t, _) -> None, t)

let rest params =
  match params.rest with
  | None -> None
  | Some (name, t, loc) -> Some (name, loc, t)

let iter f params =
  params.list |> List.iter (function
    | Simple (_, b) -> f b
    | Complex (_, bs) -> List.iter f bs);
  params.rest |> Option.iter ~f

let with_default name f params =
  match SMap.get name params.defaults with
  | Some t -> f t
  | None -> ()

let subst_binding cx map (name, t, loc) = (name, Flow.subst cx map t, loc)

let subst cx map params =
  let list = params.list |> List.map (function
    | Simple (t, b) ->
      Simple (Flow.subst cx map t, subst_binding cx map b)
    | Complex (t, bs) ->
      Complex (Flow.subst cx map t, List.map (subst_binding cx map) bs)) in
  let rest = Option.map ~f:(subst_binding cx map) params.rest in
  { params with list; rest; }
