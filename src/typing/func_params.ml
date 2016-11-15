module Ast = Spider_monkey_ast
module Anno = Type_annotation
module Flow = Flow_js
module Utils = Utils_js

open Reason
open Type
open Destructuring

type binding = string * Type.t * Loc.t
type param =
  | Simple of Type.t * binding
  | Complex of Type.t * binding list
  | Rest of Type.t * binding
type t = {
  list: param list;
  defaults: Ast.Expression.t Default.t SMap.t;
}

let empty = {
  list = [];
  defaults = SMap.empty
}

(* Ast.Function.t -> Func_params.t *)
let mk cx type_params_map ~expr func =
  let add_param_with_default params pattern default = Ast.Pattern.(
    match pattern with
    | loc, Identifier { Ast.Pattern.Identifier.
        name = (_, name);
        typeAnnotation;
        optional;
      } ->
      let reason = mk_reason (RParameter name) loc in
      let t = Anno.mk_type_annotation cx type_params_map reason typeAnnotation
      in (match default with
      | None ->
        let t =
          if optional
          then OptionalT t
          else t
        in
        Hashtbl.replace (Context.type_table cx) loc t;
        let binding = name, t, loc in
        let list = Simple (t, binding) :: params.list in
        { params with list }
      | Some expr ->
        (* TODO: assert (not optional) *)
        let binding = name, t, loc in
        { list = Simple (OptionalT t, binding) :: params.list;
          defaults = SMap.add name (Default.Expr expr) params.defaults })
    | loc, _ ->
      let reason = mk_reason RDestructuring loc in
      let typeAnnotation = type_of_pattern pattern in
      let t = typeAnnotation
        |> Anno.mk_type_annotation cx type_params_map reason in
      let default = Option.map default Default.expr in
      let rev_bindings = ref [] in
      let defaults = ref params.defaults in
      destructuring cx ~expr t None default pattern ~f:(fun loc name default t ->
        let t = match typeAnnotation with
        | None -> t
        | Some _ ->
          let reason = repos_reason loc reason in
          EvalT (t, DestructuringT (reason, Become), mk_id())
        in
        Hashtbl.replace (Context.type_table cx) loc t;
        rev_bindings := (name, t, loc) :: !rev_bindings;
        Option.iter default ~f:(fun default ->
          defaults := SMap.add name default !defaults
        )
      );
      let t = match default with
        | Some _ -> OptionalT t
        | None -> t (* TODO: assert (not optional) *)
      in
      let param = Complex (t, List.rev !rev_bindings) in
      { list = param :: params.list; defaults = !defaults }
  ) in
  let add_rest params pattern =
    match pattern with
    | loc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
        name = (_, name);
        typeAnnotation;
        _;
      } ->
      let reason = mk_reason (RRestParameter name) loc in
      let t =
        Anno.mk_type_annotation cx type_params_map reason typeAnnotation
      in
      let param = Rest (Anno.mk_rest cx t, (name, t, loc)) in
      { params with list =  param :: params.list }
    | loc, _ ->
      Flow_error.(add_output cx
        (EInternal (loc, RestArgumentNotIdentifierPattern)));
      params
  in
  let add_param params pattern =
    match pattern with
    | _, Ast.Pattern.Assignment { Ast.Pattern.Assignment.left; right; } ->
      add_param_with_default params left (Some right)
    | _ ->
      add_param_with_default params pattern None
  in
  let {Ast.Function.params = (params, rest); _} = func in
  let params = List.fold_left add_param empty params in
  let params = match rest with
    | Some (_, { Ast.Function.RestElement.argument }) ->
      add_rest params argument
    | None -> params
  in
  { params with list = List.rev params.list }

(* Ast.Type.Function.t -> Func_params.t *)
let convert cx type_params_map func = Ast.Type.Function.(
  let add_param params (loc, {Param.name; typeAnnotation; optional; _}) =
    let name = match name with
    | None -> "_"
    | Some (_, name) -> name in
    let t = Anno.convert cx type_params_map typeAnnotation in
    let t = if optional then OptionalT t else t in
    let binding = name, t, loc in
    { params with list = Simple (t, binding) :: params.list }
  in
  let add_rest params (loc, {Param.name; typeAnnotation; _}) =
    let name = match name with
    | None -> "_"
    | Some (_, name) -> name in
    let t = Anno.convert cx type_params_map typeAnnotation in
    let param = Rest (Anno.mk_rest cx t, (name, t, loc)) in
    { params with list = param :: params.list }
  in
  let (params, rest) = func.params in
  let params = List.fold_left add_param empty params in
  let params = match rest with
  | Some (_, { RestParam.argument }) -> add_rest params argument
  | None -> params
  in
  { params with list = List.rev params.list }
)

let names params =
  params.list |> List.map (function
    | Simple (_, (name, _, _))
    | Rest (_, (name, _, _)) -> name
  | Complex _ -> "_")

let tlist params =
  params.list |> List.map (function
    | Simple (t, _)
    | Complex (t, _)
    | Rest (t, _) -> t)

let iter f params =
  params.list |> List.iter (function
    | Simple (_, b)
    | Rest (_, b) -> f b
    | Complex (_, bs) -> List.iter f bs)

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
      Complex (Flow.subst cx map t, List.map (subst_binding cx map) bs)
    | Rest (t, b) ->
      Rest (Flow.subst cx map t, subst_binding cx map b)) in
  { params with list }
