(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module defines a small data structure that stores function parameters
   before substitution. This is used as part of Func_sig (and Class_sig) to hold
   constraints at bay until substitution can occur.

   Function params serve two purposes: On one hand, they describe the arguments
   that a function expects. On the other, the bindings that exist within the
   body of a function. These may not be the same due to default values and
   destructuring. *)

module Anno = Type_annotation
module Flow = Flow_js

open Reason
open Type
open Destructuring

type param = string option * Type.t
type rest = string option * Loc.t * Type.t
type default = Loc.t Ast.Expression.t Default.t
type binding = string * Loc.t * Type.t * default option

type t = {
  params_rev: param list;
  rest: rest option;
  bindings_rev: binding list;
}

let empty = {
  params_rev = [];
  rest = None;
  bindings_rev = [];
}

let add_simple cx ~tparams_map ~optional ?default loc id t x =
  let param_t = if optional || default <> None then Type.optional t else t in
  let bound_t = if default <> None then t else param_t in
  Env.add_type_table cx ~tparams_map loc t;
  let name = Option.map id ~f:(fun (id_loc, name) ->
    let id_info = name, bound_t, Type_table.Other in
    Env.add_type_table_info cx ~tparams_map id_loc id_info;
    name
  ) in
  let params_rev = (name, param_t) :: x.params_rev in
  let bindings_rev = match id with
  | None -> x.bindings_rev
  | Some (_, name) ->
    let default = Option.map default Default.expr in
    (name, loc, bound_t, default) :: x.bindings_rev
  in
  { x with params_rev; bindings_rev }

let add_complex cx ~tparams_map ~expr ?default patt t x =
  let default = Option.map default Default.expr in
  let bindings_rev = ref x.bindings_rev in
  destructuring cx ~expr t None default patt ~f:(fun ~use_op:_ loc name default t ->
    let t = match type_of_pattern patt with
    | None -> t
    | Some _ ->
      let reason = mk_reason (RIdentifier name) loc in
      EvalT (t, DestructuringT (reason, Become), mk_id())
    in
    Env.add_type_table cx ~tparams_map loc t;
    bindings_rev := (name, loc, t, default) :: !bindings_rev
  );
  let t = if default <> None then Type.optional t else t in
  let params_rev = (None, t) :: x.params_rev in
  let bindings_rev = !bindings_rev in
  { x with params_rev; bindings_rev }

let add_rest cx ~tparams_map loc id t x =
  let name = Option.map id ~f:(fun (id_loc, name) ->
    let id_info = name, t, Type_table.Other in
    Env.add_type_table_info cx ~tparams_map id_loc id_info;
    name
  ) in
  let rest = Some (name, loc, t) in
  let bindings_rev = match id with
  | None -> x.bindings_rev
  | Some (_, name) -> (name, loc, t, None) :: x.bindings_rev
  in
  { x with rest; bindings_rev }

(* Loc.t Ast.Function.t -> Func_params.t *)
let mk cx tparams_map ~expr func =
  let add_param_with_default default = function
    | loc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
        name = (_, name) as id;
        annot;
        optional;
      } ->
      let reason = mk_reason (RParameter (Some name)) loc in
      let t = Anno.mk_type_annotation cx tparams_map reason annot in
      add_simple cx ~tparams_map ~optional ?default loc (Some id) t
    | loc, _ as patt ->
      let reason = mk_reason RDestructuring loc in
      let annot = type_of_pattern patt in
      let t = Anno.mk_type_annotation cx tparams_map reason annot in
      add_complex cx ~tparams_map ~expr ?default patt t
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
      add_rest cx ~tparams_map loc (Some id) t params
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
  let params = List.fold_left (fun acc param -> add_param param acc) empty params in
  match rest with
  | Some (_, { Ast.Function.RestElement.argument }) -> add_rest argument params
  | None -> params

(* Ast.Type.Function.t -> Func_params.t *)
let convert cx tparams_map func = Ast.Type.Function.(
  let add_param (loc, {Param.name=id; annot; optional; _}) =
    let t = Anno.convert cx tparams_map annot in
    add_simple cx ~tparams_map ~optional loc id t
  in
  let add_rest (loc, {Param.name=id; annot; _}) =
    let t = Anno.convert cx tparams_map annot in
    let () =
      let name = Option.map id ~f:snd in
      let reason = mk_reason (RRestParameter name) (loc_of_t t) in
      Flow.flow cx (t, AssertRestParamT reason)
    in
    add_rest cx ~tparams_map loc id t
  in
  let (_, { Params.params; rest }) = func.Ast.Type.Function.params in
  let params = List.fold_left (fun acc param -> add_param param acc) empty params in
  match rest with
  | Some (_, { RestParam.argument }) -> add_rest argument params
  | None -> params
)

let value {params_rev; _} = List.rev params_rev

let rest {rest; _} = rest

let iter f {bindings_rev; _} = List.iter f (List.rev bindings_rev)

let subst_param cx map (name, t) = (name, Flow.subst cx map t)
let subst_rest cx map (name, loc, t) = (name, loc, Flow.subst cx map t)
let subst_binding cx map (name, loc, t, default) = (name, loc, Flow.subst cx map t, default)

let subst cx map { params_rev; rest; bindings_rev } = {
  params_rev = List.map (subst_param cx map) params_rev;
  rest = Option.map ~f:(subst_rest cx map) rest;
  bindings_rev = List.map (subst_binding cx map) bindings_rev;
}
