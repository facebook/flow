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

module Flow = Flow_js

open Reason
open Type
open Destructuring

type param = string option * Type.t
type rest = string option * Loc.t * Type.t
type default = (Loc.t, Loc.t) Flow_ast.Expression.t Default.t
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

let add_simple cx ~optional ?default loc id t x =
  let param_t = if optional || default <> None then Type.optional t else t in
  let bound_t = if default <> None then t else param_t in
  Type_table.set (Context.type_table cx) loc t;
  let name = Option.map id ~f:(fun (id_loc, name) ->
    let id_info = name, bound_t, Type_table.Other in
    Type_table.set_info id_loc id_info (Context.type_table cx);
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

let add_complex cx ~expr ?default patt t x =
  let default = Option.map default Default.expr in
  let bindings_rev = ref x.bindings_rev in
  let patt = destructuring cx ~expr t None default patt ~f:(fun ~use_op:_ loc name default t ->
    let t = match type_of_pattern patt with
    | Flow_ast.Type.Missing _ -> t
    | Flow_ast.Type.Available _ ->
      let reason = mk_reason (RIdentifier name) (loc |> ALoc.of_loc) in
      EvalT (t, DestructuringT (reason, Become), mk_id())
    in
    Type_table.set (Context.type_table cx) loc t;
    bindings_rev := (name, loc, t, default) :: !bindings_rev
  ) in
  let t = if default <> None then Type.optional t else t in
  let params_rev = (None, t) :: x.params_rev in
  let bindings_rev = !bindings_rev in
  { x with params_rev; bindings_rev }, patt

let add_rest cx loc id t x =
  let name = Option.map id ~f:(fun (id_loc, name) ->
    let id_info = name, t, Type_table.Other in
    Type_table.set_info id_loc id_info (Context.type_table cx);
    name
  ) in
  let rest = Some (name, loc, t) in
  let bindings_rev = match id with
  | None -> x.bindings_rev
  | Some (_, name) -> (name, loc, t, None) :: x.bindings_rev
  in
  { x with rest; bindings_rev }

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
