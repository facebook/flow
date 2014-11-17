(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Ast
open Utils
module Str = Str_ext
let (=~) = Str.(=~)
let mk_set = List.fold_left (fun acc x -> SSet.add x acc) SSet.empty

let all_collections = mk_set [
  "Vector";
  "ImmVector";
  "Set";
  "ImmSet";
  "Pair";
  "ImmMap";
  "Map";
]

let hh_coll_interfaces = mk_set [
  "Collection";
  "Traversable";
  "Container";
  "KeyedTraversable";
  "KeyedContainer";
  "Iterator";
  "Iterable";
  "KeyedIterable";
  "KeyedIterator";
]

(* Note that Traversable and Iterator are in both sets. *)
let global_coll_interfaces = mk_set [
  "Traversable";
  "Iterator";
  "ConstCollection";
  "OutputCollection";
  "ConstSetAccess";
  "SetAccess";
  "ConstIndexAccess";
  "IndexAccess";
  "ConstMapAccess";
  "MapAccess";
  "Stringish";
  "Indexish";
  "ConstVector";
  "MutableVector";
  "ConstMap";
  "MutableMap";
  "ConstSet";
  "MutableSet";
]

let hh_classes_and_interfaces = SSet.union all_collections hh_coll_interfaces

let inst_var p obj name = (p, Obj_get (
  obj,
  (p, Id (p, name)),
  OG_nullthrows))

let this_var p = inst_var p (p, Id (p, "$this"))

let call_func_expr_ p name args =
  Call ((p, Id (p, name)), args, [])

let call_func p name args =
  (p, call_func_expr_ p name args)

let call_inst_func_expr_ p expr name args =
  Call (
    (inst_var p expr name),
    args,
    [])

let call_inst_func p expr name args =
  (p, call_inst_func_expr_ p expr name args)

let call_this_func p = call_inst_func p (p, Id (p, "$this"))

let call_static_func_expr_ p klass name args =
  Call (
    (p, Class_const ((p, klass),(p, name))),
    args,
    []
  )
let call_static_func p klass name args =
  (p, call_static_func_expr_ p klass name args)

let call_self_func p = call_static_func p "self"

let assign_internal p lvar_exp e =
  (p, Binop ((Eq None), lvar_exp, e))

let assign p name e =
  assign_internal p (p, Lvar (p, name)) e

let assign_inst p obj name e =
  assign_internal p (inst_var p obj name) e

let assign_this p = assign_inst p (p, Id (p, "$this"))

let assign_static p klass name e =
  assign_internal p (p, (Class_get ((p, klass), (p, "$" ^ name)))) e

let assign_self p = assign_static p "self"

let default_method = {
  m_kind = [Public];
  m_tparams = [];
  m_name = (Pos.none, "");
  m_params = [];
  m_body = [Noop];
  m_user_attributes = SMap.empty;
  m_ret = None;
  m_ret_by_ref = false;
  m_fun_kind = FSync
}

let negate (p, expr_) =
  (p, Unop (Unot, (p, expr_)))

let base_collection_str name =
  (* regex to remove a \HH\ prefix or HH\ prefix *)
  if not (name =~ "\\(\\\\?HH\\\\\\)?\\(.*\\)")
  then None
  else begin
    let inner_name = Str.matched_group 2 name in
    let has_prefix = try
        ignore (Str.matched_group 1 name);
        true
      with
        Not_found -> false in
    if has_prefix && SSet.mem inner_name hh_classes_and_interfaces
    then Some ("\\HH\\" ^ inner_name)
    else begin
    (* prioritize HH interfaces over global *)
    if SSet.mem name hh_classes_and_interfaces
    then Some ("\\HH\\" ^ inner_name)
    else
      if SSet.mem inner_name global_coll_interfaces
      then Some ("\\" ^ inner_name)
      else None
    end
  end

let is_collection_str name =
  base_collection_str name |> Option.is_some

let rec is_collection_expr_ = function
  | Collection _ -> Some true
  | New ((_, Id (_, name)), _, _) when is_collection_str name -> Some true
  | New ((_, Id (_, _)), _, _) -> Some false
  | Lvar _ | Clone _ | Obj_get _ | Array_get _ | Class_get _  | Yield _
  (* casts to object are the only type we need to worry about *)
  | Cast ((_, Happly ((_, "object"), [])), _) | Eif _ | Call _
  | Unsafeexpr _ | Expr_list _ | New _ -> None
  | Binop ((Eq None), _, (_, e)) | Ref (_, e) -> is_collection_expr_ e
  | Array _  | Shape _ | Null | True | False | Class_const _ | Int _
  | Float _ | String _ | String2 _ | Yield_break | List _  | InstanceOf _
  | Efun _ | Lfun _ | Xml _ | Import _ | Id _
   (* await returns an awaitable *)
  | Await _ | Unop _ | Binop _
  | Cast _ -> Some false
