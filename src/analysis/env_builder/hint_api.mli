(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ('t, 'targs, 'args, 'props, 'children) fun_call_implicit_instantiation_hints = {
  reason: Reason.t;
  return_hints: ('t, 'targs, 'args, 'props, 'children) hint list;
  targs: 'targs Lazy.t;
  arg_list: 'args Lazy.t;
  arg_index: int;
}

and ('t, 'targs, 'args, 'props, 'children) jsx_implicit_instantiation_hints = {
  jsx_reason: Reason.t;
  jsx_name: string;
  jsx_props: 'props;
  jsx_children: 'children;
  jsx_hints: ('t, 'targs, 'args, 'props, 'children) hint list;
}

and sentinel_refinement =
  | SingletonNum of float
  | SingletonBool of bool
  | SingletonStr of string
  | SingletonBigInt of int64
  | Null
  | Void
  | Member of Reason.t

and ('t, 'targs, 'args, 'props, 'children) hint_decomposition =
  | Decomp_ObjProp of string
  | Decomp_ObjComputed of Reason.t
  | Decomp_ObjSpread
  | Decomp_ArrElement of int option
  | Decomp_ArrSpread of int
  | Decomp_Await
  | Decomp_MethodName of string
  | Decomp_MethodPrivateName of string * ALoc.t list
  | Decomp_MethodElem
  | Decomp_CallNew
  | Decomp_CallSuper
  | Decomp_FuncParam of int
  | Decomp_FuncRest of int
  | Decomp_FuncReturn
  | Comp_ImmediateFuncCall
  | Decomp_JsxProps
  | Decomp_JsxRef
  | Decomp_SentinelRefinement of sentinel_refinement SMap.t
  | Instantiate_Callee of
      ('t, 'targs, 'args, 'props, 'children) fun_call_implicit_instantiation_hints
  | Instantiate_Component of ('t, 'targs, 'args, 'props, 'children) jsx_implicit_instantiation_hints
  | Decomp_Promise

and ('t, 'targs, 'args, 'props, 'children) hint =
  | Hint_t of 't
  | Hint_Decomp of (int * ('t, 'targs, 'args, 'props, 'children) hint_decomposition) Nel.t * 't
  | Hint_Placeholder

val string_of_hint_unknown_kind :
  ('t, 'targs, 'args, 'props, 'children) hint_decomposition -> string

val string_of_hints :
  on_hint:('t -> string) -> ('t, 'targs, 'args, 'props, 'children) hint list -> string

val decompose_hints :
  ('t, 'targs, 'args, 'props, 'children) hint_decomposition ->
  ('t, 'targs, 'args, 'props, 'children) hint list ->
  ('t, 'targs, 'args, 'props, 'children) hint list

val map :
  map_base_hint:('a -> 'b) ->
  map_targs:('c -> 'd) ->
  map_arg_list:('e -> 'f) ->
  map_jsx:(Reason.t -> string -> 'g -> 'h -> 'i * 'j) ->
  ('a, 'c, 'e, 'g, 'h) hint ->
  ('b, 'd, 'f, 'i, 'j) hint
