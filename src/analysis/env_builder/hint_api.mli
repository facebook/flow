(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ('t, 'targs, 'args) fun_call_implicit_instantiation_hints = {
  reason: Reason.t;
  return_hint: ('t, 'targs, 'args) hint;
  targs: 'targs Lazy.t;
  arg_list: 'args Lazy.t;
  arg_index: int;
}

and sentinel_refinement =
  | SingletonNum of float
  | SingletonBool of bool
  | SingletonStr of string
  | Null
  | Void
  | Member of Reason.t

and ('t, 'targs, 'args) hint_decomposition =
  | Decomp_ObjProp of string
  | Decomp_ObjComputed
  | Decomp_ObjSpread
  | Decomp_ArrElement of int
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
  | Decomp_SentinelRefinement of sentinel_refinement SMap.t
  | Instantiate_Callee of ('t, 'targs, 'args) fun_call_implicit_instantiation_hints

and ('t, 'targs, 'args) hint =
  | Hint_t of 't
  | Hint_Decomp of (int * ('t, 'targs, 'args) hint_decomposition) Nel.t * 't
  | Hint_Placeholder
  | Hint_None

val string_of_hint_unknown_kind : ('t, 'targs, 'args) hint_decomposition -> string

val string_of_hint : on_hint:('t -> string) -> ('t, 'targs, 'args) hint -> string

val decompose_hint :
  ('t, 'targs, 'args) hint_decomposition -> ('t, 'targs, 'args) hint -> ('t, 'targs, 'args) hint

(** Combine two hints into one, by picking the first one if it contains useful
 *  information; otherwise picking the second hint. *)
val merge_hints : ('t, 'targs, 'args) hint -> ('t, 'targs, 'args) hint -> ('t, 'targs, 'args) hint

val is_hint_none : ('t, 'targs, 'args) hint -> bool

val map :
  map_base_hint:('a -> 'b) ->
  map_targs:('c -> 'd) ->
  map_arg_list:('e -> 'f) ->
  ('a, 'c, 'e) hint ->
  ('b, 'd, 'f) hint
