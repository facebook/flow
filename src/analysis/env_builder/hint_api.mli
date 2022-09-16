(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ('t, 'args) implicit_instantiation_hints = {
  return_hint: ('t, 'args) hint;
  arg_list: 'args;
}

and ('t, 'args) hint_decomposition =
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
  | Decomp_Instantiated of ('t, 'args) implicit_instantiation_hints Lazy.t

and ('t, 'args) hint =
  | Hint_t of 't
  | Hint_Decomp of ('t, 'args) hint_decomposition Nel.t * 't
  | Hint_Placeholder
  | Hint_None

val string_of_hint_unknown_kind : ('t, 'args) hint_decomposition -> string

val string_of_hint : on_hint:('t -> string) -> ('t, 'args) hint -> string

val decompose_hint : ('t, 'args) hint_decomposition -> ('t, 'args) hint -> ('t, 'args) hint

(** Combine two hints into one, by picking the first one if it contains useful
 *  information; otherwise picking the second hint. *)
val merge_hints : ('t, 'args) hint -> ('t, 'args) hint -> ('t, 'args) hint

val is_hint_none : ('t, 'args) hint -> bool

val map : map_base_hint:('a -> 'b) -> map_arg_list:('c -> 'd) -> ('a, 'c) hint -> ('b, 'd) hint
