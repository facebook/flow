(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type hint_decomposition =
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
  | Decomp_JsxProps

type 't hint =
  | Hint_t of 't
  | Hint_Decomp of hint_decomposition Nel.t * 't
  | Hint_Placeholder
  | Hint_None

val string_of_hint_unknown_kind : hint_decomposition -> string

val string_of_hint : on_hint:('t -> string) -> 't hint -> string

val decompose_hint : hint_decomposition -> 't hint -> 't hint

(** Combine two hints into one, by picking the first one if it contains useful
 *  information; otherwise picking the second hint. *)
val merge_hints : 't hint -> 't hint -> 't hint
