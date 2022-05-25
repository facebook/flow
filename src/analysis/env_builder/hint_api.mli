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
  | Decomp_MethodName of string
  | Decomp_MethodElem
  | Decomp_CallNew
  | Decomp_CallSuper
  | Decomp_CallSuperMem of string
  | Decomp_FuncParam of int
  | Decomp_FuncRest of int
  | Decomp_FuncReturn
  | Decomp_JsxProps

type 't hint =
  | Hint_t of 't
  | Hint_Decomp of hint_decomposition Nel.t * 't
  | Hint_None

val string_of_hint_unknown_kind : hint_decomposition -> string

val string_of_hint : on_hint:('t -> string) -> 't hint -> string

val decompose_hint : hint_decomposition -> 't hint -> 't hint
