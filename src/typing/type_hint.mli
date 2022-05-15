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
  | Decomp_ArgSpread
  | Decomp_ArrElement
  | Decomp_ArrSpread
  | Decomp_MethodName of Type.propref
  | Decomp_MethodElem of Type.t
  | Decomp_CallNew
  | Decomp_CallSuper
  | Decomp_CallSuperMem of string
  | Decomp_FuncParam of int
  | Decomp_FuncRest of int
  | Decomp_FuncReturn
  | Decomp_NullishCoalesce
  | Decomp_JsxProps
  | Decomp_JsxPropsSelect of string
  | Decomp_JsxPropsSpread
  | Decomp_JsxChildren
  | Decomp_JsxChildrenSpread

type hint =
  | Hint_t of Type.t
  | Hint_Decomp of hint_decomposition Nel.t * Type.t
  | Hint_None

val string_of_hint_unknown_kind : hint_decomposition -> string

val string_of_hint : on_hint:(Type.t -> string) -> hint -> string

val decompose_hint : hint_decomposition -> hint -> hint

val evaluate_hint : Context.t -> hint -> Type.t option
