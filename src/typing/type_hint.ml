(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* In contextual typing we often need to perform decompositions on type hints before
 * we descend deeper into an expression during type checking. For example if an
 * object literal `{ f: (x) => x + 1 }` is checked with a hint `{ f: (number) => number }`
 * then we can check the value of property `f` with the hint `(number) => number`
 * and, further, use `number` as the type of `x`. *)

type hint_decomposition =
  (* Hint on `{ f: e }` becomes hint on `e` *)
  | Decomp_ObjProp of string
  (* Hint on `{ [k]: e }` becomes hint on `e` *)
  | Decomp_ObjComputed
  (* Hint on `{ ...e }` becomes hint on `e` *)
  | Decomp_ObjSpread
  (* Hint on the argument spread `...e` becomes hint on `e` *)
  | Decomp_ArgSpread
  (* Hint on array literal `[e]` becomes hint on `e` *)
  | Decomp_ArrElement
  (* Hint on array literal `[...e]` becomes hint on `e` *)
  | Decomp_ArrSpread
  (* Type of `o` in `o.m(..)` becomes the type of `o.m` *)
  | Decomp_MethodName of Type.propref
  (* Type of `o` in `o[e](..)` becomes the type of `o[e]` *)
  | Decomp_MethodElem of Type.t
  (* Type of `C` in `new C(..)` becomes the type of the constructor of C *)
  | Decomp_CallNew
  (* Type of the super-class becomes the type of the super constructor *)
  | Decomp_CallSuper
  (* Type of the super-class becomes the type of the method in the super class *)
  | Decomp_CallSuperMem of string
  (* Type of function becomes hint on the i-th argument *)
  | Decomp_FuncParam of int
  (* Type of function becomes hint on rest argument *)
  | Decomp_FuncRest
  (* Type of functoin becomes hint on return *)
  | Decomp_FuncReturn
  (* Type of e1 in `e1 ?? e2` becomes hint on `e2` *)
  | Decomp_NullishCoalesce
  (* Type of C in `<C [props]/>` becomes hint on `props` *)
  | Decomp_JsxProps
  (* Type of properties of C in <C p={e}/>` becomes hint on `e` *)
  | Decomp_JsxPropsSelect of string
  (* Type of properties of C in <C {...e}/>` becomes hint on `e` *)
  | Decomp_JsxPropsSpread
  (* Type of C in <C>{e}</C> becomes hint on `e` *)
  | Decomp_JsxChildren
  (* Type of C in <C>{...e}</C> becomes hint on `e` *)
  | Decomp_JsxChildrenSpread

type hint =
  | Hint_t of Type.t
  | Hint_Decomp of hint_decomposition Nel.t * Type.t
  | Hint_None

let string_of_hint_unknown_kind = function
  | Decomp_ObjProp _ -> "Decomp_ObjProp"
  | Decomp_ObjComputed -> "Decomp_ObjComputed"
  | Decomp_ObjSpread -> "Decomp_ObjSpread"
  | Decomp_ArgSpread -> "Decomp_ArgSpread"
  | Decomp_ArrElement -> "Decomp_ArrElement"
  | Decomp_ArrSpread -> "Decomp_ArrSpread"
  | Decomp_MethodName _ -> "Decomp_MethodName"
  | Decomp_MethodElem _ -> "Decomp_MethodElem"
  | Decomp_CallNew -> "Decomp_CallNew"
  | Decomp_CallSuper -> "Decomp_CallSuper"
  | Decomp_CallSuperMem _ -> "Decomp_CallSuperMem"
  | Decomp_FuncParam i -> Utils_js.spf "Decomp_FuncParam (%d)" i
  | Decomp_FuncRest -> "Decomp_FuncRest"
  | Decomp_FuncReturn -> "Decomp_FuncReturn"
  | Decomp_NullishCoalesce -> "Decomp_NullishCoalesce"
  | Decomp_JsxProps -> "Decomp_JsxProps"
  | Decomp_JsxPropsSelect _ -> "Decomp_JsxPropsSelect"
  | Decomp_JsxPropsSpread -> "Decomp_JsxPropsSpread"
  | Decomp_JsxChildren -> "Decomp_JsxChildren"
  | Decomp_JsxChildrenSpread -> "Decomp_JsxChildrenSpread"

let string_of_hint ~on_hint = function
  | Hint_t t -> Utils_js.spf "Hint_t (%s)" (on_hint t)
  | Hint_Decomp (ops, t) ->
    Utils_js.spf
      "Hint_Decomp (%s)(%s)"
      (Nel.map string_of_hint_unknown_kind ops |> Nel.to_list |> String.concat ", ")
      (on_hint t)
  | Hint_None -> "Hint_None"

let decompose_hint decomp = function
  | Hint_t t -> Hint_Decomp (Nel.one decomp, t)
  | Hint_Decomp (decomps, t) -> Hint_Decomp (Nel.cons decomp decomps, t)
  | Hint_None -> Hint_None
