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
  (* Hint on `{ f: e }` becomes hint on `e` *)
  | Decomp_ObjProp of string
  (* Hint on `{ [k]: e }` becomes hint on `e` *)
  | Decomp_ObjComputed of Reason.t
  (* Hint on `{ ...e }` becomes hint on `e` *)
  | Decomp_ObjSpread
  (* Hint on array literal `[e]` becomes hint on `e` *)
  | Decomp_ArrElement of int option
  (* Hint on array literal `[...e]` becomes hint on `e` *)
  | Decomp_ArrSpread of int
  (* Type of `await e` becomes hint on `e` *)
  | Decomp_Await
  (* Type of `o` in `o.m(..)` becomes the type of `o.m` *)
  | Decomp_MethodName of string
  (* Type of `o` in `o.#m(..)` becomes the type of `o.m` *)
  | Decomp_MethodPrivateName of string * ALoc.t list
  (* Type of `o` in `o[e](..)` becomes the type of `o[e]` *)
  | Decomp_MethodElem
  (* Type of `C` in `new C(..)` becomes the type of the constructor of C *)
  | Decomp_CallNew
  (* Type of the super-class becomes the type of the super constructor *)
  | Decomp_CallSuper
  (* Type of function becomes hint on the i-th argument *)
  | Decomp_FuncParam of int
  (* Type of function becomes hint on rest argument *)
  | Decomp_FuncRest of int (* number of params before rest params *)
  (* Type of function becomes hint on return *)
  | Decomp_FuncReturn
  (* Hint on call `f()` becomes hint on `f`. This is only meant to be used for the
   * case of immediate function call `(function() {})()`. *)
  | Comp_ImmediateFuncCall
  (* Type of C in `<C [props]/>` becomes hint on `props` *)
  | Decomp_JsxProps
  (* Type of C in `<C ref={ref} ... />` becomes hint on `ref` *)
  | Decomp_JsxRef
  | Decomp_SentinelRefinement of sentinel_refinement SMap.t
  (* Type of f in f(...) is instantiated with arguments and return hint.
     Returns f if the type of f is not polymorphic. *)
  | Instantiate_Callee of
      ('t, 'targs, 'args, 'props, 'children) fun_call_implicit_instantiation_hints
  (* Type of Comp in <Comp ... /> is instantiated with props and children.
     Returns Comp if the type of Comp is not polymorphic. *)
  | Instantiate_Component of ('t, 'targs, 'args, 'props, 'children) jsx_implicit_instantiation_hints
  (* T of Promise<T> becomes hint on return in async scope *)
  | Decomp_Promise

and ('t, 'targs, 'args, 'props, 'children) hint =
  | Hint_t of 't
  | Hint_Decomp of (int * ('t, 'targs, 'args, 'props, 'children) hint_decomposition) Nel.t * 't
  (* The hint placeholder used in env_resolution to pass to expression type checkers.
     It will eventually be removed once we move all hint decomposition logic into env_resolution. *)
  | Hint_Placeholder

let string_of_hint_unknown_kind = function
  | Decomp_ObjProp _ -> "Decomp_ObjProp"
  | Decomp_ObjComputed _ -> "Decomp_ObjComputed"
  | Decomp_ObjSpread -> "Decomp_ObjSpread"
  | Decomp_ArrElement None -> "Decomp_ArrElement (no index)"
  | Decomp_ArrElement (Some i) -> Utils_js.spf "Decomp_ArrElement (%d)" i
  | Decomp_ArrSpread i -> Utils_js.spf "Decomp_ArrSpread (%d)" i
  | Decomp_MethodName _ -> "Decomp_MethodName"
  | Decomp_MethodPrivateName _ -> "Decomp_MethodPrivateName"
  | Decomp_MethodElem -> "Decomp_MethodElem"
  | Decomp_CallNew -> "Decomp_CallNew"
  | Decomp_CallSuper -> "Decomp_CallSuper"
  | Decomp_FuncParam i -> Utils_js.spf "Decomp_FuncParam (%d)" i
  | Decomp_FuncRest i -> Utils_js.spf "Decomp_FuncRest (%d)" i
  | Decomp_FuncReturn -> "Decomp_FuncReturn"
  | Comp_ImmediateFuncCall -> "Comp_ImmediateFuncCall"
  | Decomp_JsxProps -> "Decomp_JsxProps"
  | Decomp_JsxRef -> "Decomp_JsxRef"
  | Decomp_SentinelRefinement _ -> "Decomp_SentinelRefinement"
  | Decomp_Await -> "Decomp_Await"
  | Instantiate_Callee _ -> "Instantiate_Callee"
  | Instantiate_Component _ -> "Instantiate_Component"
  | Decomp_Promise -> "Decomp_Promise"

let string_of_hints ~on_hint hints =
  Utils_js.spf
    "[%s]"
    (hints
    |> Base.List.map ~f:(function
           | Hint_t t -> Utils_js.spf "Hint_t (%s)" (on_hint t)
           | Hint_Decomp (ops, t) ->
             Utils_js.spf
               "Hint_Decomp (%s)(%s)"
               (Nel.map (fun (_, op) -> string_of_hint_unknown_kind op) ops
               |> Nel.to_list
               |> String.concat ", "
               )
               (on_hint t)
           | Hint_Placeholder -> "Hint_Placeholder"
           )
    |> Base.String.concat ~sep:","
    )

let decompose_hints decomp =
  Base.List.map ~f:(function
      | Hint_t t -> Hint_Decomp (Nel.one (Reason.mk_id (), decomp), t)
      | Hint_Decomp (decomps, t) -> Hint_Decomp (Nel.cons (Reason.mk_id (), decomp) decomps, t)
      | Hint_Placeholder -> Hint_Placeholder
      )

let rec map_decomp_op ~map_base_hint ~map_targs ~map_arg_list ~map_jsx = function
  | Decomp_ObjProp prop -> Decomp_ObjProp prop
  | Decomp_ObjComputed l -> Decomp_ObjComputed l
  | Decomp_ObjSpread -> Decomp_ObjSpread
  | Decomp_ArrElement o -> Decomp_ArrElement o
  | Decomp_ArrSpread o -> Decomp_ArrSpread o
  | Decomp_Await -> Decomp_Await
  | Decomp_MethodName name -> Decomp_MethodName name
  | Decomp_MethodPrivateName (name, class_stack) -> Decomp_MethodPrivateName (name, class_stack)
  | Decomp_MethodElem -> Decomp_MethodElem
  | Decomp_CallNew -> Decomp_CallNew
  | Decomp_CallSuper -> Decomp_CallSuper
  | Decomp_FuncParam i -> Decomp_FuncParam i
  | Decomp_FuncRest i -> Decomp_FuncRest i
  | Decomp_FuncReturn -> Decomp_FuncReturn
  | Comp_ImmediateFuncCall -> Comp_ImmediateFuncCall
  | Decomp_JsxProps -> Decomp_JsxProps
  | Decomp_JsxRef -> Decomp_JsxRef
  | Decomp_SentinelRefinement checks -> Decomp_SentinelRefinement checks
  | Instantiate_Callee { reason; return_hints; targs; arg_list; arg_index } ->
    Instantiate_Callee
      {
        reason;
        return_hints = List.map (map ~map_base_hint ~map_targs ~map_arg_list ~map_jsx) return_hints;
        targs = Lazy.map map_targs targs;
        arg_list = Lazy.map map_arg_list arg_list;
        arg_index;
      }
  | Instantiate_Component { jsx_reason; jsx_name; jsx_props; jsx_children; jsx_hints } ->
    let (jsx_props, jsx_children) = map_jsx jsx_reason jsx_name jsx_props jsx_children in
    let jsx_hints = List.map (map ~map_base_hint ~map_targs ~map_arg_list ~map_jsx) jsx_hints in
    Instantiate_Component { jsx_reason; jsx_name; jsx_props; jsx_children; jsx_hints }
  | Decomp_Promise -> Decomp_Promise

and map ~map_base_hint ~map_targs ~map_arg_list ~map_jsx = function
  | Hint_t t -> Hint_t (map_base_hint t)
  | Hint_Decomp (ops, t) ->
    Hint_Decomp
      ( Nel.map
          (fun (i, op) -> (i, map_decomp_op ~map_base_hint ~map_targs ~map_arg_list ~map_jsx op))
          ops,
        map_base_hint t
      )
  | Hint_Placeholder -> Hint_Placeholder
