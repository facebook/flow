(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type

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
  | Decomp_ArrElement of int
  (* Hint on array literal `[...e]` becomes hint on `e` *)
  | Decomp_ArrSpread of int
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
  | Decomp_FuncRest of int (* number of params before rest params *)
  (* Type of function becomes hint on return *)
  | Decomp_FuncReturn
  (* Type of e1 in `e1 ?? e2` becomes hint on `e2` *)
  | Decomp_NullishCoalesce
  (* Type of C in `<C [props]/>` becomes hint on `props` *)
  | Decomp_JsxProps
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
  | Decomp_ArrElement i -> Utils_js.spf "Decomp_ArrElement (%d)" i
  | Decomp_ArrSpread i -> Utils_js.spf "Decomp_ArrSpread (%d)" i
  | Decomp_MethodName _ -> "Decomp_MethodName"
  | Decomp_MethodElem _ -> "Decomp_MethodElem"
  | Decomp_CallNew -> "Decomp_CallNew"
  | Decomp_CallSuper -> "Decomp_CallSuper"
  | Decomp_CallSuperMem _ -> "Decomp_CallSuperMem"
  | Decomp_FuncParam i -> Utils_js.spf "Decomp_FuncParam (%d)" i
  | Decomp_FuncRest i -> Utils_js.spf "Decomp_FuncRest (%d)" i
  | Decomp_FuncReturn -> "Decomp_FuncReturn"
  | Decomp_NullishCoalesce -> "Decomp_NullishCoalesce"
  | Decomp_JsxProps -> "Decomp_JsxProps"
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

(* Temporary facilty to decide if a type is resolved *)
exception Found_unresolved

let is_fully_resolved =
  let visitor =
    object (this)
      inherit [ISet.t] Type_visitor.t

      method! tvar cx pole seen _ id =
        let (id, constraints) = Context.find_constraints cx id in
        if ISet.mem id seen then
          seen
        else
          let open Type.Constraint in
          let seen = ISet.add id seen in
          match constraints with
          | FullyResolved _ -> seen
          | Resolved (_, t) -> this#type_ cx pole seen t
          | Unresolved _ -> raise Found_unresolved
    end
  in
  fun cx t ->
    match visitor#type_ cx Polarity.Neutral ISet.empty t with
    | exception Found_unresolved -> false
    | _ -> true

let dummy_reason = locationless_reason (RCustom "type hint reason")

let in_sandbox_cx cx f =
  let original_errors = Context.errors cx in
  let result = f () in
  let new_errors = Context.errors cx in
  if Flow_error.ErrorSet.equal original_errors new_errors then
    Some result
  else (
    Context.reset_errors cx original_errors;
    None
  )

let fun_t ~params ~rest_param ~return_t =
  DefT
    ( dummy_reason,
      bogus_trust (),
      FunT
        ( Unsoundness.dummy_static_any dummy_reason,
          {
            this_t = (Unsoundness.unresolved_any dummy_reason, This_Function);
            params;
            rest_param;
            return_t;
            is_predicate = false;
            def_reason = dummy_reason;
          }
        )
    )

let type_of_hint_decomposition cx op t =
  in_sandbox_cx cx (fun () ->
      match op with
      | Decomp_ArgSpread ->
        (* TODO *)
        failwith "Not implemented"
      | Decomp_ArrElement i ->
        let t =
          Tvar.mk_no_wrap_where cx dummy_reason (fun element_t ->
              let use_t =
                DestructuringT
                  ( dummy_reason,
                    DestructAnnot,
                    Elem
                      (DefT
                         ( dummy_reason,
                           bogus_trust (),
                           NumT (Literal (None, (float_of_int i, string_of_int i)))
                         )
                      ),
                    element_t,
                    Reason.mk_id ()
                  )
              in
              Flow_js.flow cx (t, use_t)
          )
        in
        annot true t
      | Decomp_ArrSpread i ->
        let t =
          Tvar.mk_no_wrap_where cx dummy_reason (fun tout ->
              let use_t =
                DestructuringT (dummy_reason, DestructAnnot, ArrRest i, tout, Reason.mk_id ())
              in
              Flow_js.flow cx (t, use_t)
          )
        in
        annot true t
      | Decomp_CallNew ->
        (* TODO *)
        failwith "Not implemented"
      | Decomp_CallSuper ->
        (* TODO *)
        failwith "Not implemented"
      | Decomp_CallSuperMem _ ->
        (* TODO *)
        failwith "Not implemented"
      | Decomp_FuncParam i ->
        let t =
          Tvar.mk_where cx dummy_reason (fun param_t ->
              let params =
                Base.Fn.apply_n_times
                  ~n:i
                  (Base.List.cons (None, Unsoundness.unresolved_any dummy_reason))
                  [(None, param_t)]
              in
              let fun_t =
                fun_t ~params ~rest_param:None ~return_t:(Unsoundness.unresolved_any dummy_reason)
              in
              Flow_js.flow_t cx (fun_t, t)
          )
        in
        annot true t
      | Decomp_FuncRest n ->
        let t =
          Tvar.mk_where cx dummy_reason (fun rest_t ->
              let params =
                Base.Fn.apply_n_times
                  ~n
                  (Base.List.cons (None, Unsoundness.unresolved_any dummy_reason))
                  []
              in
              let fun_t =
                fun_t
                  ~params
                  ~rest_param:(Some (None, ALoc.none, rest_t))
                  ~return_t:(Unsoundness.unresolved_any dummy_reason)
              in
              Flow_js.flow_t cx (fun_t, t)
          )
        in
        annot true t
      | Decomp_FuncReturn ->
        let t =
          Tvar.mk_where cx dummy_reason (fun return_t ->
              let fun_t =
                fun_t
                  ~params:[]
                  ~rest_param:(Some (None, ALoc.none, Unsoundness.unresolved_any dummy_reason))
                  ~return_t
              in
              Flow_js.flow_t cx (t, fun_t)
          )
        in
        annot true t
      | Decomp_JsxProps ->
        let t =
          Tvar.mk_no_wrap_where cx dummy_reason (fun props_t ->
              Flow_js.flow
                cx
                (t, ReactKitT (unknown_use, dummy_reason, React.GetProps (OpenT props_t)))
          )
        in
        annot true t
      | Decomp_JsxChildren ->
        (* TODO *)
        failwith "Not implemented"
      | Decomp_JsxChildrenSpread ->
        (* TODO *)
        failwith "Not implemented"
      | Decomp_MethodElem _ ->
        (* TODO *)
        failwith "Not implemented"
      | Decomp_MethodName _ ->
        (* TODO *)
        failwith "Not implemented"
      | Decomp_NullishCoalesce ->
        (* TODO *)
        failwith "Not implemented"
      | Decomp_ObjProp name ->
        let t =
          Tvar.mk_no_wrap_where cx dummy_reason (fun tout ->
              let use_t =
                DestructuringT
                  (dummy_reason, DestructAnnot, Prop (name, false), tout, Reason.mk_id ())
              in
              (* TODO:
                 Be more lenient with union branches that failed to match.
                 We should collect and return all successful branches in speculation. *)
              Flow_js.flow cx (t, use_t)
          )
        in
        annot true t
      | Decomp_ObjComputed ->
        let t =
          Tvar.mk_no_wrap_where cx dummy_reason (fun element_t ->
              let use_t =
                DestructuringT
                  ( dummy_reason,
                    DestructAnnot,
                    Elem (DefT (dummy_reason, bogus_trust (), StrT AnyLiteral)),
                    element_t,
                    Reason.mk_id ()
                  )
              in
              Flow_js.flow cx (t, use_t)
          )
        in
        annot true t
      | Decomp_ObjSpread ->
        let t =
          Tvar.mk_no_wrap_where cx dummy_reason (fun tout ->
              let use_t =
                (* We assume the object spread is at the start of the object. *)
                DestructuringT (dummy_reason, DestructAnnot, ObjRest [], tout, Reason.mk_id ())
              in
              Flow_js.flow cx (t, use_t)
          )
        in
        annot true t
  )

let rec evaluate_hint_ops cx t = function
  | [] -> Some t
  | op :: ops ->
    (match type_of_hint_decomposition cx op t with
    | Some t -> evaluate_hint_ops cx t ops
    | None -> None)

let evaluate_hint cx hint =
  match hint with
  | Hint_None -> None
  | Hint_t t when is_fully_resolved cx t -> Some t
  | Hint_Decomp (ops, t) when is_fully_resolved cx t ->
    ops |> Nel.to_list |> List.rev |> evaluate_hint_ops cx t
  | Hint_t _
  | Hint_Decomp _ ->
    None
