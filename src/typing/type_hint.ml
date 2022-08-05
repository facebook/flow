(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type
open Hint_api

(* Temporary facilty to decide if a type is resolved *)
exception Found_unresolved of int

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
          | Unresolved _ -> raise (Found_unresolved id)
    end
  in
  fun cx t ->
    match visitor#type_ cx Polarity.Neutral ISet.empty t with
    | exception Found_unresolved id ->
      Debug_js.Verbose.print_if_verbose cx [Utils_js.spf "Unresolved tvar: %d" id];
      false
    | _ -> true

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

let type_of_hint_decomposition cx op loc t =
  let dummy_reason = mk_reason (RCustom "type hint reason") loc in

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
  in

  let get_method_type t propref =
    let t =
      Tvar.mk_where cx dummy_reason (fun prop_t ->
          Flow_js.flow
            cx
            (t, MethodT (unknown_use, dummy_reason, dummy_reason, propref, NoMethodAction, prop_t))
      )
    in
    annot true t
  in

  in_sandbox_cx cx (fun () ->
      match op with
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
      | Decomp_Await ->
        let t =
          Tvar.mk_where cx dummy_reason (fun tout ->
              Flow_js.flow_t cx (t, tout);
              Flow_js.flow_t
                cx
                (Flow_js.get_builtin_typeapp cx dummy_reason (OrdinaryName "Promise") [t], tout)
          )
        in
        annot true t
      | Decomp_CallNew ->
        (* For `new A(...)`, The initial base type we have is `Class<A>`. We need to first unwrap
           it, so that we can access the `constructor` method (which is considered an instance
           method). *)
        let this_t =
          Tvar.mk_where cx dummy_reason (fun t' ->
              Flow_js.unify cx t (DefT (dummy_reason, bogus_trust (), ClassT t'))
          )
        in
        get_method_type this_t (Named (dummy_reason, OrdinaryName "constructor"))
      | Decomp_CallSuper -> get_method_type t (Named (dummy_reason, OrdinaryName "constructor"))
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
      | Decomp_MethodElem ->
        get_method_type t (Computed (DefT (dummy_reason, bogus_trust (), StrT AnyLiteral)))
      | Decomp_MethodName name -> get_method_type t (Named (dummy_reason, OrdinaryName name))
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

let rec evaluate_hint_ops cx loc t = function
  | [] -> Some t
  | op :: ops ->
    (match type_of_hint_decomposition cx op loc t with
    | Some t -> evaluate_hint_ops cx loc t ops
    | None -> None)

let evaluate_hint cx loc hint =
  match hint with
  | Hint_None -> None
  | Hint_t t when is_fully_resolved cx t -> Some t
  | Hint_Placeholder -> Some (AnyT.annot (mk_reason (RCustom "placeholder hint") ALoc.none))
  | Hint_Decomp (ops, t) when is_fully_resolved cx t ->
    ops |> Nel.to_list |> List.rev |> evaluate_hint_ops cx loc t
  | Hint_t _
  | Hint_Decomp _ ->
    None
