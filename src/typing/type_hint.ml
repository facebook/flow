(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type
open Hint_api

let in_sandbox_cx cx f =
  let original_errors = Context.errors cx in
  let result = f () in
  let new_errors = Context.errors cx in
  if Flow_error.ErrorSet.equal original_errors new_errors then
    Some (Tvar_resolver.resolved_t cx result)
  else (
    Context.reset_errors cx original_errors;
    None
  )

let type_of_hint_decomposition cx op reason t =
  let fun_t ~params ~rest_param ~return_t =
    DefT
      ( reason,
        bogus_trust (),
        FunT
          ( Unsoundness.dummy_static_any reason,
            {
              this_t = (Unsoundness.unresolved_any reason, This_Function);
              params;
              rest_param;
              return_t;
              is_predicate = false;
              def_reason = reason;
            }
          )
      )
  in

  let get_method_type t propref =
    Tvar.mk_where cx reason (fun prop_t ->
        Flow_js.flow cx (t, MethodT (unknown_use, reason, reason, propref, NoMethodAction, prop_t))
    )
  in

  in_sandbox_cx cx (fun () ->
      match op with
      | Decomp_ArrElement i ->
        Tvar.mk_no_wrap_where cx reason (fun element_t ->
            let use_t =
              DestructuringT
                ( reason,
                  DestructAnnot,
                  Elem
                    (DefT
                       ( reason,
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
      | Decomp_ArrSpread i ->
        Tvar.mk_no_wrap_where cx reason (fun tout ->
            let use_t = DestructuringT (reason, DestructAnnot, ArrRest i, tout, Reason.mk_id ()) in
            Flow_js.flow cx (t, use_t)
        )
      | Decomp_Await ->
        Tvar.mk_where cx reason (fun tout ->
            Flow_js.flow_t cx (t, tout);
            Flow_js.flow_t
              cx
              (Flow_js.get_builtin_typeapp cx reason (OrdinaryName "Promise") [t], tout)
        )
      | Decomp_CallNew ->
        (* For `new A(...)`, The initial base type we have is `Class<A>`. We need to first unwrap
           it, so that we can access the `constructor` method (which is considered an instance
           method). *)
        let this_t =
          Tvar.mk_where cx reason (fun t' ->
              Flow_js.unify cx t (DefT (reason, bogus_trust (), ClassT t'))
          )
        in
        get_method_type this_t (Named (reason, OrdinaryName "constructor"))
      | Decomp_CallSuper -> get_method_type t (Named (reason, OrdinaryName "constructor"))
      | Decomp_FuncParam i ->
        Tvar.mk_where cx reason (fun param_t ->
            let params =
              Base.Fn.apply_n_times
                ~n:i
                (Base.List.cons (None, Unsoundness.unresolved_any reason))
                [(None, param_t)]
            in
            let fun_t =
              fun_t ~params ~rest_param:None ~return_t:(Unsoundness.unresolved_any reason)
            in
            Flow_js.flow_t cx (fun_t, t)
        )
      | Decomp_FuncRest n ->
        Tvar.mk_where cx reason (fun rest_t ->
            let params =
              Base.Fn.apply_n_times ~n (Base.List.cons (None, Unsoundness.unresolved_any reason)) []
            in
            let fun_t =
              fun_t
                ~params
                ~rest_param:(Some (None, ALoc.none, rest_t))
                ~return_t:(Unsoundness.unresolved_any reason)
            in
            Flow_js.flow_t cx (fun_t, t)
        )
      | Decomp_FuncReturn ->
        Tvar.mk_where cx reason (fun return_t ->
            let fun_t =
              fun_t
                ~params:[]
                ~rest_param:(Some (None, ALoc.none, Unsoundness.unresolved_any reason))
                ~return_t
            in
            Flow_js.flow_t cx (t, fun_t)
        )
      | Comp_ImmediateFuncCall -> fun_t ~params:[] ~rest_param:None ~return_t:t
      | Decomp_JsxProps ->
        Tvar.mk_no_wrap_where cx reason (fun props_t ->
            Flow_js.flow cx (t, ReactKitT (unknown_use, reason, React.GetProps (OpenT props_t)))
        )
      | Decomp_MethodElem ->
        get_method_type t (Computed (DefT (reason, bogus_trust (), StrT AnyLiteral)))
      | Decomp_MethodName name -> get_method_type t (Named (reason, OrdinaryName name))
      | Decomp_MethodPrivateName (name, class_stack) ->
        let env = Context.environment cx in
        Context.set_environment cx { env with Loc_env.class_stack };
        let class_entries = Env.get_class_entries cx in
        let t =
          Tvar.mk_where cx reason (fun prop_t ->
              Flow_js.flow
                cx
                ( t,
                  PrivateMethodT
                    (unknown_use, reason, reason, name, class_entries, false, NoMethodAction, prop_t)
                )
          )
        in
        Context.set_environment cx env;
        t
      | Decomp_ObjProp name ->
        Tvar.mk_no_wrap_where cx reason (fun tout ->
            let use_t =
              DestructuringT (reason, DestructAnnot, Prop (name, false), tout, Reason.mk_id ())
            in
            (* TODO:
               Be more lenient with union branches that failed to match.
               We should collect and return all successful branches in speculation. *)
            Flow_js.flow cx (t, use_t)
        )
      | Decomp_ObjComputed ->
        Tvar.mk_no_wrap_where cx reason (fun element_t ->
            let use_t =
              DestructuringT
                ( reason,
                  DestructAnnot,
                  Elem (DefT (reason, bogus_trust (), StrT AnyLiteral)),
                  element_t,
                  Reason.mk_id ()
                )
            in
            Flow_js.flow cx (t, use_t)
        )
      | Decomp_ObjSpread ->
        Tvar.mk_no_wrap_where cx reason (fun tout ->
            let use_t =
              (* We assume the object spread is at the start of the object. *)
              DestructuringT (reason, DestructAnnot, ObjRest [], tout, Reason.mk_id ())
            in
            Flow_js.flow cx (t, use_t)
        )
      | Decomp_Instantiated _ -> t
  )

let rec evaluate_hint_ops cx reason t = function
  | [] -> Some t
  | op :: ops ->
    (match type_of_hint_decomposition cx op reason t with
    | Some t -> evaluate_hint_ops cx reason t ops
    | None -> None)

let evaluate_hint cx reason hint =
  match hint with
  | Hint_None -> None
  | Hint_t t -> Some (Tvar_resolver.resolved_t cx t)
  | Hint_Placeholder -> Some (AnyT.annot (mk_reason (RCustom "placeholder hint") ALoc.none))
  | Hint_Decomp (ops, t) ->
    ops |> Nel.to_list |> List.rev |> evaluate_hint_ops cx reason (Tvar_resolver.resolved_t cx t)
