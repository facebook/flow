(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type
open Hint_api

let in_sandbox_cx cx t ~f =
  let original_errors = Context.errors cx in
  let result = f (Tvar_resolver.resolved_t cx t) in
  let new_errors = Context.errors cx in
  if Flow_error.ErrorSet.equal original_errors new_errors then
    Some result
  else (
    Context.reset_errors cx original_errors;
    None
  )

let synthesis_speculation_call cx call_reason (reason, rep) targs argts =
  let intersection = IntersectionT (reason, rep) in
  (* We're not expecting to surface errors here, so an unknown_use should be benign. *)
  let use_op = unknown_use in
  let tout = (call_reason, Tvar.mk_no_wrap cx call_reason) in
  let call_speculation_hint_state = ref Speculation_hint_unset in
  let call_action =
    Funcalltype
      {
        call_this_t = global_this reason;
        call_targs = targs;
        call_args_tlist = argts;
        call_tout = tout;
        call_strict_arity = true;
        call_speculation_hint_state = Some call_speculation_hint_state;
      }
  in
  let use = CallT { use_op; reason = call_reason; call_action; return_hint = hint_unavailable } in
  Flow_js.flow cx (intersection, use);
  match !call_speculation_hint_state with
  | Speculation_hint_unset -> intersection
  | Speculation_hint_invalid -> intersection
  | Speculation_hint_set (_, t) -> t

let simplify_callee cx reason use_op func_t =
  Tvar.mk_no_wrap_where cx reason (fun t ->
      let call_action = ConcretizeCallee t in
      Flow_js.flow cx (func_t, CallT { use_op; reason; call_action; return_hint = hint_unavailable })
  )

let get_t cx = function
  | OpenT (r, id) ->
    Flow_js_utils.merge_tvar ~no_lowers:(fun _ r -> DefT (r, bogus_trust (), EmptyT)) cx r id
  | t -> t

let decomp_instantiated cx fn instantiation_hint =
  let { Hint_api.reason; targs; arg_list; return_hint = _ } = instantiation_hint in
  match get_t cx (simplify_callee cx reason unknown_use fn) with
  | IntersectionT (r, rep) ->
    let (_, result) =
      Context.run_in_synthesis_mode cx (fun () ->
          synthesis_speculation_call cx reason (r, rep) (Lazy.force targs) (Lazy.force arg_list)
      )
    in
    result
  | t -> t

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

  in_sandbox_cx cx t ~f:(fun t ->
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
      | Decomp_Instantiated instantiation_hint -> decomp_instantiated cx t instantiation_hint
  )

let evaluate_hint_ops cx reason t ops =
  let rec loop t = function
    | [] -> Some t
    | op :: ops ->
      (match type_of_hint_decomposition cx op reason t with
      | Some t -> loop t ops
      | None -> None)
  in
  (* We evaluate the decompositions in synthesis mode, but fully resolve the final result in
     checking mode, so that any unresolved tvars in the midddle won't fail the evaluation, but
     unsolved tvars in the final result will fail the evaluation. *)
  match Context.run_in_synthesis_mode cx (fun () -> loop t ops) with
  | (_, None) -> None
  | (_, Some t) -> in_sandbox_cx cx t ~f:Base.Fn.id

let evaluate_hint cx reason hint =
  match hint with
  | Hint_None -> None
  | Hint_Placeholder -> Some (AnyT.annot (mk_reason (RCustom "placeholder hint") ALoc.none))
  | Hint_t t -> evaluate_hint_ops cx reason t []
  | Hint_Decomp (ops, t) -> ops |> Nel.to_list |> List.rev |> evaluate_hint_ops cx reason t
