(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type
open Hint_api
open Utils_js
module ImplicitInstantiation = Implicit_instantiation.Pierce (Flow_js.FlowJs)

module SpeculationFlow : sig
  val flow : Context.t -> Reason.reason -> Type.t * Type.use_t -> unit

  val flow_t : Context.t -> Reason.reason -> upper_unresolved:bool -> Type.t * Type.t -> unit
end = struct
  module SpeculationKit = Speculation_kit.Make (Flow_js.FlowJs)

  let flow cx reason (l, u) =
    SpeculationKit.try_singleton_throw_on_failure
      cx
      Trace.dummy_trace
      ~upper_unresolved:true
      reason
      l
      u

  let flow_t cx reason ~upper_unresolved (l, u) =
    SpeculationKit.try_singleton_throw_on_failure
      cx
      Trace.dummy_trace
      ~upper_unresolved
      reason
      l
      (UseT (unknown_use, u))
end

let in_sandbox_cx cx t ~f =
  match f (Tvar_resolver.resolved_t cx ~on_unconstrained_tvar:Tvar_resolver.Allow t) with
  | exception Flow_js_utils.SpeculationSingletonError -> None
  | t -> Some t

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
  SpeculationFlow.flow cx call_reason (intersection, use);
  match !call_speculation_hint_state with
  | Speculation_hint_unset -> intersection
  | Speculation_hint_invalid -> intersection
  | Speculation_hint_set (_, t) -> t

let simplify_callee cx reason use_op func_t =
  Tvar.mk_no_wrap_where cx reason (fun t ->
      let call_action = ConcretizeCallee t in
      SpeculationFlow.flow
        cx
        reason
        (func_t, CallT { use_op; reason; call_action; return_hint = hint_unavailable })
  )

let get_t cx = function
  | OpenT (r, id) ->
    Flow_js_utils.merge_tvar ~no_lowers:(fun _ r -> DefT (r, bogus_trust (), EmptyT)) cx r id
  | t -> t

let rec instantiate_callee cx fn instantiation_hint =
  let { Hint_api.reason; targs; arg_list; return_hint; arg_index } = instantiation_hint in
  let t =
    match get_t cx (simplify_callee cx reason unknown_use fn) with
    | IntersectionT (r, rep) ->
      synthesis_speculation_call
        cx
        reason
        (r, rep)
        (Lazy.force targs)
        (Lazy.force arg_list |> Base.List.map ~f:snd)
    | t -> t
  in
  match get_t cx t with
  | DefT (_, _, PolyT { tparams_loc; tparams; t_out; id = _ }) ->
    let call_args_tlist =
      let checked_t t loc =
        let reason = mk_reason (TypeUtil.reason_of_t t |> Reason.desc_of_reason) loc in
        Env.find_write cx Env_api.ExpressionLoc reason
      in
      let rec loop i = function
        | [] -> []
        | (_loc, t) :: rest when i >= arg_index -> t :: loop (i + 1) rest
        | (loc, t) :: rest ->
          let t' =
            match t with
            | Arg t -> Arg (checked_t t loc)
            | SpreadArg t -> SpreadArg (checked_t t loc)
          in
          t' :: loop (i + 1) rest
      in
      loop 0 (Lazy.force arg_list)
    in
    let call_targs = Lazy.force targs in
    let return_hint = evaluate_hint cx reason return_hint in
    let check =
      Implicit_instantiation_check.of_call
        t
        (tparams_loc, tparams, t_out)
        unknown_use
        reason
        {
          call_this_t = Unsoundness.unresolved_any reason;
          call_targs;
          call_args_tlist;
          call_tout = (reason, Tvar.mk_no_wrap cx reason);
          call_strict_arity = true;
          call_speculation_hint_state = None;
        }
    in
    let subst_map =
      Context.run_in_implicit_instantiation_mode cx (fun () ->
          ImplicitInstantiation.solve_targs cx ?return_hint check
          |> Subst_name.Map.map (fun solution -> solution.Implicit_instantiation.inferred)
      )
    in
    Flow_js.subst cx subst_map t_out
  | t -> t

and instantiate_component cx component instantiation_hint =
  match get_t cx component with
  | DefT (_, _, PolyT { tparams_loc; tparams; t_out; id = _ })
    when Context.jsx cx = Options.Jsx_react ->
    let {
      Hint_api.jsx_reason = reason;
      jsx_name = _;
      jsx_props = config;
      jsx_children = children;
      jsx_hint;
    } =
      instantiation_hint
    in
    let return_hint = evaluate_hint cx reason jsx_hint in
    let check =
      Implicit_instantiation_check.of_jsx
        component
        (tparams_loc, tparams, t_out)
        unknown_use
        reason
        false
        ~component
        ~config
        ~targs:None
        children
    in
    let subst_map =
      Context.run_in_implicit_instantiation_mode cx (fun () ->
          ImplicitInstantiation.solve_targs cx ?return_hint check
          |> Subst_name.Map.map (fun solution -> solution.Implicit_instantiation.inferred)
      )
    in
    Flow_js.subst cx subst_map t_out
  | t -> t

and type_of_hint_decomposition cx op reason t =
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
        let use_t = MethodT (unknown_use, reason, reason, propref, NoMethodAction, prop_t) in
        Context.run_in_hint_decomp cx (fun () -> SpeculationFlow.flow cx reason (t, use_t))
    )
  in

  let map_intersection t ~f =
    match get_t cx t with
    | IntersectionT (r, rep) -> IntersectionT (r, InterRep.map (fun t -> f (get_t cx t)) rep)
    | t -> f t
  in

  let get_constructor_type t =
    let get_constructor_method_type t =
      get_method_type t (Named (reason, OrdinaryName "constructor"))
    in
    let mod_ctor_return instance_type = function
      | DefT
          ( reason,
            trust,
            FunT (static, { this_t; params; rest_param; return_t = _; is_predicate; def_reason })
          ) ->
        DefT
          ( reason,
            trust,
            FunT
              ( static,
                { this_t; params; rest_param; return_t = instance_type; is_predicate; def_reason }
              )
          )
      | t -> get_t cx t
    in
    match get_t cx t with
    | DefT (_, trust, PolyT { tparams_loc; tparams; t_out = instance_type; id = _ }) ->
      map_intersection (get_constructor_method_type instance_type) ~f:(function
          | DefT (_, trust, PolyT { tparams_loc; tparams = tparams2; t_out; id = _ }) ->
            let t_out = mod_ctor_return instance_type t_out in
            DefT
              ( reason,
                trust,
                PolyT
                  {
                    tparams_loc;
                    tparams = Nel.append tparams tparams2;
                    t_out;
                    id = Poly.generate_id ();
                  }
              )
          | t_out ->
            let t_out = mod_ctor_return instance_type t_out in
            DefT (reason, trust, PolyT { tparams_loc; tparams; t_out; id = Poly.generate_id () })
          )
    | t -> map_intersection (get_constructor_method_type t) ~f:(mod_ctor_return t)
  in

  in_sandbox_cx cx t ~f:(fun t ->
      match op with
      | Decomp_ArrElement i ->
        Tvar.mk_no_wrap_where cx reason (fun element_t ->
            let use_t =
              GetElemT
                ( unknown_use,
                  reason,
                  true,
                  DefT
                    ( reason,
                      bogus_trust (),
                      NumT (Literal (None, (float_of_int i, string_of_int i)))
                    ),
                  element_t
                )
            in
            Context.run_in_hint_decomp cx (fun () -> SpeculationFlow.flow cx reason (t, use_t))
        )
      | Decomp_ArrSpread i ->
        Tvar.mk_no_wrap_where cx reason (fun tout ->
            let use_t = ArrRestT (unknown_use, reason, i, OpenT tout) in
            SpeculationFlow.flow cx reason (t, use_t)
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
        let get_this_t t =
          Tvar.mk_where cx reason (fun t' ->
              SpeculationFlow.flow_t
                cx
                reason
                ~upper_unresolved:true
                (t, DefT (reason, bogus_trust (), ClassT t'))
          )
          |> get_t cx
        in
        let this_t =
          match get_t cx t with
          | DefT (reason, trust, PolyT { tparams_loc; tparams; t_out; id = _ }) ->
            DefT
              ( reason,
                trust,
                PolyT { tparams_loc; tparams; t_out = get_this_t t_out; id = Poly.generate_id () }
              )
          | t -> get_this_t t
        in
        get_constructor_type this_t
      | Decomp_CallSuper -> get_constructor_type t
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
            Context.run_in_hint_decomp cx (fun () ->
                SpeculationFlow.flow_t cx reason ~upper_unresolved:false (fun_t, t)
            )
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
            Context.run_in_hint_decomp cx (fun () ->
                SpeculationFlow.flow_t cx reason ~upper_unresolved:false (fun_t, t)
            )
        )
      | Decomp_FuncReturn ->
        Tvar.mk_where cx reason (fun return_t ->
            let fun_t =
              fun_t
                ~params:[]
                ~rest_param:(Some (None, ALoc.none, Unsoundness.unresolved_any reason))
                ~return_t
            in
            Context.run_in_hint_decomp cx (fun () ->
                SpeculationFlow.flow_t cx reason ~upper_unresolved:true (t, fun_t)
            )
        )
      | Comp_ImmediateFuncCall -> fun_t ~params:[] ~rest_param:None ~return_t:t
      | Decomp_JsxProps ->
        Tvar.mk_no_wrap_where cx reason (fun props_t ->
            SpeculationFlow.flow
              cx
              reason
              (t, ReactKitT (unknown_use, reason, React.GetConfig (OpenT props_t)))
        )
      | Decomp_JsxRef -> Flow_js.get_builtin_typeapp cx reason (OrdinaryName "React$Ref") [t]
      | Decomp_MethodElem ->
        get_method_type t (Computed (DefT (reason, bogus_trust (), StrT AnyLiteral)))
      | Decomp_MethodName name -> get_method_type t (Named (reason, OrdinaryName name))
      | Decomp_MethodPrivateName (name, class_stack) ->
        let env = Context.environment cx in
        Context.set_environment cx { env with Loc_env.class_stack };
        let class_entries = Env.get_class_entries cx in
        let t =
          Tvar.mk_where cx reason (fun prop_t ->
              SpeculationFlow.flow
                cx
                reason
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
              GetPropT
                ( unknown_use,
                  reason,
                  Some (Reason.mk_id ()),
                  Named (reason, OrdinaryName name),
                  tout
                )
            in
            Context.run_in_hint_decomp cx (fun () -> SpeculationFlow.flow cx reason (t, use_t))
        )
      | Decomp_ObjComputed ->
        Tvar.mk_no_wrap_where cx reason (fun element_t ->
            let use_t =
              GetElemT
                ( unknown_use,
                  reason,
                  true,
                  DefT (reason, bogus_trust (), StrT AnyLiteral),
                  element_t
                )
            in
            Context.run_in_hint_decomp cx (fun () -> SpeculationFlow.flow cx reason (t, use_t))
        )
      | Decomp_ObjSpread ->
        Tvar.mk_no_wrap_where cx reason (fun tout ->
            let use_t =
              (* We assume the object spread is at the start of the object. *)
              ObjRestT (reason, [], OpenT tout, Reason.mk_id ())
            in
            Context.run_in_hint_decomp cx (fun () -> SpeculationFlow.flow cx reason (t, use_t))
        )
      | Decomp_SentinelRefinement checks ->
        (match SMap.elements checks with
        | [] -> t
        | hd :: tl ->
          let predicate_of_check (prop, literal_check) =
            let other_t =
              match literal_check with
              | SingletonBool b -> DefT (reason, bogus_trust (), SingletonBoolT b)
              | SingletonNum n -> DefT (reason, bogus_trust (), SingletonNumT (n, string_of_float n))
              | SingletonStr s -> DefT (reason, bogus_trust (), SingletonStrT (OrdinaryName s))
              | Null -> DefT (reason, bogus_trust (), NullT)
              | Void -> DefT (reason, bogus_trust (), VoidT)
              | Member reason -> Env.find_write cx Env_api.ExpressionLoc reason
            in
            LeftP (SentinelProp prop, other_t)
          in
          let predicate =
            Base.List.fold tl ~init:(predicate_of_check hd) ~f:(fun acc check ->
                AndP (acc, predicate_of_check check)
            )
          in
          Tvar.mk_no_wrap_where cx reason (fun tvar ->
              SpeculationFlow.flow cx reason (t, PredicateT (predicate, tvar))
          ))
      | Instantiate_Callee instantiation_hint -> instantiate_callee cx t instantiation_hint
      | Instantiate_Component instantiation_hint -> instantiate_component cx t instantiation_hint
  )

and fully_resolve_final_result cx t =
  match Tvar_resolver.resolved_t cx ~on_unconstrained_tvar:Tvar_resolver.Exception t with
  | exception Tvar_resolver.UnconstrainedTvarException i ->
    Debug_js.Verbose.print_if_verbose cx [spf "Under-constrained tvar %d" i];
    None
  | t -> Some t

and evaluate_hint_ops cx reason t ops =
  let rec loop t = function
    | [] -> Some t
    | (id, op) :: ops ->
      let result =
        match Context.hint_eval_cache_find_opt cx id with
        | Some result -> result
        | None ->
          let result = type_of_hint_decomposition cx op reason t in
          Context.add_hint_eval_cache_entry cx id result;
          result
      in
      (match result with
      | Some t -> loop t ops
      | None -> None)
  in
  (* We evaluate the decompositions in synthesis mode, but fully resolve the final result in
     checking mode, so that any unresolved tvars in the midddle won't fail the evaluation, but
     unsolved tvars in the final result will fail the evaluation. *)
  match Context.run_in_synthesis_mode cx (fun () -> loop t ops) with
  | (_, None) -> None
  | (_, Some t) -> fully_resolve_final_result cx t

and evaluate_hint cx reason hint =
  match hint with
  | Hint_None -> None
  | Hint_Placeholder -> Some (AnyT.annot (mk_reason (RCustom "placeholder hint") ALoc.none))
  | Hint_t t -> fully_resolve_final_result cx t
  | Hint_Decomp (ops, t) -> ops |> Nel.to_list |> List.rev |> evaluate_hint_ops cx reason t

let sandbox_flow_succeeds cx (t1, t2) =
  match SpeculationFlow.flow_t cx (TypeUtil.reason_of_t t1) ~upper_unresolved:false (t1, t2) with
  | exception Flow_js_utils.SpeculationSingletonError -> false
  | () -> true
