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

let with_hint_result ~ok ~error = function
  | HintAvailable t -> ok t
  | _ -> error ()

exception UnconstrainedTvarException

module SpeculationFlow = struct
  module SpeculationKit = Speculation_kit.Make (Flow_js.FlowJs)

  let flow_t cx reason ~upper_unresolved (l, u) =
    SpeculationKit.try_singleton_throw_on_failure
      cx
      Trace.dummy_trace
      ~upper_unresolved
      reason
      l
      (UseT (unknown_use, u))

  (* Returns a list of concrete types after breaking up unions, maybe types, etc *)
  let possible_concrete_types cx reason t =
    let id = Tvar.mk_no_wrap cx reason in
    Flow_js.flow cx (t, PreprocessKitT (reason, ConcretizeTypes (ConcretizeHintT id)));
    Flow_js_utils.possible_types cx id

  let try_singleton_no_throws cx reason ~upper_unresolved t u =
    try
      SpeculationKit.try_singleton_throw_on_failure
        cx
        Trace.dummy_trace
        reason
        ~upper_unresolved
        t
        u;
      true
    with
    | Flow_js_utils.SpeculationSingletonError -> false

  let resolved_lower_flow cx r (l, u) =
    match possible_concrete_types cx r l with
    | [] -> ()
    | [l] -> Flow_js.flow cx (l, u)
    | ls ->
      if
        not
          (Base.List.fold ls ~init:false ~f:(fun acc l ->
               let r = try_singleton_no_throws cx r ~upper_unresolved:true l u in
               acc || r
           )
          )
      then
        raise Flow_js_utils.SpeculationSingletonError

  let resolved_lower_flow_t cx r (l, u) = resolved_lower_flow cx r (l, UseT (unknown_use, u))

  let resolved_upper_flow_t cx r (l, u) =
    match possible_concrete_types cx r u with
    | [] -> ()
    | [u] -> Flow_js.flow_t cx (l, u)
    | us ->
      if
        not
          (Base.List.fold us ~init:false ~f:(fun acc u ->
               let r =
                 try_singleton_no_throws cx r ~upper_unresolved:false l (UseT (unknown_use, u))
               in
               acc || r
           )
          )
      then
        raise Flow_js_utils.SpeculationSingletonError
end

let in_sandbox_cx cx t ~f =
  Context.run_and_rolled_back_cache cx (fun () ->
      let original_errors = Context.errors cx in
      let no_lowers _ = raise UnconstrainedTvarException in
      Context.reset_errors cx Flow_error.ErrorSet.empty;
      match f (Tvar_resolver.resolved_t cx ~no_lowers t) with
      | (exception Flow_js_utils.SpeculationSingletonError)
      | (exception UnconstrainedTvarException) ->
        Context.reset_errors cx original_errors;
        None
      | exception exn ->
        (* On other exceptions we still need to reset errors *)
        let exn = Exception.wrap exn in
        Context.reset_errors cx original_errors;
        Exception.reraise exn
      | t ->
        let has_new_non_lint_errors =
          Context.errors cx
          |> Flow_error.ErrorSet.filter (fun e ->
                 e |> Flow_error.msg_of_error |> Error_message.is_lint_error |> not
             )
          |> Flow_error.ErrorSet.is_empty
        in
        Context.reset_errors cx original_errors;
        if has_new_non_lint_errors then
          Some t
        else
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

let rec get_t cx ~depth = function
  | AnnotT (_, t, _) when depth >= 0 -> get_t cx ~depth:(depth - 1) t
  | OpenT (r, id) when depth >= 0 ->
    Flow_js_utils.merge_tvar ~no_lowers:(fun _ r -> DefT (r, bogus_trust (), EmptyT)) cx r id
    |> get_t cx ~depth:(depth - 1)
  | t -> t

(* We choose a depth of 3 because it's sufficient to unwrap OpenT(AnnotT(OpenT)), which is the most
   complicated case known. If we run into issues in the future, we can increase the depth limit. *)
let get_t = get_t ~depth:3

let rec instantiate_callee cx fn instantiation_hint =
  let { Hint_api.reason; targs; arg_list; return_hints; arg_index } = instantiation_hint in
  let resolve_overload_and_targs fn =
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
    let rec handle_poly = function
      | DefT
          (_, _, (ObjT { call_t = Some id; _ } | InstanceT (_, _, _, { inst_call_t = Some id; _ })))
        ->
        handle_poly (Context.find_call cx id)
      | DefT (reason, _, ClassT instance) ->
        let statics = (reason, Tvar.mk_no_wrap cx reason) in
        Flow_js.flow cx (instance, GetStaticsT statics);
        handle_poly (get_t cx (OpenT statics))
      | DefT
          ( _,
            _,
            PolyT { tparams_loc = _; tparams; t_out = ThisClassT (r, i, this, this_name); id = _ }
          ) ->
        let subst_map =
          tparams
          |> Nel.map (fun tparam -> (tparam.name, tparam.bound))
          |> Nel.to_list
          |> Subst_name.Map.of_list
        in
        let t =
          Flow_js.FlowJs.fix_this_class
            cx
            Trace.dummy_trace
            r
            (r, Flow_js.subst cx subst_map i, this, this_name)
        in
        handle_poly (get_t cx t)
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
        let return_hint =
          match evaluate_hints cx reason return_hints with
          | HintAvailable t -> Some t
          | _ -> None
        in
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
              ImplicitInstantiation.solve_targs cx ~use_op:unknown_use ?return_hint check
              |> Subst_name.Map.map (fun solution -> solution.Implicit_instantiation.inferred)
          )
        in
        Flow_js.subst cx subst_map t_out
      | t -> t
    in
    handle_poly (get_t cx t)
  in
  match get_t cx (simplify_callee cx reason unknown_use fn) with
  | UnionT (_, rep) -> UnionT (reason, UnionRep.ident_map resolve_overload_and_targs rep)
  | MaybeT (_, t) -> resolve_overload_and_targs t
  | OptionalT { type_; _ } -> resolve_overload_and_targs type_
  | fn -> resolve_overload_and_targs fn

and instantiate_component cx component instantiation_hint =
  match get_t cx component with
  | DefT (_, _, PolyT { tparams_loc; tparams; t_out; id = _ })
    when Context.jsx cx = Options.Jsx_react ->
    let {
      Hint_api.jsx_reason = reason;
      jsx_name = _;
      jsx_props = config;
      jsx_children = children;
      jsx_hints;
    } =
      instantiation_hint
    in
    let return_hint =
      match evaluate_hints cx reason jsx_hints with
      | HintAvailable t -> Some t
      | _ -> None
    in
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
          ImplicitInstantiation.solve_targs cx ~use_op:unknown_use ?return_hint check
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
          ( Obj_type.mk_with_proto cx reason ~obj_kind:Exact (ObjProtoT reason),
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
        SpeculationFlow.resolved_lower_flow cx reason (t, use_t)
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
      | Decomp_ArrElement index ->
        let num =
          match index with
          | Some i -> Literal (None, (float_of_int i, string_of_int i))
          | None -> AnyLiteral
        in
        Tvar.mk_no_wrap_where cx reason (fun element_t ->
            let use_t =
              GetElemT
                (unknown_use, reason, true, DefT (reason, bogus_trust (), NumT num), element_t)
            in
            SpeculationFlow.resolved_lower_flow cx reason (t, use_t)
        )
      | Decomp_ArrSpread i ->
        Tvar.mk_no_wrap_where cx reason (fun tout ->
            let use_t = ArrRestT (unknown_use, reason, i, OpenT tout) in
            SpeculationFlow.resolved_lower_flow cx reason (t, use_t)
        )
      | Decomp_Await ->
        Tvar.mk_where cx reason (fun tout ->
            Flow_js.flow_t cx (t, tout);
            SpeculationFlow.resolved_lower_flow_t
              cx
              reason
              (Flow_js.get_builtin_typeapp cx reason (OrdinaryName "Promise") [t], tout)
        )
      | Decomp_CallNew ->
        (* For `new A(...)`, The initial base type we have is `Class<A>`. We need to first unwrap
           it, so that we can access the `constructor` method (which is considered an instance
           method). *)
        let get_this_t t =
          Tvar.mk_where cx reason (fun t' ->
              SpeculationFlow.resolved_lower_flow_t
                cx
                reason
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
            SpeculationFlow.resolved_upper_flow_t cx reason (fun_t, t)
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
            SpeculationFlow.resolved_upper_flow_t cx reason (fun_t, t)
        )
      | Decomp_FuncReturn ->
        Tvar.mk_where cx reason (fun return_t ->
            let fun_t =
              fun_t
                ~params:[]
                ~rest_param:(Some (None, ALoc.none, Unsoundness.unresolved_any reason))
                ~return_t
            in
            SpeculationFlow.resolved_lower_flow_t cx reason (t, fun_t)
        )
      | Comp_ImmediateFuncCall -> fun_t ~params:[] ~rest_param:None ~return_t:t
      | Decomp_JsxProps ->
        Tvar.mk_no_wrap_where cx reason (fun props_t ->
            SpeculationFlow.resolved_lower_flow
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
              SpeculationFlow.resolved_lower_flow
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
            SpeculationFlow.resolved_lower_flow cx reason (t, use_t)
        )
      | Decomp_ObjComputed reason ->
        let key_t = Env.find_write cx Env_api.ExpressionLoc reason in
        Tvar.mk_no_wrap_where cx reason (fun element_t ->
            let use_t = GetElemT (unknown_use, reason, true, key_t, element_t) in
            SpeculationFlow.resolved_lower_flow cx reason (t, use_t)
        )
      | Decomp_ObjSpread ->
        Tvar.mk_no_wrap_where cx reason (fun tout ->
            let use_t =
              (* We assume the object spread is at the start of the object. *)
              ObjRestT (reason, [], OpenT tout, Reason.mk_id ())
            in
            SpeculationFlow.resolved_lower_flow cx reason (t, use_t)
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
              | SingletonBigInt n ->
                DefT (reason, bogus_trust (), SingletonBigIntT (Some n, Int64.to_string n))
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
              SpeculationFlow.resolved_lower_flow cx reason (t, PredicateT (predicate, tvar))
          ))
      | Instantiate_Callee instantiation_hint -> instantiate_callee cx t instantiation_hint
      | Instantiate_Component instantiation_hint -> instantiate_component cx t instantiation_hint
      | Decomp_Promise ->
        Tvar.mk_where cx reason (fun inner_t ->
            SpeculationFlow.resolved_lower_flow_t
              cx
              reason
              (t, Flow_js.get_builtin_typeapp cx reason (OrdinaryName "Promise") [inner_t]);
            SpeculationFlow.resolved_lower_flow_t cx reason (t, inner_t)
        )
  )

and fully_resolve_final_result cx t =
  if Tvar_resolver.has_placeholders cx t then (
    Debug_js.Verbose.print_if_verbose_lazy
      cx
      (lazy [spf "Encountered placeholder type: %s" (Debug_js.dump_t cx ~depth:3 t)]);
    EncounteredPlaceholder
  ) else
    let no_lowers _ = raise UnconstrainedTvarException in
    match Tvar_resolver.resolved_t cx ~no_lowers t with
    | exception UnconstrainedTvarException -> DecompositionError
    | t -> HintAvailable t

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
  | (_, None) -> DecompositionError
  | (_, Some t) -> fully_resolve_final_result cx t

and evaluate_hint cx reason hint =
  match hint with
  | Hint_Placeholder -> HintAvailable (AnyT.annot (mk_reason (RCustom "placeholder hint") ALoc.none))
  | Hint_t t -> fully_resolve_final_result cx t
  | Hint_Decomp (ops, t) -> ops |> Nel.to_list |> List.rev |> evaluate_hint_ops cx reason t

and evaluate_hints cx reason hints =
  Base.List.fold_until
    hints
    ~init:NoHint
    ~finish:(fun r -> r)
    ~f:(fun _ hint ->
      match evaluate_hint cx reason hint with
      | HintAvailable t -> Base.Continue_or_stop.Stop (HintAvailable t)
      | r -> Base.Continue_or_stop.Continue r)

let sandbox_flow_succeeds cx (t1, t2) =
  match SpeculationFlow.flow_t cx (TypeUtil.reason_of_t t1) ~upper_unresolved:false (t1, t2) with
  | exception Flow_js_utils.SpeculationSingletonError -> false
  | () -> true
