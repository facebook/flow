(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type
open TypeUtil
open Hint
open Utils_js
module ImplicitInstantiation = Implicit_instantiation.Pierce (Flow_js.FlowJs)
module PinTypes = Implicit_instantiation.PinTypes (Flow_js.FlowJs)
module SpeculationFlow = Speculation_flow

let with_hint_result ~ok ~error = function
  | HintAvailable (t, _) -> ok t
  | _ -> error ()

exception UnconstrainedTvarException

exception DecompFuncParamOutOfBoundsException

let in_sandbox_cx cx t ~f =
  Context.run_and_rolled_back_cache cx (fun () ->
      let original_errors = Context.errors cx in
      let no_lowers _ = raise UnconstrainedTvarException in
      Context.reset_errors cx Flow_error.ErrorSet.empty;
      match f (Tvar_resolver.resolved_t cx ~no_lowers t) with
      | (exception Flow_js_utils.SpeculationSingletonError)
      | (exception UnconstrainedTvarException)
      | (exception DecompFuncParamOutOfBoundsException) ->
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
                 e |> Flow_error.msg_of_error |> Error_message.defered_in_speculation |> not
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
        call_kind = RegularCallKind;
        call_specialized_callee = None;
      }
  in
  let use = CallT { use_op; reason = call_reason; call_action; return_hint = hint_unavailable } in
  Flow_js.flow cx (intersection, use);
  match !call_speculation_hint_state with
  | Speculation_hint_unset
  | Speculation_hint_invalid ->
    (* We add an error to fail hint decomposition. *)
    Flow_js.add_output
      cx
      Error_message.(
        EUnionSpeculationFailed
          { use_op = unknown_use; reason; reason_op = call_reason; branches = [] }
      );
    AnyT.error reason
  | Speculation_hint_set (_, t) -> t

let simplify_callee cx reason use_op func_t =
  Tvar.mk_no_wrap_where cx reason (fun t ->
      let call_action = ConcretizeCallee t in
      Flow_js.flow cx (func_t, CallT { use_op; reason; call_action; return_hint = hint_unavailable })
  )

(* A cheaper version of `instantiate_poly_with_targs` from flow_js_utils that only performs
   substitution and skips any validation. *)
let synthesis_instantiate_callee cx reason tparams tout targs =
  let (map, _) =
    Nel.fold_left
      (fun (map, ts) typeparam ->
        let (t, ts) =
          match (typeparam, ts) with
          | ({ default = Some default; _ }, []) ->
            (* fewer arguments than params and we have a default *)
            (Flow_js.subst cx map default, [])
          | ({ default = None; _ }, []) -> (AnyT (reason, AnyError None), [])
          | (_, t :: ts) -> (t, ts)
        in
        (Subst_name.Map.add typeparam.name t map, ts))
      (Subst_name.Map.empty, targs)
      tparams
  in
  Flow_js.subst cx map tout

let rec get_t cx ~depth = function
  | AnnotT (_, t, _) when depth >= 0 -> get_t cx ~depth:(depth - 1) t
  | OpenT (r, id) when depth >= 0 ->
    Flow_js_utils.merge_tvar ~no_lowers:(fun _ r -> DefT (r, EmptyT)) cx r id
    |> get_t cx ~depth:(depth - 1)
  | t -> t

(* We choose a depth of 3 because it's sufficient to unwrap OpenT(AnnotT(OpenT)), which is the most
   complicated case known. If we run into issues in the future, we can increase the depth limit. *)
let get_t = get_t ~depth:3

let rec instantiate_callee cx fn instantiation_hint =
  let { Hint.reason; targs; arg_list; return_hints; arg_index } = instantiation_hint in
  let rec handle_poly = function
    | ExactT (_, DefT (_, ObjT { call_t = Some id; _ }))
    | DefT (_, ObjT { call_t = Some id; _ })
    | DefT (_, InstanceT { inst = { inst_call_t = Some id; _ }; _ }) ->
      handle_poly (Context.find_call cx id)
    | DefT (reason, ClassT instance) ->
      let statics = (reason, Tvar.mk_no_wrap cx reason) in
      Flow_js.flow cx (instance, GetStaticsT statics);
      handle_poly (get_t cx (OpenT statics))
    | DefT
        ( _,
          PolyT
            {
              tparams_loc = _;
              tparams;
              t_out = DefT (class_r, ClassT (ThisInstanceT (inst_r, i, this, this_name)));
              id = _;
            }
        ) ->
      let subst_map =
        tparams
        |> Nel.map (fun tparam -> (tparam.name, tparam.bound))
        |> Nel.to_list
        |> Subst_name.Map.of_list
      in
      let t =
        DefT
          ( class_r,
            ClassT
              (Flow_js_utils.fix_this_instance
                 cx
                 inst_r
                 (inst_r, Type_subst.subst_instance_type cx subst_map i, this, this_name)
              )
          )
      in
      handle_poly (get_t cx t)
    | DefT (_, PolyT { tparams_loc; tparams; t_out; id = _ }) as t ->
      let call_targs = Lazy.force targs in
      (match TypeUtil.all_explicit_targ_ts call_targs with
      | Some targ_ts -> synthesis_instantiate_callee cx reason tparams t_out targ_ts
      | None ->
        let call_args_tlist =
          let checked_t t loc =
            let reason = mk_reason (TypeUtil.reason_of_t t |> Reason.desc_of_reason) loc in
            Type_env.find_write cx Env_api.ExpressionLoc reason
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
        let return_hint =
          match evaluate_hints cx reason (Lazy.force return_hints) with
          | HintAvailable (t, k) -> Some (t, k)
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
              call_kind = RegularCallKind;
              call_specialized_callee = None;
            }
        in
        let subst_map =
          Context.run_in_implicit_instantiation_mode cx (fun () ->
              ImplicitInstantiation.solve_targs cx ~use_op:unknown_use ?return_hint check
              |> Subst_name.Map.map (fun solution -> solution.Implicit_instantiation.inferred)
          )
        in
        Flow_js.subst cx subst_map t_out)
    | t -> t
  in
  let resolve_overload_and_targs fn =
    let t =
      match fn with
      | IntersectionT (r, rep) ->
        synthesis_speculation_call
          cx
          reason
          (r, rep)
          (Lazy.force targs)
          (Lazy.force arg_list |> Base.List.map ~f:snd)
      | t -> t
    in
    handle_poly (get_t cx t)
  in
  match fn with
  | UnionT (_, rep) -> UnionT (reason, UnionRep.ident_map resolve_overload_and_targs rep)
  | fn -> resolve_overload_and_targs fn

and instantiate_component cx component instantiation_hint =
  match get_t cx component with
  | DefT (_, PolyT { tparams_loc; tparams; t_out; id = _ }) when Context.jsx cx = Options.Jsx_react
    ->
    let {
      Hint.jsx_reason = reason;
      jsx_name = _;
      jsx_props = config;
      jsx_children = children;
      jsx_hints;
    } =
      instantiation_hint
    in
    let return_hint =
      match evaluate_hints cx reason (Lazy.force jsx_hints) with
      | HintAvailable (t, k) -> Some (t, k)
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
        ~config:(Lazy.force config)
        ~targs:None
        (Lazy.force children)
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
  let fun_t ~params ~rest_param ~return_t ~pred =
    let statics_reason =
      Reason.func_reason ~async:false ~generator:false (Reason.loc_of_reason reason)
    in
    let statics =
      Obj_type.mk_with_proto cx statics_reason (Type.FunProtoT reason) ~obj_kind:Type.Inexact
    in
    let predicate =
      Base.Option.map pred ~f:(function
          | PredKind -> PredBased (reason, lazy (Key_map.empty, Key_map.empty))
          | TypeGuardKind (param_loc, param_name) ->
            TypeGuardBased
              {
                param_name = (param_loc, param_name);
                type_guard = Unsoundness.unresolved_any reason;
              }
          )
    in
    let func =
      {
        this_t = (Unsoundness.unresolved_any reason, This_Function);
        params;
        rest_param;
        return_t;
        predicate;
        def_reason = reason;
        hook = AnyHook;
      }
    in
    DefT (reason, FunT (statics, func))
  in

  let map_intersection t ~f =
    match get_t cx t with
    | IntersectionT (r, rep) -> IntersectionT (r, InterRep.map (fun t -> f (get_t cx t)) rep)
    | t -> f t
  in

  let get_constructor_type t =
    let get_constructor_method_type t =
      SpeculationFlow.get_method_type_unsafe
        cx
        t
        reason
        (mk_named_prop ~reason (OrdinaryName "constructor"))
    in
    let mod_ctor_return instance_type = function
      | DefT
          ( reason,
            FunT (static, { this_t; params; rest_param; return_t = _; predicate; def_reason; hook })
          ) ->
        DefT
          ( reason,
            FunT
              ( static,
                {
                  this_t;
                  params;
                  rest_param;
                  return_t = instance_type;
                  predicate;
                  def_reason;
                  hook;
                }
              )
          )
      | t -> get_t cx t
    in
    match get_t cx t with
    | DefT (_, PolyT { tparams_loc; tparams; t_out = instance_type; id = _ }) ->
      map_intersection (get_constructor_method_type instance_type) ~f:(function
          | DefT (_, PolyT { tparams_loc; tparams = tparams2; t_out; id = _ }) ->
            let t_out = mod_ctor_return instance_type t_out in
            DefT
              ( reason,
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
            DefT (reason, PolyT { tparams_loc; tparams; t_out; id = Poly.generate_id () })
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
        Tvar.mk_no_wrap_where cx reason (fun tout ->
            let use_t =
              GetElemT
                {
                  use_op = unknown_use;
                  reason;
                  id = None;
                  from_annot = true;
                  access_iterables = true;
                  key_t = DefT (reason, NumT num);
                  tout;
                }
            in
            SpeculationFlow.resolved_lower_flow_unsafe cx reason (t, use_t)
        )
      | Decomp_ArrSpread i ->
        Tvar.mk_no_wrap_where cx reason (fun tout ->
            let use_t = ArrRestT (unknown_use, reason, i, OpenT tout) in
            SpeculationFlow.resolved_lower_flow_unsafe cx reason (t, use_t)
        )
      | Decomp_EmptyArrayElement ->
        if Tvar_resolver.has_placeholders cx t then (
          Debug_js.Verbose.print_if_verbose_lazy
            cx
            (lazy [spf "Encountered placeholder type: %s" (Debug_js.dump_t cx ~depth:3 t)]);
          Context.mk_placeholder cx reason
        ) else
          let elem_t = Tvar.mk cx reason in
          Context.run_in_implicit_instantiation_mode cx (fun () ->
              SpeculationFlow.flow_t_unsafe
                cx
                reason
                ~upper_unresolved:false
                ( DefT
                    ( reason,
                      ArrT (ArrayAT { elem_t; tuple_view = Some ([], (0, 0)); react_dro = None })
                    ),
                  t
                )
          );
          PinTypes.pin_type cx ~use_op:unknown_use reason elem_t
      | Decomp_Await ->
        Tvar.mk_where cx reason (fun tout ->
            Flow_js.flow_t cx (t, tout);
            SpeculationFlow.resolved_lower_flow_t_unsafe
              cx
              reason
              (Flow_js.get_builtin_typeapp cx reason "Promise" [t], tout)
        )
      | Decomp_CallNew ->
        (* For `new A(...)`, The initial base type we have is `Class<A>`. We need to first unwrap
           it, so that we can access the `constructor` method (which is considered an instance
           method). *)
        let get_this_t t =
          Tvar.mk_where cx reason (fun t' ->
              SpeculationFlow.resolved_lower_flow_t_unsafe cx reason (t, DefT (reason, ClassT t'))
          )
          |> get_t cx
        in
        let this_t =
          match get_t cx t with
          | DefT (reason, PolyT { tparams_loc; tparams; t_out; id = _ }) ->
            DefT
              ( reason,
                PolyT { tparams_loc; tparams; t_out = get_this_t t_out; id = Poly.generate_id () }
              )
          | t -> get_this_t t
        in
        get_constructor_type this_t
      | Decomp_CallSuper -> get_constructor_type t
      | Decomp_FuncParam (xs, i, pred) ->
        if i > List.length xs then
          (* This is an internal error. We shouldn't be creating Decomp_FuncParam
           * where [i] is not a valid index of [xs]. *)
          raise DecompFuncParamOutOfBoundsException
        else
          let xs = Base.List.take xs (i + 1) in
          Tvar.mk_where cx reason (fun param_t ->
              let params =
                Base.List.mapi xs ~f:(fun idx x ->
                    if i = idx then
                      (x, param_t)
                    else
                      (x, Unsoundness.unresolved_any reason)
                )
              in
              let fun_t =
                fun_t ~params ~rest_param:None ~return_t:(Unsoundness.unresolved_any reason) ~pred
              in
              SpeculationFlow.resolved_upper_flow_t_unsafe cx reason (fun_t, t)
          )
      | Decomp_FuncRest (xs, pred) ->
        Tvar.mk_where cx reason (fun rest_t ->
            let params = Base.List.map xs ~f:(fun x -> (x, Unsoundness.unresolved_any reason)) in
            let fun_t =
              fun_t
                ~params
                ~rest_param:(Some (None, ALoc.none, rest_t))
                ~return_t:(Unsoundness.unresolved_any reason)
                ~pred
            in
            SpeculationFlow.resolved_upper_flow_t_unsafe cx reason (fun_t, t)
        )
      | Decomp_FuncReturn ->
        Tvar.mk_where cx reason (fun return_t ->
            let fun_t =
              fun_t
                ~params:[]
                ~rest_param:(Some (None, ALoc.none, Unsoundness.unresolved_any reason))
                ~return_t
                ~pred:None
            in
            SpeculationFlow.resolved_lower_flow_t_unsafe cx reason (t, fun_t)
        )
      | Comp_ImmediateFuncCall -> fun_t ~params:[] ~rest_param:None ~return_t:t ~pred:None
      | Comp_MaybeT -> MaybeT (reason, t)
      | Decomp_JsxProps ->
        Tvar.mk_no_wrap_where cx reason (fun props_t ->
            SpeculationFlow.resolved_lower_flow_unsafe
              cx
              reason
              (t, ReactKitT (unknown_use, reason, React.GetConfig (OpenT props_t)))
        )
      | Decomp_JsxRef -> Flow_js.get_builtin_typeapp cx reason "React$Ref" [t]
      | Decomp_MethodElem ->
        SpeculationFlow.get_method_type_unsafe
          cx
          t
          reason
          (Computed (DefT (reason, StrT AnyLiteral)))
      | Decomp_MethodName name ->
        SpeculationFlow.get_method_type_unsafe
          cx
          t
          reason
          (mk_named_prop ~reason (OrdinaryName name))
      | Decomp_MethodPrivateName (name, class_stack) ->
        let env = Context.environment cx in
        Context.set_environment cx { env with Loc_env.class_stack };
        let class_entries = Type_env.get_class_entries cx in
        let t =
          Tvar.mk_where cx reason (fun prop_t ->
              SpeculationFlow.resolved_lower_flow_unsafe
                cx
                reason
                ( t,
                  PrivateMethodT
                    (unknown_use, reason, reason, name, class_entries, false, NoMethodAction prop_t)
                )
          )
        in
        Context.set_environment cx env;
        t
      | Decomp_ObjProp name ->
        Tvar.mk_no_wrap_where cx reason (fun tout ->
            let use_t =
              GetPropT
                {
                  use_op = unknown_use;
                  reason;
                  id = Some (Reason.mk_id ());
                  from_annot = false;
                  propref = mk_named_prop ~reason (OrdinaryName name);
                  tout;
                  hint = hint_unavailable;
                }
            in
            SpeculationFlow.resolved_lower_flow_unsafe cx reason (t, use_t)
        )
      | Decomp_ObjComputed reason ->
        let key_t = Type_env.find_write cx Env_api.ExpressionLoc reason in
        Tvar.mk_no_wrap_where cx reason (fun tout ->
            let use_t =
              GetElemT
                {
                  use_op = unknown_use;
                  reason;
                  id = None;
                  from_annot = true;
                  access_iterables = false;
                  key_t;
                  tout;
                }
            in
            SpeculationFlow.resolved_lower_flow_unsafe cx reason (t, use_t)
        )
      | Decomp_ObjSpread ->
        Tvar.mk_no_wrap_where cx reason (fun tout ->
            let use_t =
              (* We assume the object spread is at the start of the object. *)
              ObjRestT (reason, [], OpenT tout, Reason.mk_id ())
            in
            SpeculationFlow.resolved_lower_flow_unsafe cx reason (t, use_t)
        )
      | Decomp_PrivateProp (name, class_stack) ->
        let env = Context.environment cx in
        Context.set_environment cx { env with Loc_env.class_stack };
        let class_entries = Type_env.get_class_entries cx in
        let t =
          Tvar.mk_no_wrap_where cx reason (fun prop_t ->
              SpeculationFlow.resolved_lower_flow_unsafe
                cx
                reason
                (t, GetPrivatePropT (unknown_use, reason, name, class_entries, false, prop_t))
          )
        in
        Context.set_environment cx env;
        t
      | Decomp_SentinelRefinement checks ->
        (match SMap.elements checks with
        | [] -> t
        | hd :: tl ->
          (match Flow_js.possible_concrete_types_for_inspection cx reason t with
          | [] -> t
          | [t] -> t
          | _ ->
            let predicate_of_check (prop, literal_check) =
              let other_t =
                match literal_check with
                | SingletonBool b -> DefT (reason, SingletonBoolT b)
                | SingletonNum n -> DefT (reason, SingletonNumT (n, string_of_float n))
                | SingletonStr s -> DefT (reason, SingletonStrT (OrdinaryName s))
                | SingletonBigInt n -> DefT (reason, SingletonBigIntT (Some n, Int64.to_string n))
                | Member reason -> Type_env.find_write cx Env_api.ExpressionLoc reason
              in
              LeftP (SentinelProp prop, other_t)
            in
            let predicate =
              Base.List.fold tl ~init:(predicate_of_check hd) ~f:(fun acc check ->
                  AndP (acc, predicate_of_check check)
              )
            in
            Tvar.mk_no_wrap_where cx reason (fun tvar ->
                Flow_js.flow cx (t, PredicateT (predicate, tvar))
            )))
      | Simplify_Callee reason ->
        let simplify fn = get_t cx (simplify_callee cx reason unknown_use fn) in
        (match simplify t with
        | UnionT (_, rep) -> UnionT (reason, UnionRep.ident_map simplify rep)
        | MaybeT (_, t) -> simplify t
        | OptionalT { type_; _ } -> simplify type_
        | fn -> fn)
      | Instantiate_Callee instantiation_hint -> instantiate_callee cx t instantiation_hint
      | Instantiate_Component instantiation_hint -> instantiate_component cx t instantiation_hint
      | Decomp_Promise ->
        Tvar.mk_where cx reason (fun inner_t ->
            SpeculationFlow.resolved_lower_flow_t_unsafe
              cx
              reason
              (t, Flow_js.get_builtin_typeapp cx reason "Promise" [inner_t]);
            SpeculationFlow.resolved_lower_flow_t_unsafe cx reason (t, inner_t)
        )
  )

and fully_resolve_final_result cx t kind =
  if Tvar_resolver.has_placeholders cx t then (
    Debug_js.Verbose.print_if_verbose_lazy
      cx
      (lazy [spf "Encountered placeholder type: %s" (Debug_js.dump_t cx ~depth:3 t)]);
    EncounteredPlaceholder
  ) else
    let no_lowers _ = raise UnconstrainedTvarException in
    match Tvar_resolver.resolved_t cx ~no_lowers t with
    | exception UnconstrainedTvarException -> DecompositionError
    | t -> HintAvailable (t, kind)

and evaluate_hint_ops cx reason t kind ops =
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
  match Context.run_in_hint_eval_mode cx (fun () -> loop t ops) with
  | None -> DecompositionError
  | Some t -> fully_resolve_final_result cx t kind

and evaluate_hint cx reason hint =
  match hint with
  | Hint_Placeholder ->
    HintAvailable (AnyT.annot (mk_reason (RCustom "placeholder hint") ALoc.none), ExpectedTypeHint)
  | Hint_t (t, kind) -> fully_resolve_final_result cx t kind
  | Hint_Decomp (ops, t, kind) ->
    ops |> Nel.to_list |> List.rev |> evaluate_hint_ops cx reason t kind

and evaluate_hints cx reason hints =
  Debug_js.Verbose.print_if_verbose_lazy
    cx
    (lazy [spf "Evaluating hint %s" (string_of_hints ~on_hint:(Debug_js.dump_t cx ~depth:3) hints)]);
  let result =
    Base.List.fold_until
      hints
      ~init:NoHint
      ~finish:(fun r -> r)
      ~f:(fun _ hint ->
        match evaluate_hint cx reason hint with
        | HintAvailable (t, kind) -> Base.Continue_or_stop.Stop (HintAvailable (t, kind))
        | r -> Base.Continue_or_stop.Continue r)
  in
  Debug_js.Verbose.print_if_verbose_lazy
    cx
    ( lazy
      [
        spf "Hint %s evaluates to" (string_of_hints ~on_hint:(Debug_js.dump_t cx ~depth:3) hints);
        (match result with
        | HintAvailable (t, ExpectedTypeHint) ->
          spf "ExpectedTypeHint: %s" (Debug_js.dump_t cx ~depth:3 t)
        | HintAvailable (t, BestEffortHint) ->
          spf "BestEffortHint: %s" (Debug_js.dump_t cx ~depth:3 t)
        | NoHint -> "NoHint"
        | EncounteredPlaceholder -> "EncounteredPlaceholder"
        | DecompositionError -> "DecompositionError");
      ]
      );
  result
