(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Reason
open Polarity
module TypeParamMarked = Marked.Make (Subst_name)
module Marked = TypeParamMarked
module Check = Implicit_instantiation_check

module type OBSERVER = sig
  type output

  val on_pinned_tparam : Context.t -> Subst_name.t -> Type.typeparam -> Type.t -> output

  val on_constant_tparam_missing_bounds : Context.t -> Subst_name.t -> Type.typeparam -> output

  val on_missing_bounds :
    Context.t ->
    Subst_name.t ->
    Type.typeparam ->
    tparam_binder_reason:Reason.reason ->
    instantiation_reason:Reason.reason ->
    output

  val on_upper_non_t :
    Context.t ->
    Subst_name.t ->
    Type.use_t ->
    Type.typeparam ->
    tparam_binder_reason:Reason.reason ->
    instantiation_reason:Reason.reason ->
    output
end

module type S = sig
  type output

  module Flow : Flow_common.S

  val pin_type :
    Context.t ->
    Subst_name.t ->
    Type.typeparam ->
    Polarity.t option ->
    default_bound:Type.t option ->
    Reason.reason ->
    Type.t ->
    output

  val solve_targs : Context.t -> ?return_hint:Type.t -> Check.t -> output Subst_name.Map.t

  val run :
    Context.t ->
    Check.t ->
    on_completion:(Context.t -> output Subst_name.Map.t -> 'result) ->
    'result

  val fold :
    implicit_instantiation_cx:Context.t ->
    cx:Context.t ->
    f:(Context.t -> 'acc -> Check.t -> output Subst_name.Map.t -> 'acc) ->
    init:'acc ->
    post:(cx:Context.t -> implicit_instantiation_cx:Context.t -> unit) ->
    Check.t list ->
    'acc
end

module Make (Observer : OBSERVER) (Flow : Flow_common.S) : S with type output = Observer.output =
struct
  type output = Observer.output

  module Flow = Flow

  let get_t cx =
    let no_lowers _cx r = Type.Unsoundness.merged_any r in
    function
    | OpenT (r, id) -> Flow_js_utils.merge_tvar ~no_lowers cx r id
    | t -> t

  (* This visitor records the polarities at which BoundTs are found. We follow the bounds of each
   * type parameter as well, since some type params are only used in the bounds of another.
   *)
  class implicit_instantiation_visitor ~tparams_map =
    object (self)
      inherit [Marked.t * Subst_name.Set.t] Type_visitor.t as super

      method! type_ cx pole ((marked, tparam_names) as acc) =
        function
        | GenericT { name = s; _ } as t ->
          if Subst_name.Set.mem s tparam_names then
            match Marked.add s pole marked with
            | None -> acc
            | Some (_, marked) ->
              (match Subst_name.Map.find_opt s tparams_map with
              | None -> (marked, tparam_names)
              | Some tparam -> self#type_ cx pole (marked, tparam_names) tparam.bound)
          else
            super#type_ cx pole acc t
        (* We remove any tparam names from the map when entering a PolyT to avoid naming conflicts. *)
        | DefT (_, _, PolyT { tparams; t_out = t; _ }) ->
          let tparam_names' =
            Nel.fold_left (fun names x -> Subst_name.Set.remove x.name names) tparam_names tparams
          in
          let (marked, _) = self#type_ cx pole (marked, tparam_names') t in
          (* TODO(jmbrown): Handle defaults on type parameters *)
          (marked, tparam_names)
        | TypeAppT (_, _, c, ts) -> self#typeapp ts cx pole acc c
        (* ThisTypeAppT is created from a new expression, which cannot
         * be used as an annotation, so we do not special case it like
         * we do with TypeAppT
         *)
        | t -> super#type_ cx pole acc t

      method private typeapp =
        let rec loop cx pole seen = function
          (* Any arity erors are already handled in Flow_js *)
          | (_, []) -> seen
          | (Some [], _) -> seen
          | (None, targ :: targs) ->
            (* In the absence of tparams we will just visit the args with a
             * neutral polarity. *)
            let param_polarity = Polarity.Neutral in
            let seen = self#type_ cx param_polarity seen targ in
            loop cx pole seen (None, targs)
          | (Some (tparam :: tparams), targ :: targs) ->
            let param_polarity = Polarity.mult (pole, tparam.polarity) in
            let seen = self#type_ cx param_polarity seen targ in
            loop cx pole seen (Some tparams, targs)
        in
        fun targs cx pole acc t ->
          match get_t cx t with
          | AnnotT (_, t, _) -> self#typeapp targs cx pole acc t
          | DefT (_, _, PolyT { tparams; _ }) -> loop cx pole acc (Some (Nel.to_list tparams), targs)
          | DefT (_, _, EmptyT)
          | AnyT _ ->
            loop cx pole acc (None, targs)
          (* All other cases are errors *)
          | _ -> loop cx pole acc (None, targs)
    end

  type use_t_result =
    | UpperEmpty
    | UpperNonT of Type.use_t
    | UpperT of Type.t

  let rec t_of_use_t cx tvar u =
    let use_t_result_of_t_option = function
      | Some t -> UpperT t
      | None -> UpperEmpty
    in
    match u with
    | UseT (_, (OpenT (r, _) as t)) -> merge_upper_bounds cx r t
    | UseT (_, TypeDestructorTriggerT (_, r, _, destructor, tout)) ->
      (match destructor with
      | PropertyType _
      | ElementType _
      | ValuesType
      | CallType _
      | TypeMap _
      | ReactElementRefType ->
        UpperEmpty
      | ReactElementPropsType
      | ReactElementConfigType ->
        merge_lower_bounds cx (OpenT tout)
        |> Base.Option.map ~f:(fun config ->
               DefT
                 ( r,
                   bogus_trust (),
                   ReactAbstractComponentT { config; instance = MixedT.why r (bogus_trust ()) }
                 )
           )
        |> use_t_result_of_t_option
      | NonMaybeType ->
        merge_lower_bounds cx (OpenT tout)
        |> Base.Option.map ~f:(fun t -> MaybeT (r, t))
        |> use_t_result_of_t_option
      | ReadOnlyType
      | PartialType ->
        merge_lower_bounds cx (OpenT tout) |> use_t_result_of_t_option
      | ReactConfigType _ -> reverse_obj_kit_react_config cx tvar r (OpenT tout)
      | _ -> UpperNonT u)
    | UseT (_, t) -> UpperT t
    | MakeExactT (_, Lower (_, t)) -> UpperT t
    | MakeExactT (_, Upper use_t) -> t_of_use_t cx tvar use_t
    | ReposLowerT (_, _, use_t) -> t_of_use_t cx tvar use_t
    | ResolveSpreadT
        ( _,
          reason,
          {
            rrt_resolved = [];
            rrt_unresolved = [];
            rrt_resolve_to =
              ResolveSpreadsToMultiflowSubtypeFull (_, { params; rest_param = None; _ });
          }
        ) ->
      reverse_resolve_spread_multiflow_subtype_full_no_resolution cx tvar reason params
    | ObjKitT (_, _, _, Object.(ReadOnly | Partial), tout) ->
      merge_lower_bounds cx tout |> use_t_result_of_t_option
    | ObjKitT (_, r, _, Object.ReactConfig _, props) -> reverse_obj_kit_react_config cx tvar r props
    | _ -> UpperNonT u

  and reverse_obj_kit_react_config cx tvar r props_tvar =
    let solution = merge_upper_bounds cx r props_tvar in
    (match solution with
    | UpperT t -> Flow.flow_t cx (t, tvar)
    | _ -> ());
    solution

  and reverse_resolve_spread_multiflow_subtype_full_no_resolution cx tvar reason params =
    let tuple_members = params |> List.map (fun param -> snd param) in
    let general =
      match tuple_members with
      | [] -> EmptyT.why reason |> with_trust bogus_trust
      | [t] -> t
      | t0 :: t1 :: ts -> UnionT (reason, UnionRep.make t0 t1 ts)
    in
    let tuple = TupleAT (general, tuple_members) in
    let solution = DefT (reason, bogus_trust (), ArrT tuple) in
    Flow.flow_t cx (solution, tvar);
    UpperT solution

  and merge_upper_bounds cx upper_r tvar =
    match tvar with
    | OpenT (_, id) ->
      let constraints = Context.find_graph cx id in
      (match constraints with
      | Constraint.FullyResolved (_, (lazy t))
      | Constraint.Resolved (_, t) ->
        UpperT t
      | Constraint.Unresolved bounds ->
        let uppers = Constraint.UseTypeMap.keys bounds.Constraint.upper in
        uppers
        |> List.fold_left
             (fun acc (t, _) ->
               match (acc, t_of_use_t cx tvar t) with
               | (UpperNonT u, _) -> UpperNonT u
               | (_, UpperNonT u) -> UpperNonT u
               | (UpperEmpty, UpperT t) -> UpperT t
               | (UpperT t', UpperT t) ->
                 (match (t', t) with
                 | (IntersectionT (_, rep1), IntersectionT (_, rep2)) ->
                   UpperT (IntersectionT (upper_r, InterRep.append (InterRep.members rep2) rep1))
                 | (_, IntersectionT (_, rep)) ->
                   UpperT (IntersectionT (upper_r, InterRep.append [t'] rep))
                 | (IntersectionT (_, rep), _) ->
                   UpperT (IntersectionT (upper_r, InterRep.append [t] rep))
                 | (t', t) -> UpperT (IntersectionT (upper_r, InterRep.make t' t [])))
               | (UpperT _, UpperEmpty) -> acc
               | (UpperEmpty, UpperEmpty) -> acc)
             UpperEmpty)
    | _ -> failwith "Implicit instantiation is not an OpenT"

  and merge_lower_bounds cx t =
    match t with
    | OpenT (_, id) ->
      let constraints = Context.find_graph cx id in
      (match constraints with
      | Constraint.FullyResolved (_, (lazy t))
      | Constraint.Resolved (_, t) ->
        Some t
      | Constraint.Unresolved bounds ->
        let lowers = bounds.Constraint.lower in
        if TypeMap.cardinal lowers = 0 then
          None
        else
          Some (get_t cx t))
    | _ -> failwith "Implicit instantiation is not an OpenT"

  let on_missing_bounds cx name tparam ~default_bound ~tparam_binder_reason ~instantiation_reason =
    match default_bound with
    | Some t -> Observer.on_pinned_tparam cx name tparam t
    | None -> Observer.on_missing_bounds cx name tparam ~tparam_binder_reason ~instantiation_reason

  let use_upper_bounds
      cx
      name
      tparam
      tvar
      ~default_bound
      ?(on_upper_empty = on_missing_bounds ~default_bound)
      tparam_binder_reason
      instantiation_reason =
    let upper_t = merge_upper_bounds cx tparam_binder_reason tvar in
    match upper_t with
    | UpperEmpty -> on_upper_empty cx name tparam ~tparam_binder_reason ~instantiation_reason
    | UpperNonT u ->
      Observer.on_upper_non_t cx name ~tparam_binder_reason ~instantiation_reason u tparam
    | UpperT inferred -> Observer.on_pinned_tparam cx name tparam inferred

  let check_instantiation cx ~tparams ~marked_tparams ~implicit_instantiation =
    let { Check.lhs; operation = (use_op, reason_op, op); poly_t = (tparams_loc, _, _) } =
      implicit_instantiation
    in
    let reason_tapp = TypeUtil.reason_of_t lhs in
    let merge_targs explicit_targs =
      match explicit_targs with
      | None ->
        List.fold_right
          (fun tparam (targs, inferred_targ_and_bound_list) ->
            let targ =
              Instantiation_utils.ImplicitTypeArgument.mk_targ cx tparam reason_op reason_tapp
            in
            ( ExplicitArg targ :: targs,
              (tparam.name, targ, tparam.bound, true) :: inferred_targ_and_bound_list
            ))
          tparams
          ([], [])
      | Some explicit_targs ->
        let maximum_arity = List.length tparams in
        let reason_arity = Flow_js_utils.mk_poly_arity_reason tparams_loc in
        let minimum_arity = Flow_js_utils.poly_minimum_arity (Nel.of_list_exn tparams) in
        if List.length explicit_targs > maximum_arity then
          Flow_js_utils.add_output
            cx
            (Error_message.ETooManyTypeArgs (reason_tapp, reason_arity, maximum_arity));
        let rec loop (targs, inferred_targ_and_bound_list) tparams_rev_acc explicit_targs_rev_acc =
          match (tparams_rev_acc, explicit_targs_rev_acc) with
          | ([], _) -> (List.rev targs, List.rev inferred_targ_and_bound_list)
          | (tparam :: tparams_rest, []) ->
            let targ =
              Instantiation_utils.ImplicitTypeArgument.mk_targ cx tparam reason_op reason_tapp
            in
            if Base.Option.is_none tparam.default then
              Flow_js_utils.add_output
                cx
                (Error_message.ETooFewTypeArgs (reason_tapp, reason_arity, minimum_arity));
            loop
              ( ExplicitArg targ :: targs,
                (tparam.name, targ, tparam.bound, true) :: inferred_targ_and_bound_list
              )
              tparams_rest
              []
          | (tparam :: tparams_rest, explicit_targ :: explicit_targs_rest) ->
            (match explicit_targ with
            | ExplicitArg targ ->
              loop
                ( ExplicitArg targ :: targs,
                  (tparam.name, targ, tparam.bound, false) :: inferred_targ_and_bound_list
                )
                tparams_rest
                explicit_targs_rest
            | ImplicitArg (r, id) ->
              let reason = mk_reason RImplicitInstantiation (aloc_of_reason r) in
              let targ =
                Instantiation_utils.ImplicitTypeArgument.mk_targ cx tparam reason reason_tapp
              in
              Flow.flow cx (targ, UseT (use_op, OpenT (r, id)));
              (* It is important to convert implicit args to explicit args so there won't be
                 infinite loops between this module and flow_js. *)
              loop
                ( ExplicitArg targ :: targs,
                  (tparam.name, targ, tparam.bound, true) :: inferred_targ_and_bound_list
                )
                tparams_rest
                explicit_targs_rest)
        in
        loop ([], []) tparams explicit_targs
    in
    let (inferred_targ_list, use_t, tout) =
      match op with
      | Check.Call calltype ->
        let new_tout = Tvar.mk_no_wrap cx reason_op in
        let (call_targs, inferred_targ_list) = merge_targs calltype.call_targs in
        let call_t =
          CallT
            {
              use_op;
              reason = reason_op;
              call_action =
                Funcalltype
                  { calltype with call_targs = Some call_targs; call_tout = (reason_op, new_tout) };
              return_hint = Type.hint_unavailable;
            }
        in
        (inferred_targ_list, call_t, OpenT (reason_op, new_tout))
      | Check.Constructor (explicit_targs, call_args) ->
        let new_tout = Tvar.mk cx reason_op in
        let (call_targs, inferred_targ_list) = merge_targs explicit_targs in
        let constructor_t =
          ConstructorT
            {
              use_op;
              reason = reason_op;
              targs = Some call_targs;
              args = call_args;
              tout = new_tout;
              return_hint = Type.hint_unavailable;
            }
        in
        (inferred_targ_list, constructor_t, new_tout)
      | Check.Jsx { clone; component; config; targs; children } ->
        let new_tout = Tvar.mk cx reason_op in
        let (call_targs, inferred_targ_list) = merge_targs targs in
        let react_kit_t =
          ReactKitT
            ( use_op,
              reason_op,
              React.CreateElement
                {
                  clone;
                  component;
                  config;
                  children;
                  targs = Some call_targs;
                  tout = new_tout;
                  return_hint = Type.hint_unavailable;
                }
            )
        in
        (inferred_targ_list, react_kit_t, new_tout)
    in
    Flow.flow cx (lhs, use_t);
    (inferred_targ_list, marked_tparams, tout)

  let pin_type cx name tparam polarity ~default_bound instantiation_reason t =
    let tparam_binder_reason = TypeUtil.reason_of_t t in
    let pin_tparam inferred = Observer.on_pinned_tparam cx name tparam inferred in
    match polarity with
    | None ->
      (match merge_lower_bounds cx t with
      | None ->
        let on_upper_empty cx name tparam ~tparam_binder_reason:_ ~instantiation_reason:_ =
          Observer.on_constant_tparam_missing_bounds cx name tparam
        in
        use_upper_bounds
          cx
          name
          tparam
          t
          ~default_bound
          ~on_upper_empty
          tparam_binder_reason
          instantiation_reason
      | Some inferred -> Observer.on_pinned_tparam cx name tparam inferred)
    | Some Neutral ->
      (* TODO(jmbrown): The neutral case should also unify upper/lower bounds. In order
       * to avoid cluttering the output we are actually interested in from this module,
       * I'm not going to start doing that until we need error diff information for
       * switching to Pierce's algorithm for implicit instantiation *)
      let lower_t = merge_lower_bounds cx t in
      (match lower_t with
      | None ->
        use_upper_bounds cx name tparam t ~default_bound tparam_binder_reason instantiation_reason
      | Some inferred -> pin_tparam inferred)
    | Some Positive ->
      (match merge_lower_bounds cx t with
      | None ->
        use_upper_bounds cx name tparam t ~default_bound tparam_binder_reason instantiation_reason
      | Some inferred -> pin_tparam inferred)
    | Some Negative ->
      let on_upper_empty cx name tparam ~tparam_binder_reason ~instantiation_reason =
        match merge_lower_bounds cx t with
        | None ->
          on_missing_bounds
            cx
            name
            tparam
            ~default_bound
            ~tparam_binder_reason
            ~instantiation_reason
        | Some inferred -> Observer.on_pinned_tparam cx name tparam inferred
      in
      use_upper_bounds
        cx
        name
        tparam
        t
        ~default_bound
        ~on_upper_empty
        tparam_binder_reason
        instantiation_reason

  let pin_types
      cx
      ~has_new_errors
      ~has_return_hint
      inferred_targ_list
      marked_tparams
      tparams_map
      implicit_instantiation =
    let { Check.operation = (_, instantiation_reason, _); _ } = implicit_instantiation in
    let subst_map =
      List.fold_left
        (fun acc (name, t, _, _) -> Subst_name.Map.add name t acc)
        Subst_name.Map.empty
        inferred_targ_list
    in
    List.fold_right
      (fun (name, t, bound, is_inferred) acc ->
        let tparam = Subst_name.Map.find name tparams_map in
        let polarity =
          if has_return_hint then
            None
          else
            Marked.get name marked_tparams
        in
        let result =
          if is_inferred then
            pin_type
              cx
              name
              tparam
              polarity
              ~default_bound:
                (Base.Option.some_if has_new_errors (AnyT.error (TypeUtil.reason_of_t t)))
              instantiation_reason
              t
          else
            Observer.on_pinned_tparam cx name tparam t
        in
        let bound_t = Subst.subst cx ~use_op:unknown_use subst_map bound in
        Flow.flow_t cx (t, bound_t);
        Subst_name.Map.add name result acc)
      inferred_targ_list
      Subst_name.Map.empty

  let check_fun cx ~tparams ~tparams_map ~return_t ~implicit_instantiation =
    (* Visit the return type *)
    let visitor = new implicit_instantiation_visitor ~tparams_map in
    let tparam_names =
      tparams
      |> List.fold_left (fun set tparam -> Subst_name.Set.add tparam.name set) Subst_name.Set.empty
    in
    let (marked_tparams, _) = visitor#type_ cx Positive (Marked.empty, tparam_names) return_t in
    check_instantiation cx ~tparams ~marked_tparams ~implicit_instantiation

  let check_react_fun cx ~tparams ~tparams_map ~params ~implicit_instantiation =
    match params with
    | [] ->
      let marked_tparams = Marked.empty in
      check_instantiation cx ~tparams ~marked_tparams ~implicit_instantiation
    | (_, props) :: _ ->
      (* The return of a React component when it is createElement-ed isn't actually the return type denoted on the
       * component. Instead, it is a React.Element<typeof Component>. In order to get the
       * polarities for the type parameters in the return, it is sufficient to look at the Props
       * type and use the polarities there.
       *
       * In practice, the props accessible via the element are read-only, so a possible future improvement
       * here would only look at the properties on the Props type with a covariant polarity instead of the
       * Neutral default that will be common due to syntactic conveniences. *)
      check_fun cx ~tparams ~tparams_map ~return_t:props ~implicit_instantiation

  let check_instance cx ~tparams ~implicit_instantiation =
    let marked_tparams =
      tparams
      |> List.fold_left
           (fun marked tparam ->
             match Marked.add tparam.name tparam.polarity marked with
             | None -> marked
             | Some (_, marked) -> marked)
           Marked.empty
    in
    check_instantiation cx ~tparams ~marked_tparams ~implicit_instantiation

  let implicitly_instantiate cx implicit_instantiation =
    let { Check.poly_t = (_, tparams, t); operation; _ } = implicit_instantiation in
    let tparams = Nel.to_list tparams in
    let tparams_map =
      List.fold_left (fun map x -> Subst_name.Map.add x.name x map) Subst_name.Map.empty tparams
    in
    let (inferred_targ_list, marked_tparams, tout) =
      match get_t cx t with
      | DefT (_, _, FunT (_, funtype)) ->
        (match operation with
        | (_, _, Check.Jsx _) ->
          check_react_fun cx ~tparams ~tparams_map ~params:funtype.params ~implicit_instantiation
        | _ -> check_fun cx ~tparams ~tparams_map ~return_t:funtype.return_t ~implicit_instantiation)
      | ThisClassT (_, DefT (_, _, InstanceT (_, _, _, _insttype)), _, _) ->
        (match operation with
        | (_, reason_op, Check.Call _) ->
          (* This case is hit when calling a static function. We will implicitly
           * instantiate the type variables on the class, but using an instance's
           * type params in a static method does not make sense. We ignore this case
           * intentionally *)
          ([], Marked.empty, Tvar.mk cx reason_op)
        | (_, _, _) -> check_instance cx ~tparams ~implicit_instantiation)
      | _ ->
        (* There are no other valid cases of implicit instantiation, but it is still possible
           reach this case via non-sensical cases that usually are downstream of some other error.
           Since there's no reasonable thing to do in these cases we just ignore it. *)
        let (_, reason_op, _) = operation in
        ([], Marked.empty, AnyT.error reason_op)
    in
    (inferred_targ_list, marked_tparams, tparams_map, tout)

  let solve_targs cx ?return_hint check =
    Context.run_with_fresh_constrain_cache cx (fun () ->
        let init_errors = Context.errors cx in
        let (inferred_targ_list, marked_tparams, tparams_map, tout) =
          implicitly_instantiate cx check
        in
        let errors_before_using_return_hint = Context.errors cx in
        let has_new_errors =
          not @@ Flow_error.ErrorSet.equal init_errors errors_before_using_return_hint
        in
        Base.Option.iter return_hint ~f:(fun hint -> Flow.flow_t cx (tout, hint));
        Context.reset_errors cx Flow_error.ErrorSet.empty;
        let output =
          pin_types
            cx
            ~has_new_errors
            ~has_return_hint:(Base.Option.is_some return_hint)
            inferred_targ_list
            marked_tparams
            tparams_map
            check
        in
        let implicit_instantiation_errors =
          Context.errors cx
          |> Flow_error.ErrorSet.filter (fun error ->
                 match Flow_error.msg_of_error error with
                 | Error_message.EImplicitInstantiationUnderconstrainedError _
                 | Error_message.EImplicitInstantiationTemporaryError _ ->
                   true
                 | _ -> false
             )
        in
        (* Since we will be performing the same check again using the solution
         * of the implicit instantiation, we only need to keep errors related
         * to pinning types, eg. [underconstrained-implicit-instantiation]. *)
        Context.reset_errors cx (Flow_error.ErrorSet.union init_errors implicit_instantiation_errors);
        output
    )

  let run cx check ~on_completion =
    let subst_map = solve_targs cx check in
    on_completion cx subst_map

  let fold ~implicit_instantiation_cx ~cx ~f ~init ~post implicit_instantiation_checks =
    let r =
      Base.List.fold_left
        ~f:(fun acc check ->
          let pinned = solve_targs implicit_instantiation_cx check in
          f implicit_instantiation_cx acc check pinned)
        ~init
        implicit_instantiation_checks
    in
    post ~cx ~implicit_instantiation_cx;
    r
end

module PinTypes (Flow : Flow_common.S) = struct
  module Observer : OBSERVER with type output = Type.t = struct
    type output = Type.t

    let on_constant_tparam_missing_bounds _cx _name _tparam =
      failwith "Constant tparam is unsupported."

    let on_pinned_tparam _cx _name _tparam inferred = inferred

    let on_missing_bounds cx _name _tparam ~tparam_binder_reason ~instantiation_reason:_ =
      Tvar.mk cx tparam_binder_reason

    let on_upper_non_t cx _name _u _tparam ~tparam_binder_reason ~instantiation_reason:_ =
      Tvar.mk cx tparam_binder_reason
  end

  module M : S with type output = Type.t with module Flow = Flow = Make (Observer) (Flow)

  let pin_type cx reason t =
    let name = Subst_name.Name "" in
    let polarity = Polarity.Neutral in
    let tparam =
      {
        reason;
        name;
        bound = MixedT.why reason |> with_trust bogus_trust;
        polarity;
        default = None;
        is_this = false;
      }
    in
    M.pin_type cx name tparam (Some polarity) ~default_bound:None reason t
end

type inferred_targ = {
  tparam: Type.typeparam;
  inferred: Type.t;
}

module Observer : OBSERVER with type output = inferred_targ = struct
  type output = inferred_targ

  let any_error = AnyT.why (AnyError None)

  let on_constant_tparam_missing_bounds _cx _name tparam =
    let inferred =
      match tparam.default with
      | None -> tparam.Type.bound
      | Some t -> t
    in
    { tparam; inferred }

  let on_pinned_tparam _cx _name tparam inferred = { tparam; inferred }

  let on_missing_bounds cx name tparam ~tparam_binder_reason ~instantiation_reason =
    match tparam.default with
    | Some inferred -> { tparam; inferred }
    | None ->
      if Context.in_synthesis_mode cx then
        { tparam; inferred = Tvar.mk_placeholder cx tparam_binder_reason }
      else (
        Flow_js_utils.add_output
          cx
          (Error_message.EImplicitInstantiationUnderconstrainedError
             {
               bound = Subst_name.string_of_subst_name name;
               reason_call = instantiation_reason;
               reason_l = tparam_binder_reason;
             }
          );
        { tparam; inferred = any_error tparam_binder_reason }
      )

  let on_upper_non_t cx name u tparam ~tparam_binder_reason ~instantiation_reason:_ =
    if Context.in_synthesis_mode cx then
      { tparam; inferred = Tvar.mk_placeholder cx tparam_binder_reason }
    else
      let msg =
        Subst_name.string_of_subst_name name
        ^ " contains a non-Type.t upper bound "
        ^ Type.string_of_use_ctor u
        ^ Type.(
            match u with
            | UseT (_, TypeDestructorTriggerT (_, _, _, d, _)) ->
              " " ^ Debug_js.string_of_destructor d
            | _ -> ""
          )
      in
      Flow_js_utils.add_output
        cx
        (Error_message.EImplicitInstantiationTemporaryError
           (Reason.aloc_of_reason tparam_binder_reason, msg)
        );
      { tparam; inferred = any_error tparam_binder_reason }
end

module Pierce : functor (Flow : Flow_common.S) ->
  S with type output = inferred_targ with module Flow = Flow =
  Make (Observer)

module type KIT = sig
  module Flow : Flow_common.S

  module Instantiation_helper : Flow_js_utils.Instantiation_helper_sig

  val run :
    Context.t ->
    Implicit_instantiation_check.t ->
    return_hint:Type.lazy_hint_t ->
    ?cache:Reason.t list ->
    trace ->
    use_op:use_op ->
    reason_op:reason ->
    reason_tapp:reason ->
    Type.t
end

module Kit (FlowJs : Flow_common.S) (Instantiation_helper : Flow_js_utils.Instantiation_helper_sig) :
  KIT = struct
  module Flow = FlowJs
  module Instantiation_helper = Instantiation_helper
  module Pierce = Pierce (Flow)
  open Instantiation_helper

  let instantiate_poly_with_subst_map
      cx ?cache trace poly_t inferred_targ_map ~use_op ~reason_op ~reason_tapp =
    let inferred_targ_map =
      Subst_name.Map.map
        (fun { tparam; inferred } ->
          {
            inferred =
              cache_instantiate cx trace ~use_op ?cache tparam reason_op reason_tapp inferred;
            tparam;
          })
        inferred_targ_map
    in
    let subst_map = Subst_name.Map.map (fun { inferred; _ } -> inferred) inferred_targ_map in
    inferred_targ_map
    |> Subst_name.Map.iter (fun _ { inferred; tparam } ->
           let frame = Frame (TypeParamBound { name = tparam.name }, use_op) in
           is_subtype
             cx
             trace
             ~use_op:frame
             (inferred, Subst.subst cx ~use_op subst_map tparam.bound)
       );
    reposition cx ~trace (aloc_of_reason reason_tapp) (Subst.subst cx ~use_op subst_map poly_t)

  let run_pierce cx check ?cache trace ~use_op ~reason_op ~reason_tapp ~return_hint =
    let (_, _, t) = check.Implicit_instantiation_check.poly_t in
    let targs_map = Pierce.solve_targs cx ?return_hint check in
    instantiate_poly_with_subst_map cx ?cache trace t targs_map ~use_op ~reason_op ~reason_tapp

  let run_instantiate_poly cx check ?cache trace ~use_op ~reason_op ~reason_tapp =
    let {
      Implicit_instantiation_check.poly_t = (_, xs, _) as poly_t;
      operation = (_, _, operation);
      _;
    } =
      check
    in
    match operation with
    | Implicit_instantiation_check.Call { Type.call_targs = Some targs; _ }
    | Implicit_instantiation_check.Constructor (Some targs, _)
    | Implicit_instantiation_check.Jsx { targs = Some targs; _ } ->
      let (_, ts) =
        Nel.fold_left
          (fun (targs, ts) typeparam ->
            match targs with
            | [] -> ([], ts)
            | ExplicitArg t :: targs -> (targs, t :: ts)
            | ImplicitArg (r, id) :: targs ->
              (* `_` can introduce non-termination, just like omitting type arguments
               * can. In order to protect against that non-termination we use cache_instantiate.
               * Instead of letting instantiate_poly do that for us on every type argument, we
               * do it ourselves here so that explicit type arguments do not have their reasons
               * needlessly changed. Note that the ImplicitTypeParam reason that cache instatiations
               * introduce can also change the use_op in a flow. In the NumT ~> StrT case,
               * this can make meaningful differences in type checking behavior. Ensuring that
               * the use_op/reason change happens _only_ on actually implicitly instantiated
               * type variables helps preserve the correct type checking behavior. *)
              let reason = mk_reason RImplicitInstantiation (aloc_of_reason r) in
              let t =
                Instantiation_utils.ImplicitTypeArgument.mk_targ cx typeparam reason reason_tapp
              in
              let t_ =
                cache_instantiate cx trace ~use_op ?cache typeparam reason_op reason_tapp t
              in
              Flow.flow cx (t_, UseT (use_op, OpenT (r, id)));
              (targs, t_ :: ts))
          (targs, [])
          xs
      in
      FlowJs.instantiate_poly_with_targs
        cx
        trace
        ~use_op
        ~reason_op
        ~reason_tapp
        ?cache:None
        poly_t
        (List.rev ts)
    | _ -> FlowJs.instantiate_poly cx trace ~use_op ~reason_op ~reason_tapp ?cache poly_t

  let run
      cx check ~return_hint:(has_context, lazy_hint) ?cache trace ~use_op ~reason_op ~reason_tapp =
    if not has_context then Context.add_possibly_speculating_implicit_instantiation_check cx check;
    if Context.lti cx then
      let return_hint = lazy_hint reason_op in
      Context.run_in_implicit_instantiation_mode cx (fun () ->
          run_pierce cx ~return_hint check ?cache trace ~use_op ~reason_op ~reason_tapp
      )
    else
      run_instantiate_poly cx check ?cache trace ~use_op ~reason_op ~reason_tapp
end
