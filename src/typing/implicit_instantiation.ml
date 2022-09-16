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

  val on_constant_tparam : Context.t -> Subst_name.t -> Type.typeparam -> Type.t -> output

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
          | t ->
            failwith
            @@ "Encountered a "
            ^ string_of_ctor t
            ^ " in typeapp case of fully constrained analysis"
    end

  type use_t_result =
    | UpperEmpty
    | UpperNonT of Type.use_t
    | UpperT of Type.t

  let rec t_of_use_t cx tvar u =
    match u with
    | UseT (_, (OpenT (_, id) as t)) ->
      (match Context.find_graph cx id with
      | Constraint.FullyResolved (_, (lazy t))
      | Constraint.Resolved (_, t) ->
        UpperT t
      | Constraint.Unresolved bounds ->
        let lowers = bounds.Constraint.lower in
        if TypeMap.cardinal lowers = 0 then
          UpperEmpty
        else
          UpperT (get_t cx t))
    | UseT (_, t) -> UpperT t
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
    | _ -> UpperNonT u

  let merge_upper_bounds cx upper_r tvar =
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

  let merge_lower_bounds cx t =
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

  let check_instantiation cx ~tparams ~marked_tparams ~return_hint ~implicit_instantiation =
    let { Check.lhs; operation = (use_op, reason_op, op); _ } = implicit_instantiation in
    let (call_targs, inferred_targ_list) =
      List.fold_right
        (fun tparam (targs, inferred_targ_and_bound_list) ->
          let reason_tapp = TypeUtil.reason_of_t lhs in
          let targ =
            Instantiation_utils.ImplicitTypeArgument.mk_targ cx tparam reason_op reason_tapp
          in
          ( ExplicitArg targ :: targs,
            (tparam.name, targ, tparam.bound) :: inferred_targ_and_bound_list
          ))
        tparams
        ([], [])
    in
    let () =
      match op with
      | Check.Call calltype ->
        let new_tout = Tvar.mk_no_wrap cx reason_op in
        let call_t =
          CallT
            {
              use_op;
              reason = reason_op;
              funcalltype =
                { calltype with call_targs = Some call_targs; call_tout = (reason_op, new_tout) };
              has_context = false;
            }
        in
        Flow.flow cx (lhs, call_t);
        Base.Option.iter return_hint ~f:(fun hint ->
            Flow.flow_t cx (OpenT (reason_op, new_tout), hint)
        )
      | Check.Constructor call_args ->
        let new_tout = Tvar.mk cx reason_op in
        let constructor_t =
          ConstructorT
            {
              use_op;
              reason = reason_op;
              targs = Some call_targs;
              args = call_args;
              tout = new_tout;
              has_context = false;
            }
        in
        Flow.flow cx (lhs, constructor_t);
        Base.Option.iter return_hint ~f:(fun hint -> Flow.flow_t cx (new_tout, hint))
      | Check.Jsx { clone; component; config; children } ->
        let new_tout = Tvar.mk cx reason_op in
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
                  has_context = false;
                }
            )
        in
        Flow.flow cx (lhs, react_kit_t);
        Base.Option.iter return_hint ~f:(fun hint -> Flow.flow_t cx (new_tout, hint))
    in
    (inferred_targ_list, marked_tparams)

  let pin_types cx inferred_targ_list marked_tparams tparams_map implicit_instantiation =
    let { Check.operation = (_, instantiation_reason, _); _ } = implicit_instantiation in
    let use_upper_bounds cx name tvar tparam_binder_reason instantiation_reason =
      let tparam = Subst_name.Map.find name tparams_map in
      let upper_t = merge_upper_bounds cx tparam_binder_reason tvar in
      match upper_t with
      | UpperEmpty ->
        Observer.on_missing_bounds cx name tparam ~tparam_binder_reason ~instantiation_reason
      | UpperNonT u ->
        Observer.on_upper_non_t cx name ~tparam_binder_reason ~instantiation_reason u tparam
      | UpperT inferred -> Observer.on_pinned_tparam cx name tparam inferred
    in
    let subst_map =
      List.fold_left
        (fun acc (name, t, _) -> Subst_name.Map.add name t acc)
        Subst_name.Map.empty
        inferred_targ_list
    in
    List.fold_right
      (fun (name, t, bound) acc ->
        let tparam_binder_reason = TypeUtil.reason_of_t t in
        let tparam = Subst_name.Map.find name tparams_map in
        let result =
          match Marked.get name marked_tparams with
          | None ->
            let t = merge_lower_bounds cx t in
            (match t with
            | None -> Observer.on_constant_tparam_missing_bounds cx name tparam
            | Some inferred -> Observer.on_constant_tparam cx name tparam inferred)
          | Some Neutral ->
            (* TODO(jmbrown): The neutral case should also unify upper/lower bounds. In order
             * to avoid cluttering the output we are actually interested in from this module,
             * I'm not going to start doing that until we need error diff information for
             * switching to Pierce's algorithm for implicit instantiation *)
            let lower_t = merge_lower_bounds cx t in
            (match lower_t with
            | None -> use_upper_bounds cx name t tparam_binder_reason instantiation_reason
            | Some inferred -> Observer.on_pinned_tparam cx name tparam inferred)
          | Some Positive ->
            (match merge_lower_bounds cx t with
            | None -> use_upper_bounds cx name t tparam_binder_reason instantiation_reason
            | Some inferred -> Observer.on_pinned_tparam cx name tparam inferred)
          | Some Negative -> use_upper_bounds cx name t tparam_binder_reason instantiation_reason
        in
        let bound_t = Subst.subst cx ~use_op:unknown_use subst_map bound in
        Flow.flow_t cx (t, bound_t);
        Subst_name.Map.add name result acc)
      inferred_targ_list
      Subst_name.Map.empty

  let check_fun cx ~tparams ~tparams_map ~return_t ~return_hint ~implicit_instantiation =
    (* Visit the return type *)
    let visitor = new implicit_instantiation_visitor ~tparams_map in
    let tparam_names =
      tparams
      |> List.fold_left (fun set tparam -> Subst_name.Set.add tparam.name set) Subst_name.Set.empty
    in
    let (marked_tparams, _) = visitor#type_ cx Positive (Marked.empty, tparam_names) return_t in
    check_instantiation cx ~tparams ~marked_tparams ~return_hint ~implicit_instantiation

  let check_react_fun cx ~tparams ~tparams_map ~params ~return_hint ~implicit_instantiation =
    match params with
    | [] ->
      let marked_tparams = Marked.empty in
      check_instantiation cx ~tparams ~marked_tparams ~return_hint ~implicit_instantiation
    | (_, props) :: _ ->
      (* The return of a React component when it is createElement-ed isn't actually the return type denoted on the
       * component. Instead, it is a React.Element<typeof Component>. In order to get the
       * polarities for the type parameters in the return, it is sufficient to look at the Props
       * type and use the polarities there.
       *
       * In practice, the props accessible via the element are read-only, so a possible future improvement
       * here would only look at the properties on the Props type with a covariant polarity instead of the
       * Neutral default that will be common due to syntactic conveniences. *)
      check_fun cx ~tparams ~tparams_map ~return_t:props ~return_hint ~implicit_instantiation

  let check_instance cx ~tparams ~return_hint ~implicit_instantiation =
    let marked_tparams =
      tparams
      |> List.fold_left
           (fun marked tparam ->
             match Marked.add tparam.name tparam.polarity marked with
             | None -> marked
             | Some (_, marked) -> marked)
           Marked.empty
    in
    check_instantiation cx ~tparams ~marked_tparams ~return_hint ~implicit_instantiation

  let implicitly_instantiate cx return_hint implicit_instantiation =
    let { Check.poly_t = (_, tparams, t); operation; _ } = implicit_instantiation in
    let tparams = Nel.to_list tparams in
    let tparams_map =
      List.fold_left (fun map x -> Subst_name.Map.add x.name x map) Subst_name.Map.empty tparams
    in
    let (inferred_targ_list, marked_tparams) =
      match get_t cx t with
      | DefT (_, _, FunT (_, funtype)) ->
        (match operation with
        | (_, _, Check.Jsx _) ->
          check_react_fun
            cx
            ~tparams
            ~tparams_map
            ~params:funtype.params
            ~return_hint
            ~implicit_instantiation
        | _ ->
          check_fun
            cx
            ~tparams
            ~tparams_map
            ~return_t:funtype.return_t
            ~return_hint
            ~implicit_instantiation)
      | ThisClassT (_, DefT (_, _, InstanceT (_, _, _, _insttype)), _, _) ->
        (match operation with
        | (_, _, Check.Call _) ->
          (* This case is hit when calling a static function. We will implicitly
           * instantiate the type variables on the class, but using an instance's
           * type params in a static method does not make sense. We ignore this case
           * intentionally *)
          ([], Marked.empty)
        | (_, _, _) -> check_instance cx ~tparams ~return_hint ~implicit_instantiation)
      | _ -> failwith "No other possible lower bounds"
    in
    (inferred_targ_list, marked_tparams, tparams_map)

  let solve_targs cx ?return_hint check =
    let (inferred_targ_list, marked_tparams, tparams_map) =
      implicitly_instantiate cx return_hint check
    in
    pin_types cx inferred_targ_list marked_tparams tparams_map check

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

type inferred_targ = {
  tparam: Type.typeparam;
  inferred: Type.t;
}

module SynthesisObserver : OBSERVER with type output = inferred_targ = struct
  type output = inferred_targ

  let on_constant_tparam _cx _name tparam inferred = { tparam; inferred }

  let on_constant_tparam_missing_bounds _cx _name tparam =
    let inferred =
      match tparam.default with
      | None -> tparam.Type.bound
      | Some t -> t
    in
    { tparam; inferred }

  let on_pinned_tparam _cx _name tparam inferred = { tparam; inferred }

  let on_missing_bounds cx _name tparam ~tparam_binder_reason ~instantiation_reason:_ =
    { tparam; inferred = Tvar.mk cx tparam_binder_reason }

  let on_upper_non_t cx _name _u tparam ~tparam_binder_reason ~instantiation_reason:_ =
    { tparam; inferred = Tvar.mk cx tparam_binder_reason }
end

module Synthesis : functor (Flow : Flow_common.S) ->
  S with type output = inferred_targ with module Flow = Flow =
  Make (SynthesisObserver)

module CheckObserver : OBSERVER with type output = inferred_targ = struct
  type output = inferred_targ

  let any_error = AnyT.why (AnyError None)

  let on_constant_tparam _cx _name tparam inferred = { tparam; inferred }

  let on_constant_tparam_missing_bounds _cx _name tparam =
    let inferred =
      match tparam.default with
      | None -> tparam.Type.bound
      | Some t -> t
    in
    { tparam; inferred }

  let on_pinned_tparam _cx _name tparam inferred = { tparam; inferred }

  let on_missing_bounds cx name tparam ~tparam_binder_reason ~instantiation_reason =
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

  let on_upper_non_t cx name u tparam ~tparam_binder_reason ~instantiation_reason:_ =
    let msg =
      Subst_name.string_of_subst_name name
      ^ " contains a non-Type.t upper bound "
      ^ Type.string_of_use_ctor u
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
  Make (CheckObserver)

module type KIT = sig
  module Flow : Flow_common.S

  module Instantiation_helper : Flow_js_utils.Instantiation_helper_sig

  val run :
    Context.t ->
    Implicit_instantiation_check.t ->
    has_context:bool ->
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

  let run_pierce cx check ?cache trace ~use_op ~reason_op ~reason_tapp =
    let (_, _, t) = check.Implicit_instantiation_check.poly_t in
    let targs_map = Pierce.solve_targs cx check in
    instantiate_poly_with_subst_map cx ?cache trace t targs_map ~use_op ~reason_op ~reason_tapp

  let run_instantiate_poly cx check ?cache trace ~use_op ~reason_op ~reason_tapp =
    let poly_t = check.Implicit_instantiation_check.poly_t in
    FlowJs.instantiate_poly cx trace ~use_op ~reason_op ~reason_tapp ?cache poly_t

  let run cx check ~has_context =
    if not has_context then Context.add_possibly_speculating_implicit_instantiation_check cx check;
    (* The current Pierce's algorithm running inside flow_js does not have access to the return
       hint type, which will cause a lot of unactionable unconstrained implicit instantiation
       errors. We disable the check when we have return context for now, but we should re-enable
       it when we can use the return hint more effectively. *)
    match (Context.env_mode cx, has_context) with
    | (Options.LTI, false) -> run_pierce cx check
    | _ -> run_instantiate_poly cx check
end
