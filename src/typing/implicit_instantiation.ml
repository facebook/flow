(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Reason
open Polarity
open Utils_js
module TypeParamMarked = Marked.Make (Subst_name)
module Marked = TypeParamMarked
module Check = Implicit_instantiation_check

let rec union_flatten_list ts = Base.List.concat_map ts ~f:union_flatten

and union_flatten = function
  | UnionT (_, rep) -> union_flatten_list (UnionRep.members rep)
  | t -> [t]

(* Makes sure the solution does not "leak" a potentially overly precise type to
 * the output of a generic call, when this precision is not necessary for checking
 * the call. Condider for example
 *
 *   declare function useState<S>(init: S): [S, (S)=>void];
 *   const [st, setSt] = useState(42);
 *
 * We would like the type of `st` and `setSt` to use the general form of the type
 * for the initial state `42`, i.e. number (resp. `(number)=>void`), instead of
 * the rather impractical `42` (resp. `(42)=>void`).
 *)
let generalize_singletons cx ~call_loc ~has_syntactic_hint t =
  let needs_precise =
    has_syntactic_hint
    (* Targets the case of `obj[foo('a')]` and `obj[foo('a')] = e` *)
    || Context.get_enclosing_context_for_call cx call_loc = Some Enclosing_context.IndexContext
  in
  if needs_precise then
    t
  else
    let singleton_action loc =
      if Context.is_primitive_literal_checked cx loc then
        (* Keep singleton types that are checked against other precise types, which
         * is recorded by calling `Flow_js_utils.update_lit_type_from_annot`. *)
        Primitive_literal.KeepAsIs
      else
        Primitive_literal.DoNotKeep
    in
    Primitive_literal.convert_implicit_instantiation_literal_type cx ~singleton_action t

module Inferred_targ = struct
  type t = {
    tparam: Type.typeparam;
    inferred: Type.t;
  }

  let to_type { inferred = t; _ } = t
end

module Generalized_targ = struct
  type t = {
    tparam: Type.typeparam;
    inferred: Type.t;
    generalized: Type.t;
  }

  let to_type { generalized = t; _ } = t
end

module type OBSERVER = sig
  val on_pinned_tparam : Context.t -> Type.typeparam -> Type.t -> Inferred_targ.t

  val on_constant_tparam_missing_bounds : Context.t -> Type.typeparam -> Inferred_targ.t

  val on_missing_bounds :
    Context.t ->
    use_op:Type.use_op ->
    Type.typeparam ->
    tparam_binder_reason:Reason.reason ->
    instantiation_reason:Reason.reason ->
    Inferred_targ.t

  val on_upper_non_t :
    Context.t ->
    use_op:Type.use_op ->
    Type.use_t ->
    Type.typeparam ->
    tparam_binder_reason:Reason.reason ->
    instantiation_reason:Reason.reason ->
    Inferred_targ.t
end

module type S = sig
  module Flow : Flow_common.S

  val pin_type :
    Context.t ->
    use_op:Type.use_op ->
    Type.typeparam ->
    Polarity.t option ->
    default_bound:Type.t option ->
    Reason.reason ->
    Type.t ->
    Inferred_targ.t

  val solve_targs :
    Context.t ->
    use_op:Type.use_op ->
    ?allow_underconstrained:bool ->
    ?has_syntactic_hint:bool ->
    ?return_hint:Type.t * Hint.hint_kind ->
    Check.t ->
    Generalized_targ.t Subst_name.Map.t * (Type.t * Subst_name.Name.t) list

  val solve_conditional_type_targs :
    Context.t ->
    Type.DepthTrace.t ->
    use_op:Type.use_op ->
    reason:Reason.reason ->
    tparams:Type.typeparam list ->
    check_t:Type.t ->
    extends_t:Type.t ->
    true_t:Type.t ->
    Type.t Subst_name.Map.t option

  val fold :
    implicit_instantiation_cx:Context.t ->
    cx:Context.t ->
    f:(Context.t -> 'acc -> Check.t -> Generalized_targ.t Subst_name.Map.t -> 'acc) ->
    init:'acc ->
    post:(cx:Context.t -> implicit_instantiation_cx:Context.t -> unit) ->
    Check.t list ->
    'acc
end

let get_t cx =
  let no_lowers _cx r = Type.Unsoundness.merged_any r in
  function
  | OpenT (r, id) -> Flow_js_utils.merge_tvar ~no_lowers cx r id
  | t -> t

(* Sorting of the upper bounds is mostly done for compatibility with the version
 * before EvalTypeDestructorT was a use_t. *)
let sort_upper_bounds_for_merging xs =
  let (use_t, rest_t) =
    Base.List.partition_tf
      ~f:(function
        | (UseT _, _)
        | (EvalTypeDestructorT _, _) ->
          true
        | _ -> false)
      xs
  in
  use_t @ rest_t

module Make (Observer : OBSERVER) (Flow : Flow_common.S) : S = struct
  module Flow = Flow
  module SpeculationKit = Speculation_kit.Make (Flow)

  let speculative_subtyping_succeeds cx trace use_op l u =
    match SpeculationKit.try_singleton_throw_on_failure cx trace l (UseT (use_op, u)) with
    | exception Flow_js_utils.SpeculationSingletonError -> false
    | _ -> true

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
        | DefT (_, PolyT { tparams; t_out = t; _ }) ->
          let tparam_names' =
            Nel.fold_left (fun names x -> Subst_name.Set.remove x.name names) tparam_names tparams
          in
          let (marked, _) = self#type_ cx pole (marked, tparam_names') t in
          (* TODO(jmbrown): Handle defaults on type parameters *)
          (marked, tparam_names)
        | TypeAppT { reason = _; use_op = _; type_; targs; from_value = _; use_desc = _ } ->
          self#typeapp targs cx pole acc type_
        (* ThisTypeAppT is created from a new expression, which cannot
         * be used as an annotation, so we do not special case it like
         * we do with TypeAppT
         *)
        | t -> super#type_ cx pole acc t

      method private typeapp =
        let rec loop cx pole seen = function
          (* Any arity errors are already handled in Flow_js *)
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
          | DefT (_, PolyT { tparams; _ }) -> loop cx pole acc (Some (Nel.to_list tparams), targs)
          | DefT (_, EmptyT)
          | AnyT _ ->
            loop cx pole acc (None, targs)
          (* All other cases are errors *)
          | _ -> loop cx pole acc (None, targs)
    end

  type use_t_result =
    | UpperEmpty
    | UpperNonT of Type.use_t
    | UpperT of Type.t

  let rec t_of_use_t cx seen tvar u =
    let use_t_result_of_t_option = function
      | Some t -> UpperT t
      | None -> UpperEmpty
    in
    let merge_lower_or_upper_bounds r t =
      match merge_lower_bounds cx t with
      | Some t -> UpperT t
      | None -> merge_upper_bounds cx seen r t
    in
    let bind_use_t_result ~f = function
      | UpperEmpty -> UpperEmpty
      | UpperNonT u -> UpperNonT u
      | UpperT t -> f t
    in
    match u with
    | UseT (_, (OpenT (r, _) as t)) -> merge_upper_bounds cx seen r t
    | EvalTypeDestructorT { reason = r; destructor; tout; _ } ->
      (match destructor with
      | PropertyType _
      | ElementType _
      | OptionalIndexedAccessNonMaybeType _
      | OptionalIndexedAccessResultType _
      | ValuesType
      | ConditionalType _
      | TypeMap _
      | MappedType _ (* TODO: Mapped Type reversals *) ->
        UpperEmpty
      | EnumType ->
        merge_lower_or_upper_bounds r (OpenT tout)
        |> bind_use_t_result ~f:(fun tout ->
               Tvar.mk_no_wrap_where cx r (fun t' ->
                   Flow.flow
                     cx
                     ( tout,
                       GetEnumT
                         {
                           use_op = unknown_use;
                           reason = r;
                           orig_t = None;
                           kind = `GetEnumValue;
                           tout = OpenT t';
                         }
                     )
               )
               |> merge_lower_bounds cx
               |> use_t_result_of_t_option
           )
      | ReactElementPropsType
      | ReactElementConfigType ->
        merge_lower_or_upper_bounds r (OpenT tout)
        |> bind_use_t_result ~f:(fun config ->
               let react_node =
                 Flow.get_builtin_react_type
                   cx
                   r
                   Flow_intermediate_error_types.ReactModuleForReactNodeType
               in
               UpperT
                 (DefT
                    ( r,
                      ReactAbstractComponentT
                        {
                          config;
                          instance = ComponentInstanceAvailableAsRefSetterProp (EmptyT.why r);
                          renders = react_node;
                          component_kind = Structural;
                        }
                    )
                 )
           )
      | RestType (_, t_rest) ->
        merge_lower_or_upper_bounds r (OpenT tout)
        |> bind_use_t_result ~f:(fun tout ->
               reverse_obj_kit_rest cx r t_rest tout
               |> merge_lower_bounds cx
               |> use_t_result_of_t_option
           )
      | NonMaybeType ->
        merge_lower_or_upper_bounds r (OpenT tout)
        |> bind_use_t_result ~f:(fun t -> UpperT (MaybeT (r, t)))
      | ExactType
      | ReadOnlyType
      | ReactDRO _
      | MakeHooklike
      | PartialType
      | RequiredType ->
        merge_lower_or_upper_bounds r (OpenT tout)
      | ReactCheckComponentConfig pmap ->
        merge_lower_or_upper_bounds r (OpenT tout)
        |> bind_use_t_result ~f:(fun t ->
               reverse_component_check_config cx r pmap t
               |> merge_lower_bounds cx
               |> use_t_result_of_t_option
           )
      | SpreadType (_, todo_rev, head_slice) ->
        let acc_elements =
          Base.Option.value_map ~f:(fun x -> [Object.Spread.InlineSlice x]) ~default:[] head_slice
        in
        merge_lower_or_upper_bounds r (OpenT tout)
        |> bind_use_t_result ~f:(fun t ->
               reverse_obj_spread cx r todo_rev acc_elements t
               |> merge_lower_bounds cx
               |> use_t_result_of_t_option
           )
      | SpreadTupleType { reason_tuple = _; inexact; reason_spread; resolved_rev; unresolved } ->
        let is_spread = function
          | ResolvedArg _ -> false
          | ResolvedSpreadArg _
          | ResolvedAnySpreadArg _ ->
            true
        in
        (* Reverse if the spread is the last item in the tuple, there are
           no other spreads, and the tuple type is not inexact. *)
        (match (unresolved, Base.List.exists ~f:is_spread resolved_rev) with
        | ([], false) when not inexact ->
          let n = List.length resolved_rev in
          merge_lower_or_upper_bounds r (OpenT tout)
          |> bind_use_t_result ~f:(fun t ->
                 Tvar.mk_where cx reason_spread (fun t' ->
                     Flow.flow cx (t, ArrRestT (unknown_use, reason_spread, n, t'))
                 )
                 |> merge_lower_bounds cx
                 |> use_t_result_of_t_option
             )
        | _ -> UpperNonT u))
    | UseT (_, t) -> UpperT t
    | ArrRestT (_, _, i, tout) ->
      (match get_t cx tout with
      | DefT (r, ArrT (ArrayAT { tuple_view = None; _ } | ROArrayAT _)) ->
        identity_reverse_upper_bound cx seen tvar r tout
      | DefT (r, ArrT (ArrayAT { tuple_view = Some _; _ } | TupleAT _)) when i = 0 ->
        identity_reverse_upper_bound cx seen tvar r tout
      | _ -> UpperEmpty)
    (* Call related upper bounds are ignored because there is not enough info to reverse. *)
    | BindT _
    | CallT _
    | ConditionalT _
    | MethodT _
    | PrivateMethodT _
    | ConstructorT _
    | ToStringT _
    | MapTypeT _
    (* Get/set-prop related upper bounds are ignored because there is not enough info to reverse. *)
    | SetPropT _
    | SetPrivatePropT _
    | GetElemT _
    | SetElemT _
    | CallElemT _
    | GetTypeFromNamespaceT _
    | GetPropT _
    | GetPrivatePropT _
    | TestPropT _
    | GetStaticsT _
    | GetProtoT _
    | SetProtoT _
    | ObjTestProtoT _
    | HasOwnPropT _
    | LookupT _
    | DestructuringT _
    | OptionalChainT _
    | OptionalIndexedAccessT _
    | GetKeysT _
    | GetValuesT _
    | GetDictValuesT _
    (* Class/interface related upper bounds won't occur during implicit instantiation *)
    | SuperT _
    | ImplementsT _
    | MixinT _
    | ExtendsUseT _
    (* The following upper bounds won't occur during implicit instantiation,
       because they are operations on values. *)
    | ObjTestT _
    | TypeCastT _
    | EnumCastT _
    | EnumExhaustiveCheckT _
    | GetEnumT _
    | CondT _
    | CheckUnusedPromiseT _
    | WriteComputedObjPropCheckT _
    | CheckReactImmutableT _
    | ConvertEmptyPropsToMixedT _
    | ValueToTypeReferenceT _
    | ExitRendersT _ ->
      UpperEmpty (* Remaining unhandled upper bounds *)
    | SpecializeT _
    | ThisSpecializeT _
    | ConcretizeTypeAppsT _
    | ObjRestT _
    | ElemT _
    | ReactKitT _
    | ConcretizeT _
    | FilterOptionalT _
    | FilterMaybeT _
    | ExtractReactRefT _
    | SealGenericT _ ->
      UpperNonT u
    | DeepReadOnlyT (((r, _) as tout), _) -> identity_reverse_upper_bound cx seen tvar r (OpenT tout)
    | HooklikeT ((r, _) as tout) -> identity_reverse_upper_bound cx seen tvar r (OpenT tout)
    | ReposLowerT { use_t; _ } -> t_of_use_t cx seen tvar use_t
    | ReposUseT (_, _, _use_op, t) ->
      Flow.flow_t cx (t, tvar);
      UpperT t
    | ResolveSpreadT (_, reason, { rrt_resolved; rrt_unresolved = []; rrt_resolve_to }) ->
      (match rrt_resolve_to with
      | ResolveSpreadsToMultiflowSubtypeFull (_, { params; rest_param; _ }) ->
        (match
           reverse_resolve_spread_multiflow_subtype_full_partial_resolution
             cx
             tvar
             reason
             rrt_resolved
             params
             rest_param
         with
        | None -> UpperNonT u
        | Some solution -> UpperT solution)
      | ResolveSpreadsToTupleType _
      | ResolveSpreadsToArrayLiteral _
      | ResolveSpreadsToArray _
      | ResolveSpreadsToMultiflowCallFull _
      | ResolveSpreadsToMultiflowPartial _ ->
        UpperNonT u)
    | ResolveSpreadT _ -> UpperNonT u
    | ResolveUnionT { reason = _; unresolved = _; resolved = _; upper = u; id = _ } ->
      t_of_use_t cx seen tvar u
    | ObjKitT (_, r, _, tool, tout) ->
      (match tool with
      | Object.MakeExact
      | Object.ReadOnly
      | Object.Partial
      | Object.Required
      | Object.ObjectRep
      | Object.ReactConfig { ref_manipulation = Object.ReactConfig.FilterRef; _ }
      | Object.ReactConfig { ref_manipulation = Object.ReactConfig.KeepRef; _ } ->
        identity_reverse_upper_bound cx seen tvar r tout
      | Object.ReactConfig { ref_manipulation = Object.ReactConfig.AddRef ref_t; _ } ->
        let solution = merge_upper_bounds cx seen r tout in
        (match solution with
        | UpperEmpty -> UpperEmpty
        | UpperNonT u -> UpperNonT u
        | UpperT t ->
          let pmap =
            NameUtils.Map.singleton
              (OrdinaryName "ref")
              (Field
                 {
                   preferred_def_locs = None;
                   key_loc = None;
                   type_ = ref_t;
                   polarity = Polarity.Neutral;
                 }
              )
          in
          (match reverse_component_check_config cx r pmap t |> merge_lower_bounds cx with
          | None -> UpperEmpty
          | Some reversed ->
            Flow.flow_t cx (reversed, tvar);
            UpperT reversed))
      | Object.ReactCheckComponentConfig pmap ->
        let solution = merge_upper_bounds cx seen r tout in
        (match solution with
        | UpperEmpty -> UpperEmpty
        | UpperNonT u -> UpperNonT u
        | UpperT t ->
          (match reverse_component_check_config cx r pmap t |> merge_lower_bounds cx with
          | None -> UpperEmpty
          | Some reversed ->
            Flow.flow_t cx (reversed, tvar);
            UpperT reversed))
      | Object.ObjectMap _ -> UpperEmpty (* TODO: reverse mapped types *)
      | Object.Spread (_, { Object.Spread.todo_rev; acc; _ }) ->
        let solution = merge_upper_bounds cx seen r tout in
        (match solution with
        | UpperEmpty -> UpperEmpty
        | UpperNonT u -> UpperNonT u
        | UpperT t ->
          (match reverse_obj_spread cx r todo_rev acc t |> merge_lower_bounds cx with
          | None -> UpperEmpty
          | Some reversed ->
            Flow.flow_t cx (reversed, tvar);
            UpperT reversed))
      | Object.Rest (_, Object.Rest.One t_rest) ->
        merge_upper_bounds cx seen r tout
        |> bind_use_t_result ~f:(fun t ->
               match reverse_obj_kit_rest cx r t_rest t |> merge_lower_bounds cx with
               | None -> UpperEmpty
               | Some reversed ->
                 Flow.flow_t cx (reversed, tvar);
                 UpperT reversed
           )
      | Object.Rest (_, Object.Rest.Done _) -> UpperNonT u)

  and identity_reverse_upper_bound cx seen tvar r tout =
    let solution = merge_upper_bounds cx seen r tout in
    (match solution with
    | UpperT t -> Flow.flow_t cx (t, tvar)
    | _ -> ());
    solution

  and reverse_obj_spread cx r todo_rev acc_elements tout =
    let inline_slice_to_t { Object.Spread.prop_map; dict; reachable_targs; _ } =
      Obj_type.mk_with_proto
        cx
        r
        ~obj_kind:(Obj_type.obj_kind_from_optional_dict ~dict ~otherwise:Exact)
        ~props:prop_map
        ~reachable_targs
        (ObjProtoT r)
    in
    let slice_to_t (s : Object.slice) =
      Obj_type.mk_with_proto
        cx
        r
        ~obj_kind:s.Object.flags.obj_kind
        ~props:
          (NameUtils.Map.map
             (fun { Object.prop_t; is_own = _; is_method = _; polarity = _; key_loc } ->
               Field
                 { preferred_def_locs = None; key_loc; type_ = prop_t; polarity = Polarity.Neutral })
             s.Object.props
          )
        ~reachable_targs:s.Object.reachable_targs
        (ObjProtoT r)
    in
    let operand_to_t = function
      | Object.Spread.Slice s -> inline_slice_to_t s
      | Object.Spread.Type t -> t
    in
    let acc_element_to_ts = function
      | Object.Spread.InlineSlice s -> [inline_slice_to_t s]
      | Object.Spread.ResolvedSlice slices -> slices |> Nel.to_list |> Base.List.map ~f:slice_to_t
    in
    let rest_type l rest =
      let open Object in
      Tvar.mk_where cx r (fun tout ->
          let u =
            ObjKitT (unknown_use, r, Resolve Next, Rest (Rest.SpreadReversal, Rest.One rest), tout)
          in
          Flow.flow cx (l, u)
      )
    in
    let tout = Tvar.mk_where cx r (fun t' -> Flow.flow_t cx (tout, t')) in
    let tout =
      Base.List.fold acc_elements ~init:tout ~f:(fun l e ->
          Base.List.fold (acc_element_to_ts e) ~init:l ~f:rest_type
      )
    in
    Base.List.fold todo_rev ~init:tout ~f:(fun l o -> rest_type l (operand_to_t o))

  and reverse_component_check_config cx reason pmap tout =
    if NameUtils.Map.is_empty pmap then
      tout
    else
      Tvar.mk_where cx reason (fun t' ->
          let rest =
            Obj_type.mk_with_proto cx reason ~props:pmap ~obj_kind:Type.Exact (ObjProtoT reason)
          in
          let u =
            let open Object in
            ObjKitT
              (unknown_use, reason, Resolve Next, Rest (Rest.SpreadReversal, Rest.One rest), t')
          in
          Flow.flow cx (tout, u)
      )

  and reverse_obj_kit_rest cx reason t_rest tout =
    Tvar.mk_no_wrap_where cx reason (fun t' ->
        let u =
          Object.(
            Object.Spread.(
              let tool = Resolve Next in
              let options = Value { make_seal = Obj_type.mk_seal ~as_const:false ~frozen:false } in
              let state =
                {
                  todo_rev = [Type t_rest];
                  acc = [];
                  spread_id = Reason.mk_id ();
                  union_reason = None;
                  curr_resolve_idx = 0;
                }
              in
              ObjKitT (unknown_use, reason, tool, Spread (options, state), OpenT t')
            )
          )
        in
        Flow.flow cx (tout, u)
    )

  and reverse_resolve_spread_multiflow_subtype_full_no_resolution cx tvar reason params rest_param =
    let (tuple_elements_rev, tuple_ts) =
      Base.List.fold params ~init:([], []) ~f:(fun (els, ts) (name, t) ->
          let el = TypeUtil.mk_tuple_element ?name (TypeUtil.reason_of_t t) t in
          (el :: els, t :: ts)
      )
    in
    let (reason, arr_type) =
      match rest_param with
      | None ->
        let elem_t =
          match tuple_ts with
          | [] -> EmptyT.why reason
          | [t] -> t
          | t0 :: t1 :: ts ->
            UnionT (reason, UnionRep.make ~kind:UnionRep.ImplicitInstiationKind t0 t1 ts)
        in
        let len = Base.List.length tuple_ts in
        let t =
          TupleAT
            {
              elem_t;
              elements = Base.List.rev tuple_elements_rev;
              arity = (len, len);
              inexact = false;
              react_dro = None;
            }
        in
        let reason = update_desc_reason (fun _ -> RTupleType) reason in
        (reason, t)
      | Some (_, _, rest_param_t) ->
        let rest_elem_t =
          Tvar.mk_no_wrap_where cx reason (fun tout ->
              Flow.flow
                cx
                ( rest_param_t,
                  GetElemT
                    {
                      use_op = unknown_use;
                      reason;
                      id = None;
                      from_annot = true;
                      skip_optional = false;
                      access_iterables = false;
                      key_t = NumModuleT.make reason;
                      tout;
                    }
                )
          )
        in
        let elem_t =
          match tuple_ts with
          | [] -> rest_elem_t
          | t :: ts -> UnionT (reason, UnionRep.make rest_elem_t t ts)
        in
        let t = ArrayAT { elem_t; tuple_view = None; react_dro = None } in
        let reason = update_desc_reason (fun _ -> RArray) reason in
        (reason, t)
    in
    let solution = DefT (reason, ArrT arr_type) in
    Flow.flow_t cx (solution, tvar);
    solution

  and reverse_resolve_spread_multiflow_subtype_full_partial_resolution
      cx tvar reason resolved params rest_param =
    (* We remove resolved params one by one from the start.
     * When we run out of params but we still have resolved_params,
     * we record the number of rest_param we need to remove,
     * and later perform an ArrayRest to remove them. *)
    let rec loop = function
      | ([], params) -> Some (params, 0)
      | (_resolved :: resolved_rest, _params :: params_rest) -> loop (resolved_rest, params_rest)
      | (resolved, []) ->
        (match rest_param with
        | None -> None
        | Some _ -> Some ([], List.length resolved))
    in
    match loop (resolved, params) with
    | None -> None
    | Some (params, _rest_index) ->
      Some
        (reverse_resolve_spread_multiflow_subtype_full_no_resolution
           cx
           tvar
           reason
           params
           rest_param
        )

  and merge_upper_bounds cx seen upper_r tvar =
    let filter_placeholder t =
      if Flow_js_utils.TvarVisitors.has_placeholders cx t then
        UpperEmpty
      else
        UpperT t
    in
    let equal = Concrete_type_eq.eq cx in
    match tvar with
    | OpenT (_, id) ->
      if ISet.mem id seen then
        UpperEmpty
      else
        let constraints = Context.find_graph cx id in
        (match constraints with
        | Constraint.FullyResolved s -> filter_placeholder (Context.force_fully_resolved_tvar cx s)
        | Constraint.Resolved t -> filter_placeholder t
        | Constraint.Unresolved bounds ->
          let uppers = Constraint.UseTypeMap.keys bounds.Constraint.upper in
          uppers
          |> sort_upper_bounds_for_merging
          |> List.fold_left
               (fun acc (t, _) ->
                 match (acc, t_of_use_t cx (ISet.add id seen) tvar t) with
                 | (UpperNonT u, _) -> UpperNonT u
                 | (_, UpperNonT u) -> UpperNonT u
                 | (UpperEmpty, UpperT t) -> filter_placeholder t
                 | (UpperT _, UpperT t) when Flow_js_utils.TvarVisitors.has_placeholders cx t -> acc
                 | (UpperT t', UpperT t) ->
                   (match (t', t) with
                   | (IntersectionT (_, rep1), IntersectionT (_, rep2)) ->
                     UpperT
                       (IntersectionT
                          ( upper_r,
                            InterRep.append
                              ~kind:InterRep.ImplicitInstiationKind
                              (InterRep.members rep2)
                              rep1
                          )
                       )
                   | (_, IntersectionT (_, rep)) ->
                     if Base.List.mem (InterRep.members rep) t' ~equal then
                       UpperT t
                     else
                       UpperT
                         (IntersectionT
                            (upper_r, InterRep.append ~kind:InterRep.ImplicitInstiationKind [t'] rep)
                         )
                   | (IntersectionT (_, rep), _) ->
                     if Base.List.mem (InterRep.members rep) t ~equal then
                       UpperT t'
                     else
                       UpperT
                         (IntersectionT
                            (upper_r, InterRep.append ~kind:InterRep.ImplicitInstiationKind [t] rep)
                         )
                   | (t', t) ->
                     if equal t' t then
                       UpperT t
                     else if
                       TypeUtil.quick_subtype t t'
                       || speculative_subtyping_succeeds cx DepthTrace.dummy_trace unknown_use t t'
                     then
                       UpperT t
                     else if
                       TypeUtil.quick_subtype t' t
                       || speculative_subtyping_succeeds cx DepthTrace.dummy_trace unknown_use t' t
                     then
                       UpperT t'
                     else
                       UpperT t')
                 | (UpperT _, UpperEmpty) -> acc
                 | (UpperEmpty, UpperEmpty) -> acc)
               UpperEmpty)
    | t -> UpperT t

  and merge_lower_bounds cx t =
    (* When the input tvar has a ReposUseT upper bound it means that we might be
     * discounting lower bounds that are just waiting to be added as soon as the
     * ReposUseT fires. Here, we make sure we record the result of the ReposUseT
     * before we make a decision based on lower bounds. *)
    let t =
      match t with
      | OpenT (_r, id) ->
        let constraints = Context.find_graph cx id in
        begin
          match constraints with
          | Constraint.Unresolved bounds ->
            let upper = Constraint.UseTypeMap.keys bounds.Constraint.upper in
            Base.List.iter upper ~f:(function
                | (ReposUseT (_, _, _, l), _) -> Flow.flow_t cx (l, t)
                | _ -> ()
                );
            t
          | _ -> t
        end
      | t -> t
    in
    match t with
    | OpenT (r, id) ->
      let constraints = Context.find_graph cx id in
      (match constraints with
      | Constraint.FullyResolved s ->
        let t = Context.force_fully_resolved_tvar cx s in
        if Flow_js_utils.TvarVisitors.has_placeholders cx t then
          None
        else
          Some t
      | Constraint.Resolved t ->
        if Flow_js_utils.TvarVisitors.has_placeholders cx t then
          None
        else
          Some t
      | Constraint.Unresolved bounds ->
        let lowers = bounds.Constraint.lower in
        if TypeMap.cardinal lowers = 0 then
          None
        else
          TypeMap.keys lowers
          |> Flow_js_utils.collect_lowers cx (ISet.singleton id) [] ~filter_empty:false
          |> union_flatten_list
          |> Base.List.filter ~f:(fun t -> not @@ Flow_js_utils.TvarVisitors.has_placeholders cx t)
          |> TypeUtil.union_of_ts_opt ~kind:UnionRep.ImplicitInstiationKind r)
    | t -> Some t

  let on_missing_bounds cx ~use_op tparam ~default_bound ~tparam_binder_reason ~instantiation_reason
      =
    match default_bound with
    | Some t -> Observer.on_pinned_tparam cx tparam t
    | None ->
      Observer.on_missing_bounds ~use_op cx tparam ~tparam_binder_reason ~instantiation_reason

  let use_upper_bounds
      cx
      ~use_op
      tparam
      tvar
      ~default_bound
      ?(on_upper_empty = on_missing_bounds ~use_op ~default_bound)
      tparam_binder_reason
      instantiation_reason =
    let upper_t = merge_upper_bounds cx ISet.empty tparam_binder_reason tvar in
    match upper_t with
    | UpperEmpty -> on_upper_empty cx tparam ~tparam_binder_reason ~instantiation_reason
    | UpperNonT u ->
      Observer.on_upper_non_t cx ~use_op ~tparam_binder_reason ~instantiation_reason u tparam
    | UpperT inferred -> Observer.on_pinned_tparam cx tparam inferred

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
        let arity_loc = tparams_loc in
        let minimum_arity = Flow_js_utils.poly_minimum_arity (Nel.of_list_exn tparams) in
        if List.length explicit_targs > maximum_arity then
          Flow_js_utils.add_output
            cx
            (Error_message.ETooManyTypeArgs { reason_tapp; arity_loc; maximum_arity });
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
                (Error_message.ETooFewTypeArgs { reason_tapp; arity_loc; minimum_arity });
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
              let reason = mk_reason RImplicitInstantiation (loc_of_reason r) in
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
    let (inferred_targ_list, lower_t, use_t, tout) =
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
        (inferred_targ_list, lhs, call_t, Some (OpenT (reason_op, new_tout)))
      | Check.SubtypeLowerPoly u ->
        let (_, inferred_targ_list) = merge_targs None in
        let targs = Base.List.map ~f:(fun (_, t, _, _) -> t) inferred_targ_list in
        ( inferred_targ_list,
          Flow.mk_typeapp_instance_annot
            cx
            ~use_op
            ~reason_op
            ~reason_tapp
            ~from_value:false
            lhs
            targs,
          UseT (use_op, u),
          None
        )
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
              specialized_ctor = None;
            }
        in
        (inferred_targ_list, lhs, constructor_t, Some new_tout)
      | Check.ReactJSX { component; jsx_props; targs } ->
        let new_tout = Tvar.mk cx reason_op in
        let (_, inferred_targ_list) = merge_targs targs in
        let react_kit_t =
          ReactKitT
            ( use_op,
              reason_op,
              React.CreateElement
                {
                  component;
                  jsx_props;
                  targs = None;
                  tout = new_tout;
                  return_hint = Type.hint_unavailable;
                  record_monomorphized_result = false;
                  inferred_targs = None;
                  specialized_component = None;
                }
            )
        in
        let lower =
          Flow.mk_typeapp_instance_annot
            cx
            ~use_op
            ~reason_op
            ~reason_tapp
            ~from_value:true
            lhs
            (Base.List.map ~f:(fun (_, t, _, _) -> t) inferred_targ_list)
        in
        (inferred_targ_list, lower, react_kit_t, Some new_tout)
    in
    Flow.flow cx (lower_t, use_t);
    (inferred_targ_list, marked_tparams, tout)

  let pin_type cx ~use_op tparam polarity ~default_bound instantiation_reason t =
    let tparam_binder_reason = TypeUtil.reason_of_t t in
    let pin_tparam inferred = Observer.on_pinned_tparam cx tparam inferred in
    match polarity with
    | None ->
      (match merge_lower_bounds cx t with
      | None ->
        let on_upper_empty cx tparam ~tparam_binder_reason:_ ~instantiation_reason:_ =
          Observer.on_constant_tparam_missing_bounds cx tparam
        in
        use_upper_bounds
          cx
          ~use_op
          tparam
          t
          ~default_bound
          ~on_upper_empty
          tparam_binder_reason
          instantiation_reason
      | Some inferred -> Observer.on_pinned_tparam cx tparam inferred)
    | Some Neutral ->
      (* TODO(jmbrown): The neutral case should also unify upper/lower bounds. In order
       * to avoid cluttering the output we are actually interested in from this module,
       * I'm not going to start doing that until we need error diff information for
       * switching to Pierce's algorithm for implicit instantiation *)
      let lower_t = merge_lower_bounds cx t in
      (match lower_t with
      | None ->
        use_upper_bounds
          cx
          ~use_op
          tparam
          t
          ~default_bound
          tparam_binder_reason
          instantiation_reason
      | Some inferred -> pin_tparam inferred)
    | Some Positive ->
      (match merge_lower_bounds cx t with
      | None ->
        use_upper_bounds
          cx
          ~use_op
          tparam
          t
          ~default_bound
          tparam_binder_reason
          instantiation_reason
      | Some inferred -> pin_tparam inferred)
    | Some Negative ->
      let on_upper_empty cx tparam ~tparam_binder_reason ~instantiation_reason =
        match merge_lower_bounds cx t with
        | None ->
          on_missing_bounds
            cx
            ~use_op
            tparam
            ~default_bound
            ~tparam_binder_reason
            ~instantiation_reason
        | Some inferred -> Observer.on_pinned_tparam cx tparam inferred
      in
      use_upper_bounds
        cx
        ~use_op
        tparam
        t
        ~default_bound
        ~on_upper_empty
        tparam_binder_reason
        instantiation_reason

  let pin_types
      cx
      ~use_op
      ~has_new_errors
      ~allow_underconstrained
      ~has_syntactic_hint
      inferred_targ_list
      marked_tparams
      tparams_map
      implicit_instantiation =
    let { Check.operation = (_, instantiation_reason, op); _ } = implicit_instantiation in
    let subst_map =
      List.fold_left
        (fun acc (name, t, _, _) -> Subst_name.Map.add name t acc)
        Subst_name.Map.empty
        inferred_targ_list
    in
    List.fold_right
      (fun (name, t, bound, is_inferred) (acc, inferred_targ_list_acc) ->
        let tparam = Subst_name.Map.find name tparams_map in
        let polarity =
          if allow_underconstrained then
            None
          else
            Marked.get name marked_tparams
        in
        let { Inferred_targ.tparam; inferred } =
          if is_inferred then
            let default_bound =
              if has_new_errors then
                match Context.typing_mode cx with
                | Context.CheckingMode -> Some (AnyT.error (TypeUtil.reason_of_t t))
                | Context.SynthesisMode _
                | Context.HintEvaluationMode ->
                  Some (AnyT.placeholder (TypeUtil.reason_of_t t))
              else
                None
            in
            pin_type cx ~use_op tparam polarity ~default_bound instantiation_reason t
          else
            Observer.on_pinned_tparam cx tparam t
        in
        let bound_t = Type_subst.subst cx ~use_op:unknown_use subst_map bound in
        Flow.flow_t cx (t, bound_t);
        let call_loc = loc_of_reason instantiation_reason in
        let generalized =
          match op with
          | Check.Call _
          | Check.Constructor _
            when tparam.is_const ->
            (* Adjust 'const' type parameters. Keeping container reasons for
             * compatibility with existing code. *)
            Primitive_literal.convert_literal_type_to_const ~loc_range:call_loc cx inferred
          | _ ->
            (* Prevent leaking of precise singleton types *)
            generalize_singletons cx ~call_loc ~has_syntactic_hint inferred
        in
        let result = { Generalized_targ.tparam; inferred; generalized } in
        (Subst_name.Map.add name result acc, (generalized, name) :: inferred_targ_list_acc))
      inferred_targ_list
      (Subst_name.Map.empty, [])

  let check_fun cx ~tparams ~tparams_map ~return_t ~implicit_instantiation =
    (* Visit the return type *)
    let visitor = new implicit_instantiation_visitor ~tparams_map in
    let tparam_names =
      tparams
      |> List.fold_left (fun set tparam -> Subst_name.Set.add tparam.name set) Subst_name.Set.empty
    in
    let (marked_tparams, _) = visitor#type_ cx Positive (Marked.empty, tparam_names) return_t in
    check_instantiation cx ~tparams ~marked_tparams ~implicit_instantiation

  let check_react_fun cx ~tparams ~tparams_map ~props ~implicit_instantiation =
    match props with
    | None ->
      let marked_tparams = Marked.empty in
      check_instantiation cx ~tparams ~marked_tparams ~implicit_instantiation
    | Some props ->
      (* The return of a React component when it is createElement-ed isn't actually the return type denoted on the
       * component. Instead, it is a React$Element<typeof Component>. In order to get the
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
      let (_, _, op_kind) = operation in
      match (get_t cx t, op_kind) with
      | (_, Check.SubtypeLowerPoly _) ->
        let marked_tparams = Marked.empty in
        check_instantiation cx ~tparams ~marked_tparams ~implicit_instantiation
      | (DefT (_, ReactAbstractComponentT { config; _ }), _) ->
        check_react_fun cx ~tparams ~tparams_map ~props:(Some config) ~implicit_instantiation
      | (DefT (_, FunT (_, funtype)), Check.ReactJSX _) ->
        let props =
          match funtype.params with
          | (_, props) :: _ -> Some props
          | [] -> None
        in
        check_react_fun cx ~tparams ~tparams_map ~props ~implicit_instantiation
      | (DefT (_, FunT (_, funtype)), _) ->
        check_fun cx ~tparams ~tparams_map ~return_t:funtype.return_t ~implicit_instantiation
      | (DefT (_, ClassT (ThisInstanceT _)), Check.Call _) ->
        (* This case is hit when calling a static function. We will implicitly
         * instantiate the type variables on the class, but using an instance's
         * type params in a static method does not make sense. We ignore this case
         * intentionally *)
        ([], Marked.empty, None)
      | (DefT (_, ClassT (ThisInstanceT _)), _) ->
        check_instance cx ~tparams ~implicit_instantiation
      | _ ->
        (* There are no other valid cases of implicit instantiation, but it is still possible
           reach this case via non-sensical cases that usually are downstream of some other error.
           Since there's no reasonable thing to do in these cases we just ignore it. *)
        ([], Marked.empty, None)
    in
    (inferred_targ_list, marked_tparams, tparams_map, tout)

  let solve_targs
      cx ~use_op ?(allow_underconstrained = false) ?(has_syntactic_hint = false) ?return_hint check
      =
    Context.run_and_rolled_back_cache cx (fun () ->
        let init_errors = Context.errors cx in
        let cache_snapshot = Context.take_cache_snapshot cx in
        let (inferred_targ_list, marked_tparams, tparams_map, tout) =
          implicitly_instantiate cx check
        in
        let errors_before_using_return_hint = Context.errors cx in
        let has_new_errors = init_errors != errors_before_using_return_hint in
        let (inferred_targ_list, marked_tparams, tparams_map, has_new_errors) =
          match (return_hint, tout) with
          | (_, None)
          | (None, _) ->
            (inferred_targ_list, marked_tparams, tparams_map, has_new_errors)
          | (Some (hint, kind), Some tout) ->
            (* Protect the effect of return hint constraining against speculative exns *)
            let speculative_exn =
              match Flow.flow_t cx (tout, hint) with
              | exception (Flow_js_utils.SpeculativeError _ as e) -> Some (Exception.wrap e)
              | () -> None
            in
            let errors_after_using_return_hint = Context.errors cx in
            let return_hint_has_errors =
              errors_before_using_return_hint != errors_after_using_return_hint
            in
            if
              (Base.Option.is_some speculative_exn || return_hint_has_errors)
              && kind = Hint.BestEffortHint
            then (
              (* Restore state *)
              Context.restore_cache_snapshot cx cache_snapshot;
              Context.reset_errors cx init_errors;
              (* Re-run the implicit instantiation *)
              let (inferred_targ_list, marked_tparams, tparams_map, _tout) =
                implicitly_instantiate cx check
              in
              let has_new_errors = init_errors != Context.errors cx in
              (inferred_targ_list, marked_tparams, tparams_map, has_new_errors)
            ) else (
              (* We're keeping the results with the current hint, but if there was
               * an exception that we caught, we need to rethrow it. *)
              Base.Option.iter speculative_exn ~f:Exception.reraise;
              (inferred_targ_list, marked_tparams, tparams_map, has_new_errors)
            )
        in
        Context.reset_errors cx Flow_error.ErrorSet.empty;
        Exception.protect
          ~f:(fun () ->
            pin_types
              cx
              ~use_op
              ~has_new_errors
              ~allow_underconstrained
              ~has_syntactic_hint
              inferred_targ_list
              marked_tparams
              tparams_map
              check)
          ~finally:(fun () ->
            let implicit_instantiation_errors =
              Context.errors cx
              |> Flow_error.ErrorSet.filter (fun error ->
                     match Flow_error.msg_of_error error with
                     | Error_message.EImplicitInstantiationUnderconstrainedError _
                     | Error_message.EInternal _ ->
                       true
                     | _ -> false
                 )
            in
            (* Since we will be performing the same check again using the solution
             * of the implicit instantiation, we only need to keep errors related
             * to pinning types, eg. [underconstrained-implicit-instantiation]. *)
            Context.reset_errors
              cx
              (Flow_error.ErrorSet.union init_errors implicit_instantiation_errors))
    )

  let solve_conditional_type_targs cx trace ~use_op ~reason ~tparams ~check_t ~extends_t ~true_t =
    let (subst_map, inferred_targ_list) =
      Base.List.fold
        tparams
        ~init:(Subst_name.Map.empty, [])
        ~f:(fun (subst_map, inferred_targ_and_bound_list) tparam ->
          let targ = Instantiation_utils.ImplicitTypeArgument.mk_targ cx tparam reason reason in
          ( Subst_name.Map.add tparam.name targ subst_map,
            (tparam.name, targ, tparam.bound) :: inferred_targ_and_bound_list
          )
      )
    in
    if
      speculative_subtyping_succeeds
        cx
        trace
        use_op
        check_t
        (Type_subst.subst cx ~use_op:unknown_use subst_map extends_t)
    then
      let (tparams_map, tparams_set) =
        Base.List.fold
          tparams
          ~init:(Subst_name.Map.empty, Subst_name.Set.empty)
          ~f:(fun (tparams_map, tparams_set) tparam ->
            ( Subst_name.Map.add tparam.name tparam tparams_map,
              Subst_name.Set.add tparam.name tparams_set
            )
        )
      in
      let (marked_tparams, _) =
        let visitor = new implicit_instantiation_visitor ~tparams_map in
        visitor#type_ cx Polarity.Positive (Marked.empty, tparams_set) true_t
      in
      Base.List.fold_until
        inferred_targ_list
        ~init:Subst_name.Map.empty
        ~finish:(fun r -> Some r)
        ~f:(fun map (name, targ, bound) ->
          let tparam = Subst_name.Map.find name tparams_map in
          let polarity = Marked.get name marked_tparams in
          let { Inferred_targ.inferred; _ } =
            pin_type cx ~use_op tparam polarity ~default_bound:(Some bound) reason targ
          in
          if speculative_subtyping_succeeds cx trace unknown_use inferred bound then
            Base.Continue_or_stop.Continue (Subst_name.Map.add name inferred map)
          else
            Base.Continue_or_stop.Stop None)
    else
      None

  let fold ~implicit_instantiation_cx ~cx ~f ~init ~post implicit_instantiation_checks =
    let r =
      Base.List.fold_left
        ~f:(fun acc check ->
          let { Implicit_instantiation_check.operation = (use_op, _, _); _ } = check in
          let (pinned, _) = solve_targs ~use_op implicit_instantiation_cx check in
          f implicit_instantiation_cx acc check pinned)
        ~init
        implicit_instantiation_checks
    in
    post ~cx ~implicit_instantiation_cx;
    r
end

module PinTypes (Flow : Flow_common.S) = struct
  module Observer : OBSERVER = struct
    let on_constant_tparam_missing_bounds cx tparam =
      Flow_js_utils.add_output
        cx
        Error_message.(
          EInternal
            ( loc_of_reason tparam.Type.reason,
              ImplicitInstantiationInvariant "Constant tparam is unsupported."
            )
        );
      { Inferred_targ.tparam; inferred = Type.AnyT.error tparam.Type.reason }

    let on_pinned_tparam _cx tparam inferred = { Inferred_targ.tparam; inferred }

    let on_missing_bounds cx ~use_op:_ tparam ~tparam_binder_reason ~instantiation_reason:_ =
      { Inferred_targ.tparam; inferred = Tvar.mk cx tparam_binder_reason }

    let on_upper_non_t cx ~use_op:_ _u tparam ~tparam_binder_reason ~instantiation_reason:_ =
      { Inferred_targ.tparam; inferred = Tvar.mk cx tparam_binder_reason }
  end

  module M : S with module Flow = Flow = Make (Observer) (Flow)

  let pin_type cx ~use_op reason t =
    let polarity = Polarity.Neutral in
    let tparam =
      {
        reason;
        name = Subst_name.Name "";
        bound = MixedT.why reason;
        polarity;
        default = None;
        is_this = false;
        is_const = false;
      }
    in
    let { Inferred_targ.inferred; _ } =
      M.pin_type cx ~use_op tparam (Some polarity) ~default_bound:None reason t
    in
    inferred
end

module Observer : OBSERVER = struct
  let any_error = AnyT.why (AnyError None)

  let mod_inferred_bound =
    TypeUtil.mod_reason_of_t (Reason.update_desc_reason (fun desc -> RTypeParamBound desc))

  let mod_inferred_default =
    TypeUtil.mod_reason_of_t (Reason.update_desc_reason (fun desc -> RTypeParamDefault desc))

  let on_constant_tparam_missing_bounds cx tparam =
    let inferred =
      match tparam.default with
      | None -> mod_inferred_bound tparam.Type.bound
      | Some t -> mod_inferred_default t
    in
    Debug_js.Verbose.print_if_verbose_lazy
      cx
      ( lazy
        [
          spf
            "Constant type parameter %s is pinned to %s"
            (Subst_name.string_of_subst_name tparam.name)
            (Debug_js.dump_t cx ~depth:3 inferred);
        ]
        );
    { Inferred_targ.tparam; inferred }

  let on_pinned_tparam cx tparam inferred =
    Debug_js.Verbose.print_if_verbose_lazy
      cx
      ( lazy
        [
          spf
            "Type parameter %s is pinned to %s"
            (Subst_name.string_of_subst_name tparam.name)
            (Debug_js.dump_t cx ~depth:3 inferred);
        ]
        );
    { Inferred_targ.tparam; inferred }

  let on_missing_bounds cx ~use_op tparam ~tparam_binder_reason ~instantiation_reason =
    match tparam.default with
    | Some inferred -> { Inferred_targ.tparam; inferred = mod_inferred_default inferred }
    | None ->
      if Context.typing_mode cx <> Context.CheckingMode then
        { Inferred_targ.tparam; inferred = Context.mk_placeholder cx tparam_binder_reason }
      else (
        Flow_js_utils.add_output
          cx
          (Error_message.EImplicitInstantiationUnderconstrainedError
             {
               bound = Subst_name.string_of_subst_name tparam.name;
               reason_call = instantiation_reason;
               reason_tparam = tparam_binder_reason;
               use_op;
             }
          );
        { Inferred_targ.tparam; inferred = any_error tparam_binder_reason }
      )

  let on_upper_non_t cx ~use_op u tparam ~tparam_binder_reason ~instantiation_reason =
    if Context.typing_mode cx <> Context.CheckingMode then
      { Inferred_targ.tparam; inferred = Context.mk_placeholder cx tparam_binder_reason }
    else (
      Debug_js.Verbose.print_if_verbose_lazy
        cx
        (lazy ["Underconstrained due to upper_non_t"; Type.string_of_use_ctor u]);
      Flow_js_utils.add_output
        cx
        (Error_message.EImplicitInstantiationUnderconstrainedError
           {
             bound = Subst_name.string_of_subst_name tparam.name;
             reason_call = instantiation_reason;
             reason_tparam = tparam_binder_reason;
             use_op;
           }
        );
      { Inferred_targ.tparam; inferred = any_error tparam_binder_reason }
    )
end

module Make_instantiation_solver : functor (Flow : Flow_common.S) -> S with module Flow = Flow =
  Make (Observer)

module type KIT = sig
  module Flow : Flow_common.S

  module Instantiation_helper : Flow_js_utils.Instantiation_helper_sig

  val run_call :
    Context.t ->
    Implicit_instantiation_check.t ->
    return_hint:Type.lazy_hint_t ->
    Type.DepthTrace.t ->
    use_op:use_op ->
    reason_op:reason ->
    reason_tapp:reason ->
    Type.t * (Type.t * Subst_name.Name.t) list

  val run_monomorphize :
    Context.t ->
    Type.DepthTrace.t ->
    use_op:Type.use_op ->
    reason_op:Reason.reason ->
    reason_tapp:Reason.reason ->
    Type.typeparam Nel.t ->
    Type.t ->
    Type.t

  val run_conditional :
    Context.t ->
    Type.DepthTrace.t ->
    use_op:Type.use_op ->
    reason:Reason.reason ->
    tparams:Type.typeparam list ->
    check_t:Type.t ->
    extends_t:Type.t ->
    true_t:Type.t ->
    false_t:Type.t ->
    Type.t

  val run_ref_extractor :
    Context.t -> use_op:Type.use_op -> reason:Reason.reason -> Type.t -> Type.t

  val run_render_extractor :
    Context.t -> use_op:Type.use_op -> reason:Reason.reason -> Type.t -> Type.t

  val run_await : Context.t -> use_op:Type.use_op -> reason:Reason.reason -> Type.t -> Type.t
end

module Kit (FlowJs : Flow_common.S) (Instantiation_helper : Flow_js_utils.Instantiation_helper_sig) :
  KIT = struct
  module Flow = FlowJs
  module Instantiation_helper = Instantiation_helper
  module Instantiation_solver = Make_instantiation_solver (Flow)
  module SpeculationKit = Speculation_kit.Make (Flow)
  open Instantiation_helper

  let instantiate_poly_with_subst_map cx trace poly_t subst_map ~use_op ~reason_op ~reason_tapp =
    let generalized_targ_map =
      Subst_name.Map.map
        (fun { Generalized_targ.tparam; inferred; generalized } ->
          (* Create an OpenT indirection of inferred result.
           * We specifically want the inferred type argument to have RTypeParam reason desc,
           * so that we can detect loops in type expansion.
           * See Instantiation_utils.ImplicitTypeArgument.abstract_targ.
           *
           * This OpenT indirection is also needed for performance purposes, since it prevents
           * unnecessary deep substitution traversals. *)
          let generalized' =
            Instantiation_utils.ImplicitTypeArgument.mk_targ cx tparam reason_op reason_tapp
          in
          FlowJs.rec_unify cx trace ~use_op ~unify_any:true generalized generalized';
          { Generalized_targ.tparam; inferred; generalized = generalized' })
        subst_map
    in
    let subst_map =
      Subst_name.Map.map
        (fun { Generalized_targ.generalized; _ } -> generalized)
        generalized_targ_map
    in
    generalized_targ_map
    |> Subst_name.Map.iter (fun _ { Generalized_targ.generalized; tparam; _ } ->
           let frame = Frame (TypeParamBound { name = tparam.name }, use_op) in
           is_subtype
             cx
             trace
             ~use_op:frame
             (generalized, Type_subst.subst cx ~use_op subst_map tparam.bound)
       );
    reposition cx ~trace (loc_of_reason reason_tapp) (Type_subst.subst cx ~use_op subst_map poly_t)

  let run_call cx check ~(return_hint : lazy_hint_t) trace ~use_op ~reason_op ~reason_tapp =
    let (_, lazy_hint) = return_hint in
    let (allow_underconstrained, return_hint) =
      match lazy_hint ~expected_only:false reason_op with
      | HintAvailable (t, kind) -> (true, Some (t, kind))
      | DecompositionError -> (true, None)
      | NoHint
      | EncounteredPlaceholder ->
        (false, None)
    in
    (* The `has_syntactic_hint` flag will inform the decision on whether to keep
     * precise literal types in the instantiation result. Here, we avoid using
     * Type_env.has_hint, as this function always returns `false` in non-checking
     * mode. *)
    let has_syntactic_hint =
      let { Loc_env.hint_map; _ } = Context.environment cx in
      Loc_collections.ALocMap.find_opt (Reason.loc_of_reason reason_op) hint_map
      |> Base.Option.value_map ~f:fst ~default:false
    in
    Context.run_in_implicit_instantiation_mode cx (fun () ->
        let (_, _, t) = check.Implicit_instantiation_check.poly_t in
        let (soln, inferred_targs) =
          Instantiation_solver.solve_targs
            cx
            ~use_op
            ~allow_underconstrained
            ~has_syntactic_hint
            ?return_hint
            check
        in
        let t = instantiate_poly_with_subst_map cx trace t soln ~use_op ~reason_op ~reason_tapp in
        (t, inferred_targs)
    )

  let run_ref_extractor cx ~use_op ~reason t =
    let lhs =
      Flow_js_utils.ImportExportUtils.get_implicitly_imported_react_type
        cx
        (loc_of_reason reason)
        ~singleton_concretize_type_for_imports_exports:
          FlowJs.singleton_concretize_type_for_imports_exports
        ~purpose:Flow_intermediate_error_types.ReactModuleForReactRefSetterType
    in
    match get_t cx lhs with
    | DefT (_, PolyT { tparams_loc; tparams = ({ name; _ }, []) as ids; t_out; _ }) ->
      let poly_t = (tparams_loc, ids, t_out) in
      let check =
        {
          Implicit_instantiation_check.lhs;
          poly_t;
          operation = (use_op, reason, Implicit_instantiation_check.SubtypeLowerPoly t);
        }
      in
      Context.run_in_implicit_instantiation_mode cx (fun () ->
          let (map, _) =
            Instantiation_solver.solve_targs cx ~use_op ~allow_underconstrained:false check
          in
          let { Generalized_targ.generalized; _ } = Subst_name.Map.find name map in
          generalized
      )
    | _ ->
      (* If the internal definition for React$RefSetter isn't polymorphic, either we're running with
         no-flowlib or things have gone majorly sideways. Either way, just use mixed *)
      MixedT.make reason

  let run_render_extractor cx ~use_op ~reason t =
    let name = Subst_name.Name "T" in
    let bound = MixedT.make reason in
    let generic_t =
      let id = Context.make_generic_id cx name (loc_of_reason reason) in
      GenericT { reason; name; bound; no_infer = false; id }
    in
    let tparam =
      {
        reason;
        name : Subst_name.t;
        bound;
        polarity = Polarity.Positive;
        default = None;
        is_this = false;
        is_const = false;
      }
    in
    match
      Context.run_in_implicit_instantiation_mode cx (fun () ->
          Instantiation_solver.solve_conditional_type_targs
            cx
            Type.DepthTrace.dummy_trace
            ~use_op
            ~reason
            ~tparams:[tparam]
            ~check_t:t
            ~extends_t:
              (DefT
                 ( reason,
                   ReactAbstractComponentT
                     {
                       config = EmptyT.why reason;
                       instance = ComponentInstanceAvailableAsRefSetterProp (EmptyT.why reason);
                       renders = generic_t;
                       component_kind = Structural;
                     }
                 )
              )
            ~true_t:generic_t
      )
    with
    | Some subst_map -> Type_subst.subst cx ~use_op:unknown_use subst_map generic_t
    | None -> EmptyT.why reason

  (* TODO: await should look up Promise in the environment instead of going directly to
     the core definition. Otherwise, the following won't work with a polyfilled Promise! **)
  (* If argument is a Promise<T>, then (await argument) returns T.
     otherwise it just returns the argument type.
     TODO update this comment when recursive unwrapping of Promise is done.
  *)
  let run_await cx ~use_op ~reason t =
    let name = Subst_name.Name "T" in
    let bound = MixedT.make reason in
    let generic_t =
      let id = Context.make_generic_id cx name (loc_of_reason reason) in
      GenericT { reason; name; bound; no_infer = false; id }
    in
    let tparam =
      {
        reason;
        name : Subst_name.t;
        bound;
        polarity = Polarity.Positive;
        default = None;
        is_this = false;
        is_const = false;
      }
    in
    match
      Context.run_in_implicit_instantiation_mode cx (fun () ->
          Instantiation_solver.solve_conditional_type_targs
            cx
            Type.DepthTrace.dummy_trace
            ~use_op
            ~reason
            ~tparams:[tparam]
            ~check_t:t
            ~extends_t:
              (let t = Flow_js_utils.lookup_builtin_type cx "Promise" reason in
               TypeUtil.typeapp ~from_value:false ~use_desc:false reason t [generic_t]
              )
            ~true_t:generic_t
      )
    with
    | Some subst_map ->
      Debug_js.Verbose.print_if_verbose cx ["We are awaiting a Promise."];
      Type_subst.subst cx ~use_op:unknown_use subst_map generic_t
    | None ->
      Debug_js.Verbose.print_if_verbose cx ["We are awaiting a non-Promise."];
      t

  let run_monomorphize cx trace ~use_op ~reason_op ~reason_tapp tparams t =
    let subst_map =
      Nel.fold_left
        (fun subst_map tparam ->
          let { Inferred_targ.tparam; inferred } =
            Instantiation_solver.pin_type
              cx
              ~use_op
              tparam
              None
              ~default_bound:None
              reason_op
              (Instantiation_utils.ImplicitTypeArgument.mk_targ cx tparam reason_op reason_tapp)
          in
          let targ = { Generalized_targ.tparam; inferred; generalized = inferred } in
          Subst_name.Map.add tparam.name targ subst_map)
        Subst_name.Map.empty
        tparams
    in
    instantiate_poly_with_subst_map cx trace t subst_map ~use_op ~reason_op ~reason_tapp

  let run_conditional cx trace ~use_op ~reason ~tparams ~check_t ~extends_t ~true_t ~false_t =
    if
      Flow_js_utils.TvarVisitors.has_placeholders cx check_t
      || Flow_js_utils.TvarVisitors.has_placeholders cx extends_t
      || Flow_js_utils.TvarVisitors.has_placeholders cx true_t
      || Flow_js_utils.TvarVisitors.has_placeholders cx false_t
    then (
      Debug_js.Verbose.print_if_verbose
        cx
        ["Conditional type refuses to evaluate because we have placeholders"];
      (* Placeholder in, placeholder out *)
      Context.mk_placeholder cx reason
    ) else if
        Context.in_implicit_instantiation cx
        && (Flow_js_utils.TvarVisitors.has_unresolved_tvars cx check_t
           || Flow_js_utils.TvarVisitors.has_unresolved_tvars cx extends_t
           || Flow_js_utils.TvarVisitors.has_unresolved_tvars cx true_t
           || Flow_js_utils.TvarVisitors.has_unresolved_tvars cx false_t
           )
      then
      (* When we are in nested instantiation, we can't meaningfully decide which branch to take,
         so we will give up and produce placeholder instead. *)
      Context.mk_placeholder cx reason
    else
      let t =
        match
          Context.run_in_implicit_instantiation_mode cx (fun () ->
              Instantiation_solver.solve_conditional_type_targs
                cx
                trace
                ~use_op
                ~reason
                ~tparams
                ~check_t
                ~extends_t
                ~true_t
          )
        with
        (* If the subtyping can succeed even when the GenericTs are still abstract, then it must
           succeed under every possible instantiation, so we can take the true branch. *)
        | Some subst_map ->
          Debug_js.Verbose.print_if_verbose cx ["Conditional type evaluates to the true branch."];
          Type_subst.subst cx ~use_op:unknown_use subst_map true_t
        | None ->
          let free_vars =
            Subst_name.Set.union
              (Type_subst.free_var_finder cx check_t)
              (Type_subst.free_var_finder
                 cx
                 ~bound:
                   (tparams
                   |> Base.List.map ~f:(fun tparam -> tparam.name)
                   |> Subst_name.Set.of_list
                   )
                 extends_t
              )
          in
          if Subst_name.Set.is_empty free_vars then (
            Debug_js.Verbose.print_if_verbose cx ["Conditional type evaluates to the false branch."];
            false_t
          ) else
            let any_subst_map =
              Subst_name.Set.fold
                (fun name acc -> Subst_name.Map.add name (AnyT.placeholder reason) acc)
                free_vars
                Subst_name.Map.empty
            in
            let any_subst_map =
              Base.List.fold tparams ~init:any_subst_map ~f:(fun acc tparam ->
                  Subst_name.Map.add tparam.name (AnyT.placeholder reason) acc
              )
            in
            let check_t = Type_subst.subst cx ~use_op:unknown_use any_subst_map check_t in
            let extends_t = Type_subst.subst cx ~use_op:unknown_use any_subst_map extends_t in
            (match
               SpeculationKit.try_singleton_throw_on_failure
                 cx
                 trace
                 check_t
                 (UseT (use_op, extends_t))
             with
            | exception Flow_js_utils.SpeculationSingletonError ->
              (* When all the GenericT and infer types are replaced with any, and subtyping
                 check still cannot succeed, then we can safely conclude that, in every possible
                 instantiation, we will always take the false branch *)
              Debug_js.Verbose.print_if_verbose
                cx
                [
                  "Conditional type evaluates to the false branch because we will always enter the false branch.";
                ];
              false_t
            | _ ->
              Debug_js.Verbose.print_if_verbose cx ["Conditional type is kept abstract."];
              (* A conditional type with GenericTs in check type and extends type is tricky.
                 We cannot conservatively decide which branch we will take. To maintain
                 soundness in this general case, we make the type abstract. *)
              let name =
                Subst_name.Synthetic
                  { name = "conditional type"; op_kind = Some Subst_name.Conditional; ts = [] }
              in
              let id = Context.make_generic_id cx name (loc_of_reason reason) in
              let reason = update_desc_reason invalidate_rtype_alias reason in
              let bound = UnionT (reason, UnionRep.make true_t false_t []) in
              GenericT { reason; name; id; bound; no_infer = false })
      in
      reposition cx ~trace (loc_of_reason reason) t
end
