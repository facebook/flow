(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module describes the subtyping algorithm that forms the core of
   typechecking. The algorithm (in its basic form) is described in Francois
   Pottier's thesis. The main data structures maintained by the algorithm are:
   (1) for every type variable, which type variables form its lower and upper
   bounds (i.e., flow in and out of the type variable); and (2) for every type
   variable, which concrete types form its lower and upper bounds. Every new
   subtyping constraint added to the system is deconstructed into its subparts,
   until basic flows between type variables and other type variables or concrete
   types remain; these flows are then viewed as links in a chain, bringing
   together further concrete types and type variables to participate in
   subtyping. This process continues till a fixpoint is reached---which itself
   is guaranteed to exist, and is usually reached in very few steps. *)

open Flow_js_utils
open Utils_js
open Reason
open Type
open TypeUtil
open Constraint
open Debug_js.Verbose
module FlowError = Flow_error
module IICheck = Implicit_instantiation_check

(**************************************************************)

(* Check that id1 is not linked to id2. *)
let not_linked (id1, _bounds1) (_id2, bounds2) =
  (* It suffices to check that id1 is not already in the lower bounds of
     id2. Equivalently, we could check that id2 is not already in the upper
     bounds of id1. *)
  not (IMap.mem id1 bounds2.lowertvars)

(********************************************************************)

module ImplicitTypeArgument = Instantiation_utils.ImplicitTypeArgument
module TypeAppExpansion = Instantiation_utils.TypeAppExpansion
module Cache = Flow_cache

(********************)
(* subtype relation *)
(********************)

(* Sometimes we expect types to be def types. For example, when we see a flow
   constraint from type l to type u, we expect l to be a def type. As another
   example, when we see a unification constraint between t1 and t2, we expect
   both t1 and t2 to be def types. *)

(* Recursion limiter. We proxy recursion depth with trace depth,
   which is either equal or pretty close.
   When check is called with a trace whose depth exceeds a constant
   limit, we throw a LimitExceeded exception.
*)

module RecursionCheck : sig
  exception LimitExceeded of Type.trace

  val check : Context.t -> Type.trace -> unit
end = struct
  exception LimitExceeded of Type.trace

  (* check trace depth as a proxy for recursion depth
     and throw when limit is exceeded *)
  let check cx trace =
    if Trace.trace_depth trace >= Context.recursion_limit cx then raise (LimitExceeded trace)
end

(* The main problem with constant folding is infinite recursion. Consider a loop
 * that keeps adding 1 to a variable x, which is initialized to 0. If we
 * constant fold x naively, we'll recurse forever, inferring that x has the type
 * (0 | 1 | 2 | 3 | 4 | etc). What we need to do is recognize loops and stop
 * doing constant folding.
 *
 * One solution is for constant-folding-location to keep count of how many times
 * we have seen a reason at a given position in the array.
 * Then, when we've seen it multiple times in the same place, we can decide
 * to stop doing constant folding.
 *)

module ConstFoldExpansion : sig
  val guard : Context.t -> int -> reason * int -> (int -> 't) -> 't
end = struct
  let get_rmap cache id = IMap.find_opt id cache |> Base.Option.value ~default:ConstFoldMap.empty

  let increment reason_with_pos rmap =
    match ConstFoldMap.find_opt reason_with_pos rmap with
    | None -> (0, ConstFoldMap.add reason_with_pos 1 rmap)
    | Some count -> (count, ConstFoldMap.add reason_with_pos (count + 1) rmap)

  let guard cx id reason_with_pos f =
    let cache = Context.const_fold_cache cx in
    let (count, rmap) = get_rmap !cache id |> increment reason_with_pos in
    cache := IMap.add id rmap !cache;
    f count
end

(* Sometimes we expect to see only proper def types. Proper def types make sense
   as use types. *)
let expect_proper_def t =
  if not (is_proper_def t) then assert_false (spf "Did not expect %s" (string_of_ctor t))

let expect_proper_def_use t = lift_to_use expect_proper_def t

let subst = Type_subst.subst ~placeholder_no_infer:false

let check_canceled =
  let count = ref 0 in
  fun () ->
    let n = (!count + 1) mod 128 in
    count := n;
    if n = 0 then WorkerCancel.check_should_cancel ()

let strict_equatable_error cond_context (l, r) =
  let comparison_error =
    lazy
      (match cond_context with
      | Some (SwitchTest { case_test_reason; switch_discriminant_reason }) ->
        let use_op =
          Op (RefinementCheck { test = case_test_reason; discriminant = switch_discriminant_reason })
        in
        Error_message.EIncompatibleWithUseOp
          { reason_lower = reason_of_t l; reason_upper = reason_of_t r; use_op }
      | _ ->
        let reasons = FlowError.ordered_reasons (reason_of_t l, reason_of_t r) in
        Error_message.EComparison reasons)
  in
  match (l, r) with
  | (AnyT _, _)
  | (_, AnyT _) ->
    None
  (* No comparisons of enum objects are allowed. *)
  | (DefT (_, EnumObjectT _), _)
  | (_, DefT (_, EnumObjectT _)) ->
    Some (Lazy.force comparison_error)
  (* We allow comparison between enums of the same type. *)
  | (DefT (_, EnumT { enum_id = id1; _ }), DefT (_, EnumT { enum_id = id2; _ }))
    when ALoc.equal_id id1 id2 ->
    None
  (* We allow the comparison of enums to null and void outside of switches. *)
  | (DefT (_, EnumT _), DefT (_, (NullT | VoidT)))
  | (DefT (_, (NullT | VoidT)), DefT (_, EnumT _)) -> begin
    match cond_context with
    | Some (SwitchTest _) -> Some (Lazy.force comparison_error)
    | None
    | Some _ ->
      None
  end
  (* We don't allow the comparison of enums and other types in general. *)
  | (DefT (_, EnumT _), _)
  | (_, DefT (_, EnumT _)) ->
    Some (Lazy.force comparison_error)
  (* We don't check other strict equality comparisons. *)
  | _ -> None

let strict_equatable cond_context args =
  strict_equatable_error cond_context args |> Base.Option.is_none

let is_concrete t =
  match t with
  | EvalT _
  | AnnotT _
  | ExactT _
  | MaybeT _
  | OptionalT _
  | TypeAppT _
  | ThisTypeAppT _
  | OpenT _ ->
    false
  | _ -> true

let is_literal_type t =
  match t with
  | DefT (_, SingletonStrT _)
  | DefT (_, SingletonNumT _)
  | DefT (_, SingletonBoolT _)
  | DefT (_, SingletonBigIntT _) ->
    true
  | _ -> false

let inherited_method = function
  | OrdinaryName "constructor" -> false
  | _ -> true

let find_resolved_opt cx ~default ~f id =
  let constraints = Context.find_graph cx id in
  match constraints with
  | Resolved t
  | FullyResolved (lazy t) ->
    f t
  | Unresolved _ -> default

let rec drop_resolved cx t =
  match t with
  | GenericT { reason; name; id = g_id; bound = OpenT (_, id); no_infer } ->
    find_resolved_opt cx id ~default:t ~f:(fun t ->
        GenericT { reason; name; id = g_id; bound = drop_resolved cx t; no_infer }
    )
  | OpenT (_, id) -> find_resolved_opt cx id ~default:t ~f:(drop_resolved cx)
  | _ -> t

(********************** start of slab **********************************)
module M__flow
    (FlowJs : Flow_common.S)
    (ReactJs : React_kit.REACT)
    (CheckPolarity : Flow_common.CHECK_POLARITY)
    (CustomFunKit : Custom_fun_kit.CUSTOM_FUN)
    (ObjectKit : Object_kit.OBJECT)
    (SpeculationKit : Speculation_kit.OUTPUT)
    (SubtypingKit : Subtyping_kit.OUTPUT) =
struct
  open SubtypingKit

  module InstantiationHelper = struct
    (* Given a type parameter, a supplied type argument for specializing it, and a
       reason for specialization, either return the type argument or, when directed,
       look up the instantiation cache for an existing type argument for the same
       purpose and unify it with the supplied type argument. *)
    let cache_instantiate cx trace ~use_op ?(cache = false) typeparam reason_op reason_tapp t =
      if cache then (
        match desc_of_reason reason_tapp with
        (* This reason description cannot be trusted for caching purposes. *)
        | RTypeAppImplicit _ -> t
        | _ ->
          let t_ = ImplicitTypeArgument.mk_targ cx typeparam reason_op reason_tapp in
          FlowJs.rec_unify cx trace ~use_op ~unify_any:true t t_;
          t_
      ) else
        t

    let mk_targ = ImplicitTypeArgument.mk_targ

    let is_subtype = FlowJs.rec_flow_t

    let unify cx trace ~use_op (t1, t2) = FlowJs.rec_unify cx trace ~use_op ~unify_any:true t1 t2

    let reposition = FlowJs.reposition ?desc:None ?annot_loc:None
  end

  module InstantiationKit = Instantiation_kit (InstantiationHelper)
  module ImplicitInstantiationKit = Implicit_instantiation.Kit (FlowJs) (InstantiationHelper)

  module Import_export_helper :
    Flow_js_utils.Import_export_helper_sig with type r = Type.t -> unit = struct
    type r = Type.t -> unit

    let reposition = FlowJs.reposition ~trace:Trace.dummy_trace ?desc:None ?annot_loc:None

    let return cx t tout = FlowJs.rec_flow_t cx ~use_op:unknown_use Trace.dummy_trace (t, tout)

    let export_named cx (reason, value_exports_tmap, type_exports_tmap, export_kind) module_t tout =
      FlowJs.rec_flow
        cx
        Trace.dummy_trace
        ( module_t,
          Type.ExportNamedT { reason; value_exports_tmap; type_exports_tmap; export_kind; tout }
        )

    let export_named_fresh_var
        cx (reason, value_exports_tmap, type_exports_tmap, export_kind) module_t =
      Tvar.mk_where cx reason (fun tout ->
          FlowJs.rec_flow
            cx
            Trace.dummy_trace
            ( module_t,
              Type.ExportNamedT { reason; value_exports_tmap; type_exports_tmap; export_kind; tout }
            )
      )

    let export_type cx (reason, name_loc, preferred_def_locs, export_name, target_module_t) export_t
        =
      Tvar.mk_where cx reason (fun tout ->
          FlowJs.rec_flow
            cx
            Trace.dummy_trace
            ( export_t,
              ExportTypeT
                { reason; name_loc; preferred_def_locs; export_name; target_module_t; tout }
            )
      )

    let cjs_extract_named_exports cx (reason, local_module) proto_t =
      Tvar.mk_where cx reason (fun t ->
          FlowJs.rec_flow
            cx
            Trace.dummy_trace
            (proto_t, CJSExtractNamedExportsT (reason, local_module, t))
      )
  end

  module AssertExportIsTypeTKit = AssertExportIsTypeT_kit (Import_export_helper)
  module CopyNamedExportsTKit = CopyNamedExportsT_kit (Import_export_helper)
  module CopyTypeExportsTKit = CopyTypeExportsT_kit (Import_export_helper)
  module ExportTypeTKit = ExportTypeT_kit (Import_export_helper)
  module CJSExtractNamedExportsTKit = CJSExtractNamedExportsT_kit (Import_export_helper)
  include InstantiationKit

  (* get prop *)

  let perform_lookup_action cx trace propref p target_kind lreason ureason =
    let open FlowJs in
    function
    | LookupProp (use_op, up) -> rec_flow_p cx ~trace ~use_op lreason ureason propref (p, up)
    | SuperProp (use_op, lp) -> rec_flow_p cx ~trace ~use_op ureason lreason propref (lp, p)
    | ReadProp { use_op; obj_t; tout } ->
      let react_dro =
        match obj_t with
        | OpenT _ -> failwith "Expected concrete type"
        | DefT (_, ObjT o) -> o.flags.react_dro
        | _ -> None
      in
      FlowJs.perform_read_prop_action cx trace use_op propref p ureason react_dro tout
    | WriteProp { use_op; obj_t = _; tin; write_ctx; prop_tout; mode } -> begin
      match (Property.write_t_of_property_type ~ctx:write_ctx p, target_kind, mode) with
      | (Some t, IndexerProperty, Delete) ->
        (* Always OK to delete a property we found via an indexer *)
        let void = VoidT.why (reason_of_t t) in
        Base.Option.iter
          ~f:(fun prop_tout -> rec_flow_t cx trace ~use_op:unknown_use (void, prop_tout))
          prop_tout
      | (Some t, _, _) ->
        rec_flow cx trace (tin, UseT (use_op, t));
        Base.Option.iter
          ~f:(fun prop_tout -> rec_flow_t cx trace ~use_op:unknown_use (t, prop_tout))
          prop_tout
      | (None, _, _) ->
        let reason_prop = reason_of_propref propref in
        let prop_name = name_of_propref propref in
        let msg = Error_message.EPropNotWritable { reason_prop; prop_name; use_op } in
        add_output cx ~trace msg
    end
    | MatchProp { use_op; drop_generic = drop_generic_; prop_t = tin } -> begin
      match Property.read_t_of_property_type p with
      | Some t ->
        let t =
          if drop_generic_ then
            drop_generic t
          else
            t
        in
        rec_flow cx trace (tin, UseT (use_op, t))
      | None ->
        let reason_prop = reason_of_propref propref in
        let prop_name = name_of_propref propref in
        add_output cx ~trace (Error_message.EPropNotReadable { reason_prop; prop_name; use_op })
    end

  let enum_proto cx ~reason (enum_reason, enum) =
    let enum_object_t = DefT (enum_reason, EnumObjectT enum) in
    let enum_t = DefT (enum_reason, EnumT enum) in
    let { representation_t; _ } = enum in
    FlowJs.get_builtin_typeapp cx reason "$EnumProto" [enum_object_t; enum_t; representation_t]

  let mk_react_dro cx use_op dro t =
    let id = Eval.generate_id () in
    FlowJs.mk_possibly_evaluated_destructor cx use_op (reason_of_t t) t (ReactDRO dro) id

  let mk_hooklike cx use_op t =
    let id = Eval.generate_id () in
    FlowJs.mk_possibly_evaluated_destructor cx use_op (reason_of_t t) t MakeHooklike id

  module Get_prop_helper = struct
    type r = Type.tvar -> unit

    let error_type cx trace reason tout =
      FlowJs.rec_flow_t cx ~use_op:unknown_use trace (AnyT.error reason, OpenT tout)

    let return cx ~use_op trace t tout = FlowJs.rec_flow_t cx ~use_op trace (t, OpenT tout)

    let dict_read_check = FlowJs.rec_flow_t

    let enum_proto = enum_proto

    let reposition = FlowJs.reposition ?desc:None ?annot_loc:None

    let cg_lookup
        cx trace ~obj_t ~method_accessible t (reason_op, lookup_kind, propref, use_op, ids) tout =
      FlowJs.rec_flow
        cx
        trace
        ( t,
          LookupT
            {
              reason = reason_op;
              lookup_kind;
              try_ts_on_failure = [];
              propref;
              lookup_action = ReadProp { use_op; obj_t; tout };
              method_accessible;
              ids = Some ids;
              ignore_dicts = false;
            }
        )

    let cg_get_prop cx trace t (use_op, access_reason, id, (prop_reason, name)) tout =
      FlowJs.rec_flow
        cx
        trace
        ( t,
          GetPropT
            {
              use_op;
              reason = access_reason;
              id;
              from_annot = false;
              propref = mk_named_prop ~reason:prop_reason name;
              tout;
              hint = hint_unavailable;
            }
        )

    let mk_react_dro = mk_react_dro

    let mk_hooklike = mk_hooklike
  end

  module GetPropTKit = GetPropT_kit (Get_prop_helper)

  (** NOTE: Do not call this function directly. Instead, call the wrapper
      functions `rec_flow`, `join_flow`, or `flow_opt` (described below) inside
      this module, and the function `flow` outside this module. **)
  let rec __flow cx ((l : Type.t), (u : Type.use_t)) trace =
    if ground_subtype (l, u) then
      print_types_if_verbose cx trace (l, u)
    else if Cache.FlowConstraint.get cx (l, u) then
      print_types_if_verbose cx trace ~note:"(cached)" (l, u)
    else (
      print_types_if_verbose cx trace (l, u);

      (* limit recursion depth *)
      RecursionCheck.check cx trace;

      (* Check if this worker has been told to cancel *)
      check_canceled ();

      (* Expect that l is a def type. On the other hand, u may be a use type or a
         def type: the latter typically when we have annotations. *)

      (* Types that are classified as def types but don't make sense as use types
         should not appear as use types. *)
      expect_proper_def_use u;

      (* Before processing the flow action, check that it is not deferred. If it
         is, then when speculation is complete, the action either fires or is
         discarded depending on whether the case that created the action is
         selected or not. *)
      if Speculation.defer_action cx (Speculation_state.FlowAction (l, u)) then
        print_if_verbose cx ~trace ~indent:1 ["deferred during speculation"]
      else if
        match l with
        | AnyT _ ->
          (* Either propagate AnyT through the use type, or short-circuit because any <: u trivially *)
          any_propagated cx trace l u
        | GenericT { bound; name; reason; id; no_infer } ->
          handle_generic cx trace ~no_infer bound reason id name u
        | _ -> false
        (* Either propagate AnyT through the def type, or short-circuit because l <: any trivially *)
      then
        ()
      else if
        match u with
        | UseT (use_op, (AnyT _ as any)) -> any_propagated_use cx trace use_op any l
        | _ -> false
      then
        ()
      else if
        match l with
        | DefT (_, EmptyT) -> empty_success u
        | _ -> false
      then
        ()
      else
        (* START OF PATTERN MATCH *)
        match (l, u) with
        (********)
        (* eval *)
        (********)
        | (EvalT (_, _, id1), UseT (_, EvalT (_, _, id2))) when Type.Eval.equal_id id1 id2 ->
          if Context.is_verbose cx then prerr_endline "EvalT ~> EvalT fast path"
        | (EvalT (t, TypeDestructorT (use_op', reason, d), id), _) ->
          let result = mk_type_destructor cx ~trace use_op' reason t d id in
          rec_flow cx trace (result, u)
        | (_, UseT (use_op, EvalT (t, TypeDestructorT (use_op', reason, d), id))) ->
          let result = mk_type_destructor cx ~trace use_op' reason t d id in
          rec_flow cx trace (result, ReposUseT (reason, false, use_op, l))
        (******************)
        (* process X ~> Y *)
        (******************)
        | (OpenT (_, tvar1), UseT (use_op, OpenT (_, tvar2))) ->
          let (id1, constraints1) = Context.find_constraints cx tvar1 in
          let (id2, constraints2) = Context.find_constraints cx tvar2 in
          (match (constraints1, constraints2) with
          | (Unresolved bounds1, Unresolved bounds2) ->
            if not_linked (id1, bounds1) (id2, bounds2) then (
              add_upper_edges ~new_use_op:use_op cx trace (id1, bounds1) (id2, bounds2);
              add_lower_edges cx trace ~new_use_op:use_op (id1, bounds1) (id2, bounds2);
              flows_across cx trace ~use_op bounds1.lower bounds2.upper
            )
          | (Unresolved bounds1, (Resolved t2 | FullyResolved (lazy t2))) ->
            let t2_use = flow_use_op cx unknown_use (UseT (use_op, t2)) in
            edges_and_flows_to_t cx trace (id1, bounds1) t2_use
          | ((Resolved t1 | FullyResolved (lazy t1)), Unresolved bounds2) ->
            edges_and_flows_from_t cx trace ~new_use_op:use_op t1 (id2, bounds2)
          | ((Resolved t1 | FullyResolved (lazy t1)), (Resolved t2 | FullyResolved (lazy t2))) ->
            let t2_use = flow_use_op cx unknown_use (UseT (use_op, t2)) in
            rec_flow cx trace (t1, t2_use))
        (******************)
        (* process Y ~> U *)
        (******************)
        | (OpenT (r, tvar), t2) ->
          let t2 =
            match desc_of_reason r with
            | RTypeParam _ -> mod_use_op_of_use_t (fun op -> Frame (ImplicitTypeParam, op)) t2
            | _ -> t2
          in
          let (id1, constraints1) = Context.find_constraints cx tvar in
          (match constraints1 with
          | Unresolved bounds1 -> edges_and_flows_to_t cx trace (id1, bounds1) t2
          | Resolved t1
          | FullyResolved (lazy t1) ->
            rec_flow cx trace (t1, t2))
        (******************)
        (* process L ~> X *)
        (******************)
        | (t1, UseT (use_op, OpenT (_, tvar))) ->
          let (id2, constraints2) = Context.find_constraints cx tvar in
          (match constraints2 with
          | Unresolved bounds2 ->
            edges_and_flows_from_t cx trace ~new_use_op:use_op t1 (id2, bounds2)
          | Resolved t2
          | FullyResolved (lazy t2) ->
            rec_flow cx trace (t1, UseT (use_op, t2)))
        (************************)
        (* Eval type destructor *)
        (************************)
        | (l, EvalTypeDestructorT { destructor_use_op; reason; repos; destructor; tout }) ->
          let l =
            match repos with
            | None -> l
            | Some (reason, use_desc) -> reposition_reason cx ~trace reason ~use_desc l
          in
          eval_destructor cx ~trace destructor_use_op reason l destructor tout
        (************)
        (* Subtyping *)
        (*************)
        | (_, UseT (use_op, u)) -> rec_sub_t cx use_op l u trace
        (*****************)
        (* Predicate NoP *)
        (*****************)
        | (_, PredicateT (NoP, u))
        | (_, PredicateT (NotP NoP, u)) ->
          (* When the predicate is a no-op, we'd like to behave as close as possible
           * to not having created the PredicateT constraint at all. We forward the
           * lower-bound as is. *)
          rec_flow_t ~use_op:unknown_use cx trace (l, OpenT u)
        (************************)
        (* Full type resolution *)
        (************************)

        (* Full resolution of a type involves (1) walking the type to collect a
           bunch of unresolved tvars (2) emitting constraints that, once those tvars
           are resolved, recursively trigger the process for the resolved types (3)
           finishing when no unresolved tvars remain.

           (1) is covered in ResolvableTypeJob. Below, we cover (2) and (3).

           For (2), we emit a FullyResolveType constraint on any unresolved tvar
           found by (1). These unresolved tvars are chosen so that they have the
           following nice property, called '0->1': they remain unresolved until, at
           some point, they are unified with a concrete type. Moreover, the act of
           resolution coincides with the appearance of one (the first and the last)
           upper bound. (In general, unresolved tvars can accumulate an arbitrary
           number of lower and upper bounds over its lifetime.) More details can be
           found in bindings_of_jobs.

           For (3), we create a special "goal" tvar that acts like a promise for
           fully resolving the original type, and emit a Trigger constraint on the
           goal when no more work remains.

           The main client of full type resolution is checking union and
           intersection types. The check itself is modeled by a TryFlow constraint,
           which is guarded by a goal tvar that corresponds to some full type
           resolution requirement. Eventually, this goal is "triggered," which in
           turn triggers the check. (The name "TryFlow" refers to the technique used
           in the check, which literally tries each branch of the union or
           intersection in turn, maintaining some matching state as it goes: see
           speculative_matches for details). *)
        | (t, ChoiceKitUseT (reason, FullyResolveType id)) ->
          SpeculationKit.fully_resolve_type cx trace reason id t
        | (InternalT (ChoiceKitT (_, Trigger)), ChoiceKitUseT (reason, TryFlow (i, spec))) ->
          SpeculationKit.speculative_matches cx trace reason i spec
        (* Intersection types need a preprocessing step before they can be checked;
           this step brings it closer to parity with the checking of union types,
           where the preprocessing effectively happens "automatically." This
           apparent asymmetry is explained in prep_try_intersection.

           Here, it suffices to note that the preprocessing step involves
           concretizing some types. Type concretization is distinct from full type
           resolution. Whereas full type resolution is a recursive process that
           needs careful orchestration, type concretization is a relatively simple
           one-step process: a tvar is concretized when any lower bound appears on
           it. Also, unlike full type resolution, the tvars that are concretized
           don't necessarily have the 0->1 property: they could be concretized at
           different types, as more and more lower bounds appear. *)
        | (UnionT (_, urep), PreprocessKitT (_, ConcretizeTypes _)) ->
          flow_all_in_union cx trace urep u
        | (MaybeT (lreason, t), PreprocessKitT (_, ConcretizeTypes _)) ->
          let lreason = replace_desc_reason RNullOrVoid lreason in
          rec_flow cx trace (NullT.make lreason, u);
          rec_flow cx trace (VoidT.make lreason, u);
          rec_flow cx trace (t, u)
        | (OptionalT { reason = r; type_ = t; use_desc }, PreprocessKitT (_, ConcretizeTypes _)) ->
          rec_flow cx trace (VoidT.why_with_use_desc ~use_desc r, u);
          rec_flow cx trace (t, u)
        | (AnnotT (r, t, use_desc), PreprocessKitT (_, ConcretizeTypes _)) ->
          (* TODO: directly derive loc and desc from the reason of tvar *)
          let loc = loc_of_reason r in
          let desc =
            if use_desc then
              Some (desc_of_reason r)
            else
              None
          in
          rec_flow cx trace (reposition ~trace cx loc ?annot_loc:(annot_loc_of_reason r) ?desc t, u)
        | ( t,
            PreprocessKitT
              (reason, ConcretizeTypes (ConcretizeIntersectionT (unresolved, resolved, r, rep, u)))
          ) ->
          SpeculationKit.prep_try_intersection cx trace reason unresolved (t :: resolved) u r rep
        (*************)
        (* Debugging *)
        (*************)
        | (_, DebugPrintT reason) ->
          let str = Debug_js.dump_t ~depth:10 cx l in
          add_output cx ~trace (Error_message.EDebugPrint (reason, str))
        | (DefT (_, NumT (Literal (_, (n, _)))), DebugSleepT _) ->
          let n = ref n in
          while !n > 0.0 do
            WorkerCancel.check_should_cancel ();
            Unix.sleepf (min !n 1.0);
            n := !n -. 1.
          done
        | (DefT (reason, EmptyT), ConvertEmptyPropsToMixedT (_, tout)) ->
          rec_flow_t cx trace ~use_op:unknown_use (MixedT.make reason, tout)
        | (_, ConvertEmptyPropsToMixedT (_, tout)) ->
          rec_flow_t cx trace ~use_op:unknown_use (l, tout)
        (***************)
        (* annotations *)
        (***************)

        (* Special cases where we want to recursively concretize types within the
           lower bound. *)
        | (UnionT (r, rep), ReposUseT (reason, use_desc, use_op, l)) ->
          let rep =
            UnionRep.ident_map
              (annot ~in_implicit_instantiation:(Context.in_implicit_instantiation cx) use_desc)
              rep
          in
          let loc = loc_of_reason reason in
          let annot_loc = annot_loc_of_reason reason in
          let r = opt_annot_reason ?annot_loc @@ repos_reason loc r in
          let r =
            if use_desc then
              replace_desc_reason (desc_of_reason reason) r
            else
              r
          in
          rec_flow cx trace (l, UseT (use_op, UnionT (r, rep)))
        | (MaybeT (r, u), ReposUseT (reason, use_desc, use_op, l)) ->
          let loc = loc_of_reason reason in
          let annot_loc = annot_loc_of_reason reason in
          let r = opt_annot_reason ?annot_loc @@ repos_reason loc r in
          let r =
            if use_desc then
              replace_desc_reason (desc_of_reason reason) r
            else
              r
          in
          rec_flow
            cx
            trace
            ( l,
              UseT
                ( use_op,
                  MaybeT
                    ( r,
                      annot
                        ~in_implicit_instantiation:(Context.in_implicit_instantiation cx)
                        use_desc
                        u
                    )
                )
            )
        | ( OptionalT { reason = r; type_ = u; use_desc = use_desc_optional_t },
            ReposUseT (reason, use_desc, use_op, l)
          ) ->
          let loc = loc_of_reason reason in
          let annot_loc = annot_loc_of_reason reason in
          let r = opt_annot_reason ?annot_loc @@ repos_reason loc r in
          let r =
            if use_desc then
              replace_desc_reason (desc_of_reason reason) r
            else
              r
          in
          rec_flow
            cx
            trace
            ( l,
              UseT
                ( use_op,
                  OptionalT
                    {
                      reason = r;
                      type_ =
                        annot
                          ~in_implicit_instantiation:(Context.in_implicit_instantiation cx)
                          use_desc
                          u;
                      use_desc = use_desc_optional_t;
                    }
                )
            )
        | ( DefT
              ( r,
                RendersT
                  (StructuralRenders { renders_variant; renders_structural_type = UnionT (_, rep) })
              ),
            ReposUseT (reason, use_desc, use_op, l)
          ) ->
          let rep =
            UnionRep.ident_map
              (annot ~in_implicit_instantiation:(Context.in_implicit_instantiation cx) use_desc)
              rep
          in
          let loc = loc_of_reason reason in
          let annot_loc = annot_loc_of_reason reason in
          let r = opt_annot_reason ?annot_loc @@ repos_reason loc r in
          let r =
            if use_desc then
              replace_desc_reason (desc_of_reason reason) r
            else
              r
          in
          rec_flow
            cx
            trace
            ( l,
              UseT
                ( use_op,
                  DefT
                    ( r,
                      RendersT
                        (StructuralRenders
                           { renders_variant; renders_structural_type = UnionT (r, rep) }
                        )
                    )
                )
            )
        (* Waits for a def type to become concrete, repositions it as an upper UseT
           using the stored reason. This can be used to store a reason as it flows
           through a tvar. *)
        | (u_def, ReposUseT (reason, use_desc, use_op, l)) ->
          let u = reposition_reason cx ~trace reason ~use_desc u_def in
          rec_flow cx trace (l, UseT (use_op, u))
        (* The source component of an annotation flows out of the annotated
           site to downstream uses. *)
        | (AnnotT (r, t, use_desc), u) ->
          let t = reposition_reason ~trace cx r ~use_desc t in
          rec_flow cx trace (t, u)
        (***************************)
        (* type cast e.g. `(x: T)` *)
        (***************************)
        | (DefT (reason, EnumT enum), TypeCastT (use_op, cast_to_t)) ->
          rec_flow cx trace (cast_to_t, EnumCastT { use_op; enum = (reason, enum) })
        | (UnionT _, TypeCastT (_, (UnionT _ as u)))
          when union_optimization_guard cx ~equiv:false TypeUtil.quick_subtype l u ->
          ()
        | (UnionT (_, rep1), TypeCastT _) -> flow_all_in_union cx trace rep1 u
        | (_, TypeCastT (use_op, cast_to_t)) -> rec_flow cx trace (l, UseT (use_op, cast_to_t))
        (**********************************************************************)
        (* enum cast e.g. `(x: T)` where `x` is an `EnumT`                    *)
        (* We allow enums to be explicitly cast to their representation type. *)
        (* When we specialize `TypeCastT` when the LHS is an `EnumT`, the     *)
        (* `cast_to_t` of `TypeCastT` must then be resolved. So we call flow  *)
        (* with it on the LHS, and `EnumCastT` on the RHS. When we actually   *)
        (* turn this into a `UseT`, it must placed back on the RHS.           *)
        (**********************************************************************)
        | (cast_to_t, EnumCastT { use_op; enum = (_, { representation_t; _ }) })
          when TypeUtil.quick_subtype representation_t cast_to_t ->
          rec_flow cx trace (representation_t, UseT (use_op, cast_to_t))
        | (cast_to_t, EnumCastT { use_op; enum = (reason, enum) }) ->
          rec_flow cx trace (DefT (reason, EnumT enum), UseT (use_op, cast_to_t))
        (******************)
        (* Module exports *)
        (******************)
        | ( ModuleT m,
            ExportNamedT { reason = _; value_exports_tmap; type_exports_tmap; export_kind; tout }
          ) ->
          ExportNamedTKit.mod_ModuleT cx (value_exports_tmap, type_exports_tmap, export_kind) m;
          rec_flow_t cx ~use_op:unknown_use trace (l, tout)
        | (_, AssertExportIsTypeT (_, name, t_out)) ->
          AssertExportIsTypeTKit.on_concrete_type cx name l t_out
        | (ModuleT m, CopyNamedExportsT (reason, target_module_t, t_out)) ->
          CopyNamedExportsTKit.on_ModuleT cx (reason, target_module_t) m t_out
        | (ModuleT m, CopyTypeExportsT (reason, target_module_t, t_out)) ->
          CopyTypeExportsTKit.on_ModuleT cx (reason, target_module_t) m t_out
        | ( _,
            ExportTypeT { reason; name_loc; preferred_def_locs; export_name; target_module_t; tout }
          ) ->
          ExportTypeTKit.on_concrete_type
            cx
            (reason, name_loc, preferred_def_locs, export_name, target_module_t)
            l
            tout
        | (AnyT (lreason, _), CopyNamedExportsT (reason, target_module, t)) ->
          CopyNamedExportsTKit.on_AnyT cx lreason (reason, target_module) t
        | (AnyT (lreason, _), CopyTypeExportsT (reason, target_module, t)) ->
          CopyTypeExportsTKit.on_AnyT cx lreason (reason, target_module) t
        | (_, CJSExtractNamedExportsT (reason, local_module, t_out)) ->
          CJSExtractNamedExportsTKit.on_concrete_type cx (reason, local_module) l t_out
        (*****************)
        (* Import checks *)
        (*****************)
        (* Raise an error if an untyped module is imported. *)
        | ((ModuleT _ | DefT (_, ObjT _)), CheckUntypedImportT _) -> ()
        | (AnyT (lreason, _), CheckUntypedImportT (reason, import_kind)) ->
          Flow_js_utils.check_untyped_import cx import_kind lreason reason
        | (_, AssertImportIsValueT (reason, name)) ->
          let test = function
            | TypeT _
            | ClassT (DefT (_, InstanceT { inst = { inst_kind = InterfaceKind _; _ }; _ })) ->
              add_output cx ~trace (Error_message.EImportTypeAsValue (reason, name))
            | _ -> ()
          in
          (* Imported polymorphic types will always have a concrete def_t, so
           * unwrapping here without concretizing is safe. *)
          (match l with
          | DefT (_, PolyT { t_out = DefT (_, def_t); _ })
          | DefT (_, def_t) ->
            test def_t
          | _ -> ())
        (******************************)
        (* optional chaining - part A *)
        (******************************)
        | (DefT (_, VoidT), OptionalChainT { reason; lhs_reason; voided_out; t_out; _ }) ->
          CalleeRecorder.add_callee_use cx CalleeRecorder.Tast l t_out;
          Context.mark_optional_chain cx (loc_of_reason reason) lhs_reason ~useful:true;
          rec_flow_t ~use_op:unknown_use cx trace (l, voided_out)
        | (DefT (r, NullT), OptionalChainT { reason; lhs_reason; voided_out; t_out; _ }) ->
          CalleeRecorder.add_callee_use cx CalleeRecorder.Tast l t_out;
          let void =
            match desc_of_reason r with
            | RNull ->
              (* to avoid error messages like "null is incompatible with null",
                 give VoidT that arise from `null` annotations a new description
                 explaining why it is void and not null *)
              DefT (replace_desc_reason RVoidedNull r, VoidT)
            | _ -> DefT (r, VoidT)
          in
          Context.mark_optional_chain cx (loc_of_reason reason) lhs_reason ~useful:true;
          rec_flow_t ~use_op:unknown_use cx trace (void, voided_out)
        (***************************)
        (* optional indexed access *)
        (***************************)
        | (DefT (r, (EmptyT | VoidT | NullT)), OptionalIndexedAccessT { use_op; tout_tvar; _ }) ->
          rec_flow_t ~use_op cx trace (EmptyT.why r, OpenT tout_tvar)
        | ((MaybeT (_, t) | OptionalT { type_ = t; _ }), OptionalIndexedAccessT _) ->
          rec_flow cx trace (t, u)
        | (UnionT (_, rep), OptionalIndexedAccessT { use_op; reason; index; tout_tvar }) ->
          let (t0, (t1, ts)) = UnionRep.members_nel rep in
          let f t =
            Tvar.mk_no_wrap_where cx reason (fun tvar ->
                rec_flow
                  cx
                  trace
                  (t, OptionalIndexedAccessT { use_op; reason; index; tout_tvar = tvar })
            )
          in
          let rep = UnionRep.make (f t0) (f t1) (Base.List.map ts ~f) in
          rec_unify cx trace ~use_op:unknown_use (UnionT (reason, rep)) (OpenT tout_tvar)
        | (_, OptionalIndexedAccessT { use_op; reason; index; tout_tvar })
          when match l with
               | IntersectionT _ -> false
               | _ -> true ->
          let u =
            match index with
            | OptionalIndexedAccessStrLitIndex name ->
              let reason_op = replace_desc_reason (RProperty (Some name)) reason in
              GetPropT
                {
                  use_op;
                  reason;
                  id = None;
                  from_annot = true;
                  propref = mk_named_prop ~reason:reason_op ~from_indexed_access:true name;
                  tout = tout_tvar;
                  hint = hint_unavailable;
                }
            | OptionalIndexedAccessTypeIndex key_t ->
              GetElemT
                {
                  use_op;
                  reason;
                  id = None;
                  from_annot = true;
                  access_iterables = false;
                  key_t;
                  tout = tout_tvar;
                }
          in
          rec_flow cx trace (l, u)
        (*************)
        (* invariant *)
        (*************)
        | (_, InvariantT r') ->
          Context.mark_invariant
            cx
            (loc_of_reason r')
            (reason_of_t l)
            ~useful:
              (match Type_filter.not_exists cx l with
              | DefT (_, EmptyT) -> false
              | _ -> true)
        (*************)
        (* DRO and hooklike *)
        (*************)
        | (OptionalT ({ type_; _ } as o), DeepReadOnlyT (((r, _) as tout), dro_loc, dro_type)) ->
          rec_flow_t
            cx
            trace
            ~use_op:unknown_use
            ( OptionalT
                {
                  o with
                  type_ =
                    Tvar.mk_no_wrap_where cx r (fun tvar ->
                        rec_flow cx trace (type_, DeepReadOnlyT (tvar, dro_loc, dro_type))
                    );
                },
              OpenT tout
            )
        | (OptionalT ({ type_; _ } as o), HooklikeT ((r, _) as tout)) ->
          rec_flow_t
            cx
            trace
            ~use_op:unknown_use
            ( OptionalT
                {
                  o with
                  type_ =
                    Tvar.mk_no_wrap_where cx r (fun tvar ->
                        rec_flow cx trace (type_, HooklikeT tvar)
                    );
                },
              OpenT tout
            )
        | (MaybeT (rl, t), DeepReadOnlyT (((r, _) as tout), dro_loc, dro_type)) ->
          rec_flow_t
            cx
            trace
            ~use_op:unknown_use
            ( MaybeT
                ( rl,
                  Tvar.mk_no_wrap_where cx r (fun tvar ->
                      rec_flow cx trace (t, DeepReadOnlyT (tvar, dro_loc, dro_type))
                  )
                ),
              OpenT tout
            )
        | (MaybeT (rl, t), HooklikeT ((r, _) as tout)) ->
          rec_flow_t
            cx
            trace
            ~use_op:unknown_use
            ( MaybeT
                (rl, Tvar.mk_no_wrap_where cx r (fun tvar -> rec_flow cx trace (t, HooklikeT tvar))),
              OpenT tout
            )
        | (UnionT (reason, rep), DeepReadOnlyT (tout, dro_loc, dro_type)) ->
          if not (UnionRep.is_optimized_finally rep) then
            UnionRep.optimize_enum_only ~flatten:(Type_mapper.union_flatten cx) rep;
          if Option.is_some (UnionRep.check_enum rep) then
            rec_flow_t ~use_op:unknown_use cx trace (l, OpenT tout)
          else
            let dro_union =
              map_union
                ~f:(fun cx trace t tout ->
                  let tout = open_tvar tout in
                  rec_flow cx trace (t, DeepReadOnlyT (tout, dro_loc, dro_type)))
                cx
                trace
                rep
                reason
            in
            rec_flow_t ~use_op:unknown_use cx trace (dro_union, OpenT tout)
        | (UnionT (reason, rep), HooklikeT tout) ->
          if not (UnionRep.is_optimized_finally rep) then
            UnionRep.optimize_enum_only ~flatten:(Type_mapper.union_flatten cx) rep;
          if Option.is_some (UnionRep.check_enum rep) then
            rec_flow_t ~use_op:unknown_use cx trace (l, OpenT tout)
          else
            let hook_union =
              map_union
                ~f:(fun cx trace t tout ->
                  let tout = open_tvar tout in
                  rec_flow cx trace (t, HooklikeT tout))
                cx
                trace
                rep
                reason
            in
            rec_flow_t ~use_op:unknown_use cx trace (hook_union, OpenT tout)
        | (DefT (r, ObjT ({ Type.flags; _ } as o)), DeepReadOnlyT (tout, dro_loc, dro_type)) ->
          rec_flow_t
            ~use_op:unknown_use
            cx
            trace
            ( DefT
                (r, ObjT { o with Type.flags = { flags with react_dro = Some (dro_loc, dro_type) } }),
              OpenT tout
            )
        | ( DefT (r, ArrT (TupleAT { elem_t; elements; arity; react_dro = _ })),
            DeepReadOnlyT (tout, dro_loc, dro_type)
          ) ->
          rec_flow_t
            ~use_op:unknown_use
            cx
            trace
            ( DefT
                (r, ArrT (TupleAT { elem_t; elements; arity; react_dro = Some (dro_loc, dro_type) })),
              OpenT tout
            )
        | ( DefT (r, ArrT (ArrayAT { elem_t; tuple_view; react_dro = _ })),
            DeepReadOnlyT (tout, dro_loc, dro_type)
          ) ->
          rec_flow_t
            ~use_op:unknown_use
            cx
            trace
            ( DefT (r, ArrT (ArrayAT { elem_t; tuple_view; react_dro = Some (dro_loc, dro_type) })),
              OpenT tout
            )
        | (DefT (r, ArrT (ROArrayAT (t, _))), DeepReadOnlyT (tout, dro_loc, dro_type)) ->
          rec_flow_t
            ~use_op:unknown_use
            cx
            trace
            (DefT (r, ArrT (ROArrayAT (t, Some (dro_loc, dro_type)))), OpenT tout)
        | (DefT (r, FunT (s, ({ hook = NonHook; _ } as funtype))), HooklikeT tout) ->
          rec_flow_t
            ~use_op:unknown_use
            cx
            trace
            (DefT (r, FunT (s, { funtype with hook = AnyHook })), OpenT tout)
        | ( DefT
              ( rp,
                PolyT ({ t_out = DefT (r, FunT (s, ({ hook = NonHook; _ } as funtype))); _ } as poly)
              ),
            HooklikeT tout
          ) ->
          rec_flow_t
            ~use_op:unknown_use
            cx
            trace
            ( DefT
                (rp, PolyT { poly with t_out = DefT (r, FunT (s, { funtype with hook = AnyHook })) }),
              OpenT tout
            )
        | (DefT (r, ObjT ({ call_t = Some id; _ } as obj)), HooklikeT tout) ->
          let t =
            match Context.find_call cx id with
            | DefT (rf, FunT (s, ({ hook = NonHook; _ } as funtype))) ->
              let call = DefT (rf, FunT (s, { funtype with hook = AnyHook })) in
              let id = Context.make_call_prop cx call in
              DefT (r, ObjT { obj with call_t = Some id })
            | DefT
                ( rp,
                  PolyT
                    ({ t_out = DefT (rf, FunT (s, ({ hook = NonHook; _ } as funtype))); _ } as poly)
                ) ->
              let call =
                DefT
                  ( rp,
                    PolyT { poly with t_out = DefT (rf, FunT (s, { funtype with hook = AnyHook })) }
                  )
              in
              let id = Context.make_call_prop cx call in
              DefT (r, ObjT { obj with call_t = Some id })
            | _ -> l
          in
          rec_flow_t ~use_op:unknown_use cx trace (t, OpenT tout)
        | ( (IntersectionT _ | OpaqueT _ | DefT (_, PolyT _)),
            (DeepReadOnlyT (tout, _, _) | HooklikeT tout)
          ) ->
          rec_flow_t ~use_op:unknown_use cx trace (l, OpenT tout)
        (***************)
        (* maybe types *)
        (***************)

        (* The type maybe(T) is the same as null | undefined | UseT *)
        | (DefT (r, (NullT | VoidT)), FilterMaybeT (use_op, tout)) ->
          rec_flow_t cx trace ~use_op (EmptyT.why r, tout)
        | (DefT (r, MixedT Mixed_everything), FilterMaybeT (use_op, tout)) ->
          rec_flow_t cx trace ~use_op (DefT (r, MixedT Mixed_non_maybe), tout)
        | (OptionalT { reason = _; type_ = tout; use_desc = _ }, FilterMaybeT _)
        | (MaybeT (_, tout), FilterMaybeT _) ->
          rec_flow cx trace (tout, u)
        | (DefT (_, EmptyT), FilterMaybeT (use_op, tout)) -> rec_flow_t cx trace ~use_op (l, tout)
        | (MaybeT _, ReposLowerT (reason_op, use_desc, u)) ->
          (* Don't split the maybe type into its constituent members. Instead,
             reposition the entire maybe type. *)
          let loc = loc_of_reason reason_op in
          let desc =
            if use_desc then
              Some (desc_of_reason reason_op)
            else
              None
          in
          rec_flow cx trace (reposition cx ~trace loc ?desc l, u)
        | (MaybeT (r, t), DestructuringT (reason, DestructAnnot, s, tout, _)) ->
          let f t =
            Tvar.mk_no_wrap_where cx reason (fun tvar ->
                rec_flow
                  cx
                  trace
                  (t, DestructuringT (reason, DestructAnnot, s, tvar, Reason.mk_id ()))
            )
          in
          let void_t = VoidT.why r in
          let null_t = NullT.why r in
          let t = push_type_alias_reason r t in
          let rep = UnionRep.make (f void_t) (f null_t) [f t] in
          rec_unify cx trace ~use_op:unknown_use (UnionT (reason, rep)) (OpenT tout)
        | (MaybeT (_, t), ObjAssignFromT (_, _, _, _, ObjAssign _)) ->
          (* This isn't correct, but matches the existing incorrectness of spreads
           * today. In particular, spreading `null` and `void` become {}. The wrong
           * part is that spreads should distribute through unions, so `{...?T}`
           * should be `{...null}|{...void}|{...T}`, which simplifies to `{}`. *)
          rec_flow cx trace (t, u)
        | (MaybeT _, ResolveUnionT { reason; resolved; unresolved; upper; id }) ->
          resolve_union cx trace reason id resolved unresolved l upper
        | (MaybeT (reason, t), _) ->
          let reason = replace_desc_reason RNullOrVoid reason in
          let t = push_type_alias_reason reason t in
          rec_flow cx trace (NullT.make reason, u);
          rec_flow cx trace (VoidT.make reason, u);
          rec_flow cx trace (t, u)
        (******************)
        (* optional types *)
        (******************)

        (* The type optional(T) is the same as undefined | UseT *)
        | (DefT (r, VoidT), FilterOptionalT (use_op, tout)) ->
          rec_flow_t cx trace ~use_op (EmptyT.why r, tout)
        | (OptionalT { reason = _; type_ = tout; use_desc = _ }, FilterOptionalT _) ->
          rec_flow cx trace (tout, u)
        | (OptionalT _, ReposLowerT (reason, use_desc, u)) ->
          (* Don't split the optional type into its constituent members. Instead,
             reposition the entire optional type. *)
          rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)
        | ( OptionalT { reason = r; type_ = t; use_desc },
            DestructuringT (reason, DestructAnnot, s, tout, _)
          ) ->
          let f t =
            Tvar.mk_no_wrap_where cx reason (fun tvar ->
                rec_flow
                  cx
                  trace
                  (t, DestructuringT (reason, DestructAnnot, s, tvar, Reason.mk_id ()))
            )
          in
          let void_t = VoidT.why_with_use_desc ~use_desc r in
          let rep = UnionRep.make (f void_t) (f t) [] in
          rec_unify cx trace ~use_op:unknown_use (UnionT (reason, rep)) (OpenT tout)
        | ( OptionalT { reason = _; type_ = t; use_desc = _ },
            ObjAssignFromT (_, _, _, _, ObjAssign _)
          ) ->
          (* This isn't correct, but matches the existing incorrectness of spreads
           * today. In particular, spreading `null` and `void` become {}. The wrong
           * part is that spreads should distribute through unions, so `{...?T}`
           * should be `{...null}|{...void}|{...T}`, which simplifies to `{}`. *)
          rec_flow cx trace (t, u)
        | (OptionalT _, ResolveUnionT { reason; resolved; unresolved; upper; id }) ->
          resolve_union cx trace reason id resolved unresolved l upper
        | (OptionalT { reason = r; type_ = t; use_desc }, _) ->
          let void = VoidT.why_with_use_desc ~use_desc r in
          rec_flow cx trace (void, u);
          rec_flow cx trace (t, u)
        (**************************)
        (* logical types - part A *)
        (**************************)
        | (UnionT (_, rep), (AndT _ | OrT _ | NullishCoalesceT _))
          when not (UnionRep.is_optimized_finally rep) ->
          flow_all_in_union cx trace rep u
        | (left, AndT (_, right, u)) ->
          begin
            match left with
            | DefT (reason, NumT _) ->
              add_output
                cx
                ~trace
                (Error_message.ESketchyNumberLint (Lints.SketchyNumberAnd, reason))
            | _ -> ()
          end;

          (* a falsy && b ~> a
             a truthy && b ~> b
             a && b ~> a falsy | b *)
          (match Type_filter.exists cx left with
          | DefT (_, EmptyT) ->
            (* falsy *)
            rec_flow cx trace (left, PredicateT (NotP ExistsP, u))
          | _ ->
            (match Type_filter.not_exists cx left with
            | DefT (_, EmptyT) ->
              (* truthy *)
              rec_flow cx trace (right, UseT (unknown_use, OpenT u))
            | _ ->
              rec_flow cx trace (left, PredicateT (NotP ExistsP, u));
              rec_flow cx trace (right, UseT (unknown_use, OpenT u))))
        | (left, OrT (_, right, u)) ->
          (* a truthy || b ~> a
             a falsy || b ~> b
             a || b ~> a truthy | b *)
          (match Type_filter.not_exists cx left with
          | DefT (_, EmptyT) ->
            (* truthy *)
            rec_flow cx trace (left, PredicateT (ExistsP, u))
          | _ ->
            (match Type_filter.exists cx left with
            | DefT (_, EmptyT) ->
              (* falsy *)
              rec_flow cx trace (right, UseT (unknown_use, OpenT u))
            | _ ->
              rec_flow cx trace (left, PredicateT (ExistsP, u));
              rec_flow cx trace (right, UseT (unknown_use, OpenT u))))
        (* a not-nullish ?? b ~> a
           a nullish ?? b ~> b
           a ?? b ~> a not-nullish | b *)
        | (left, NullishCoalesceT (_, right, u)) ->
          (match Type_filter.maybe cx left with
          | DefT (_, EmptyT)
          (* This `AnyT` case is required to have similar behavior to the other logical operators. *)
          | AnyT _ ->
            (* not-nullish *)
            rec_flow cx trace (left, PredicateT (NotP MaybeP, u))
          | _ ->
            (match Type_filter.not_maybe cx left with
            | DefT (_, EmptyT) ->
              (* nullish *)
              rec_flow cx trace (right, UseT (unknown_use, OpenT u))
            | _ ->
              rec_flow cx trace (left, PredicateT (NotP MaybeP, u));
              rec_flow cx trace (right, UseT (unknown_use, OpenT u))))
        | ( _,
            ReactKitT
              ( use_op,
                reason_op,
                React.CreateElement0 { clone; config; children; tout; return_hint }
              )
          ) ->
          let tool =
            React.CreateElement
              {
                clone;
                component = l;
                config;
                children;
                tout;
                targs = None;
                return_hint;
                record_monomorphized_result = false;
              }
          in
          rec_flow cx trace (l, ReactKitT (use_op, reason_op, tool))
        | (_, ExtractReactRefT (reason, tout)) ->
          let t_ = ImplicitInstantiationKit.run_ref_extractor cx ~use_op:unknown_use ~reason l in
          rec_flow_t cx ~use_op:unknown_use trace (t_, tout)
        (*********************)
        (* type applications *)
        (*********************)

        (* Sometimes a polymorphic class may have a polymorphic method whose return
           type is a type application on the same polymorphic class, possibly
           expanded. See Array#map or Array#concat, e.g. It is not unusual for
           programmers to reuse variables, assigning the result of a method call on
           a variable to itself, in which case we could get into cycles of unbounded
           instantiation. We use caching to cut these cycles. Caching relies on
           reasons (see module Cache.I). This is OK since intuitively, there should
           be a unique instantiation of a polymorphic definition for any given use
           of it in the source code.

           In principle we could use caching more liberally, but we don't because
           not all use types arise from source code, and because reasons are not
           perfect. Indeed, if we tried caching for all use types, we'd lose
           precision and report spurious errors.

           Also worth noting is that we can never safely cache def types. This is
           because substitution of type parameters in def types does not affect
           their reasons, so we'd trivially lose precision. *)
        | (ThisTypeAppT (reason_tapp, c, this, ts), _) ->
          let reason_op = reason_of_use_t u in
          instantiate_this_class cx trace ~reason_op ~reason_tapp c ts this (Upper u)
        | (TypeAppT _, ReposLowerT (reason, use_desc, u)) ->
          rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)
        | ( TypeAppT { reason = reason_tapp; use_op; type_; targs; from_value; use_desc = _ },
            MethodT (_, _, _, _, _)
          )
        | ( TypeAppT { reason = reason_tapp; use_op; type_; targs; from_value; use_desc = _ },
            PrivateMethodT (_, _, _, _, _, _, _)
          ) ->
          let reason_op = reason_of_use_t u in
          let t =
            mk_typeapp_instance_annot
              cx
              ~trace
              ~use_op
              ~reason_op
              ~reason_tapp
              ~from_value
              ~cache:true
              type_
              targs
          in
          rec_flow cx trace (t, u)
        (* This is the second step in checking a TypeAppT (c, ts) ~> TypeAppT (c, ts).
         * The first step is in subtyping_kit.ml, and concretizes the c for our
         * upper bound TypeAppT.
         *
         * When we have done that, then we want to concretize the lower bound. We
         * flip all our arguments to ConcretizeTypeAppsT and set the final element
         * to false to signal that we have concretized the upper bound's c.
         *
         * If the upper bound's c is not a PolyT then we will fall down to an
         * incompatible use error. *)
        | ( (DefT (_, PolyT _) as c2),
            ConcretizeTypeAppsT (use_op, (ts2, fv2, op2, r2), (c1, ts1, fv1, op1, r1), true)
          ) ->
          rec_flow
            cx
            trace
            (c1, ConcretizeTypeAppsT (use_op, (ts1, fv1, op1, r1), (c2, ts2, fv2, op2, r2), false))
        (* When we have concretized the c for our lower bound TypeAppT then we can
         * finally run our TypeAppT ~> TypeAppT logic. If we have referentially the
         * same PolyT for each TypeAppT then we want to check the type arguments
         * only. (Checked in the when condition.) If we do not have the same PolyT
         * for each TypeAppT then we want to expand our TypeAppTs and compare the
         * expanded results.
         *
         * If the lower bound's c is not a PolyT then we will fall down to an
         * incompatible use error.
         *
         * The upper bound's c should always be a PolyT here since we could not have
         * made it here if it was not given the logic of our earlier case. *)
        | ( DefT (_, PolyT { tparams_loc; tparams; id = id1; t_out; _ }),
            ConcretizeTypeAppsT
              (use_op, (ts1, fv1, _, r1), (DefT (_, PolyT { id = id2; _ }), ts2, fv2, _, r2), false)
          )
          when id1 = id2
               && List.length ts1 = List.length ts2
               && (not (wraps_mapped_type cx t_out))
               && fv1 = fv2 ->
          let targs = List.map2 (fun t1 t2 -> (t1, t2)) ts1 ts2 in
          type_app_variance_check cx trace use_op r1 r2 targs tparams_loc tparams
        (* This is the case which implements the expansion for our
         * TypeAppT (c, ts) ~> TypeAppT (c, ts) when the cs are unequal. *)
        | ( DefT (_, PolyT { tparams_loc = tparams_loc1; tparams = xs1; t_out = t1; id = id1 }),
            ConcretizeTypeAppsT
              ( use_op,
                (ts1, fv1, op1, r1),
                ( DefT (_, PolyT { tparams_loc = tparams_loc2; tparams = xs2; t_out = t2; id = id2 }),
                  ts2,
                  fv2,
                  op2,
                  r2
                ),
                false
              )
          ) ->
          let (op1, op2) =
            match root_of_use_op use_op with
            | UnknownUse -> (op1, op2)
            | _ -> (use_op, use_op)
          in
          let t1 =
            mk_typeapp_instance_of_poly
              cx
              trace
              ~use_op:op2
              ~reason_op:r2
              ~reason_tapp:r1
              ~from_value:fv1
              id1
              tparams_loc1
              xs1
              t1
              ts1
          in
          let t2 =
            mk_typeapp_instance_of_poly
              cx
              trace
              ~use_op:op1
              ~reason_op:r1
              ~reason_tapp:r2
              ~from_value:fv2
              id2
              tparams_loc2
              xs2
              t2
              ts2
          in
          rec_flow cx trace (t1, UseT (use_op, t2))
        | (TypeAppT { reason = reason_tapp; use_op; type_; targs; from_value; use_desc = _ }, _) ->
          if TypeAppExpansion.push_unless_loop cx (type_, targs) then (
            let reason_op = reason_of_use_t u in
            let t =
              mk_typeapp_instance_annot
                cx
                ~trace
                ~use_op
                ~reason_op
                ~reason_tapp
                ~from_value
                type_
                targs
            in
            rec_flow cx trace (t, u);
            TypeAppExpansion.pop cx
          )
        (* Concretize types for type inspection purpose up to this point. The rest are
           recorded as lower bound to the target tvar. *)
        | (t, PreprocessKitT (reason, ConcretizeTypes (ConcretizeForImportsExports tvar))) ->
          rec_flow_t cx trace ~use_op:unknown_use (t, OpenT (reason, tvar))
        (* Namespace and type qualification *)
        | ( NamespaceT { values_type; types_tmap },
            GetTypeFromNamespaceT
              { reason = reason_op; use_op; prop_ref = (prop_ref_reason, prop_name); tout }
          ) ->
          (match
             NameUtils.Map.find_opt prop_name (Context.find_props cx types_tmap)
             |> Base.Option.bind ~f:Type.Property.read_t
           with
          | Some prop ->
            let t = reposition cx ~trace (loc_of_reason reason_op) prop in
            rec_flow_t cx ~use_op trace (t, OpenT tout)
          | None ->
            rec_flow
              cx
              trace
              ( values_type,
                GetPropT
                  {
                    use_op;
                    reason = reason_op;
                    id = None;
                    from_annot = false;
                    propref =
                      Named
                        { reason = prop_ref_reason; name = prop_name; from_indexed_access = false };
                    tout;
                    hint = hint_unavailable;
                  }
              ))
        | ( _,
            GetTypeFromNamespaceT
              { reason = reason_op; use_op; prop_ref = (prop_ref_reason, prop_name); tout }
          ) ->
          rec_flow
            cx
            trace
            ( l,
              GetPropT
                {
                  use_op;
                  reason = reason_op;
                  id = None;
                  from_annot = false;
                  propref =
                    Named
                      { reason = prop_ref_reason; name = prop_name; from_indexed_access = false };
                  tout;
                  hint = hint_unavailable;
                }
            )
        (* unwrap namespace type into object type, drop all information about types in the namespace *)
        | (NamespaceT { values_type; types_tmap = _ }, _) -> rec_flow cx trace (values_type, u)
        (***************************************)
        (* transform values to type references *)
        (***************************************)
        | (l, ValueToTypeReferenceT (use_op, reason_op, type_t_kind, tout)) ->
          let t =
            Flow_js_utils.ValueToTypeReferenceTransform.run_on_concrete_type
              cx
              ~trace
              ~use_op
              reason_op
              type_t_kind
              l
          in
          rec_unify cx trace ~use_op t tout
        (**********************)
        (*    opaque types    *)
        (**********************)

        (* Repositioning should happen before opaque types are considered so that we can
         * have the "most recent" location when we do look at the opaque type *)
        | (OpaqueT _, ReposLowerT (reason, use_desc, u)) ->
          rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)
        (* Store the opaque type when doing `ToStringT`, so we can use that
           rather than just `string` if the underlying is `string`. *)
        | (OpaqueT (r, { underlying_t = Some t; _ }), ToStringT { reason; t_out; _ })
          when ALoc.source (loc_of_reason r) = ALoc.source (def_loc_of_reason r) ->
          rec_flow cx trace (t, ToStringT { orig_t = Some l; reason; t_out })
        (* Store the opaque type when doing `ToStringT`, so we can use that
           rather than just `string` if the supertype is `string`. *)
        | (OpaqueT (_, { super_t = Some t; _ }), ToStringT { reason; t_out; _ }) ->
          rec_flow cx trace (t, ToStringT { orig_t = Some l; reason; t_out })
        (* If the type is still in the same file it was defined, we allow it to
         * expose its underlying type information *)
        | (OpaqueT (r, { underlying_t = Some t; _ }), _)
          when ALoc.source (loc_of_reason r) = ALoc.source (def_loc_of_reason r) ->
          rec_flow cx trace (t, u)
        (*****************************************************************)
        (* Intersection type preprocessing for certain object predicates *)
        (*****************************************************************)

        (* Predicate refinements on intersections of object types need careful
           handling. An intersection of object types passes a predicate when any of
           those object types passes the predicate: however, the refined type must
           be the intersection as a whole, not the particular object type that
           passes the predicate! (For example, we may check some condition on
           property x and property y of { x: ... } & { y: ... } in sequence, and not
           expect to get property-not-found errors in the process.)

           Although this seems like a special case, it's not. An intersection of
           object types should behave more or less the same as a "concatenated"
           object type with all the properties of those object types. The added
           complication arises as an implementation detail, because we do not
           concatenate those object types explicitly. *)
        | (_, PreprocessKitT (_, SentinelPropTest (sense, key, t, inter, tvar))) ->
          sentinel_prop_test_generic key cx trace tvar inter (sense, l, t)
        | (_, PreprocessKitT (_, PropExistsTest (sense, key, reason, inter, tvar, preds))) ->
          prop_exists_test_generic key reason cx trace tvar inter sense preds l
        (*****************************************************)
        (* keys (NOTE: currently we only support string keys *)
        (*****************************************************)
        | (KeysT _, ToStringT { t_out; _ }) ->
          (* KeysT outputs strings, so we know ToStringT will be a no-op. *)
          rec_flow cx trace (l, t_out)
        | (KeysT (reason1, o1), _) ->
          (* flow all keys of o1 to u *)
          rec_flow cx trace (o1, GetKeysT (reason1, u))
        (* Concretize types for type inspection purpose up to this point. The rest are
           recorded as lower bound to the target tvar. *)
        | (t, PreprocessKitT (reason, ConcretizeTypes (ConcretizeForInspection tvar))) ->
          rec_flow_t cx trace ~use_op:unknown_use (t, OpenT (reason, tvar))
        (* helpers *)
        | ( DefT (reason_o, ObjT { props_tmap = mapr; flags; _ }),
            HasOwnPropT (use_op, reason_op, key)
          ) ->
          (match (drop_generic key, flags.obj_kind) with
          (* If we have a literal string and that property exists *)
          | (DefT (_, StrT (Literal (_, x))), _) when Context.has_prop cx mapr x -> ()
          (* If we have a dictionary, try that next *)
          | (_, Indexed { key = expected_key; _ }) ->
            rec_flow_t ~use_op cx trace (mod_reason_of_t (Fun.const reason_op) key, expected_key)
          | _ ->
            let (prop, suggestion) =
              match drop_generic key with
              | DefT (_, StrT (Literal (_, prop))) ->
                (Some prop, prop_typo_suggestion cx [mapr] (display_string_of_name prop))
              | _ -> (None, None)
            in
            let err =
              Error_message.EPropNotFound
                {
                  prop_name = prop;
                  reason_prop = reason_op;
                  reason_obj = reason_o;
                  use_op;
                  suggestion;
                }
            in
            add_output cx ~trace err)
        | ( DefT (reason_o, InstanceT { inst; _ }),
            HasOwnPropT
              ( use_op,
                reason_op,
                ( ( DefT (_, StrT (Literal (_, x)))
                  | GenericT { bound = DefT (_, StrT (Literal (_, x))); _ } ) as key
                )
              )
          ) ->
          let own_props = Context.find_props cx inst.own_props in
          (match NameUtils.Map.find_opt x own_props with
          | Some _ -> ()
          | None ->
            let err =
              Error_message.EPropNotFound
                {
                  prop_name = Some x;
                  reason_prop = reason_op;
                  reason_obj = reason_o;
                  use_op;
                  suggestion = prop_typo_suggestion cx [inst.own_props] (display_string_of_name x);
                }
            in
            (match inst.inst_dict with
            | Some { key = dict_key; _ } ->
              rec_flow_t ~use_op cx trace (mod_reason_of_t (Fun.const reason_op) key, dict_key)
            | None -> add_output cx ~trace err))
        | (DefT (reason_o, InstanceT _), HasOwnPropT (use_op, reason_op, _)) ->
          let err =
            Error_message.EPropNotFound
              {
                prop_name = None;
                reason_prop = reason_op;
                reason_obj = reason_o;
                use_op;
                suggestion = None;
              }
          in
          add_output cx ~trace err
        (* AnyT has every prop *)
        | (AnyT _, HasOwnPropT _) -> ()
        | (DefT (_, ObjT { flags; props_tmap; _ }), GetKeysT (reason_op, keys)) ->
          let dict_t = Obj_type.get_dict_opt flags.obj_kind in
          (* flow the union of keys of l to keys *)
          let keylist =
            Flow_js_utils.keylist_of_props (Context.find_props cx props_tmap) reason_op
          in
          rec_flow cx trace (union_of_ts reason_op keylist, keys);
          Base.Option.iter dict_t ~f:(fun { key; _ } ->
              rec_flow cx trace (key, ToStringT { orig_t = None; reason = reason_op; t_out = keys })
          )
        | (DefT (_, InstanceT { inst; _ }), GetKeysT (reason_op, keys)) ->
          (* methods are not enumerable, so only walk fields *)
          let own_props = Context.find_props cx inst.own_props in
          let keylist = Flow_js_utils.keylist_of_props own_props reason_op in
          rec_flow cx trace (union_of_ts reason_op keylist, keys);
          (match inst.inst_dict with
          | Some { key = dict_key; _ } ->
            rec_flow
              cx
              trace
              (dict_key, ToStringT { orig_t = None; reason = reason_op; t_out = keys })
          | None -> ())
        | (AnyT _, GetKeysT (reason_op, keys)) -> rec_flow cx trace (StrT.why reason_op, keys)
        (* In general, typechecking is monotonic in the sense that more constraints
           produce more errors. However, sometimes we may want to speculatively try
           out constraints, backtracking if they produce errors (and removing the
           errors produced). This is useful to typecheck union types and
           intersection types: see below. **)
        (* NOTE: It is important that any def type that simplifies to a union or
           intersection of other def types be processed before we process unions
           and intersections: otherwise we may get spurious errors. **)

        (**********)
        (* values *)
        (**********)
        | (DefT (_, ObjT o), GetValuesT (reason, values)) ->
          let values_l = Flow_js_utils.get_values_type_of_obj_t cx o reason in
          rec_flow_t ~use_op:unknown_use cx trace (values_l, values)
        | ( DefT (_, InstanceT { inst = { own_props; inst_dict; _ }; _ }),
            GetValuesT (reason, values)
          ) ->
          let values_l =
            Flow_js_utils.get_values_type_of_instance_t cx own_props inst_dict reason
          in
          rec_flow_t ~use_op:unknown_use cx trace (values_l, values)
        (* Any will always be ok *)
        | (AnyT (_, src), GetValuesT (reason, values)) ->
          rec_flow_t ~use_op:unknown_use cx trace (AnyT.why src reason, values)
        (***********************************************)
        (* Values of a dictionary - `mixed` otherwise. *)
        (***********************************************)
        | ( DefT
              ( _,
                ObjT
                  { flags = { obj_kind = Indexed { value; dict_polarity; _ }; _ }; props_tmap; _ }
              ),
            GetDictValuesT (_, result)
          )
          when Context.find_props cx props_tmap |> NameUtils.Map.is_empty
               && Polarity.compat (dict_polarity, Polarity.Positive) ->
          rec_flow cx trace (value, result)
        | (DefT (_, ObjT _), GetDictValuesT (reason, result))
        | (DefT (_, InstanceT _), GetDictValuesT (reason, result)) ->
          rec_flow cx trace (MixedT.why reason, result)
        (* Any will always be ok *)
        | (AnyT (_, src), GetDictValuesT (reason, result)) ->
          rec_flow cx trace (AnyT.why src reason, result)
        (********************************)
        (* union and intersection types *)
        (********************************)
        (* We don't want to miss any union optimizations because of unevaluated type destructors, so
           if our union contains any of these problematic types, we force it to resolve its elements before
           considering its upper bound *)
        | (_, ResolveUnionT { reason; resolved; unresolved; upper; id }) ->
          resolve_union cx trace reason id resolved unresolved l upper
        | (UnionT (reason, rep), FilterMaybeT (use_op, tout)) ->
          let quick_subtype = TypeUtil.quick_subtype in
          let void = VoidT.why reason in
          let null = NullT.why reason in
          let filter_void t = quick_subtype t void in
          let filter_null t = quick_subtype t null in
          let filter_null_and_void t = filter_void t || filter_null t in
          begin
            match UnionRep.check_enum rep with
            | Some _ ->
              rec_flow_t
                ~use_op
                cx
                trace
                (remove_predicate_from_union reason cx filter_null_and_void rep, tout)
            | None ->
              let non_maybe_union =
                map_union
                  ~f:(fun cx trace t tout -> rec_flow cx trace (t, FilterMaybeT (use_op, tout)))
                  cx
                  trace
                  rep
                  reason
              in
              rec_flow_t ~use_op cx trace (non_maybe_union, tout)
          end
        | (UnionT (reason, rep), upper) when UnionRep.members rep |> List.exists is_union_resolvable
          ->
          iter_resolve_union ~f:rec_flow cx trace reason rep upper
        (* Don't split the union type into its constituent members. Instead,
           reposition the entire union type. *)
        | (UnionT _, ReposLowerT (reason, use_desc, u)) ->
          rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)
        | (UnionT (reason, rep), MakeExactT (reason_op, k)) ->
          let ts = UnionRep.members rep in
          let f t = ExactT (reason_op, t) in
          let ts' = Base.List.map ts ~f in
          let reason' = repos_reason (loc_of_reason reason_op) reason in
          continue cx trace (union_of_ts reason' ts') k
        | (UnionT _, SealGenericT { reason = _; id; name; cont; no_infer }) ->
          let reason = reason_of_t l in
          continue cx trace (GenericT { reason; id; name; bound = l; no_infer }) cont
        | (UnionT (_, rep), DestructuringT (reason, DestructAnnot, s, tout, _)) ->
          let (t0, (t1, ts)) = UnionRep.members_nel rep in
          let f t =
            Tvar.mk_no_wrap_where cx reason (fun tvar ->
                rec_flow
                  cx
                  trace
                  (t, DestructuringT (reason, DestructAnnot, s, tvar, Reason.mk_id ()))
            )
          in
          let rep = UnionRep.make (f t0) (f t1) (Base.List.map ts ~f) in
          rec_unify cx trace ~use_op:unknown_use (UnionT (reason, rep)) (OpenT tout)
        | (UnionT _, ObjKitT (use_op, reason, resolve_tool, tool, tout)) ->
          ObjectKit.run trace cx use_op reason resolve_tool tool ~tout l
        | ((UnionT (_, rep1) as u1), EqT { arg = UnionT _ as u2; _ }) ->
          if union_optimization_guard cx ~equiv:true (curry equatable) u1 u2 then begin
            if Context.is_verbose cx then prerr_endline "UnionT ~> EqT fast path"
          end else
            flow_all_in_union cx trace rep1 u
        | ((UnionT (_, rep1) as u1), StrictEqT { arg = UnionT _ as u2; cond_context; _ }) ->
          if union_optimization_guard cx ~equiv:true (curry (strict_equatable cond_context)) u1 u2
          then begin
            if Context.is_verbose cx then prerr_endline "UnionT ~> StrictEqT fast path"
          end else
            flow_all_in_union cx trace rep1 u
        | (UnionT _, EqT { reason; flip; arg }) when needs_resolution arg || is_generic arg ->
          rec_flow cx trace (arg, EqT { reason; flip = not flip; arg = l })
        | (UnionT _, StrictEqT { reason; cond_context; flip; arg })
          when needs_resolution arg || is_generic arg ->
          rec_flow cx trace (arg, StrictEqT { reason; cond_context; flip = not flip; arg = l })
        | (UnionT (r, rep), SentinelPropTestT (_reason, l, sense, sentinel, result)) ->
          (* we have the check l.key === sentinel where l.key is a union *)
          if sense then
            match sentinel with
            | UnionEnum.One enum ->
              let def =
                match enum with
                | UnionEnum.Str v -> SingletonStrT v
                | UnionEnum.Num v -> SingletonNumT (v, string_of_float v)
                | UnionEnum.Bool v -> SingletonBoolT v
                | UnionEnum.BigInt v -> SingletonBigIntT v
                | UnionEnum.Void -> VoidT
                | UnionEnum.Null -> NullT
              in
              (match
                 UnionRep.quick_mem_enum ~quick_subtype:TypeUtil.quick_subtype (DefT (r, def)) rep
               with
              | UnionRep.No -> () (* provably unreachable, so prune *)
              | UnionRep.Yes -> rec_flow_t ~use_op:unknown_use cx trace (l, OpenT result)
              | UnionRep.Conditional _
              | UnionRep.Unknown ->
                (* inconclusive: the union is not concretized *)
                flow_all_in_union cx trace rep u)
            | UnionEnum.Many enums ->
              let acc =
                UnionEnumSet.fold
                  (fun enum acc ->
                    let def =
                      match enum with
                      | UnionEnum.Str v -> SingletonStrT v
                      | UnionEnum.Num v -> SingletonNumT (v, string_of_float v)
                      | UnionEnum.Bool v -> SingletonBoolT v
                      | UnionEnum.BigInt v -> SingletonBigIntT v
                      | UnionEnum.Void -> VoidT
                      | UnionEnum.Null -> NullT
                    in
                    UnionRep.join_quick_mem_results
                      ( acc,
                        UnionRep.quick_mem_enum
                          ~quick_subtype:TypeUtil.quick_subtype
                          (DefT (r, def))
                          rep
                      ))
                  enums
                  UnionRep.No
              in
              begin
                match acc with
                | UnionRep.No -> () (* provably unreachable, so prune *)
                | UnionRep.Yes -> rec_flow_t ~use_op:unknown_use cx trace (l, OpenT result)
                | UnionRep.Conditional _
                | UnionRep.Unknown ->
                  (* inconclusive: the union is not concretized *)
                  flow_all_in_union cx trace rep u
              end
          else
            (* for l.key !== sentinel where l.key is a union, we can't really prove
               that the check is guaranteed to fail (assuming the union doesn't
               degenerate to a singleton) *)
            rec_flow_t ~use_op:unknown_use cx trace (l, OpenT result)
        | (UnionT (_, rep), PredicateT (((MaybeP | NotP MaybeP | ExistsP | NotP ExistsP) as p), t))
          when UnionRep.is_optimized_finally rep ->
          predicate cx trace t l p
        | ( UnionT (_, rep),
            ElemT (use_op, reason, obj, ReadElem { id; from_annot = true; access_iterables; tout })
          ) ->
          let reason = update_desc_reason invalidate_rtype_alias reason in
          let (t0, (t1, ts)) = UnionRep.members_nel rep in
          let f t =
            Tvar.mk_no_wrap_where cx reason (fun tvar ->
                rec_flow
                  cx
                  trace
                  ( t,
                    ElemT
                      ( use_op,
                        reason,
                        obj,
                        ReadElem { id; from_annot = true; access_iterables; tout = tvar }
                      )
                  )
            )
          in
          let rep = UnionRep.make (f t0) (f t1) (Base.List.map ts ~f) in
          rec_flow_t cx trace ~use_op:unknown_use (UnionT (reason, rep), OpenT tout)
        | ( UnionT (reason, rep),
            PromoteRendersRepresentationT
              {
                reason = renders_reason;
                resolved_elem;
                use_op = _;
                tout;
                should_distribute;
                promote_structural_components;
                renders_variant;
              }
          ) ->
          let rep =
            UnionRep.ident_map
              (fun t ->
                let reason = update_desc_reason (fun desc -> RRenderType desc) (reason_of_t t) in
                let destructor =
                  TypeDestructorT
                    ( unknown_use,
                      reason,
                      ReactPromoteRendersRepresentation
                        {
                          should_distribute = false;
                          promote_structural_components;
                          renders_variant;
                          resolved_elem;
                        }
                    )
                in
                EvalT (t, destructor, Eval.generate_id ()))
              rep
          in
          let reason = update_desc_reason (fun desc -> RRenderType desc) reason in
          let t =
            if should_distribute then
              DefT
                ( renders_reason,
                  RendersT
                    (StructuralRenders
                       { renders_variant; renders_structural_type = UnionT (reason, rep) }
                    )
                )
            else
              UnionT (reason, rep)
          in
          rec_flow_t cx trace ~use_op:unknown_use (t, tout)
        | (UnionT (_, rep), _)
          when match u with
               (* For l.key !== sentinel when sentinel has a union type, don't split the union. This
                  prevents a drastic blowup of cases which can cause perf problems. *)
               | PredicateT (RightP (SentinelProp _, _), _)
               | PredicateT (NotP (RightP (SentinelProp _, _)), _)
               | WriteComputedObjPropCheckT _
               | ExtractReactRefT _ ->
                 false
               | ConditionalT { distributive_tparam_name; _ } ->
                 Option.is_some distributive_tparam_name
               | _ -> true ->
          flow_all_in_union cx trace rep u
        | (_, FilterOptionalT (use_op, u)) -> rec_flow_t cx trace ~use_op (l, u)
        | (_, FilterMaybeT (use_op, u)) -> rec_flow_t cx trace ~use_op (l, u)
        (* special treatment for some operations on intersections: these
           rules fire for particular UBs whose constraints can (or must)
           be resolved against intersection LBs as a whole, instead of
           by decomposing the intersection into its parts.
        *)
        (* lookup of properties **)
        | ( IntersectionT (_, rep),
            LookupT
              {
                reason;
                lookup_kind;
                try_ts_on_failure;
                propref;
                lookup_action;
                ids;
                method_accessible;
                ignore_dicts;
              }
          ) ->
          let ts = InterRep.members rep in
          assert (ts <> []);

          (* Since s could be in any object type in the list ts, we try to look it
             up in the first element of ts, pushing the rest into the list
             try_ts_on_failure (see below). *)
          rec_flow
            cx
            trace
            ( List.hd ts,
              LookupT
                {
                  reason;
                  lookup_kind;
                  try_ts_on_failure = List.tl ts @ try_ts_on_failure;
                  propref;
                  lookup_action;
                  ids;
                  method_accessible;
                  ignore_dicts;
                }
            )
        (* Cases of an intersection need to produce errors on non-existent
           properties instead of a default, so that other cases may be tried
           instead and succeed. *)
        | ( IntersectionT _,
            GetPropT { use_op; reason; id = Some _; from_annot; propref; tout; hint }
          ) ->
          rec_flow
            cx
            trace
            (l, GetPropT { use_op; reason; id = None; from_annot; propref; tout; hint })
        | (IntersectionT _, TestPropT { use_op; reason; id = _; propref; tout; hint }) ->
          rec_flow
            cx
            trace
            (l, GetPropT { use_op; reason; id = None; from_annot = false; propref; tout; hint })
        | ( IntersectionT _,
            OptionalChainT
              ( {
                  t_out = GetPropT { use_op; reason; id = Some _; from_annot; propref; tout; hint };
                  _;
                } as opt_chain
              )
          ) ->
          rec_flow
            cx
            trace
            ( l,
              OptionalChainT
                {
                  opt_chain with
                  t_out = GetPropT { use_op; reason; id = None; from_annot; propref; tout; hint };
                }
            )
        | ( IntersectionT _,
            OptionalChainT
              ({ t_out = TestPropT { use_op; reason; id = _; propref; tout; hint }; _ } as opt_chain)
          ) ->
          rec_flow
            cx
            trace
            ( l,
              OptionalChainT
                {
                  opt_chain with
                  t_out =
                    GetPropT { use_op; reason; id = None; from_annot = false; propref; tout; hint };
                }
            )
        | (IntersectionT _, DestructuringT (reason, kind, selector, tout, id)) ->
          destruct cx ~trace reason kind l selector tout id
        (* extends **)
        | (IntersectionT (_, rep), ExtendsUseT (use_op, reason, try_ts_on_failure, l, u)) ->
          let (t, ts) = InterRep.members_nel rep in
          let try_ts_on_failure = Nel.to_list ts @ try_ts_on_failure in
          (* Since s could be in any object type in the list ts, we try to look it
             up in the first element of ts, pushing the rest into the list
             try_ts_on_failure (see below). *)
          rec_flow cx trace (t, ExtendsUseT (use_op, reason, try_ts_on_failure, l, u))
        (* consistent override of properties **)
        | (IntersectionT (_, rep), SuperT (use_op, reason, derived)) ->
          InterRep.members rep
          |> List.iter (fun t ->
                 let u =
                   match use_op with
                   | Op (ClassExtendsCheck c) ->
                     let use_op = Op (ClassExtendsCheck { c with extends = reason_of_t t }) in
                     SuperT (use_op, reason, derived)
                   | _ -> u
                 in
                 rec_flow cx trace (t, u)
             )
        (* structural subtype multiple inheritance **)
        | (IntersectionT (_, rep), ImplementsT (use_op, this)) ->
          InterRep.members rep
          |> List.iter (fun t ->
                 let u =
                   match use_op with
                   | Op (ClassImplementsCheck c) ->
                     let use_op = Op (ClassImplementsCheck { c with implements = reason_of_t t }) in
                     ImplementsT (use_op, this)
                   | _ -> u
                 in
                 rec_flow cx trace (t, u)
             )
        (* predicates: prevent a predicate upper bound from prematurely decomposing
           an intersection lower bound *)
        | (IntersectionT _, PredicateT (pred, tout)) -> predicate cx trace tout l pred
        (* same for guards *)
        | (IntersectionT _, GuardT (pred, result, tout)) -> guard cx trace l pred result tout
        (* ObjAssignFromT copies multiple properties from its incoming LB.
           Here we simulate a merged object type by iterating over the
           entire intersection. *)
        | (IntersectionT (_, rep), ObjAssignFromT (use_op, reason_op, proto, tout, kind)) ->
          let tvar =
            List.fold_left
              (fun tout t ->
                let tvar =
                  match Cache.Fix.find cx false t with
                  | Some tvar -> tvar
                  | None ->
                    Tvar.mk_where cx reason_op (fun tvar ->
                        Cache.Fix.add cx false t tvar;
                        rec_flow cx trace (t, ObjAssignFromT (use_op, reason_op, proto, tvar, kind))
                    )
                in
                rec_flow_t cx ~use_op trace (tvar, tout);
                tvar)
              (Tvar.mk cx reason_op)
              (InterRep.members rep)
          in
          rec_flow_t cx ~use_op trace (tvar, tout)
        (* This duplicates the (_, ReposLowerT u) near the end of this pattern
           match but has to appear here to preempt the (IntersectionT, _) in
           between so that we reposition the entire intersection. *)
        | (IntersectionT _, ReposLowerT (reason, use_desc, u)) ->
          rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)
        | (IntersectionT _, ObjKitT (use_op, reason, resolve_tool, tool, tout)) ->
          ObjectKit.run trace cx use_op reason resolve_tool tool ~tout l
        | (IntersectionT _, SealGenericT { reason = _; id; name; cont; no_infer }) ->
          let reason = reason_of_t l in
          continue cx trace (GenericT { reason; id; name; bound = l; no_infer }) cont
        | (IntersectionT _, CallT { use_op; call_action = ConcretizeCallee tout; _ }) ->
          rec_flow_t cx trace ~use_op (l, OpenT tout)
        (* CallT uses that arise from the CallType type destructor are processed
           without preparation (see below). This is because in these cases, the
           return type is intended to be 0-1, whereas preparation (as implemented
           currently) destroys 0-1 behavior. *)
        | ( IntersectionT (r, rep),
            CallT
              {
                use_op = _;
                reason = _;
                call_action = Funcalltype { call_kind = CallTypeKind | MapTypeKind; _ };
                return_hint = _;
              }
          ) ->
          SpeculationKit.try_intersection cx trace u r rep
        (* All other pairs with an intersection lower bound come here. Before
           further processing, we ensure that the upper bound is concretized. See
           prep_try_intersection for details. **)
        (* (After the above preprocessing step, try the branches of the intersection
           in turn, with the goal of selecting the correct branch. This process is
           reused for unions as well. See comments on try_union and
           try_intersection.) *)
        | (IntersectionT (r, rep), u) ->
          let u' =
            match u with
            | OptionalChainT { t_out; _ } -> t_out
            | u -> u
          in
          (* We only call CalleeRecorder here for sig-help information. As far as
           * the typed AST is concerned when dealing with intersections we record
           * the specific branch that was selected. Therefore, we do not record
           * intersections when they hit a CallT constraint. The only time when an
           * intersection is allowed is when we have exhausted the branches of a
           * speculation job (this is a Flow error) and fall back to the
           * intersection as the type for the callee node. (This happens in
           * Default_resolver.) *)
          CalleeRecorder.add_callee_use cx CalleeRecorder.SigHelp l u';
          let unresolved = parts_to_replace cx u in
          SpeculationKit.prep_try_intersection cx trace (reason_of_use_t u) unresolved [] u r rep
        (******************************)
        (* optional chaining - part B *)
        (******************************)
        (* The remaining cases of OptionalChainT will be handled after union-like,
         * intersections and type applications have been resolved *)
        | (_, OptionalChainT { reason; lhs_reason; t_out; voided_out = _ }) ->
          Context.mark_optional_chain
            cx
            (loc_of_reason reason)
            lhs_reason
            ~useful:
              (match l with
              | AnyT (_, AnyError _) -> false
              | DefT (_, MixedT _)
              | AnyT _ ->
                true
              | _ -> false);
          rec_flow cx trace (l, t_out)
        (**************************)
        (* logical types - part B *)
        (**************************)

        (* !x when x is of unknown truthiness *)
        | (DefT (_, BoolT None), NotT (reason, tout))
        | (DefT (_, StrT AnyLiteral), NotT (reason, tout))
        | (DefT (_, NumT AnyLiteral), NotT (reason, tout)) ->
          rec_flow_t ~use_op:unknown_use cx trace (BoolT.at (loc_of_reason reason), OpenT tout)
        (* !x when x is falsy *)
        | (DefT (_, BoolT (Some false)), NotT (reason, tout))
        | (DefT (_, SingletonBoolT false), NotT (reason, tout))
        | (DefT (_, StrT (Literal (_, OrdinaryName ""))), NotT (reason, tout))
        | (DefT (_, SingletonStrT (OrdinaryName "")), NotT (reason, tout))
        | (DefT (_, NumT (Literal (_, (0., _)))), NotT (reason, tout))
        | (DefT (_, SingletonNumT (0., _)), NotT (reason, tout))
        | (DefT (_, NullT), NotT (reason, tout))
        | (DefT (_, VoidT), NotT (reason, tout)) ->
          let reason = replace_desc_reason (RBooleanLit true) reason in
          rec_flow_t ~use_op:unknown_use cx trace (DefT (reason, BoolT (Some true)), OpenT tout)
        (* !x when x is truthy *)
        | (_, NotT (reason, tout)) ->
          let reason = replace_desc_reason (RBooleanLit false) reason in
          rec_flow_t ~use_op:unknown_use cx trace (DefT (reason, BoolT (Some false)), OpenT tout)
        (************)
        (* matching *)
        (************)
        (* TODO get rid of this  *)
        | (MatchingPropT _, _) when is_use u -> ()
        (*************************)
        (* Resolving rest params *)
        (*************************)

        (* `any` is obviously fine as a spread element. `Object` is fine because
         * any Iterable can be spread, and `Object` is the any type that covers
         * iterable objects. *)
        | ( AnyT (r, src),
            ResolveSpreadT (use_op, reason_op, { rrt_resolved; rrt_unresolved; rrt_resolve_to })
          ) ->
          let rrt_resolved = ResolvedAnySpreadArg (r, src) :: rrt_resolved in
          resolve_spread_list_rec
            cx
            ~trace
            ~use_op
            ~reason_op
            (rrt_resolved, rrt_unresolved)
            rrt_resolve_to
        | (_, ResolveSpreadT (use_op, reason_op, { rrt_resolved; rrt_unresolved; rrt_resolve_to }))
          ->
          let reason = reason_of_t l in
          let (lt, generic) =
            match l with
            | GenericT { bound; id; reason; _ } -> (reposition_reason cx reason bound, Some id)
            | _ -> (l, None)
          in
          let arrtype =
            match lt with
            | DefT (_, ArrT arrtype) ->
              (* Arrays *)
              (match (rrt_resolve_to, arrtype) with
              | (ResolveSpreadsToTupleType _, (ArrayAT _ | ROArrayAT _)) ->
                (* Only tuples can be spread into tuple types. *)
                add_output
                  cx
                  ~trace
                  (Error_message.ETupleInvalidTypeSpread
                     { reason_spread = reason_op; reason_arg = reason }
                  );
                ArrayAT { elem_t = AnyT.error reason; tuple_view = None; react_dro = None }
              | _ -> arrtype)
            | _ ->
              (* Non-array non-any iterables, opaque arrays, etc *)
              let resolve_to =
                match rrt_resolve_to with
                (* Spreading iterables in a type context is always OK *)
                | ResolveSpreadsToMultiflowSubtypeFull _ -> `Iterable
                (* Function.prototype.apply takes array-likes, not iterables *)
                | ResolveSpreadsToCallT _ -> `ArrayLike
                (* Otherwise we're spreading values *)
                | ResolveSpreadsToArray _
                | ResolveSpreadsToArrayLiteral _
                | ResolveSpreadsToCustomFunCall _
                | ResolveSpreadsToMultiflowCallFull _
                | ResolveSpreadsToMultiflowPartial _ ->
                  (* Babel's "loose mode" array spread transform deviates from
                   * the spec by assuming the spread argument is always an
                   * array. If the babel_loose_array_spread option is set, model
                   * this assumption.
                   *)
                  if Context.babel_loose_array_spread cx then
                    `Array
                  else
                    `Iterable
                | ResolveSpreadsToTupleType _ -> `Tuple
              in
              let elem_t = Tvar.mk cx reason in
              let resolve_to_type =
                match resolve_to with
                | `ArrayLike ->
                  get_builtin_typeapp
                    cx
                    (replace_desc_new_reason (RCustom "Array-like object expected for apply") reason)
                    "$ArrayLike"
                    [elem_t]
                | `Iterable ->
                  let targs =
                    [
                      elem_t;
                      Unsoundness.why ResolveSpread reason;
                      Unsoundness.why ResolveSpread reason;
                    ]
                  in
                  get_builtin_typeapp
                    cx
                    (replace_desc_new_reason (RCustom "Iterable expected for spread") reason)
                    "$Iterable"
                    targs
                | `Array ->
                  DefT
                    ( replace_desc_new_reason (RCustom "Array expected for spread") reason,
                      ArrT (ROArrayAT (elem_t, None))
                    )
                | `Tuple ->
                  add_output
                    cx
                    ~trace
                    (Error_message.ETupleInvalidTypeSpread
                       { reason_spread = reason_op; reason_arg = reason }
                    );
                  AnyT.error reason
              in
              rec_flow_t ~use_op:unknown_use cx trace (l, resolve_to_type);
              ArrayAT { elem_t; tuple_view = None; react_dro = None }
          in
          let elemt = elemt_of_arrtype arrtype in
          begin
            match rrt_resolve_to with
            (* Any ResolveSpreadsTo* which does some sort of constant folding needs to
             * carry an id around to break the infinite recursion that constant
             * constant folding can trigger *)
            | ResolveSpreadsToTupleType (id, elem_t, tout)
            | ResolveSpreadsToArrayLiteral (id, elem_t, tout) ->
              (* You might come across code like
               *
               * for (let x = 1; x < 3; x++) { foo = [...foo, x]; }
               *
               * where every time you spread foo, you flow another type into foo. So
               * each time `l ~> ResolveSpreadT` is processed, it might produce a new
               * `l ~> ResolveSpreadT` with a new `l`.
               *
               * Here is how we avoid this:
               *
               * 1. We use ConstFoldExpansion to detect when we see a ResolveSpreadT
               *    upper bound multiple times
               * 2. When a ResolveSpreadT upper bound multiple times, we change it into
               *    a ResolveSpreadT upper bound that resolves to a more general type.
               *    This should prevent more distinct lower bounds from flowing in
               * 3. rec_flow caches (l,u) pairs.
               *)
              let reason_elemt = reason_of_t elemt in
              let pos = Base.List.length rrt_resolved in
              ConstFoldExpansion.guard cx id (reason_elemt, pos) (fun recursion_depth ->
                  match recursion_depth with
                  | 0 ->
                    (* The first time we see this, we process it normally *)
                    let rrt_resolved =
                      ResolvedSpreadArg (reason, arrtype, generic) :: rrt_resolved
                    in
                    resolve_spread_list_rec
                      cx
                      ~trace
                      ~use_op
                      ~reason_op
                      (rrt_resolved, rrt_unresolved)
                      rrt_resolve_to
                  | 1 ->
                    (* To avoid infinite recursion, let's deconstruct to a simpler case
                     * where we no longer resolve to a tuple but instead just resolve to
                     * an array. *)
                    rec_flow
                      cx
                      trace
                      ( l,
                        ResolveSpreadT
                          ( use_op,
                            reason_op,
                            {
                              rrt_resolved;
                              rrt_unresolved;
                              rrt_resolve_to = ResolveSpreadsToArray (elem_t, tout);
                            }
                          )
                      )
                  | _ ->
                    (* We've already deconstructed, so there's nothing left to do *)
                    ()
              )
            | ResolveSpreadsToMultiflowCallFull (id, _)
            | ResolveSpreadsToMultiflowSubtypeFull (id, _)
            | ResolveSpreadsToCustomFunCall (id, _, _, _)
            | ResolveSpreadsToMultiflowPartial (id, _, _, _) ->
              let reason_elemt = reason_of_t elemt in
              let pos = Base.List.length rrt_resolved in
              ConstFoldExpansion.guard cx id (reason_elemt, pos) (fun recursion_depth ->
                  match recursion_depth with
                  | 0 ->
                    (* The first time we see this, we process it normally *)
                    let rrt_resolved =
                      ResolvedSpreadArg (reason, arrtype, generic) :: rrt_resolved
                    in
                    resolve_spread_list_rec
                      cx
                      ~trace
                      ~use_op
                      ~reason_op
                      (rrt_resolved, rrt_unresolved)
                      rrt_resolve_to
                  | 1 ->
                    (* Consider
                     *
                     * function foo(...args) { foo(1, ...args); }
                     * foo();
                     *
                     * Because args is unannotated, we try to infer it. However, due to
                     * the constant folding we do with spread arguments, we'll first
                     * infer that it is [], then [] | [1], then [] | [1] | [1,1] ...etc
                     *
                     * We can recognize that we're stuck in a constant folding loop. But
                     * how to break it?
                     *
                     * In this case, we are constant folding by recognizing when args is
                     * a tuple or an array literal. We can break the loop by turning
                     * tuples or array literals into simple arrays.
                     *)
                    let new_arrtype =
                      match arrtype with
                      (* These can get us into constant folding loops *)
                      | ArrayAT { elem_t; tuple_view = Some _; react_dro }
                      | TupleAT { elem_t; react_dro; _ } ->
                        ArrayAT { elem_t; tuple_view = None; react_dro }
                      (* These cannot *)
                      | ArrayAT { tuple_view = None; _ }
                      | ROArrayAT _ ->
                        arrtype
                    in
                    let rrt_resolved =
                      ResolvedSpreadArg (reason, new_arrtype, generic) :: rrt_resolved
                    in
                    resolve_spread_list_rec
                      cx
                      ~trace
                      ~use_op
                      ~reason_op
                      (rrt_resolved, rrt_unresolved)
                      rrt_resolve_to
                  | _ -> ()
              )
            (* no caching *)
            | ResolveSpreadsToArray _
            | ResolveSpreadsToCallT _ ->
              let rrt_resolved = ResolvedSpreadArg (reason, arrtype, generic) :: rrt_resolved in
              resolve_spread_list_rec
                cx
                ~trace
                ~use_op
                ~reason_op
                (rrt_resolved, rrt_unresolved)
                rrt_resolve_to
          end
        (**************)
        (* conditional type *)
        (**************)
        | (DefT (_, EmptyT), ConditionalT { use_op; tout; _ }) ->
          rec_flow_t cx trace ~use_op (l, OpenT tout)
        | ( check_t,
            ConditionalT
              {
                use_op;
                reason;
                distributive_tparam_name = Some name;
                infer_tparams;
                extends_t;
                true_t;
                false_t;
                tout;
              }
          ) ->
          let subst = mk_distributive_tparam_subst_fn cx ~use_op name check_t in
          rec_flow
            cx
            trace
            ( check_t,
              ConditionalT
                {
                  use_op;
                  reason;
                  distributive_tparam_name = None;
                  infer_tparams =
                    Base.List.map infer_tparams ~f:(fun tparam ->
                        { tparam with bound = subst tparam.bound }
                    );
                  extends_t = subst extends_t;
                  true_t = subst true_t;
                  false_t = subst false_t;
                  tout;
                }
            )
        | ( check_t,
            ConditionalT
              {
                use_op;
                reason;
                distributive_tparam_name = None;
                infer_tparams;
                extends_t;
                true_t;
                false_t;
                tout;
              }
          ) ->
          let result =
            ImplicitInstantiationKit.run_conditional
              cx
              trace
              ~use_op
              ~reason
              ~tparams:infer_tparams
              ~check_t
              ~extends_t
              ~true_t
              ~false_t
          in
          rec_flow_t cx trace ~use_op (result, OpenT tout)
        (* singleton lower bounds are equivalent to the corresponding
           primitive with a literal constraint. These conversions are
           low precedence to allow equality exploits above, such as
           the UnionT membership check, to fire.
           TODO we can move to a single representation for singletons -
           either SingletonFooT or (FooT <literal foo>) - if we can
           ensure that their meaning as upper bounds is unambiguous.
           Currently a SingletonFooT means the constrained type,
           but the literal in (FooT <literal>) is a no-op.
           Abstractly it should be totally possible to scrub literals
           from the latter kind of flow, but it's unclear how difficult
           it would be in practice.
        *)
        | ( DefT (_, (SingletonStrT _ | SingletonNumT _ | SingletonBoolT _)),
            ReposLowerT (reason, use_desc, u)
          ) ->
          rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)
        | (DefT (reason, SingletonStrT key), _) ->
          rec_flow cx trace (DefT (reason, StrT (Literal (None, key))), u)
        | (DefT (reason, SingletonNumT lit), _) ->
          rec_flow cx trace (DefT (reason, NumT (Literal (None, lit))), u)
        | (DefT (reason, SingletonBoolT b), _) ->
          rec_flow cx trace (DefT (reason, BoolT (Some b)), u)
        | (DefT (reason, SingletonBigIntT lit), _) ->
          rec_flow cx trace (DefT (reason, BigIntT (Literal (None, lit))), u)
        (* NullProtoT is necessary as an upper bound, to distinguish between
           (ObjT _, NullProtoT _) constraints and (ObjT _, DefT (_, NullT)), but as
           a lower bound, it's the same as DefT (_, NullT) *)
        | (NullProtoT reason, _) -> rec_flow cx trace (DefT (reason, NullT), u)
        (************************************************************************)
        (* exact object types *)
        (************************************************************************)

        (* ExactT<X> comes from annotation, may behave as LB or UB *)

        (* when $Exact<LB> ~> UB, forward to MakeExactT *)
        | (ExactT (r, t), _) ->
          let t = push_type_alias_reason r t in
          rec_flow cx trace (t, MakeExactT (r, Upper u))
        (* Classes/Functions are "inexact" *)
        (* LB ~> MakeExactT (_, UB) exactifies LB, then flows result to UB *)
        (* exactify incoming LB object type, flow to UB *)
        | (DefT (r, ObjT obj), MakeExactT (reason_op, Upper u)) ->
          let exactobj = TypeUtil.make_exact_object ~reason_obj:r obj ~reason_op in
          rec_flow cx trace (exactobj, u)
        (* exactify incoming UB object type, flow to LB *)
        | (DefT (ru, ObjT obj_u), MakeExactT (reason_op, Lower (use_op, l))) ->
          (* forward to standard obj ~> obj *)
          let ru = repos_reason (loc_of_reason reason_op) ru in
          let xu = TypeUtil.make_exact_object ~reason_obj:ru obj_u ~reason_op in
          rec_flow cx trace (l, UseT (use_op, xu))
        | (AnyT (_, src), MakeExactT (reason_op, k)) -> continue cx trace (AnyT.why src reason_op) k
        | (DefT (_, VoidT), MakeExactT (reason_op, k)) -> continue cx trace (VoidT.why reason_op) k
        | (DefT (_, EmptyT), MakeExactT (reason_op, k)) ->
          continue cx trace (EmptyT.why reason_op) k
        (* unsupported kind *)
        | (_, MakeExactT (reason_op, k)) ->
          add_output cx ~trace (Error_message.EUnsupportedExact (reason_op, reason_of_t l));
          continue cx trace (AnyT.error reason_op) k
        (*******************************************)
        (* Refinement based on function predicates *)
        (*******************************************)

        (* Call to predicated (latent) functions *)

        (* Calls to functions appearing in predicate refinement contexts dispatch
           to this case. Here, the callee function type holds the predicate
           that will refine the incoming `unrefined_t` and flow a filtered
           (refined) version of this type into `fresh_t`.

           Problematic cases (e.g. when the refining index is out of bounds w.r.t.
           `params`) raise errors, but also propagate the unrefined types (as if the
           refinement never took place).
        *)
        | ( DefT (lreason, FunT (_, { params; predicate = Some predicate; _ })),
            CallLatentPredT { use_op = _; reason; targs = _; argts = _; sense; idx; tin; tout }
          ) -> begin
          (* TODO: for the moment we only support simple keys (empty projection)
             that exactly correspond to the function's parameters *)
          match (Base.List.nth params (idx - 1), predicate) with
          | (None, _) ->
            let msg =
              Error_message.EPredicateFuncTooShort
                {
                  loc = loc_of_reason reason;
                  pred_func = lreason;
                  pred_func_param_num = List.length params;
                  index = idx;
                }
            in
            add_output cx ~trace msg;
            rec_flow_t ~use_op:unknown_use cx trace (tin, OpenT tout)
          | (Some (None, _), _) ->
            (* Already an unsupported-syntax error on the definition side of the function. *)
            rec_flow_t ~use_op:unknown_use cx trace (tin, OpenT tout)
          | (Some (Some name, _), PredBased (_, (lazy (pmap, nmap)))) ->
            let key = (OrdinaryName name, []) in
            let preds =
              if sense then
                pmap
              else
                nmap
            in
            begin
              match Key_map.find_opt key preds with
              | Some p -> rec_flow cx trace (tin, PredicateT (p, tout))
              | None -> rec_flow_t ~use_op:unknown_use cx trace (tin, OpenT tout)
            end
          | (Some (Some name, _), TypeGuardBased { param_name = (_, param_name); type_guard }) ->
            let t =
              if param_name <> name then
                (* This is not the refined parameter. *)
                tin
              else if sense then
                intersect cx tin type_guard
              else
                type_guard_diff cx tin type_guard
            in
            rec_flow_t ~use_op:unknown_use cx trace (t, OpenT tout)
        end
        | ( DefT (reason_tapp, PolyT { tparams_loc; tparams = ids; t_out = t; _ }),
            CallLatentPredT { use_op; reason; targs; argts; sense; idx; tin; tout }
          ) ->
          let tvar = (reason, Tvar.mk_no_wrap cx reason) in
          let calltype = mk_functioncalltype ~call_kind:RegularCallKind reason targs argts tvar in
          let check = lazy (IICheck.of_call l (tparams_loc, ids, t) unknown_use reason calltype) in
          let lparts = (reason_tapp, tparams_loc, ids, t) in
          let uparts = (use_op, reason, calltype.call_targs, Type.hint_unavailable) in
          let t_ = instantiate_poly_call_or_new cx trace ~cache:true lparts uparts check in
          rec_flow
            cx
            trace
            (t_, CallLatentPredT { use_op; reason; targs = None; argts; sense; idx; tin; tout })
        (* Fall through all the remaining cases *)
        | (_, CallLatentPredT { tin; tout; _ }) ->
          rec_flow_t ~use_op:unknown_use cx trace (tin, OpenT tout)
        (********************)
        (* mixin conversion *)
        (********************)

        (* A class can be viewed as a mixin by extracting its immediate properties,
           and "erasing" its static and super *)
        | ( DefT (class_reason, ClassT (ThisInstanceT (_, { inst; _ }, is_this, this_name))),
            MixinT (r, tvar)
          ) ->
          let static = ObjProtoT r in
          let super = ObjProtoT r in
          rec_flow
            cx
            trace
            ( DefT
                ( class_reason,
                  ClassT
                    (ThisInstanceT (r, { static; super; implements = []; inst }, is_this, this_name))
                ),
              UseT (unknown_use, tvar)
            )
        | ( DefT
              ( _,
                PolyT
                  {
                    tparams_loc;
                    tparams = xs;
                    t_out =
                      DefT (class_r, ClassT (ThisInstanceT (_, { inst; _ }, is_this, this_name)));
                    _;
                  }
              ),
            MixinT (r, tvar)
          ) ->
          let static = ObjProtoT r in
          let super = ObjProtoT r in
          let instance = { static; super; implements = []; inst } in
          rec_flow
            cx
            trace
            ( poly_type
                (Type.Poly.generate_id ())
                tparams_loc
                xs
                (DefT (class_r, ClassT (ThisInstanceT (r, instance, is_this, this_name)))),
              UseT (unknown_use, tvar)
            )
        | (AnyT (_, src), MixinT (r, tvar)) ->
          rec_flow_t ~use_op:unknown_use cx trace (AnyT.why src r, tvar)
        (* TODO: it is conceivable that other things (e.g. functions) could also be
           viewed as mixins (e.g. by extracting properties in their prototypes), but
           such enhancements are left as future work. *)
        (***************************************)
        (* generic function may be specialized *)
        (***************************************)

        (* Instantiate a polymorphic definition using the supplied type
           arguments. Use the instantiation cache if directed to do so by the
           operation. (SpecializeT operations are created when processing TypeAppT
           types, so the decision to cache or not originates there.) *)
        | ( DefT (_, PolyT { tparams_loc; tparams = xs; t_out = t; id }),
            SpecializeT (use_op, reason_op, reason_tapp, cache, ts, tvar)
          ) ->
          let ts = Base.Option.value ts ~default:[] in
          let t_ =
            mk_typeapp_of_poly
              cx
              trace
              ~use_op
              ~reason_op
              ~reason_tapp
              ~cache
              id
              tparams_loc
              xs
              t
              ts
          in
          rec_flow_t ~use_op:unknown_use cx trace (t_, tvar)
        | (DefT (_, PolyT { tparams = tps; _ }), VarianceCheckT (_, tparams, targs, polarity)) ->
          variance_check cx ~trace tparams polarity (Nel.to_list tps, targs)
        | (ThisInstanceT _, VarianceCheckT (_, _, [], _)) ->
          (* We will emit this constraint when walking an extends clause which does
           * not have explicit type arguments. The class has an implicit this type
           * parameter which needs to be specialized to the inheriting class, but
           * that is uninteresting for the variance check machinery. *)
          ()
        (* empty targs specialization of non-polymorphic classes is a no-op *)
        | (DefT (_, ClassT _), SpecializeT (_, _, _, _, None, tvar)) ->
          rec_flow_t ~use_op:unknown_use cx trace (l, tvar)
        | (AnyT _, SpecializeT (_, _, _, _, _, tvar)) ->
          rec_flow_t ~use_op:unknown_use cx trace (l, tvar)
        (* this-specialize a this-abstracted class by substituting This *)
        | (DefT (_, ClassT (ThisInstanceT (inst_r, i, _, this_name))), ThisSpecializeT (r, this, k))
          ->
          let i = Type_subst.subst_instance_type cx (Subst_name.Map.singleton this_name this) i in
          continue_repos cx trace r (DefT (inst_r, InstanceT i)) k
        (* this-specialization of non-this-abstracted classes is a no-op *)
        | (DefT (_, ClassT i), ThisSpecializeT (r, _this, k)) ->
          (* TODO: check that this is a subtype of i? *)
          continue_repos cx trace r i k
        | (AnyT _, ThisSpecializeT (r, _, k)) -> continue_repos cx trace r l k
        | (DefT (_, PolyT _), ReposLowerT (reason, use_desc, u)) ->
          rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)
        | (DefT (_, ClassT (ThisInstanceT _)), ReposLowerT (reason, use_desc, u)) ->
          rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)
        (* Special case for `_ instanceof C` where C is polymorphic *)
        | ( DefT (reason_tapp, PolyT { tparams_loc; tparams = ids; t_out = t; _ }),
            PredicateT ((RightP (InstanceofTest, _) | NotP (RightP (InstanceofTest, _))), _)
          ) ->
          let l =
            instantiate_poly_default_args
              cx
              trace
              ~use_op:unknown_use
              ~reason_op:(reason_of_use_t u)
              ~reason_tapp
              (tparams_loc, ids, t)
          in
          rec_flow cx trace (l, u)
        | (DefT (_, PolyT _), PredicateT (p, t)) -> predicate cx trace t l p
        (* The rules below are hit when a polymorphic type appears outside a
           type application expression - i.e. not followed by a type argument list
           delimited by angle brackets.
           We want to require full expressions in type positions like annotations,
           but allow use of polymorphically-typed values - for example, in class
           extends clauses and at function call sites - without explicit type
           arguments, since typically they're easily inferred from context.
        *)
        (* We are calling the static callable method of a class. We need to be careful
         * not to apply the targs at this point, because this PolyT represents the class
         * and not the static function that's being called. We implicitly instantiate
         * the instance's tparams using the bounds and then forward the result original call
         * instead of consuming the method call's type arguments.
         *
         * We use the bounds to explicitly instantiate so that we don't create yet another implicit
         * instantiation here that would be un-annotatable. *)
        | ( DefT
              ( reason_tapp,
                PolyT
                  { tparams_loc; tparams = ids; t_out = DefT (_, ClassT (ThisInstanceT _)) as t; _ }
              ),
            CallT { use_op; reason = reason_op; _ }
          ) ->
          let targs = Nel.map (fun tparam -> ExplicitArg tparam.bound) ids in
          let t_ =
            instantiate_with_targs
              cx
              trace
              (tparams_loc, ids, t)
              (Nel.to_list targs)
              ~use_op
              ~reason_op
              ~reason_tapp
          in
          rec_flow cx trace (t_, u)
        (* We use the ConcretizeCallee action to simplify types for hint decomposition.
           After having instantiated polymorphic classes on static calls (case above),
           we can just return the remaining polymorphic types, since there is not
           much we can do about them here. These will be handled by the hint
           decomposition code that has some knowledge of the call arguments.
        *)
        | (DefT (_, PolyT _), CallT { use_op; call_action = ConcretizeCallee tout; _ }) ->
          rec_flow_t cx trace ~use_op (l, OpenT tout)
        (* Calls to polymorphic functions may cause non-termination, e.g. when the
           results of the calls feed back as subtle variations of the original
           arguments. This is similar to how we may have non-termination with
           method calls on type applications. Thus, it makes sense to replicate
           the specialization caching mechanism used in TypeAppT ~> MethodT to
           avoid non-termination in PolyT ~> CallT.

           As it turns out, we need a bit more work here. A call may invoke
           different cases of an overloaded polymorphic function on different
           arguments, so we use the reasons of arguments in addition to the reason
           of the call as keys for caching instantiations.

           On the other hand, even the reasons of arguments may not offer sufficient
           distinguishing power when the arguments have not been concretized:
           differently typed arguments could be incorrectly summarized by common
           type variables they flow to, causing spurious errors. In particular, we
           don't cache calls involved in the execution of mapped type operations
           ($TupleMap, $ObjectMap, $ObjectMapi) to avoid this problem.

           NOTE: This is probably not the final word on non-termination with
           generics. We need to separate the double duty of reasons in the current
           implementation as error positions and as caching keys. As error
           positions we should be able to subject reasons to arbitrary tweaking,
           without fearing regressions in termination guarantees.
        *)
        | ( DefT (reason_tapp, PolyT { tparams_loc; tparams = ids; t_out = t; _ }),
            CallT { use_op; reason = reason_op; call_action = Funcalltype calltype; return_hint }
          ) ->
          let check = lazy (IICheck.of_call l (tparams_loc, ids, t) use_op reason_op calltype) in
          let lparts = (reason_tapp, tparams_loc, ids, t) in
          let uparts = (use_op, reason_op, calltype.call_targs, return_hint) in
          let t_ = instantiate_poly_call_or_new cx trace ~cache:true lparts uparts check in
          let u =
            CallT
              {
                use_op;
                reason = reason_op;
                call_action = Funcalltype { calltype with call_targs = None };
                return_hint;
              }
          in
          rec_flow cx trace (t_, u)
        | ( DefT (reason_tapp, PolyT { tparams_loc; tparams = ids; t_out = t; _ }),
            ConstructorT { use_op; reason = reason_op; targs; args; tout; return_hint }
          ) ->
          let check = lazy (IICheck.of_ctor l (tparams_loc, ids, t) use_op reason_op targs args) in
          let lparts = (reason_tapp, tparams_loc, ids, t) in
          let uparts = (use_op, reason_op, targs, return_hint) in
          let t_ = instantiate_poly_call_or_new cx trace lparts uparts check in
          let u =
            ConstructorT { use_op; reason = reason_op; targs = None; args; tout; return_hint }
          in
          rec_flow cx trace (t_, u)
        | ( DefT (reason_tapp, PolyT { tparams_loc; tparams = ids; t_out = t; _ }),
            ReactKitT
              ( use_op,
                reason_op,
                React.CreateElement
                  {
                    clone;
                    component;
                    config;
                    children;
                    return_hint;
                    targs;
                    tout;
                    record_monomorphized_result = _;
                  }
              )
          ) ->
          let lparts = (reason_tapp, tparams_loc, ids, t) in
          let uparts = (use_op, reason_op, targs, return_hint) in
          let check =
            let poly_t = (tparams_loc, ids, t) in
            lazy (IICheck.of_jsx l poly_t use_op reason_op clone ~component ~config ~targs children)
          in
          let t_ = instantiate_poly_call_or_new cx trace lparts uparts check in
          let u =
            ReactKitT
              ( use_op,
                reason_op,
                React.CreateElement
                  {
                    clone;
                    component;
                    config;
                    children;
                    return_hint;
                    targs = None;
                    tout;
                    record_monomorphized_result = true;
                  }
              )
          in
          rec_flow cx trace (t_, u)
        | ( DefT (reason_tapp, PolyT { tparams_loc = _; tparams; t_out; _ }),
            ReactKitT (use_op, reason_op, (React.GetProps _ | React.GetConfig _ | React.GetRef _))
          ) ->
          let t_ =
            ImplicitInstantiationKit.run_monomorphize
              cx
              trace
              ~use_op
              ~reason_op
              ~reason_tapp
              tparams
              t_out
          in
          rec_flow cx trace (t_, u)
        (******************************)
        (* functions statics - part A *)
        (******************************)
        | ( ( DefT (reason, FunT (static, _))
            | DefT (_, PolyT { t_out = DefT (reason, FunT (static, _)); _ }) ),
            MethodT (use_op, reason_call, reason_lookup, propref, action)
          ) ->
          let method_type =
            Tvar.mk_no_wrap_where cx reason_lookup (fun tout ->
                let u =
                  GetPropT
                    {
                      use_op;
                      reason = reason_lookup;
                      id = None;
                      from_annot = false;
                      propref;
                      tout;
                      hint = hint_unavailable;
                    }
                in
                rec_flow cx trace (static, ReposLowerT (reason, false, u))
            )
          in
          apply_method_action cx trace method_type use_op reason_call l action
        | (DefT (reason_tapp, PolyT { tparams_loc; tparams = ids; t_out = t; _ }), _) ->
          let reason_op = reason_of_use_t u in
          let use_op =
            match use_op_of_use_t u with
            | Some use_op -> use_op
            | None -> unknown_use
          in
          let unify_bounds =
            match u with
            | MethodT (_, _, _, _, NoMethodAction _) -> true
            | _ -> false
          in
          let (t_, _) =
            instantiate_poly
              cx
              trace
              ~use_op
              ~reason_op
              ~reason_tapp
              ~unify_bounds
              (tparams_loc, ids, t)
          in
          rec_flow cx trace (t_, u)
        (* when a this-abstracted class flows to upper bounds, fix the class *)
        | (DefT (class_r, ClassT (ThisInstanceT (r, i, this, this_name))), _) ->
          let reason = reason_of_use_t u in
          rec_flow
            cx
            trace
            (DefT (class_r, ClassT (fix_this_instance cx reason (r, i, this, this_name))), u)
        | (ThisInstanceT (r, i, this, this_name), _) ->
          let reason = reason_of_use_t u in
          rec_flow cx trace (fix_this_instance cx reason (r, i, this, this_name), u)
        (*****************************)
        (* React Abstract Components *)
        (*****************************)
        (* When looking at properties of an AbstractComponent, we delegate to a union of
         * function component and class component
         *)
        | ( DefT (r, ReactAbstractComponentT _),
            (TestPropT _ | GetPropT _ | SetPropT _ | GetElemT _ | SetElemT _)
          ) ->
          let statics = get_builtin_type cx ~trace r "React$AbstractComponentStatics" in
          rec_flow cx trace (statics, u)
        (* Render Type Promotion *)
        (* A named AbstractComponent is turned into its corresponding render type *)
        | ( DefT
              ( _,
                ReactAbstractComponentT
                  {
                    component_kind = Nominal (renders_id, renders_name);
                    renders = renders_super;
                    _;
                  }
              ),
            PromoteRendersRepresentationT
              {
                reason;
                tout;
                resolved_elem = Some _;
                use_op = _;
                should_distribute;
                promote_structural_components = _;
                renders_variant;
              }
          ) ->
          let result =
            let t =
              DefT (reason, RendersT (NominalRenders { renders_id; renders_name; renders_super }))
            in
            if should_distribute then
              DefT
                ( reason,
                  RendersT (StructuralRenders { renders_variant; renders_structural_type = t })
                )
            else
              t
          in
          (* Intentional unknown_use when flowing to tout *)
          rec_flow_t cx trace ~use_op:unknown_use (result, tout)
        | ( OpaqueT
              ( _,
                {
                  opaque_id;
                  super_t = Some (DefT (_, ObjT { props_tmap; _ }));
                  opaque_type_args = (_, _, component_t, _) :: _;
                  _;
                }
              ),
            PromoteRendersRepresentationT
              {
                use_op;
                reason;
                tout;
                resolved_elem = None;
                should_distribute;
                renders_variant;
                promote_structural_components;
              }
          )
          when Some opaque_id = Flow_js_utils.builtin_react_element_opaque_id cx ->
          (match Context.find_monomorphized_component cx props_tmap with
          | Some mono_component ->
            rec_flow
              cx
              trace
              ( mono_component,
                PromoteRendersRepresentationT
                  {
                    use_op;
                    reason;
                    tout;
                    should_distribute;
                    renders_variant;
                    promote_structural_components;
                    resolved_elem = Some l;
                  }
              )
          | None ->
            if promote_structural_components then
              (* We only want to promote if this is actually a React of a component, otherwise we want
               * to flow the original object to the tout.
               *
               * We perform a speculative subtyping check and then use ComponentRenders to
               * extract the render type of the component. This type gets forwarded to
               * TryRenderTypePromotionT, and we continue with renders subtyping if we get a RendersT
               * from ComponentRenders, otherwise we error, as we've already checked for structural
               * compatibility in subtyping kit. *)
              let top_abstract_component =
                let config = EmptyT.why reason in
                let instance = MixedT.why reason in
                let renders = get_builtin_type cx reason "React$Node" in
                DefT
                  ( reason,
                    ReactAbstractComponentT
                      { config; instance; renders; component_kind = Structural }
                  )
              in
              if speculative_subtyping_succeeds cx component_t top_abstract_component then
                let render_type =
                  get_builtin_typeapp cx (reason_of_t l) "React$ComponentRenders" [component_t]
                in
                rec_flow_t cx trace ~use_op:unknown_use (render_type, tout)
              else
                rec_flow_t cx trace ~use_op:unknown_use (l, tout)
            else
              rec_flow
                cx
                trace
                ( component_t,
                  PromoteRendersRepresentationT
                    {
                      use_op;
                      reason;
                      tout;
                      resolved_elem = Some l;
                      should_distribute;
                      renders_variant;
                      promote_structural_components;
                    }
                ))
        | ( DefT (_, (RendersT _ as renders)),
            PromoteRendersRepresentationT
              {
                use_op = _;
                reason;
                tout;
                resolved_elem = _;
                should_distribute = _;
                promote_structural_components = _;
                renders_variant = _;
              }
          ) ->
          rec_flow_t cx trace ~use_op:unknown_use (DefT (reason, renders), tout)
        | ( _,
            PromoteRendersRepresentationT
              {
                use_op = _;
                reason;
                tout;
                resolved_elem;
                should_distribute;
                promote_structural_components = _;
                renders_variant;
              }
          ) ->
          let result =
            let t =
              match resolved_elem with
              | Some t -> t
              | None -> l
            in
            if should_distribute then
              DefT
                ( reason,
                  RendersT (StructuralRenders { renders_variant; renders_structural_type = t })
                )
            else
              t
          in
          (* Intentional unknown_use when flowing to tout *)
          rec_flow_t cx trace ~use_op:unknown_use (result, tout)
        (******************)
        (* React GetProps *)
        (******************)

        (* props is invariant in the class *)
        | (DefT (r, ClassT _), (ReactPropsToOut (_, props) | ReactInToProps (_, props))) ->
          rec_flow_t ~use_op:unknown_use cx trace (l, ReactJs.component_class cx r props)
        (* Functions with rest params or that are predicates cannot be React components *)
        | ( DefT (reason, FunT (_, { params; rest_param = None; predicate = None; _ })),
            ReactPropsToOut (_, props)
          ) ->
          (* Contravariance *)
          Base.List.hd params
          |> Base.Option.value_map ~f:snd ~default:(Obj_type.mk ~obj_kind:Exact cx reason)
          |> fun t -> rec_flow_t ~use_op:unknown_use cx trace (t, props)
        | ( DefT
              ( reason,
                FunT
                  ( _,
                    {
                      params;
                      return_t;
                      rest_param = None;
                      predicate = None;
                      hook = NonHook | AnyHook;
                      _;
                    }
                  )
              ),
            ReactInToProps (reason_op, props)
          ) ->
          (* Contravariance *)
          Base.List.hd params
          |> Base.Option.value_map ~f:snd ~default:(Obj_type.mk ~obj_kind:Exact cx reason)
          |> fun t ->
          rec_flow_t ~use_op:unknown_use cx trace (props, t);
          if not (Context.in_implicit_instantiation cx) then
            rec_flow_t
              ~use_op:unknown_use
              cx
              trace
              (return_t, get_builtin_type cx reason_op "React$Node")
        | ( DefT (r, FunT _),
            (ReactInToProps (reason_op, props) | ReactPropsToOut (reason_op, props))
          ) ->
          ReactJs.err_incompatible cx trace ~use_op:unknown_use r (React.GetProps props);
          rec_flow_t ~use_op:unknown_use cx trace (AnyT.error reason_op, props)
        | ( DefT (r, ObjT { call_t = Some id; _ }),
            (ReactInToProps (reason_op, props) | ReactPropsToOut (reason_op, props))
          ) -> begin
          match Context.find_call cx id with
          | ( DefT (_, FunT (_, { rest_param = None; predicate = None; _ }))
            | DefT (_, PolyT { t_out = DefT (_, FunT _); _ }) ) as fun_t ->
            (* Keep the object's reason for better error reporting *)
            rec_flow cx trace (Fun.const r |> Fun.flip mod_reason_of_t fun_t, u)
          | _ ->
            ReactJs.err_incompatible cx trace ~use_op:unknown_use r (React.GetProps props);
            rec_flow_t ~use_op:unknown_use cx trace (AnyT.error reason_op, props)
        end
        | (AnyT _, ReactPropsToOut (_, props)) -> rec_flow_t ~use_op:unknown_use cx trace (l, props)
        | (AnyT _, ReactInToProps (_, props)) -> rec_flow_t ~use_op:unknown_use cx trace (props, l)
        | (DefT (r, _), (ReactPropsToOut (reason_op, props) | ReactInToProps (reason_op, props))) ->
          ReactJs.err_incompatible cx trace ~use_op:unknown_use r (React.GetProps props);
          rec_flow_t ~use_op:unknown_use cx trace (AnyT.error reason_op, props)
        | ( _,
            TryRenderTypePromotionT
              { use_op; reason; reason_obj; upper_renders; tried_promotion = false }
          ) ->
          let promotion_eval_t =
            EvalT
              ( l,
                TypeDestructorT
                  ( use_op,
                    reason,
                    ReactPromoteRendersRepresentation
                      {
                        should_distribute = true;
                        promote_structural_components = true;
                        renders_variant = RendersNormal;
                        resolved_elem = None;
                      }
                  ),
                Eval.generate_id ()
              )
          in
          rec_flow
            cx
            trace
            ( promotion_eval_t,
              TryRenderTypePromotionT
                { use_op; reason; reason_obj; upper_renders; tried_promotion = true }
            )
        (**** Renders Subtyping Cont. *****)
        (* We successfully promoted an ObjT to a Render type. Proceed with normal RendersT subtyping *)
        | ( DefT (_, RendersT _),
            TryRenderTypePromotionT
              { use_op; reason; reason_obj; upper_renders; tried_promotion = true }
          ) ->
          let l = reposition_reason cx ~trace reason_obj ~use_desc:true l in
          let renders_t = DefT (reason, RendersT upper_renders) in
          rec_flow_t cx trace ~use_op (l, renders_t)
        (* We did not successfully promote the ObjT and we have a RendersT on the RHS. We already
         * tried to do structural subtyping, so this is an error *)
        | ( _,
            TryRenderTypePromotionT
              { use_op; reason; reason_obj; upper_renders = _; tried_promotion = true }
          ) ->
          add_output
            cx
            ~trace
            (Error_message.EIncompatibleWithUseOp
               { reason_lower = reason_obj; reason_upper = reason; use_op }
            )
        (***********************************************)
        (* function types deconstruct into their parts *)
        (***********************************************)

        (* FunT ~> CallT *)
        | (DefT (_, FunT _), CallT { use_op; call_action = ConcretizeCallee tout; _ }) ->
          rec_flow_t cx trace ~use_op (l, OpenT tout)
        | ( DefT (reason_fundef, FunT (_, funtype)),
            CallT
              {
                use_op;
                reason = reason_callsite;
                call_action = Funcalltype calltype;
                return_hint = _;
              }
          ) ->
          let { this_t = (o1, _); params = _; return_t = t1; _ } = funtype in
          let {
            call_this_t = o2;
            call_targs;
            call_args_tlist = tins2;
            call_tout = t2;
            call_strict_arity;
            call_speculation_hint_state = _;
            call_kind = _;
            call_specialized_callee;
          } =
            calltype
          in
          rec_flow cx trace (o2, UseT (use_op, o1));
          CalleeRecorder.add_callee cx CalleeRecorder.All l call_specialized_callee;

          Base.Option.iter call_targs ~f:(fun _ ->
              add_output
                cx
                ~trace
                Error_message.(
                  ECallTypeArity
                    {
                      call_loc = loc_of_reason reason_callsite;
                      is_new = false;
                      reason_arity = reason_fundef;
                      expected_arity = 0;
                    }
                )
          );

          if call_strict_arity then
            multiflow_call cx trace ~use_op reason_callsite tins2 funtype
          else
            multiflow_subtype cx trace ~use_op reason_callsite tins2 funtype;

          (* flow return type of function to the tvar holding the return type of the
             call. clears the op stack because the result of the call is not the
             call itself. *)
          rec_flow_t
            ~use_op:unknown_use
            cx
            trace
            (reposition cx ~trace (loc_of_reason reason_callsite) t1, OpenT t2)
        | (AnyT _, CallT { use_op; call_action = ConcretizeCallee tout; _ }) ->
          rec_flow_t cx trace ~use_op (l, OpenT tout)
        | ( AnyT (reason_fundef, src),
            CallT
              { use_op; reason = reason_op; call_action = Funcalltype calltype; return_hint = _ }
          ) ->
          let {
            call_this_t;
            call_targs = _;
            (* An untyped receiver can't do anything with type args *)
            call_args_tlist;
            call_tout;
            call_strict_arity = _;
            call_speculation_hint_state = _;
            call_kind = _;
            call_specialized_callee;
          } =
            calltype
          in
          CalleeRecorder.add_callee cx CalleeRecorder.All l call_specialized_callee;
          let src = any_mod_src_keep_placeholder Untyped src in
          let any = AnyT.why src reason_fundef in
          rec_flow_t cx ~use_op trace (call_this_t, any);
          call_args_iter (fun t -> rec_flow cx trace (t, UseT (use_op, any))) call_args_tlist;
          rec_flow_t cx ~use_op trace (AnyT.why src reason_op, OpenT call_tout)
        | (_, ImplicitVoidReturnT { use_op; action = PropagateVoid { return; void_t }; _ }) ->
          rec_flow cx trace (void_t, UseT (use_op, return))
        | (_, ImplicitVoidReturnT { action = NoImplicitReturns r; _ }) ->
          add_output cx ~trace Error_message.(EComponentMissingReturn r)
        (* Special handlers for builtin functions *)
        | ( CustomFunT
              ( _,
                ( ObjectAssign | ObjectGetPrototypeOf | ObjectSetPrototypeOf | DebugPrint
                | DebugThrow | DebugSleep | Compose _ | ReactCreateElement | ReactCloneElement
                | ReactElementFactory _ )
              ),
            CallT { use_op; call_action = ConcretizeCallee tout; _ }
          ) ->
          rec_flow_t cx trace ~use_op (l, OpenT tout)
        | ( CustomFunT (_, ObjectAssign),
            CallT
              {
                use_op;
                reason = reason_op;
                call_action =
                  Funcalltype { call_targs = None; call_args_tlist = dest_t :: ts; call_tout; _ };
                return_hint = _;
              }
          ) ->
          let dest_t = extract_non_spread cx ~trace dest_t in
          let t = chain_objects cx ~trace reason_op dest_t ts in
          rec_flow_t cx ~use_op trace (t, OpenT call_tout)
        | ( CustomFunT (_, ObjectGetPrototypeOf),
            CallT
              {
                use_op = _;
                reason = reason_op;
                call_action =
                  Funcalltype { call_targs = None; call_args_tlist = arg :: _; call_tout; _ };
                return_hint = _;
              }
          ) ->
          let l = extract_non_spread cx ~trace arg in
          rec_flow cx trace (l, GetProtoT (reason_op, call_tout))
        | ( CustomFunT (_, ObjectSetPrototypeOf),
            CallT
              {
                use_op;
                reason = reason_op;
                call_action =
                  Funcalltype
                    { call_targs = None; call_args_tlist = arg1 :: arg2 :: _; call_tout; _ };
                return_hint = _;
              }
          ) ->
          let target = extract_non_spread cx ~trace arg1 in
          let proto = extract_non_spread cx ~trace arg2 in
          rec_flow cx trace (target, SetProtoT (reason_op, proto));
          rec_flow_t cx ~use_op trace (BoolT.why reason_op, OpenT call_tout)
        | (DefT (reason, CharSetT _), _) -> rec_flow cx trace (StrT.why reason, u)
        | (_, ReactKitT (use_op, reason_op, tool)) -> ReactJs.run cx trace ~use_op reason_op l tool
        (* Facebookisms are special Facebook-specific functions that are not
           expressable with our current type syntax, so we've hacked in special
           handling. Terminate with extreme prejudice. *)
        | ( CustomFunT (_, DebugPrint),
            CallT
              {
                use_op;
                reason = reason_op;
                call_action = Funcalltype { call_targs = None; call_args_tlist; call_tout; _ };
                return_hint = _;
              }
          ) ->
          List.iter
            (fun arg ->
              match arg with
              | Arg t -> rec_flow cx trace (t, DebugPrintT reason_op)
              | SpreadArg t ->
                add_output cx ~trace Error_message.(EUnsupportedSyntax (loc_of_t t, SpreadArgument)))
            call_args_tlist;
          rec_flow_t cx ~use_op trace (VoidT.why reason_op, OpenT call_tout)
        | ( CustomFunT (_, DebugThrow),
            CallT { use_op = _; reason = reason_op; call_action = _; return_hint = _ }
          ) ->
          raise (Error_message.EDebugThrow (loc_of_reason reason_op))
        | ( CustomFunT (_, DebugSleep),
            CallT
              {
                use_op;
                reason = reason_op;
                call_action =
                  Funcalltype { call_targs = None; call_args_tlist = arg1 :: _; call_tout; _ };
                return_hint = _;
              }
          ) ->
          let t = extract_non_spread cx ~trace arg1 in
          rec_flow cx trace (t, DebugSleepT reason_op);
          rec_flow_t cx ~use_op trace (VoidT.why reason_op, OpenT call_tout)
        | ( CustomFunT
              ( lreason,
                ( (Compose _ | ReactCreateElement | ReactCloneElement | ReactElementFactory _) as
                kind
                )
              ),
            CallT { use_op; reason = reason_op; call_action = Funcalltype calltype; return_hint }
          ) ->
          let {
            call_targs;
            call_args_tlist = args;
            call_tout = tout;
            call_this_t = _;
            call_strict_arity = _;
            call_speculation_hint_state = _;
            call_kind = _;
            call_specialized_callee;
          } =
            calltype
          in
          CalleeRecorder.add_callee cx CalleeRecorder.All l call_specialized_callee;
          (* None of the supported custom funs are polymorphic, so error here
             instead of threading targs into spread resolution. *)
          Base.Option.iter call_targs ~f:(fun _ ->
              add_output
                cx
                ~trace
                Error_message.(
                  ECallTypeArity
                    {
                      call_loc = loc_of_reason reason_op;
                      is_new = false;
                      reason_arity = lreason;
                      expected_arity = 0;
                    }
                )
          );
          let make_op_nonlocal = function
            | FunCall op -> FunCall { op with local = false }
            | FunCallMethod op -> FunCallMethod { op with local = false }
            | op -> op
          in
          let use_op = mod_root_of_use_op make_op_nonlocal use_op in
          resolve_call_list
            cx
            ~trace
            ~use_op
            reason_op
            args
            (ResolveSpreadsToCustomFunCall (mk_id (), kind, OpenT tout, return_hint))
        | ( CustomFunT (_, (ObjectAssign | ObjectGetPrototypeOf | ObjectSetPrototypeOf)),
            MethodT (use_op, reason_call, _, Named { name = OrdinaryName "call"; _ }, action)
          ) ->
          apply_method_action cx trace l use_op reason_call l action
        (* Custom functions are still functions, so they have all the prototype properties *)
        | (CustomFunT (reason, _), MethodT (use_op, call_r, lookup_r, propref, action)) ->
          let method_type =
            Tvar.mk_no_wrap_where cx lookup_r (fun tout ->
                let u =
                  GetPropT
                    {
                      use_op;
                      reason = lookup_r;
                      id = None;
                      from_annot = false;
                      propref;
                      tout;
                      hint = hint_unavailable;
                    }
                in
                rec_flow cx trace (FunProtoT reason, u)
            )
          in
          apply_method_action cx trace method_type use_op call_r l action
        | (CustomFunT (r, _), _) when function_like_op u -> rec_flow cx trace (FunProtoT r, u)
        (****************************************)
        (* You can cast an object to a function *)
        (****************************************)
        | ( DefT (reason, (ObjT _ | InstanceT _)),
            CallT { use_op; reason = reason_op; call_action = _; return_hint = _ }
          ) ->
          let prop_name = Some (OrdinaryName "$call") in
          let fun_t =
            match l with
            | DefT (_, ObjT { call_t = Some id; _ })
            | DefT (_, InstanceT { inst = { inst_call_t = Some id; _ }; _ }) ->
              Context.find_call cx id
            | _ ->
              let reason_prop = replace_desc_reason (RProperty prop_name) reason_op in
              let error_message =
                Error_message.EPropNotFound
                  { reason_prop; reason_obj = reason; prop_name; use_op; suggestion = None }
              in
              add_output cx ~trace error_message;
              AnyT.error reason_op
          in
          rec_flow cx trace (reposition cx ~trace (loc_of_reason reason) fun_t, u)
        | (AnyT (_, src), ObjTestT (reason_op, _, u)) ->
          rec_flow_t ~use_op:unknown_use cx trace (AnyT.why src reason_op, u)
        | (_, ObjTestT (reason_op, default, u)) ->
          let u = ReposLowerT (reason_op, false, UseT (unknown_use, u)) in
          if object_like l then
            rec_flow cx trace (l, u)
          else
            rec_flow cx trace (default, u)
        | (AnyT (_, src), ObjTestProtoT (reason_op, u)) ->
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.why src reason_op, u)
        | (DefT (_, NullT), ObjTestProtoT (reason_op, u)) ->
          rec_flow_t cx trace ~use_op:unknown_use (NullProtoT.why reason_op, u)
        | (_, ObjTestProtoT (reason_op, u)) ->
          let proto =
            if object_like l then
              reposition cx ~trace (loc_of_reason reason_op) l
            else
              let () =
                add_output
                  cx
                  ~trace
                  (Error_message.EInvalidPrototype (loc_of_reason reason_op, reason_of_t l))
              in
              ObjProtoT.why reason_op
          in
          rec_flow_t cx trace ~use_op:unknown_use (proto, u)
        (**************************************************)
        (* instances of classes follow declared hierarchy *)
        (**************************************************)
        | ( DefT (reason, InstanceT { super; implements; inst; static }),
            ExtendsUseT
              ( use_op,
                reason_op,
                try_ts_on_failure,
                l,
                ( DefT
                    ( reason_u,
                      InstanceT
                        {
                          inst = inst_super;
                          static = static_super;
                          super = super_super;
                          implements = _super_impls;
                        }
                    ) as u
                )
              )
          ) ->
          if is_same_instance_type inst inst_super then
            let { type_args = tmap1; _ } = inst in
            let { type_args = tmap2; _ } = inst_super in
            let ureason =
              update_desc_reason
                (function
                  | RExtends desc -> desc
                  | desc -> desc)
                reason_op
            in
            flow_type_args cx trace ~use_op reason ureason tmap1 tmap2
          else if
            (* We are subtyping a class from platform-specific impl file against the interface file *)
            TypeUtil.nominal_id_have_same_logical_module
              ~file_options:Context.((metadata cx).file_options)
              (inst.class_id, inst.class_name)
              (inst_super.class_id, inst_super.class_name)
            && List.length inst.type_args = List.length inst_super.type_args
          then (
            let implements_use_op =
              Op (ClassImplementsCheck { def = reason; name = reason; implements = reason_u })
            in
            (* We need to ensure that the shape of the class instances match. *)
            let inst_type_to_obj_type reason inst =
              inst_type_to_obj_type
                cx
                reason
                (inst.own_props, inst.proto_props, inst.inst_call_t, inst.inst_dict)
            in
            rec_unify
              cx
              trace
              ~use_op:implements_use_op
              (inst_type_to_obj_type reason inst)
              (inst_type_to_obj_type reason_u inst_super);
            (* We need to ensure that the shape of the class statics match. *)
            let spread_of reason t =
              (* Spread to keep only own props and own methods. *)
              let id = Eval.generate_id () in
              let destructor = SpreadType (Object.Spread.Annot { make_exact = false }, [], None) in
              mk_possibly_evaluated_destructor cx unknown_use reason t destructor id
            in
            rec_unify
              cx
              trace
              ~use_op:implements_use_op
              (spread_of reason static)
              (spread_of reason_u static_super);
            (* We need to ensure that the classes have the same nominal hierarchy *)
            rec_flow_t cx trace ~use_op (super, super_super);
            (* We need to ensure that the classes have the matching targs *)
            flow_type_args cx trace ~use_op reason reason_u inst.type_args inst_super.type_args
          ) else
            (* If this instance type has declared implementations, any structural
               tests have already been performed at the declaration site. We can
               then use the ExtendsT use type to search for a nominally matching
               implementation, thereby short-circuiting a potentially expensive
               structural test at the use site. *)
            let u = ExtendsUseT (use_op, reason_op, try_ts_on_failure @ implements, l, u) in
            rec_flow cx trace (super, ReposLowerT (reason, false, u))
        (*********************************************************)
        (* class types derive instance types (with constructors) *)
        (*********************************************************)
        | ( DefT (reason, ClassT this),
            ConstructorT { use_op; reason = reason_op; targs; args; tout = t; return_hint }
          ) ->
          let reason_o = replace_desc_reason RConstructorVoidReturn reason in
          let annot_loc = loc_of_reason reason_op in
          (* early error if type args passed to non-polymorphic class *)
          Base.Option.iter targs ~f:(fun _ ->
              add_output
                cx
                ~trace
                Error_message.(
                  ECallTypeArity
                    {
                      call_loc = annot_loc;
                      is_new = true;
                      reason_arity = reason_of_t this;
                      expected_arity = 0;
                    }
                )
          );
          (* call this.constructor(args) *)
          let ret =
            Tvar.mk_no_wrap_where cx reason_op (fun t ->
                let funtype = mk_methodcalltype None args t in
                let propref = mk_named_prop ~reason:reason_o (OrdinaryName "constructor") in
                rec_flow
                  cx
                  trace
                  ( this,
                    MethodT
                      ( use_op,
                        reason_op,
                        reason_o,
                        propref,
                        (* TODO(jmbrown) return_hint threading unblocked by ConstructorT *)
                        CallM { methodcalltype = funtype; return_hint; specialized_callee = None }
                      )
                  )
            )
          in
          (* return this *)
          rec_flow cx trace (ret, ObjTestT (annot_reason ~annot_loc reason_op, this, t))
        | ( AnyT (_, src),
            ConstructorT { use_op; reason = reason_op; targs; args; tout = t; return_hint = _ }
          ) ->
          ignore targs;

          let src = any_mod_src_keep_placeholder Untyped src in
          (* An untyped receiver can't do anything with type args *)
          call_args_iter
            (fun t -> rec_flow cx trace (t, UseT (use_op, AnyT.why src reason_op)))
            args;
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.why src reason_op, t)
        (* Only classes (and `any`) can be constructed. *)
        | ( _,
            ConstructorT
              { use_op; reason = reason_op; tout = t; args = _; targs = _; return_hint = _ }
          ) ->
          add_output cx ~trace Error_message.(EInvalidConstructor (reason_of_t l));
          rec_flow_t cx trace ~use_op (AnyT.error reason_op, t)
        (* Since we don't know the signature of a method on AnyT, assume every
           parameter is an AnyT. *)
        | (AnyT (_, src), MethodT (_, _, _, propref, NoMethodAction prop_t)) ->
          let src = any_mod_src_keep_placeholder Untyped src in
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.why src (reason_of_propref propref), prop_t)
        | ( AnyT (_, src),
            MethodT
              ( use_op,
                reason_op,
                _,
                _,
                CallM
                  {
                    methodcalltype = { meth_args_tlist; meth_tout; _ };
                    return_hint = _;
                    specialized_callee;
                  }
              )
          ) ->
          let src = any_mod_src_keep_placeholder Untyped src in
          let any = AnyT.why src reason_op in
          call_args_iter (fun t -> rec_flow cx trace (t, UseT (use_op, any))) meth_args_tlist;
          CalleeRecorder.add_callee cx CalleeRecorder.Tast l specialized_callee;
          rec_flow_t cx trace ~use_op:unknown_use (any, OpenT meth_tout)
        | (AnyT (_, src), MethodT (use_op, reason_op, _, _, (ChainM _ as chain))) ->
          let src = any_mod_src_keep_placeholder Untyped src in
          let any = AnyT.why src reason_op in
          apply_method_action cx trace any use_op reason_op l chain
        (*************************)
        (* statics can be read   *)
        (*************************)
        | (DefT (_, InstanceT { static; _ }), GetStaticsT ((reason_op, _) as tout)) ->
          rec_flow cx trace (static, ReposLowerT (reason_op, false, UseT (unknown_use, OpenT tout)))
        | (AnyT (_, src), GetStaticsT ((reason_op, _) as tout)) ->
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.why src reason_op, OpenT tout)
        | (ObjProtoT _, GetStaticsT ((reason_op, _) as tout)) ->
          (* ObjProtoT not only serves as the instance type of the root class, but
             also as the statics of the root class. *)
          rec_flow cx trace (l, ReposLowerT (reason_op, false, UseT (unknown_use, OpenT tout)))
        (********************)
        (* __proto__ getter *)
        (********************)

        (* TODO: Fix GetProtoT for InstanceT (and ClassT).
           The __proto__ object of an instance is an ObjT having the properties in
           insttype.methods_tmap, not the super instance. *)
        | (DefT (_, InstanceT { super; _ }), GetProtoT (reason_op, t)) ->
          let proto = reposition cx ~trace (loc_of_reason reason_op) super in
          rec_flow_t cx trace ~use_op:unknown_use (proto, OpenT t)
        | (DefT (_, ObjT { proto_t; _ }), GetProtoT (reason_op, t)) ->
          let proto = reposition cx ~trace (loc_of_reason reason_op) proto_t in
          rec_flow_t cx trace ~use_op:unknown_use (proto, OpenT t)
        | (ObjProtoT _, GetProtoT (reason_op, t)) ->
          let proto = NullT.why reason_op in
          rec_flow_t cx trace ~use_op:unknown_use (proto, OpenT t)
        | (FunProtoT reason, GetProtoT (reason_op, t)) ->
          let proto = ObjProtoT (repos_reason (loc_of_reason reason_op) reason) in
          rec_flow_t cx trace ~use_op:unknown_use (proto, OpenT t)
        | (AnyT (_, src), GetProtoT (reason_op, t)) ->
          let src = any_mod_src_keep_placeholder Untyped src in
          let proto = AnyT.why src reason_op in
          rec_flow_t cx trace ~use_op:unknown_use (proto, OpenT t)
        (********************)
        (* __proto__ setter *)
        (********************)
        | (AnyT _, SetProtoT _) -> ()
        | (_, SetProtoT (reason_op, _)) ->
          add_output cx ~trace (Error_message.EUnsupportedSetProto reason_op)
        (********************************************************)
        (* instances of classes may have their fields looked up *)
        (********************************************************)
        | ( DefT (lreason, InstanceT { super; inst; _ }),
            LookupT
              {
                reason = reason_op;
                lookup_kind = kind;
                try_ts_on_failure;
                propref;
                lookup_action = action;
                ids;
                method_accessible;
                ignore_dicts;
              }
          ) ->
          let use_op = use_op_of_lookup_action action in
          (match
             GetPropTKit.get_instance_prop cx trace ~use_op ~ignore_dicts inst propref reason_op
           with
          | Some (p, target_kind) ->
            let p =
              check_method_unbinding cx trace ~use_op ~method_accessible ~reason_op ~propref p
            in
            (match kind with
            | NonstrictReturning (_, Some (id, _)) -> Context.test_prop_hit cx id
            | _ -> ());
            perform_lookup_action
              cx
              trace
              propref
              (Property.type_ p)
              target_kind
              lreason
              reason_op
              action
          | None ->
            rec_flow
              cx
              trace
              ( super,
                LookupT
                  {
                    reason = reason_op;
                    lookup_kind = kind;
                    try_ts_on_failure;
                    propref;
                    lookup_action = action;
                    method_accessible;
                    ids =
                      Base.Option.map ids ~f:(fun ids ->
                          if
                            Properties.Set.mem inst.own_props ids
                            || Properties.Set.mem inst.proto_props ids
                          then
                            ids
                          else
                            Properties.Set.add inst.own_props ids
                            |> Properties.Set.add inst.proto_props
                      );
                    ignore_dicts;
                  }
              ))
        (********************************)
        (* ... and their fields written *)
        (********************************)
        | ( DefT (reason_instance, InstanceT _),
            SetPropT (use_op, reason_op, propref, mode, write_ctx, tin, prop_tout)
          ) ->
          let lookup_action = WriteProp { use_op; obj_t = l; prop_tout; tin; write_ctx; mode } in
          let method_accessible = true in
          let lookup_kind =
            instance_lookup_kind
              cx
              trace
              ~reason_instance
              ~reason_op
              ~method_accessible
              l
              propref
              lookup_action
          in
          rec_flow
            cx
            trace
            ( l,
              LookupT
                {
                  reason = reason_op;
                  lookup_kind;
                  try_ts_on_failure = [];
                  propref;
                  lookup_action;
                  ids = None;
                  method_accessible;
                  ignore_dicts = true;
                }
            )
        | (DefT (reason_c, InstanceT _), SetPrivatePropT (use_op, reason_op, x, _, [], _, _, _, _))
          ->
          add_output
            cx
            ~trace
            (Error_message.EPrivateLookupFailed ((reason_op, reason_c), OrdinaryName x, use_op))
        | ( DefT (reason_c, InstanceT { inst; _ }),
            SetPrivatePropT
              (use_op, reason_op, x, mode, scope :: scopes, static, write_ctx, tin, prop_tout)
          ) ->
          if not (ALoc.equal_id scope.class_binding_id inst.class_id) then
            rec_flow
              cx
              trace
              ( l,
                SetPrivatePropT
                  (use_op, reason_op, x, mode, scopes, static, write_ctx, tin, prop_tout)
              )
          else
            let map =
              if static then
                inst.class_private_static_fields
              else
                inst.class_private_fields
            in
            let name = OrdinaryName x in
            (match NameUtils.Map.find_opt name (Context.find_props cx map) with
            | None ->
              add_output
                cx
                ~trace
                (Error_message.EPrivateLookupFailed ((reason_op, reason_c), name, use_op))
            | Some p ->
              let action = WriteProp { use_op; obj_t = l; prop_tout; tin; write_ctx; mode } in
              let propref = mk_named_prop ~reason:reason_op name in
              perform_lookup_action
                cx
                trace
                propref
                (Property.type_ p)
                PropertyMapProperty
                reason_c
                reason_op
                action)
        (*****************************)
        (* ... and their fields read *)
        (*****************************)
        | ( DefT (r, InstanceT _),
            GetPropT { propref = Named { name = OrdinaryName "constructor"; _ }; tout; hint = _; _ }
          ) ->
          let t = TypeUtil.class_type ?annot_loc:(annot_loc_of_reason r) l in
          rec_flow_t cx trace ~use_op:unknown_use (t, OpenT tout)
        | ( DefT (reason_instance, InstanceT { super; inst; _ }),
            GetPropT { use_op; reason = reason_op; from_annot; id; propref; tout; hint = _ }
          ) ->
          let method_accessible = from_annot in
          let lookup_action = ReadProp { use_op; obj_t = l; tout } in
          let lookup_kind =
            instance_lookup_kind
              cx
              trace
              ~reason_instance
              ~reason_op
              ~method_accessible
              l
              propref
              lookup_action
          in
          GetPropTKit.read_instance_prop
            cx
            trace
            ~use_op
            ~instance_t:l
            ~id
            ~method_accessible
            ~super
            ~lookup_kind
            inst
            propref
            reason_op
            tout
        | ( DefT (reason_c, InstanceT { inst; _ }),
            GetPrivatePropT (use_op, reason_op, prop_name, scopes, static, tout)
          ) ->
          get_private_prop
            ~cx
            ~allow_method_access:false
            ~trace
            ~l
            ~reason_c
            ~instance:inst
            ~use_op
            ~reason_op
            ~prop_name
            ~scopes
            ~static
            ~tout
        (********************************)
        (* ... and their methods called *)
        (********************************)
        | ( DefT (reason_instance, InstanceT { super; inst; _ }),
            MethodT (use_op, reason_call, reason_lookup, propref, action)
          ) ->
          let funt =
            Tvar.mk_no_wrap_where cx reason_lookup (fun tout ->
                let lookup_action = ReadProp { use_op; obj_t = l; tout } in
                let method_accessible = true in
                let lookup_kind =
                  instance_lookup_kind
                    cx
                    trace
                    ~reason_instance
                    ~reason_op:reason_lookup
                    ~method_accessible
                    l
                    propref
                    lookup_action
                in
                GetPropTKit.read_instance_prop
                  cx
                  trace
                  ~use_op
                  ~instance_t:l
                  ~id:None
                  ~method_accessible:true
                  ~super
                  ~lookup_kind
                  inst
                  propref
                  reason_call
                  tout
            )
          in
          (* suppress ops while calling the function. if `funt` is a `FunT`, then
             `CallT` will set its own ops during the call. if `funt` is something
             else, then something like `VoidT ~> CallT` doesn't need the op either
             because we want to point at the call and undefined thing. *)
          apply_method_action cx trace funt use_op reason_call l action
        | ( DefT (reason_c, InstanceT { inst; _ }),
            PrivateMethodT
              (use_op, reason_op, reason_lookup, prop_name, scopes, static, method_action)
          ) ->
          let tvar = Tvar.mk_no_wrap cx reason_lookup in
          let funt = OpenT (reason_lookup, tvar) in
          let l =
            if static then
              TypeUtil.class_type l
            else
              l
          in
          get_private_prop
            ~cx
            ~allow_method_access:true
            ~trace
            ~l
            ~reason_c
            ~instance:inst
            ~use_op
            ~reason_op
            ~prop_name
            ~scopes
            ~static
            ~tout:(reason_lookup, tvar);
          apply_method_action cx trace funt use_op reason_op l method_action
        (*
           In traditional type systems, object types are not extensible.  E.g., an
           object {x: 0, y: ""} has type {x: number; y: string}. While it is
           possible to narrow the object's type to hide some of its properties (aka
           width subtyping), extending its type to model new properties is
           impossible. This is not without reason: all object types would then be
           equatable via subtyping, thereby making them unsound.

           In JavaScript, on the other hand, objects can grow dynamically, and
           doing so is a common idiom during initialization (i.e., before they
           become available for general use). Objects that typically grow
           dynamically include not only object literals, but also prototypes,
           export objects, and so on. Thus, it is important to model this idiom.

           To balance utility and soundness, Flow's object types are extensible by
           default, but become sealed as soon as they are subject to width
           subtyping. However, implementing this simple idea needs a lot of care.

           To ensure that aliases have the same underlying type, object types are
           represented indirectly as pointers to records (rather than directly as
           records). And to ensure that typing is independent of the order in which
           fragments of code are analyzed, new property types can be added on gets
           as well as sets (and due to indirection, the new property types become
           immediately available to aliases).

           Looking up properties of an object, e.g. for the purposes of copying,
           when it is not fully initialized is prone to races, and requires careful
           manual reasoning about escape to avoid surprising results.

           Prototypes cause further complications. In JavaScript, objects inherit
           properties of their prototypes, and may override those properties. (This
           is similar to subclasses inheriting and overriding methods of
           superclasses.) At the same time, prototypes are extensible just as much
           as the objects they derive are. In other words, we want to maintain the
           invariant that an object's type is a subtype of its prototype's type,
           while letting them be extensible by default. This invariant is achieved
           by constraints that unify a property's type if and when that property
           exists both on the object and its prototype.

           Here's some example code with type calculations in comments. (We use the
           symbol >=> to denote a flow between a pair of types. The direction of
           flow roughly matches the pattern 'rvalue' >=> 'lvalue'.)

              var o = {}; // o:T, UseT |-> {}
              o.x = 4; // UseT |-> {x:X}, number >=> X
              var s:string = o.x; // ERROR: number >=> string

              function F() { } // F.prototype:P, P |-> {}
              var f = new F(); // f:O, O |-> {}&P

              F.prototype.m = function() { this.y = 4; } // P |-> {m:M}, ... >=> M
              f.m(); // O |-> {y:Y}&P, number >=> Y

        *)
        (**********************************************************************)
        (* objects can be assigned, i.e., their properties can be set in bulk *)
        (**********************************************************************)

        (* Special case any. Otherwise this will lead to confusing errors when any tranforms to an
           object type. *)
        | (AnyT _, ObjAssignToT (use_op, _, _, t, _)) -> rec_flow_t cx ~use_op trace (l, t)
        | (to_obj, ObjAssignToT (use_op, reason, from_obj, t, kind)) ->
          rec_flow cx trace (from_obj, ObjAssignFromT (use_op, reason, to_obj, t, kind))
        (* When some object-like type O1 flows to
           ObjAssignFromT(_,O2,X,ObjAssign), the properties of O1 are copied to
           O2, and O2 is linked to X to signal that the copying is done; the
           intention is that when those properties are read through X, they should
           be found (whereas this cannot be guaranteed when those properties are
           read through O2). However, there is an additional twist: this scheme
           may not work when O2 is unresolved. In particular, when O2 is
           unresolved, the constraints that copy the properties from O1 may race
           with reads of those properties through X as soon as O2 is resolved. To
           avoid this race, we make O2 flow to ObjAssignToT(_,O1,X,ObjAssign);
           when O2 is resolved, we make the switch. **)
        | ( DefT (lreason, ObjT { props_tmap = mapr; flags; _ }),
            ObjAssignFromT (use_op, reason_op, to_obj, t, ObjAssign _)
          ) ->
          Context.iter_props cx mapr (fun name p ->
              (* move the reason to the call site instead of the definition, so
                 that it is in the same scope as the Object.assign, so that
                 strictness rules apply. *)
              let reason_prop =
                lreason
                |> update_desc_reason (fun desc -> RPropertyOf (name, desc))
                |> repos_reason (loc_of_reason reason_op)
              in
              match Property.read_t p with
              | Some t ->
                let propref = mk_named_prop ~reason:reason_prop name in
                let t = filter_optional cx ~trace reason_prop t in
                rec_flow
                  cx
                  trace
                  ( to_obj,
                    SetPropT
                      (use_op, reason_prop, propref, Assign, Normal, OpenT (reason_prop, t), None)
                  )
              | None ->
                add_output
                  cx
                  ~trace
                  (Error_message.EPropNotReadable { reason_prop; prop_name = Some name; use_op })
          );
          (match flags.obj_kind with
          | Indexed _ -> rec_flow_t cx trace ~use_op (AnyT.make Untyped reason_op, t)
          | Exact
          | Inexact ->
            rec_flow_t cx trace ~use_op (to_obj, t))
        | ( DefT (lreason, InstanceT { inst = { own_props; proto_props; _ }; _ }),
            ObjAssignFromT (use_op, reason_op, to_obj, t, ObjAssign _)
          ) ->
          let own_props = Context.find_props cx own_props in
          let proto_props = Context.find_props cx proto_props in
          let props = NameUtils.Map.union own_props proto_props in
          props
          |> NameUtils.Map.iter (fun name p ->
                 match Property.read_t p with
                 | Some t ->
                   let propref = mk_named_prop ~reason:reason_op name in
                   rec_flow
                     cx
                     trace
                     (to_obj, SetPropT (use_op, reason_op, propref, Assign, Normal, t, None))
                 | None ->
                   add_output
                     cx
                     ~trace
                     (Error_message.EPropNotReadable
                        { reason_prop = lreason; prop_name = Some name; use_op }
                     )
             );
          rec_flow_t cx ~use_op trace (to_obj, t)
        (* AnyT has every prop, each one typed as `any`, so spreading it into an
           existing object destroys all of the keys, turning the result into an
           AnyT as well. TODO: wait for `to_obj` to be resolved, and then call
           `SetPropT (_, _, _, AnyT, _)` on all of its props. *)
        | (AnyT (_, src), ObjAssignFromT (use_op, reason, _, t, ObjAssign _)) ->
          rec_flow_t cx ~use_op trace (AnyT.make src reason, t)
        | (AnyT _, ObjAssignFromT (use_op, _, _, t, _)) -> rec_flow_t cx ~use_op trace (l, t)
        | (ObjProtoT _, ObjAssignFromT (use_op, _, to_obj, t, ObjAssign _)) ->
          rec_flow_t cx ~use_op trace (to_obj, t)
        (* Object.assign semantics *)
        | (DefT (_, (NullT | VoidT)), ObjAssignFromT (use_op, _, to_obj, tout, ObjAssign _)) ->
          rec_flow_t cx ~use_op trace (to_obj, tout)
        (* {...mixed} is the equivalent of {...{[string]: mixed}} *)
        | (DefT (reason, MixedT _), ObjAssignFromT (_, _, _, _, ObjAssign _)) ->
          let dict =
            {
              dict_name = None;
              key = StrT.make reason;
              value = l;
              dict_polarity = Polarity.Neutral;
            }
          in
          let o = Obj_type.mk_with_proto cx reason (ObjProtoT reason) ~obj_kind:(Indexed dict) in
          rec_flow cx trace (o, u)
        | (DefT (reason_arr, ArrT arrtype), ObjAssignFromT (use_op, r, o, t, ObjSpreadAssign)) ->
        begin
          match arrtype with
          | ArrayAT { elem_t; tuple_view = None; react_dro = _ }
          | ROArrayAT (elem_t, _) ->
            (* Object.assign(o, ...Array<x>) -> Object.assign(o, x) *)
            rec_flow cx trace (elem_t, ObjAssignFromT (use_op, r, o, t, default_obj_assign_kind))
          | TupleAT { elements; _ } ->
            (* Object.assign(o, ...[x,y,z]) -> Object.assign(o, x, y, z) *)
            List.iteri
              (fun n (TupleElement { t = from; polarity; name; optional = _; reason = _ }) ->
                if not @@ Polarity.compat (polarity, Polarity.Positive) then
                  add_output
                    cx
                    ~trace
                    (Error_message.ETupleElementNotReadable
                       { use_op; reason = reason_arr; index = n; name }
                    );
                rec_flow cx trace (from, ObjAssignFromT (use_op, r, o, t, default_obj_assign_kind)))
              elements
          | ArrayAT { tuple_view = Some (elements, _arity); _ } ->
            (* Object.assign(o, ...[x,y,z]) -> Object.assign(o, x, y, z) *)
            let ts = tuple_ts_of_elements elements in
            List.iter
              (fun from ->
                rec_flow cx trace (from, ObjAssignFromT (use_op, r, o, t, default_obj_assign_kind)))
              ts
        end
        (*************************)
        (* objects can be copied *)
        (*************************)
        | ( DefT (reason_obj, ObjT { props_tmap; flags = { obj_kind; _ }; reachable_targs; _ }),
            ObjRestT (reason_op, xs, t, id)
          ) ->
          ConstFoldExpansion.guard cx id (reason_obj, 0) (function
              | 0 ->
                let o =
                  Flow_js_utils.objt_to_obj_rest
                    cx
                    props_tmap
                    ~reachable_targs
                    ~obj_kind
                    ~reason_op
                    ~reason_obj
                    xs
                in
                rec_flow_t cx trace ~use_op:unknown_use (o, t)
              | _ -> ()
              )
        | (DefT (reason, InstanceT { super; inst; _ }), ObjRestT (reason_op, xs, t, _)) ->
          (* Spread fields from super into an object *)
          let obj_super =
            Tvar.mk_where cx reason_op (fun tvar ->
                let u = ObjRestT (reason_op, xs, tvar, Reason.mk_id ()) in
                rec_flow cx trace (super, ReposLowerT (reason, false, u))
            )
          in
          let o =
            Flow_js_utils.objt_to_obj_rest
              cx
              inst.own_props
              ~reachable_targs:[]
              ~obj_kind:Exact
              ~reason_op
              ~reason_obj:reason
              xs
          in
          (* Combine super and own props. *)
          let use_op = Op (ObjectSpread { op = reason_op }) in
          let spread_tool = Object.Resolve Object.Next in
          let spread_target = Object.Spread.Value { make_seal = Obj_type.mk_seal ~frozen:false } in
          let spread_state =
            {
              Object.Spread.todo_rev = [Object.Spread.Type o];
              acc = [];
              spread_id = Reason.mk_id ();
              union_reason = None;
              curr_resolve_idx = 0;
            }
          in
          let o =
            Tvar.mk_where cx reason_op (fun tvar ->
                rec_flow
                  cx
                  trace
                  ( obj_super,
                    ObjKitT
                      ( use_op,
                        reason_op,
                        spread_tool,
                        Type.Object.Spread (spread_target, spread_state),
                        tvar
                      )
                  )
            )
          in
          rec_flow_t cx ~use_op trace (o, t)
        | (AnyT (_, src), ObjRestT (reason, _, t, _)) ->
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.why src reason, t)
        | (ObjProtoT _, ObjRestT (reason, _, t, _)) ->
          let obj = Obj_type.mk_with_proto cx reason ~obj_kind:Exact l in
          rec_flow_t cx trace ~use_op:unknown_use (obj, t)
        | (DefT (_, (NullT | VoidT)), ObjRestT (reason, _, t, _)) ->
          let o = Obj_type.mk ~obj_kind:Exact cx reason in
          rec_flow_t cx trace ~use_op:unknown_use (o, t)
        (*******************************************)
        (* objects may have their fields looked up *)
        (*******************************************)
        | ( DefT (reason_obj, ObjT o),
            LookupT
              {
                reason = reason_op;
                lookup_kind;
                try_ts_on_failure;
                propref;
                lookup_action = action;
                ids;
                method_accessible;
                ignore_dicts;
              }
          ) ->
          (match GetPropTKit.get_obj_prop cx trace o propref reason_op with
          | Some (p, target_kind) ->
            (match lookup_kind with
            | NonstrictReturning (_, Some (id, _)) -> Context.test_prop_hit cx id
            | _ -> ());
            perform_lookup_action cx trace propref p target_kind reason_obj reason_op action
          | None ->
            rec_flow
              cx
              trace
              ( o.proto_t,
                LookupT
                  {
                    reason = reason_op;
                    lookup_kind;
                    try_ts_on_failure;
                    propref;
                    lookup_action = action;
                    method_accessible;
                    ids = Base.Option.map ids ~f:(Properties.Set.add o.props_tmap);
                    ignore_dicts;
                  }
              ))
        | ( AnyT (reason, src),
            LookupT
              {
                reason = reason_op;
                lookup_kind;
                try_ts_on_failure = _;
                propref;
                lookup_action = action;
                ids = _;
                method_accessible = _;
                ignore_dicts = _;
              }
          ) ->
          (match action with
          | SuperProp (_, lp) when Property.write_t_of_property_type lp = None ->
            (* Without this exception, we will call rec_flow_p where
             * `write_t lp = None` and `write_t up = Some`, which is a polarity
             * mismatch error. Instead of this, we could "read" `mixed` from
             * covariant props, which would always flow into `any`. *)
            ()
          | _ ->
            let src = any_mod_src_keep_placeholder Untyped src in
            let p = OrdinaryField { type_ = AnyT.why src reason_op; polarity = Polarity.Neutral } in
            (match lookup_kind with
            | NonstrictReturning (_, Some (id, _)) -> Context.test_prop_hit cx id
            | _ -> ());
            perform_lookup_action cx trace propref p DynamicProperty reason reason_op action)
        (*****************************************)
        (* ... and their fields written *)
        (*****************************************)
        | ( DefT (_, ObjT { flags; _ }),
            SetPropT
              ( use_op,
                _,
                Named { reason = reason_prop; name = OrdinaryName "constructor"; _ },
                _,
                _,
                _,
                _
              )
          ) ->
          if obj_is_readonlyish flags then
            let use_op =
              match flags.react_dro with
              | Some dro -> Frame (ReactDeepReadOnly dro, use_op)
              | None -> use_op
            in
            add_output
              cx
              ~trace
              (Error_message.EPropNotWritable
                 { reason_prop; prop_name = Some (OrdinaryName "constructor"); use_op }
              )
        (* o.x = ... has the additional effect of o[_] = ... **)
        | (DefT (_, ObjT { flags; _ }), SetPropT (use_op, _, propref, _, _, _, _))
          when obj_is_readonlyish flags && not (is_exception_to_react_dro propref) ->
          let reason_prop = reason_of_propref propref in
          let prop_name = name_of_propref propref in
          let use_op =
            match flags.react_dro with
            | Some dro -> Frame (ReactDeepReadOnly dro, use_op)
            | None -> use_op
          in
          add_output cx ~trace (Error_message.EPropNotWritable { reason_prop; prop_name; use_op })
        | (DefT (reason_obj, ObjT o), SetPropT (use_op, reason_op, propref, mode, _, tin, prop_t))
          ->
          write_obj_prop cx trace ~use_op ~mode o propref reason_obj reason_op tin prop_t
        (* Since we don't know the type of the prop, use AnyT. *)
        | (AnyT (_, src), SetPropT (use_op, reason_op, _, _, _, t, prop_t)) ->
          let src = any_mod_src_keep_placeholder Untyped src in
          Base.Option.iter
            ~f:(fun t -> rec_flow_t cx trace ~use_op:unknown_use (AnyT.why src reason_op, t))
            prop_t;
          rec_flow cx trace (t, UseT (use_op, AnyT.why src reason_op))
        (*****************************)
        (* ... and their fields read *)
        (*****************************)
        | ( DefT (_, ObjT _),
            GetPropT { reason; propref = Named { name = OrdinaryName "constructor"; _ }; tout; _ }
          ) ->
          rec_flow_t cx trace ~use_op:unknown_use (Unsoundness.why Constructor reason, OpenT tout)
        | ( DefT (reason_obj, ObjT o),
            GetPropT { use_op; reason = reason_op; id; from_annot = _; propref; tout; hint = _ }
          ) ->
          let lookup_info =
            Base.Option.map id ~f:(fun id ->
                let lookup_default_tout =
                  Tvar.mk_where cx reason_op (fun tvar ->
                      rec_flow_t ~use_op cx trace (tvar, OpenT tout)
                  )
                in
                (id, lookup_default_tout)
            )
          in
          GetPropTKit.read_obj_prop cx trace ~use_op o propref reason_obj reason_op lookup_info tout
        | ( AnyT (_, src),
            GetPropT { use_op = _; reason; id; from_annot = _; propref = _; tout; hint = _ }
          ) ->
          Base.Option.iter id ~f:(Context.test_prop_hit cx);
          let src = any_mod_src_keep_placeholder Untyped src in
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.why src reason, OpenT tout)
        (********************************)
        (* ... and their methods called *)
        (********************************)
        | ( DefT (_, ObjT _),
            MethodT (_, reason_call, _, Named { name = OrdinaryName "constructor"; _ }, action)
          ) ->
          add_specialized_callee_method_action cx trace (AnyT.untyped reason_call) action
        | (DefT (reason_obj, ObjT o), MethodT (use_op, reason_call, reason_lookup, propref, action))
          ->
          let t =
            Tvar.mk_no_wrap_where cx reason_lookup (fun tout ->
                GetPropTKit.read_obj_prop
                  cx
                  trace
                  ~use_op
                  o
                  propref
                  reason_obj
                  reason_lookup
                  None
                  tout
            )
          in
          apply_method_action cx trace t use_op reason_call l action
        (******************************************)
        (* strings may have their characters read *)
        (******************************************)
        | ( DefT (reason_s, StrT _),
            GetElemT
              {
                use_op;
                reason = reason_op;
                id = _;
                from_annot = _;
                access_iterables = _;
                key_t;
                tout;
              }
          ) ->
          rec_flow cx trace (key_t, UseT (use_op, NumT.why reason_s));
          rec_flow_t cx trace ~use_op:unknown_use (StrT.why reason_op, OpenT tout)
        (* Expressions may be used as keys to access objects and arrays. In
           general, we cannot evaluate such expressions at compile time. However,
           in some idiomatic special cases, we can; in such cases, we know exactly
           which strings/numbers the keys may be, and thus, we can use precise
           properties and indices to resolve the accesses. *)
        (**********************************************************************)
        (* objects/arrays may have their properties/elements written and read *)
        (**********************************************************************)
        | ( (DefT (_, (ObjT _ | ArrT _ | InstanceT _)) | AnyT _),
            SetElemT (use_op, reason_op, key, mode, tin, tout)
          ) ->
          rec_flow cx trace (key, ElemT (use_op, reason_op, l, WriteElem { tin; tout; mode }))
        | ( (DefT (_, (ObjT _ | ArrT _ | InstanceT _)) | AnyT _),
            GetElemT { use_op; reason = reason_op; id; from_annot; access_iterables; key_t; tout }
          ) ->
          rec_flow
            cx
            trace
            ( key_t,
              ElemT (use_op, reason_op, l, ReadElem { id; from_annot; access_iterables; tout })
            )
        | ( (DefT (_, (ObjT _ | ArrT _ | InstanceT _)) | AnyT _),
            CallElemT (use_op, reason_call, reason_lookup, key, action)
          ) ->
          let action = CallElem (reason_call, action) in
          rec_flow cx trace (key, ElemT (use_op, reason_lookup, l, action))
        (* If we are accessing `Iterable<T>` with a number, and have `access_iterables = true`,
           then output `T`. *)
        | ( DefT (_, NumT _),
            ElemT
              ( use_op,
                _,
                DefT
                  ( _,
                    InstanceT
                      { super = _; inst = { class_id; type_args = (_, _, t, _) :: _; _ }; _ }
                  ),
                ReadElem { access_iterables = true; tout; _ }
              )
          )
          when is_builtin_iterable_class_id class_id cx ->
          rec_flow_t cx trace ~use_op (t, OpenT tout)
        | (_, ElemT (use_op, reason_op, (DefT (_, (ObjT _ | InstanceT _)) as obj), action)) ->
          elem_action_on_obj cx trace ~use_op l obj reason_op action
        | (_, ElemT (use_op, reason_op, (AnyT (_, src) as obj), action)) ->
          let value = AnyT.why src reason_op in
          perform_elem_action cx trace ~use_op ~restrict_deletes:false reason_op obj value action
        (* It is not safe to write to an unknown index in a tuple. However, any is
         * a source of unsoundness, so that's ok. `tup[(0: any)] = 123` should not
         * error when `tup[0] = 123` does not. *)
        | (AnyT _, ElemT (use_op, reason_op, (DefT (reason_tup, ArrT arrtype) as arr), action)) ->
          let react_dro =
            match (action, arrtype) with
            | ( WriteElem _,
                (ROArrayAT _ | TupleAT { react_dro = Some _; _ } | ArrayAT { react_dro = Some _; _ })
              ) ->
              let reasons = (reason_op, reason_tup) in
              let use_op =
                match arrtype with
                | TupleAT { react_dro = Some dro; _ }
                | ArrayAT { react_dro = Some dro; _ } ->
                  Frame (ReactDeepReadOnly dro, use_op)
                | _ -> use_op
              in
              add_output cx ~trace (Error_message.EROArrayWrite (reasons, use_op));
              None
            | ( ReadElem _,
                (ROArrayAT (_, react_dro) | TupleAT { react_dro; _ } | ArrayAT { react_dro; _ })
              ) ->
              react_dro
            | _ -> None
          in
          let value = elemt_of_arrtype arrtype in
          let value =
            match react_dro with
            | Some dro -> mk_react_dro cx use_op dro value
            | None -> value
          in
          perform_elem_action cx trace ~use_op ~restrict_deletes:false reason_op arr value action
        | ( DefT (_, NumT _),
            ElemT (use_op, reason, (DefT (reason_tup, ArrT arrtype) as arr), action)
          ) ->
          let (write_action, read_action) =
            match action with
            | ReadElem _ -> (false, true)
            | CallElem _ -> (false, false)
            | WriteElem _ -> (true, false)
          in
          let (value, is_tuple, use_op, react_dro) =
            array_elem_check ~write_action cx trace l use_op reason reason_tup arrtype
          in
          let value =
            match react_dro with
            | Some dro when read_action -> mk_react_dro cx use_op dro value
            | _ -> value
          in
          perform_elem_action cx trace ~use_op ~restrict_deletes:is_tuple reason arr value action
        | ( DefT (_, ArrT _),
            GetPropT
              {
                use_op = _;
                reason;
                id = _;
                from_annot = _;
                propref = Named { name = OrdinaryName "constructor"; _ };
                tout;
                hint = _;
              }
          ) ->
          rec_flow_t cx trace ~use_op:unknown_use (Unsoundness.why Constructor reason, OpenT tout)
        | ( DefT (_, ArrT _),
            SetPropT (_, _, Named { name = OrdinaryName "constructor"; _ }, _, _, _, _)
          ) ->
          ()
        | ( DefT (_, ArrT _),
            MethodT (_, reason_call, _, Named { name = OrdinaryName "constructor"; _ }, action)
          ) ->
          add_specialized_callee_method_action cx trace (AnyT.untyped reason_call) action
        (* computed properties *)
        | (t, PreprocessKitT (reason, ConcretizeTypes (ConcretizeComputedPropsT tvar))) ->
          rec_flow_t cx trace ~use_op:unknown_use (t, OpenT (reason, tvar))
        (**************************************************)
        (* array pattern can consume the rest of an array *)
        (**************************************************)
        | (DefT (_, ArrT arrtype), ArrRestT (_, reason, i, tout)) ->
          let arrtype =
            match arrtype with
            | ArrayAT { tuple_view = None; _ }
            | ROArrayAT _ ->
              arrtype
            | ArrayAT { elem_t; tuple_view = Some (elements, (num_req, num_total)); react_dro } ->
              let elements = Base.List.drop elements i in
              let arity = (max (num_req - i) 0, max (num_total - i) 0) in
              ArrayAT { elem_t; tuple_view = Some (elements, arity); react_dro }
            | TupleAT { elem_t; elements; arity = (num_req, num_total); react_dro } ->
              TupleAT
                {
                  elem_t;
                  elements = Base.List.drop elements i;
                  arity = (max (num_req - i) 0, max (num_total - i) 0);
                  react_dro;
                }
          in
          let a = DefT (reason, ArrT arrtype) in
          rec_flow_t cx trace ~use_op:unknown_use (a, tout)
        | (AnyT (_, src), ArrRestT (_, reason, _, tout)) ->
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.why src reason, tout)
        (*****************)
        (* destructuring *)
        (*****************)
        | (_, DestructuringT (reason, kind, selector, tout, id)) ->
          destruct cx ~trace reason kind l selector tout id
        (**************)
        (* object kit *)
        (**************)
        | (_, ObjKitT (use_op, reason, resolve_tool, tool, tout)) ->
          ObjectKit.run trace cx use_op reason resolve_tool tool ~tout l
        (**************************************************)
        (* function types can be mapped over a structure  *)
        (**************************************************)
        | (AnyT (_, src), MapTypeT (_, reason_op, _, tout)) ->
          let src = any_mod_src_keep_placeholder Untyped src in
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.why src reason_op, tout)
        | (DefT (_, ArrT arrtype), MapTypeT (use_op, reason_op, TupleMap funt, tout)) ->
          let f x =
            let use_op = Frame (TupleMapFunCompatibility { value = reason_of_t x }, use_op) in
            EvalT
              ( funt,
                TypeDestructorT (use_op, reason_op, CallType { from_maptype = true; args = [x] }),
                Eval.generate_id ()
              )
          in
          let arrtype =
            match arrtype with
            | ArrayAT { elem_t; tuple_view; react_dro } ->
              ArrayAT
                {
                  elem_t = f elem_t;
                  react_dro;
                  tuple_view =
                    Base.Option.map
                      ~f:(fun (elements, arity) ->
                        let elements =
                          Base.List.map
                            ~f:(fun (TupleElement { name; t; polarity; optional; reason }) ->
                              TupleElement { name; t = f t; polarity; optional; reason })
                            elements
                        in
                        (elements, arity))
                      tuple_view;
                }
            | TupleAT { elem_t; elements; arity; react_dro } ->
              TupleAT
                {
                  elem_t = f elem_t;
                  react_dro;
                  elements =
                    Base.List.map
                      ~f:(fun (TupleElement { name; t; polarity; optional; reason }) ->
                        TupleElement { name; t = f t; polarity; optional; reason })
                      elements;
                  arity;
                }
            | ROArrayAT (elemt, dro) -> ROArrayAT (f elemt, dro)
          in
          let t =
            let reason = replace_desc_reason RArrayType reason_op in
            DefT (reason, ArrT arrtype)
          in
          rec_flow_t cx trace ~use_op:unknown_use (t, tout)
        | (_, MapTypeT (use_op, reason, TupleMap funt, tout)) ->
          let iter = Flow_js_utils.lookup_builtin_value cx "$iterate" reason in
          let elemt =
            EvalT
              ( iter,
                TypeDestructorT (use_op, reason, CallType { from_maptype = true; args = [l] }),
                Eval.generate_id ()
              )
          in
          let t = DefT (reason, ArrT (ROArrayAT (elemt, None))) in
          rec_flow cx trace (t, MapTypeT (use_op, reason, TupleMap funt, tout))
        | (DefT (_, ObjT o), MapTypeT (use_op, reason_op, ObjectMap funt, tout)) ->
          let map_t _ t =
            let (t, opt) =
              match t with
              | OptionalT { reason = _; type_ = t; use_desc = _ } -> (t, true)
              | _ -> (t, false)
            in
            let use_op = Frame (ObjMapFunCompatibility { value = reason_of_t t }, use_op) in
            let t =
              EvalT
                ( funt,
                  TypeDestructorT (use_op, reason_op, CallType { from_maptype = true; args = [t] }),
                  Eval.generate_id ()
                )
            in
            if opt then
              optional t
            else
              t
          in
          let map_field k t = map_t k t in
          let mapped_t = Flow_js_utils.map_obj cx o reason_op ~map_t ~map_field in
          rec_flow_t cx trace ~use_op:unknown_use (mapped_t, tout)
        | (DefT (_, ObjT o), MapTypeT (use_op, reason_op, ObjectMapi funt, tout)) ->
          let map_t key t =
            let (t, opt) =
              match t with
              | OptionalT { reason = _; type_ = t; use_desc = _ } -> (t, true)
              | _ -> (t, false)
            in
            let use_op =
              Frame
                (ObjMapiFunCompatibility { key = reason_of_t key; value = reason_of_t t }, use_op)
            in
            let t =
              EvalT
                ( funt,
                  TypeDestructorT
                    (use_op, reason_op, CallType { from_maptype = true; args = [key; t] }),
                  Eval.generate_id ()
                )
            in
            if opt then
              optional t
            else
              t
          in
          let map_field key t =
            let reason = replace_desc_reason (RStringLit key) reason_op in
            map_t (DefT (reason, SingletonStrT key)) t
          in
          let mapped_t = Flow_js_utils.map_obj cx o reason_op ~map_t ~map_field in
          rec_flow_t cx trace ~use_op:unknown_use (mapped_t, tout)
        | (DefT (_, ObjT o), MapTypeT (_, reason_op, ObjectKeyMirror, tout)) ->
          rec_flow_t cx trace ~use_op:unknown_use (obj_key_mirror cx o reason_op, tout)
        | (DefT (_, ObjT o), MapTypeT (_, reason_op, ObjectMapConst target, tout)) ->
          rec_flow_t cx trace ~use_op:unknown_use (obj_map_const cx o reason_op target, tout)
        (***************************************************************)
        (* functions may be called by passing a receiver and arguments *)
        (***************************************************************)
        | ( FunProtoCallT _,
            CallT
              {
                use_op;
                reason = reason_op;
                call_action = Funcalltype ({ call_this_t = func; call_args_tlist; _ } as funtype);
                return_hint;
              }
          ) ->
          (* Drop the first argument in the use_op. *)
          let use_op =
            match use_op with
            | Op (FunCall { op; fn; args = _ :: args; local }) ->
              Op (FunCall { op; fn; args; local })
            | Op (FunCallMethod { op; fn; prop; args = _ :: args; local }) ->
              Op (FunCallMethod { op; fn; prop; args; local })
            | _ -> use_op
          in
          begin
            match call_args_tlist with
            (* func.call() *)
            | [] ->
              let funtype =
                { funtype with call_this_t = VoidT.why reason_op; call_args_tlist = [] }
              in
              rec_flow
                cx
                trace
                ( func,
                  CallT
                    { use_op; reason = reason_op; call_action = Funcalltype funtype; return_hint }
                )
            (* func.call(this_t, ...call_args_tlist) *)
            | Arg call_this_t :: call_args_tlist ->
              let funtype = { funtype with call_this_t; call_args_tlist } in
              rec_flow
                cx
                trace
                ( func,
                  CallT
                    { use_op; reason = reason_op; call_action = Funcalltype funtype; return_hint }
                )
            (* func.call(...call_args_tlist) *)
            | (SpreadArg _ as first_arg) :: _ ->
              let call_this_t = extract_non_spread cx ~trace first_arg in
              let funtype = { funtype with call_this_t } in
              rec_flow
                cx
                trace
                ( func,
                  CallT
                    { use_op; reason = reason_op; call_action = Funcalltype funtype; return_hint }
                )
          end
        (*******************************************)
        (* ... or a receiver and an argument array *)
        (*******************************************)

        (* resolves the arguments... *)
        | ( FunProtoApplyT lreason,
            CallT
              {
                use_op;
                reason = reason_op;
                call_action = Funcalltype ({ call_this_t = func; call_args_tlist; _ } as funtype);
                return_hint;
              }
          ) ->
          (* Drop the specific AST derived argument reasons. Our new arguments come
           * from arbitrary positions in the array. *)
          let use_op =
            match use_op with
            | Op (FunCall { op; fn; args = _; local }) -> Op (FunCall { op; fn; args = []; local })
            | Op (FunCallMethod { op; fn; prop; args = _; local }) ->
              Op (FunCallMethod { op; fn; prop; args = []; local })
            | _ -> use_op
          in
          begin
            match call_args_tlist with
            (* func.apply() *)
            | [] ->
              let funtype =
                { funtype with call_this_t = VoidT.why reason_op; call_args_tlist = [] }
              in
              rec_flow
                cx
                trace
                ( func,
                  CallT
                    { use_op; reason = reason_op; call_action = Funcalltype funtype; return_hint }
                )
            (* func.apply(this_arg) *)
            | [Arg this_arg] ->
              let funtype = { funtype with call_this_t = this_arg; call_args_tlist = [] } in
              rec_flow
                cx
                trace
                ( func,
                  CallT
                    { use_op; reason = reason_op; call_action = Funcalltype funtype; return_hint }
                )
            (* func.apply(this_arg, ts) *)
            | [first_arg; Arg ts] ->
              let call_this_t = extract_non_spread cx ~trace first_arg in
              let call_args_tlist = [SpreadArg ts] in
              let funtype = { funtype with call_this_t; call_args_tlist } in
              (* Ignoring `this_arg`, we're basically doing func(...ts). Normally
               * spread arguments are resolved for the multiflow application, however
               * there are a bunch of special-cased functions like bind(), call(),
               * apply, etc which look at the arguments a little earlier. If we delay
               * resolving the spread argument, then we sabotage them. So we resolve
               * it early *)
              let t =
                Tvar.mk_where cx reason_op (fun t ->
                    let resolve_to = ResolveSpreadsToCallT (funtype, t) in
                    resolve_call_list cx ~trace ~use_op reason_op call_args_tlist resolve_to
                )
              in
              rec_flow_t cx trace ~use_op:unknown_use (func, t)
            | [SpreadArg t1; SpreadArg t2] ->
              add_output cx ~trace Error_message.(EUnsupportedSyntax (loc_of_t t1, SpreadArgument));
              add_output cx ~trace Error_message.(EUnsupportedSyntax (loc_of_t t2, SpreadArgument))
            | [SpreadArg t]
            | [Arg _; SpreadArg t] ->
              add_output cx ~trace Error_message.(EUnsupportedSyntax (loc_of_t t, SpreadArgument))
            | _ :: _ :: _ :: _ ->
              Error_message.EFunctionCallExtraArg
                (mk_reason RFunctionUnusedArgument (loc_of_reason lreason), lreason, 2, use_op)
              |> add_output cx ~trace
          end
        (************************************************************************)
        (* functions may be bound by passing a receiver and (partial) arguments *)
        (************************************************************************)
        | (FunProtoBindT _, CallT { use_op; call_action = ConcretizeCallee tout; _ }) ->
          rec_flow_t cx trace ~use_op (l, OpenT tout)
        | ( FunProtoBindT lreason,
            CallT
              {
                use_op;
                reason = reason_op;
                call_action =
                  Funcalltype
                    ( {
                        call_this_t = func;
                        call_targs;
                        call_args_tlist = first_arg :: call_args_tlist;
                        _;
                      } as funtype
                    );
                return_hint = _;
              }
          ) ->
          Base.Option.iter call_targs ~f:(fun _ ->
              add_output
                cx
                ~trace
                Error_message.(
                  ECallTypeArity
                    {
                      call_loc = loc_of_reason reason_op;
                      is_new = false;
                      reason_arity = lreason;
                      expected_arity = 0;
                    }
                )
          );
          let call_this_t = extract_non_spread cx ~trace first_arg in
          let call_targs = None in
          let funtype = { funtype with call_this_t; call_targs; call_args_tlist } in
          rec_flow cx trace (func, BindT (use_op, reason_op, funtype))
        | ( DefT (reason, FunT (_, ({ this_t = (o1, _); _ } as ft))),
            BindT (use_op, reason_op, calltype)
          ) ->
          let {
            call_this_t = o2;
            call_targs = _;
            (* always None *)
            call_args_tlist = tins2;
            call_tout;
            call_strict_arity = _;
            call_speculation_hint_state = _;
            call_kind = _;
            call_specialized_callee;
          } =
            calltype
          in
          CalleeRecorder.add_callee cx CalleeRecorder.All l call_specialized_callee;
          (* TODO: closure *)
          rec_flow_t cx trace ~use_op (o2, o1);

          let resolve_to =
            ResolveSpreadsToMultiflowPartial (mk_id (), ft, reason_op, OpenT call_tout)
          in
          resolve_call_list cx ~trace ~use_op reason tins2 resolve_to
        | (DefT (_, ObjT { call_t = Some id; _ }), BindT _) ->
          rec_flow cx trace (Context.find_call cx id, u)
        | (DefT (_, InstanceT { inst = { inst_call_t = Some id; _ }; _ }), BindT _) ->
          rec_flow cx trace (Context.find_call cx id, u)
        | (AnyT (_, src), BindT (use_op, reason, calltype)) ->
          let {
            call_this_t;
            call_targs = _;
            (* always None *)
            call_args_tlist;
            call_tout;
            call_strict_arity = _;
            call_speculation_hint_state = _;
            call_kind = _;
            call_specialized_callee;
          } =
            calltype
          in
          CalleeRecorder.add_callee cx CalleeRecorder.All l call_specialized_callee;
          let src = any_mod_src_keep_placeholder Untyped src in
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.why src reason, call_this_t);
          call_args_iter
            (fun param_t -> rec_flow cx trace (AnyT.why src reason, UseT (use_op, param_t)))
            call_args_tlist;
          rec_flow_t cx trace ~use_op:unknown_use (l, OpenT call_tout)
        (***************************************************************)
        (* Enable structural subtyping for upperbounds like interfaces *)
        (***************************************************************)
        | ((ObjProtoT _ | FunProtoT _ | DefT (_, NullT)), ImplementsT _) -> ()
        | ( DefT
              ( reason_inst,
                InstanceT
                  {
                    super;
                    inst =
                      {
                        own_props;
                        proto_props;
                        inst_call_t;
                        inst_kind = InterfaceKind _;
                        inst_dict;
                        _;
                      };
                    _;
                  }
              ),
            ImplementsT (use_op, t)
          ) ->
          structural_subtype
            cx
            trace
            ~use_op
            t
            reason_inst
            (own_props, proto_props, inst_call_t, inst_dict);
          rec_flow cx trace (super, ReposLowerT (reason_inst, false, ImplementsT (use_op, t)))
        | (_, ImplementsT _) ->
          add_output cx ~trace (Error_message.EUnsupportedImplements (reason_of_t l))
        (*********************************************************************)
        (* class A is a base class of class B iff                            *)
        (* properties in B that override properties in A or its base classes *)
        (* have the same signatures                                          *)
        (*********************************************************************)

        (* The purpose of SuperT is to establish consistency between overriding
           properties with overridden properties. As such, the lookups performed
           for the inherited properties are non-strict: they are not required to
           exist. **)
        | ( DefT (ureason, InstanceT { static = st; _ }),
            SuperT (use_op, reason, Derived { own; proto; static })
          ) ->
          let check_super l = check_super cx trace ~use_op reason ureason l in
          NameUtils.Map.iter (check_super l) own;
          NameUtils.Map.iter (fun x p -> if inherited_method x then check_super l x p) proto;

          (* TODO: inherited_method logic no longer applies for statics. It used to
             when call properties were included in the props, but that is no longer
             the case. All that remains is the "constructor" prop, which has no
             special meaning on the static object. *)
          NameUtils.Map.iter (fun x p -> if inherited_method x then check_super st x p) static
        (***********************)
        (* opaque types part 2 *)
        (***********************)

        (* Don't refine opaque types based on its bound *)
        | (OpaqueT _, PredicateT (p, t)) -> predicate cx trace t l p
        | (OpaqueT _, GuardT (pred, result, sink)) -> guard cx trace l pred result sink
        | (OpaqueT _, SealGenericT { reason = _; id; name; cont; no_infer }) ->
          let reason = reason_of_t l in
          continue cx trace (GenericT { reason; id; name; bound = l; no_infer }) cont
        (* Preserve OpaqueT as consequent, but branch based on the bound *)
        | (OpaqueT (_, { super_t = Some t; _ }), CondT (r, then_t_opt, else_t, tout)) ->
          let then_t_opt =
            match then_t_opt with
            | Some _ -> then_t_opt
            | None -> Some l
          in
          rec_flow cx trace (t, CondT (r, then_t_opt, else_t, tout))
        (* Opaque types may be treated as their supertype when they are a lower bound for a use *)
        | (OpaqueT (opaque_t_reason, { super_t = Some t; _ }), _) ->
          rec_flow
            cx
            trace
            ( t,
              mod_use_op_of_use_t
                (fun use_op -> Frame (OpaqueTypeBound { opaque_t_reason }, use_op))
                u
            )
        (***********************************************************)
        (* binary arithmetic operators                             *)
        (***********************************************************)
        | (l, ArithT { use_op; reason; flip; rhs_t; result_t; kind }) ->
          flow_arith cx trace use_op reason flip l rhs_t result_t kind
        (**************************)
        (* relational comparisons *)
        (**************************)
        | (l, ComparatorT { reason; flip; arg = r }) -> flow_comparator cx trace reason flip l r
        | (l, EqT { reason; flip; arg = r }) -> flow_eq cx trace reason flip l r
        | (l, StrictEqT { reason; cond_context; flip; arg = r }) ->
          flow_strict_eq cx trace reason cond_context flip l r
        (******************************)
        (* unary arithmetic operators *)
        (******************************)
        | (l, UnaryArithT { reason; result_t; kind }) ->
          let t = flow_unary_arith ~trace cx l reason kind in
          rec_flow_t cx trace ~use_op:unknown_use (t, result_t)
        (************************)
        (* binary `in` operator *)
        (************************)

        (* the left-hand side of a `(x in y)` expression is a string or number
           TODO: also, symbols *)
        | (DefT (_, StrT _), AssertBinaryInLHST _) -> ()
        | (DefT (_, NumT _), AssertBinaryInLHST _) -> ()
        | (_, AssertBinaryInLHST _) ->
          add_output cx ~trace (Error_message.EBinaryInLHS (reason_of_t l))
        (* the right-hand side of a `(x in y)` expression must be object-like *)
        | (DefT (_, ArrT _), AssertBinaryInRHST _) -> ()
        | (_, AssertBinaryInRHST _) when object_like l -> ()
        | (_, AssertBinaryInRHST _) ->
          add_output cx ~trace (Error_message.EBinaryInRHS (reason_of_t l))
        (************************)
        (* Assert non component *)
        (************************)
        | (DefT (reason_type, MixedT _), AssertNonComponentLikeT (def_loc, use_reason)) ->
          add_output
            cx
            ~trace
            Error_message.(
              EReactIntrinsicOverlap
                { use = use_reason; def = def_loc; type_ = loc_of_reason reason_type; mixed = true }
            )
        | ( DefT
              ( reason_type,
                (ObjT { call_t = Some _; _ } | FunT _ | ClassT _ | ReactAbstractComponentT _)
              ),
            AssertNonComponentLikeT (def_loc, use_reason)
          ) ->
          add_output
            cx
            ~trace
            Error_message.(
              EReactIntrinsicOverlap
                {
                  use = use_reason;
                  def = def_loc;
                  type_ = loc_of_reason reason_type;
                  mixed = false;
                }
            )
        | (_, AssertNonComponentLikeT _) -> ()
        (******************)
        (* `for...in` RHS *)
        (******************)

        (* objects are allowed. arrays _could_ be, but are not because it's
           generally safer to use a for or for...of loop instead. *)
        | (_, AssertForInRHST _) when object_like l -> ()
        | ((AnyT _ | ObjProtoT _), AssertForInRHST _) -> ()
        (* null/undefined are allowed *)
        | (DefT (_, (NullT | VoidT)), AssertForInRHST _) -> ()
        | (DefT (enum_reason, EnumObjectT _), AssertForInRHST _) ->
          add_output
            cx
            ~trace
            (Error_message.EEnumNotIterable { reason = enum_reason; for_in = true })
        | (_, AssertForInRHST _) -> add_output cx ~trace (Error_message.EForInRHS (reason_of_t l))
        (********************)
        (* `instanceof` RHS *)
        (* right side of an `instanceof` binary expression must be an object *)
        (********************)
        | (_, AssertInstanceofRHST _) when object_like l -> ()
        | (DefT (_, ArrT _), AssertInstanceofRHST _) ->
          (* arrays are objects too, but not in `object_like` *)
          ()
        | (AnyT _, AssertInstanceofRHST _) -> ()
        | (_, AssertInstanceofRHST _) ->
          add_output cx ~trace (Error_message.EInstanceofRHS (reason_of_t l))
        (***********************************)
        (* iterable (e.g. RHS of `for..of` *)
        (***********************************)
        | (DefT (enum_reason, EnumObjectT _), AssertIterableT _) ->
          Default_resolve.default_resolve_touts
            ~flow:(rec_flow_t cx trace ~use_op:unknown_use)
            cx
            (reason_of_t l |> loc_of_reason)
            u;
          add_output
            cx
            ~trace
            (Error_message.EEnumNotIterable { reason = enum_reason; for_in = false })
        | (AnyT (_, src), AssertIterableT { use_op; reason; async = _; targs }) ->
          let src = any_mod_src_keep_placeholder (AnyError None) src in
          Base.List.iter targs ~f:(fun t ->
              rec_unify cx trace ~use_op ~unify_any:true t (AnyT.why src reason)
          )
        | (_, AssertIterableT { use_op; reason; async; targs }) ->
          let iterable =
            if async then
              get_builtin_typeapp cx reason "$IterableOrAsyncIterableInternal" (l :: targs)
            else
              get_builtin_typeapp cx reason "$Iterable" targs
          in
          rec_flow_t cx trace ~use_op (l, iterable)
        (**************************************)
        (* types may be refined by predicates *)
        (**************************************)
        | (_, PredicateT (p, t)) -> predicate cx trace t l p
        | (_, GuardT (pred, result, sink)) -> guard cx trace l pred result sink
        | (_, SentinelPropTestT (reason, obj, sense, enum, result)) ->
          let t = Type_filter.sentinel_refinement l reason obj sense enum in
          rec_flow_t cx trace ~use_op:unknown_use (t, OpenT result)
        (******************************)
        (* functions statics - part B *)
        (******************************)
        | (DefT (reason, FunT (static, _)), _) when object_like_op u ->
          rec_flow cx trace (static, ReposLowerT (reason, false, u))
        (*****************************************)
        (* classes can have their prototype read *)
        (*****************************************)
        | ( DefT (reason, ClassT instance),
            GetPropT
              {
                use_op = _;
                reason = _;
                id = _;
                from_annot = _;
                propref = Named { name = OrdinaryName "prototype"; _ };
                tout;
                hint = _;
              }
          ) ->
          let instance = reposition cx ~trace (loc_of_reason reason) instance in
          rec_flow_t cx trace ~use_op:unknown_use (instance, OpenT tout)
        (*****************)
        (* class statics *)
        (*****************)

        (* For Get/SetPrivatePropT or PrivateMethodT, the instance id is needed to determine whether
         * or not the private static field exists on that class. Since we look through the scopes for
         * the type of the field, there is no need to look at the static member of the instance.
         * Instead, we just flip the boolean flag to true, indicating that when the
         * InstanceT ~> Set/GetPrivatePropT or PrivateMethodT constraint is processed that we should
         * look at the private static fields instead of the private instance fields. *)
        | (DefT (reason, ClassT instance), GetPrivatePropT (use_op, reason_op, x, scopes, _, tout))
          ->
          let u = GetPrivatePropT (use_op, reason_op, x, scopes, true, tout) in
          rec_flow cx trace (instance, ReposLowerT (reason, false, u))
        | ( DefT (reason, ClassT instance),
            SetPrivatePropT (use_op, reason_op, x, mode, scopes, _, wr_ctx, tout, tp)
          ) ->
          let u = SetPrivatePropT (use_op, reason_op, x, mode, scopes, true, wr_ctx, tout, tp) in
          rec_flow cx trace (instance, ReposLowerT (reason, false, u))
        | ( DefT (reason, ClassT instance),
            PrivateMethodT (use_op, reason_op, reason_lookup, prop_name, scopes, _, action)
          ) ->
          let u =
            PrivateMethodT (use_op, reason_op, reason_lookup, prop_name, scopes, true, action)
          in
          rec_flow cx trace (instance, ReposLowerT (reason, false, u))
        | ( DefT (reason, ClassT instance),
            MethodT (use_op, reason_call, reason_lookup, propref, action)
          ) ->
          let statics = (reason, Tvar.mk_no_wrap cx reason) in
          rec_flow cx trace (instance, GetStaticsT statics);
          let method_type =
            Tvar.mk_no_wrap_where cx reason_lookup (fun tout ->
                let u =
                  GetPropT
                    {
                      use_op;
                      reason = reason_lookup;
                      id = None;
                      from_annot = false;
                      propref;
                      tout;
                      hint = hint_unavailable;
                    }
                in
                rec_flow cx trace (OpenT statics, ReposLowerT (reason, false, u))
            )
          in
          apply_method_action cx trace method_type use_op reason_call l action
        | (DefT (reason, ClassT instance), _) when object_like_op u ->
          let statics = (reason, Tvar.mk_no_wrap cx reason) in
          rec_flow cx trace (instance, GetStaticsT statics);
          rec_flow cx trace (OpenT statics, u)
        (************************)
        (* classes as functions *)
        (************************)

        (* When a class value flows to a function annotation or call site, check for
           the presence of a call property in the former (as a static) compatible
           with the latter.

           TODO: Call properties are excluded from the subclass compatibility
           checks, which makes it unsafe to call a Class<T> type like this.
           For example:

               declare class A { static (): string };
               declare class B extends A { static (): number }
               var klass: Class<A> = B;
               var foo: string = klass(); // passes, but `foo` is a number

           The same issue is also true for constructors, which are similarly
           excluded from subclass compatibility checks, but are allowed on ClassT
           types.
        *)
        | (DefT (reason, ClassT instance), CallT _) ->
          let statics = (reason, Tvar.mk_no_wrap cx reason) in
          rec_flow cx trace (instance, GetStaticsT statics);
          rec_flow cx trace (OpenT statics, u)
        (*********)
        (* enums *)
        (*********)
        | ( DefT (enum_reason, EnumObjectT enum),
            GetPropT
              {
                use_op;
                reason = access_reason;
                id = _;
                from_annot = _;
                propref = Named { reason = prop_reason; name = member_name; _ };
                tout;
                hint = _;
              }
          ) ->
          let access = (use_op, access_reason, None, (prop_reason, member_name)) in
          GetPropTKit.on_EnumObjectT cx trace enum_reason enum access tout
        | (DefT (_, EnumObjectT _), TestPropT { use_op = _; reason; id = _; propref; tout; hint })
          ->
          rec_flow
            cx
            trace
            ( l,
              GetPropT
                {
                  use_op = Op (GetProperty reason);
                  reason;
                  id = None;
                  from_annot = false;
                  propref;
                  tout;
                  hint;
                }
            )
        | ( DefT (enum_reason, EnumObjectT enum),
            MethodT (use_op, call_reason, lookup_reason, (Named _ as propref), action)
          ) ->
          let t =
            Tvar.mk_no_wrap_where cx lookup_reason (fun tout ->
                rec_flow
                  cx
                  trace
                  ( enum_proto cx ~reason:lookup_reason (enum_reason, enum),
                    GetPropT
                      {
                        use_op;
                        reason = lookup_reason;
                        id = None;
                        from_annot = false;
                        propref;
                        tout;
                        hint = hint_unavailable;
                      }
                  )
            )
          in
          apply_method_action cx trace t use_op call_reason l action
        | (DefT (enum_reason, EnumObjectT _), GetElemT { key_t; tout; _ }) ->
          let reason = reason_of_t key_t in
          add_output
            cx
            ~trace
            (Error_message.EEnumInvalidMemberAccess
               { member_name = None; suggestion = None; reason; enum_reason }
            );
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.error reason, OpenT tout)
        | (DefT (enum_reason, EnumObjectT _), SetPropT (_, op_reason, _, _, _, _, tout))
        | (DefT (enum_reason, EnumObjectT _), SetElemT (_, op_reason, _, _, _, tout)) ->
          add_output
            cx
            ~trace
            (Error_message.EEnumModification { loc = loc_of_reason op_reason; enum_reason });
          Base.Option.iter tout ~f:(fun tout ->
              rec_flow_t cx trace ~use_op:unknown_use (AnyT.error op_reason, tout)
          )
        | (DefT (enum_reason, EnumObjectT _), GetValuesT (op_reason, tout)) ->
          add_output
            cx
            ~trace
            (Error_message.EEnumInvalidObjectUtilType { reason = op_reason; enum_reason });
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.error op_reason, tout)
        | (DefT (enum_reason, EnumObjectT _), GetDictValuesT (reason, result)) ->
          add_output cx ~trace (Error_message.EEnumInvalidObjectFunction { reason; enum_reason });
          rec_flow cx trace (AnyT.error reason, result)
        (* Entry point to exhaustive checking logic - when resolving the discriminant as an enum. *)
        | ( DefT (enum_reason, EnumT enum),
            EnumExhaustiveCheckT
              {
                reason = check_reason;
                check =
                  EnumExhaustiveCheckPossiblyValid
                    { tool = EnumResolveDiscriminant; possible_checks; checks; default_case };
                incomplete_out;
                discriminant_after_check;
              }
          ) ->
          enum_exhaustive_check
            cx
            ~trace
            ~check_reason
            ~enum_reason
            ~enum
            ~possible_checks
            ~checks
            ~default_case
            ~incomplete_out
            ~discriminant_after_check
        (* Resolving the case tests. *)
        | ( _,
            EnumExhaustiveCheckT
              {
                reason = check_reason;
                check =
                  EnumExhaustiveCheckPossiblyValid
                    {
                      tool = EnumResolveCaseTest { discriminant_reason; discriminant_enum; check };
                      possible_checks;
                      checks;
                      default_case;
                    };
                incomplete_out;
                discriminant_after_check;
              }
          ) ->
          let (EnumCheck { member_name; _ }) = check in
          let { enum_id = enum_id_discriminant; members; _ } = discriminant_enum in
          let checks =
            match l with
            | DefT (_, EnumObjectT { enum_id = enum_id_check; _ })
              when ALoc.equal_id enum_id_discriminant enum_id_check && SMap.mem member_name members
              ->
              check :: checks
            (* If the check is not the same enum type, ignore it and continue. The user will
             * still get an error as the comparison between discriminant and case test will fail. *)
            | _ -> checks
          in
          enum_exhaustive_check
            cx
            ~trace
            ~check_reason
            ~enum_reason:discriminant_reason
            ~enum:discriminant_enum
            ~possible_checks
            ~checks
            ~default_case
            ~incomplete_out
            ~discriminant_after_check
        | ( DefT (enum_reason, EnumT { members; _ }),
            EnumExhaustiveCheckT
              {
                reason;
                check = EnumExhaustiveCheckInvalid reasons;
                incomplete_out;
                discriminant_after_check = _;
              }
          ) ->
          let example_member = SMap.choose_opt members |> Base.Option.map ~f:fst in
          List.iter
            (fun reason ->
              add_output cx (Error_message.EEnumInvalidCheck { reason; enum_reason; example_member }))
            reasons;
          enum_exhaustive_check_incomplete cx ~trace ~reason incomplete_out
        (* If the discriminant is empty, the check is successful. *)
        | ( DefT (_, EmptyT),
            EnumExhaustiveCheckT
              {
                check =
                  ( EnumExhaustiveCheckInvalid _
                  | EnumExhaustiveCheckPossiblyValid { tool = EnumResolveDiscriminant; _ } );
                _;
              }
          ) ->
          ()
        (* Non-enum discriminants.
         * If `discriminant_after_check` is empty (e.g. because the discriminant has been refined
         * away by each case), then `trigger` will be empty, which will prevent the implicit void
         * return that could occur otherwise. *)
        | ( _,
            EnumExhaustiveCheckT
              {
                reason;
                check =
                  ( EnumExhaustiveCheckInvalid _
                  | EnumExhaustiveCheckPossiblyValid { tool = EnumResolveDiscriminant; _ } );
                incomplete_out;
                discriminant_after_check;
              }
          ) ->
          enum_exhaustive_check_incomplete
            cx
            ~trace
            ~reason
            ?trigger:discriminant_after_check
            incomplete_out
        (**************************************************************************)
        (* TestPropT is emitted for property reads in the context of branch tests.
           Such tests are always non-strict, in that we don't immediately report an
           error if the property is not found not in the object type. Instead, if
           the property is not found, we control the result type of the read based
           on the flags on the object type. For exact object types, the
           result type is `void`; otherwise, it is "unknown". Indeed, if the
           property is not found in an exact object type, we can be sure it
           won't exist at run time, so the read will return undefined; but for other
           object types, the property *might* exist at run time, and since we don't
           know what the type of the property would be, we set things up so that the
           result of the read cannot be used in any interesting way. *)
        (**************************************************************************)
        | (DefT (_, NullT), TestPropT { use_op; reason; id; propref; tout; hint }) ->
          (* The wildcard TestPropT implementation forwards the lower bound to
             LookupT. This is unfortunate, because LookupT is designed to terminate
             (successfully) on NullT, but property accesses on null should be type
             errors. Ideally, we should prevent LookupT constraints from being
             syntax-driven, in order to preserve the delicate invariants that
             surround it. *)
          rec_flow
            cx
            trace
            (l, GetPropT { use_op; reason; id = Some id; from_annot = false; propref; tout; hint })
        | (DefT (r, MixedT (Mixed_truthy | Mixed_non_maybe)), TestPropT { use_op; id; tout; _ }) ->
          (* Special-case property tests of definitely non-null/non-void values to
             return mixed and treat them as a hit. *)
          Context.test_prop_hit cx id;
          rec_flow_t cx trace ~use_op (DefT (r, MixedT Mixed_everything), OpenT tout)
        | (_, TestPropT { use_op; reason = reason_op; id; propref; tout; hint = _ }) ->
          (* NonstrictReturning lookups unify their result, but we don't want to
             unify with the tout tvar directly, so we create an indirection here to
             ensure we only supply lower bounds to tout. *)
          let lookup_default =
            Tvar.mk_where cx reason_op (fun tvar -> rec_flow_t ~use_op cx trace (tvar, OpenT tout))
          in
          let name = name_of_propref propref in
          let reason_prop =
            match propref with
            | Named { reason; _ } -> reason
            | Computed _ -> reason_op
          in
          let test_info = Some (id, (reason_prop, reason_of_t l)) in
          let lookup_default =
            match l with
            | DefT (_, ObjT { flags; _ }) when Obj_type.is_exact flags.obj_kind ->
              let r = replace_desc_reason (RMissingProperty name) reason_op in
              Some (DefT (r, VoidT), lookup_default)
            | _ ->
              (* Note: a lot of other types could in principle be considered
                 "exact". For example, new instances of classes could have exact
                 types; so could `super` references (since they are statically
                 rather than dynamically bound). However, currently we don't support
                 any other exact types. Considering exact types inexact is sound, so
                 there is no problem falling back to the same conservative
                 approximation we use for inexact types in those cases. *)
              let r = replace_desc_reason (RUnknownProperty name) reason_op in
              Some (DefT (r, MixedT Mixed_everything), lookup_default)
          in
          let lookup_kind = NonstrictReturning (lookup_default, test_info) in
          rec_flow
            cx
            trace
            ( l,
              LookupT
                {
                  reason = reason_op;
                  lookup_kind;
                  try_ts_on_failure = [];
                  propref;
                  lookup_action = ReadProp { use_op; obj_t = l; tout };
                  method_accessible =
                    begin
                      match (l, propref) with
                      | (_, Named { name = OrdinaryName "constructor"; _ }) -> true
                      | (DefT (_, InstanceT _), _) -> false
                      | _ -> true
                    end;
                  ids = Some Properties.Set.empty;
                  ignore_dicts = false;
                }
            )
        (***************************)
        (* conditional type switch *)
        (***************************)

        (* Use our alternate if our lower bound is empty. *)
        | (DefT (_, EmptyT), CondT (_, _, else_t, tout)) ->
          rec_flow_t cx trace ~use_op:unknown_use (else_t, tout)
        (* Otherwise continue by Flowing out lower bound to tout. *)
        | (_, CondT (_, then_t_opt, _, tout)) ->
          let then_t =
            match then_t_opt with
            | Some t -> t
            | None -> l
          in
          rec_flow_t cx trace ~use_op:unknown_use (then_t, tout)
        (*****************)
        (* repositioning *)
        (*****************)

        (* waits for a lower bound to become concrete, and then repositions it to
           the location stored in the ReposLowerT, which is usually the location
           where that lower bound was used; the lower bound's location (which is
           being overwritten) is where it was defined. *)
        | (_, ReposLowerT (reason, use_desc, u)) ->
          rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)
        (***********************************************************)
        (* generics                                                *)
        (***********************************************************)
        | (_, SealGenericT { reason = _; id; name; cont; no_infer }) ->
          let reason = reason_of_t l in
          continue cx trace (GenericT { reason; id; name; bound = l; no_infer }) cont
        | (GenericT { reason; bound; _ }, _) ->
          rec_flow cx trace (reposition_reason cx reason bound, u)
        (***************)
        (* unsupported *)
        (***************)

        (* Lookups can be strict or non-strict, as denoted by the presence or
           absence of strict_reason in the following two pattern matches.
           Strictness derives from whether the object is sealed and was
           created in the same scope in which the lookup occurs - see
           mk_strict_lookup_reason below. The failure of a strict lookup
           to find the desired property causes an error; a non-strict one
           does not.
        *)
        | ( (DefT (_, NullT) | ObjProtoT _),
            LookupT
              {
                reason;
                lookup_kind;
                try_ts_on_failure = next :: try_ts_on_failure;
                propref;
                lookup_action;
                method_accessible;
                ids;
                ignore_dicts;
              }
          ) ->
          (* When s is not found, we always try to look it up in the next element in
             the list try_ts_on_failure. *)
          rec_flow
            cx
            trace
            ( next,
              LookupT
                {
                  reason;
                  lookup_kind;
                  try_ts_on_failure;
                  propref;
                  lookup_action;
                  method_accessible;
                  ids;
                  ignore_dicts;
                }
            )
        | ( (ObjProtoT _ | FunProtoT _),
            LookupT
              {
                reason = reason_op;
                lookup_kind = _;
                try_ts_on_failure = [];
                propref = Named { name = OrdinaryName "__proto__"; _ };
                lookup_action = ReadProp { use_op = _; obj_t = l; tout };
                ids = _;
                method_accessible = _;
                ignore_dicts = _;
              }
          ) ->
          (* __proto__ is a getter/setter on Object.prototype *)
          rec_flow cx trace (l, GetProtoT (reason_op, tout))
        | ( (ObjProtoT _ | FunProtoT _),
            LookupT
              {
                reason = reason_op;
                lookup_kind = _;
                try_ts_on_failure = [];
                propref = Named { name = OrdinaryName "__proto__"; _ };
                lookup_action =
                  WriteProp { use_op = _; obj_t = l; prop_tout = _; tin; write_ctx = _; mode = _ };
                method_accessible = _;
                ids = _;
                ignore_dicts = _;
              }
          ) ->
          (* __proto__ is a getter/setter on Object.prototype *)
          rec_flow cx trace (l, SetProtoT (reason_op, tin))
        | ( ObjProtoT _,
            LookupT { reason = reason_op; try_ts_on_failure = []; propref = Named { name; _ }; _ }
          )
          when is_object_prototype_method name ->
          (* TODO: These properties should go in Object.prototype. Currently we
             model Object.prototype as a ObjProtoT, as an optimization against a
             possible deluge of shadow properties on Object.prototype, since it
             is shared by every object. **)
          rec_flow cx trace (get_builtin_type cx ~trace reason_op "Object", u)
        | (FunProtoT _, LookupT { reason = reason_op; propref = Named { name; _ }; _ })
          when is_function_prototype name ->
          (* TODO: Ditto above comment for Function.prototype *)
          rec_flow cx trace (get_builtin_type cx ~trace reason_op "Function", u)
        | ( (DefT (reason, NullT) | ObjProtoT reason | FunProtoT reason),
            LookupT
              {
                reason = reason_op;
                lookup_kind = Strict strict_reason;
                try_ts_on_failure = [];
                propref = Named { reason = reason_prop; name; _ } as propref;
                lookup_action = action;
                method_accessible = _;
                ids;
                ignore_dicts = _;
              }
          ) ->
          let error_message =
            let use_op = use_op_of_lookup_action action in
            let suggestion =
              Base.Option.bind ids ~f:(fun ids ->
                  prop_typo_suggestion cx (Properties.Set.elements ids) (display_string_of_name name)
              )
            in
            Error_message.EPropNotFound
              { reason_prop; reason_obj = strict_reason; prop_name = Some name; use_op; suggestion }
          in
          add_output cx ~trace error_message;
          let p =
            OrdinaryField
              { type_ = AnyT.error_of_kind UnresolvedName reason_op; polarity = Polarity.Neutral }
          in
          perform_lookup_action cx trace propref p DynamicProperty reason reason_op action
        | ( (DefT (reason, NullT) | ObjProtoT reason | FunProtoT reason),
            LookupT
              {
                reason = reason_op;
                lookup_kind = Strict strict_reason;
                try_ts_on_failure = [];
                propref = Computed elem_t as propref;
                lookup_action = action;
                method_accessible = _;
                ids = _;
                ignore_dicts = _;
              }
          ) ->
          (match elem_t with
          | OpenT _ ->
            let loc = loc_of_t elem_t in
            add_output cx ~trace Error_message.(EInternal (loc, PropRefComputedOpen))
          | DefT (_, StrT (Literal _)) ->
            let loc = loc_of_t elem_t in
            add_output cx ~trace Error_message.(EInternal (loc, PropRefComputedLiteral))
          | AnyT (_, src) ->
            let src = any_mod_src_keep_placeholder Untyped src in
            let p = OrdinaryField { type_ = AnyT.why src reason_op; polarity = Polarity.Neutral } in
            perform_lookup_action cx trace propref p DynamicProperty reason reason_op action
          | _ ->
            let reason_prop = reason_op in
            let error_message =
              let use_op = use_op_of_lookup_action action in
              Error_message.EPropNotFound
                {
                  reason_prop;
                  reason_obj = strict_reason;
                  prop_name = None;
                  use_op;
                  suggestion = None;
                }
            in
            add_output cx ~trace error_message)
        (* LookupT is a non-strict lookup *)
        | ( (DefT (_, NullT) | ObjProtoT _ | FunProtoT _),
            LookupT
              {
                lookup_kind = NonstrictReturning (t_opt, test_opt);
                try_ts_on_failure = [];
                propref;
                lookup_action = action;
                ids;
                _;
              }
          ) ->
          (* don't fire

             ...unless a default return value is given. Two examples:

             1. A failure could arise when an unchecked module was looked up and
             not found declared, in which case we consider that module's exports to
             be `any`.

             2. A failure could arise also when an object property is looked up in
             a condition, in which case we consider the object's property to be
             `mixed`.
          *)
          let use_op =
            Base.Option.value ~default:unknown_use (Some (use_op_of_lookup_action action))
          in
          Base.Option.iter test_opt ~f:(fun (id, reasons) ->
              let suggestion =
                match propref with
                | Named { name = OrdinaryName name; _ } ->
                  Base.Option.bind ids ~f:(fun ids ->
                      prop_typo_suggestion cx (Properties.Set.elements ids) name
                  )
                | _ -> None
              in
              if Context.typing_mode cx <> Context.HintEvaluationMode then
                Context.test_prop_miss cx id (name_of_propref propref) reasons use_op suggestion
          );

          begin
            match t_opt with
            | Some (not_found, t) -> rec_unify cx trace ~use_op ~unify_any:true t not_found
            | None -> ()
          end
        (* SuperT only involves non-strict lookups *)
        | (DefT (_, NullT), SuperT _)
        | (ObjProtoT _, SuperT _)
        | (FunProtoT _, SuperT _) ->
          ()
        (* ExtendsT searches for a nominal superclass. The search terminates with
           either failure at the root or a structural subtype check. **)
        | (AnyT (_, src), ExtendsUseT (use_op, reason_op, ts, t1, t2)) ->
          Base.List.iter ts ~f:(fun t -> rec_flow_t cx trace ~use_op (AnyT.why src reason_op, t));
          rec_flow_t cx trace ~use_op (AnyT.why src reason_op, t1);
          rec_flow_t cx trace ~use_op (AnyT.why src reason_op, t2)
        | (DefT (lreason, ObjT { proto_t; _ }), ExtendsUseT _) ->
          let l = reposition cx ~trace (loc_of_reason lreason) proto_t in
          rec_flow cx trace (l, u)
        | (DefT (reason, ClassT instance), ExtendsUseT _) ->
          let statics = (reason, Tvar.mk_no_wrap cx reason) in
          rec_flow cx trace (instance, GetStaticsT statics);
          rec_flow cx trace (OpenT statics, u)
        | (DefT (_, NullT), ExtendsUseT (use_op, reason, next :: try_ts_on_failure, l, u)) ->
          (* When seaching for a nominal superclass fails, we always try to look it
             up in the next element in the list try_ts_on_failure. *)
          rec_flow cx trace (next, ExtendsUseT (use_op, reason, try_ts_on_failure, l, u))
        | ( DefT (_, NullT),
            ExtendsUseT
              ( use_op,
                _,
                [],
                l,
                DefT
                  ( reason_inst,
                    InstanceT
                      {
                        super;
                        inst =
                          {
                            own_props;
                            proto_props;
                            inst_call_t;
                            inst_kind = InterfaceKind _;
                            inst_dict;
                            _;
                          };
                        _;
                      }
                  )
              )
          ) ->
          structural_subtype
            cx
            trace
            ~use_op
            l
            reason_inst
            (own_props, proto_props, inst_call_t, inst_dict);
          rec_flow cx trace (l, UseT (use_op, super))
        (* Unwrap deep readonly *)
        | (_, (DeepReadOnlyT (tout, _, _) | HooklikeT tout)) ->
          rec_flow_t ~use_op:unknown_use cx trace (l, OpenT tout)
        (* Render Type Misc Uses *)
        | (DefT (_, RendersT (NominalRenders _)), ExitRendersT { renders_reason; u })
        | (DefT (renders_reason, RendersT (NominalRenders _)), u) ->
          let mixed_element = get_builtin_type cx ~trace renders_reason "React$MixedElement" in
          rec_flow cx trace (mixed_element, u)
        | ( DefT
              ( r,
                RendersT
                  (StructuralRenders
                    { renders_variant = RendersNormal; renders_structural_type = t }
                    )
              ),
            u
          ) ->
          let u' = ExitRendersT { renders_reason = r; u } in
          rec_flow cx trace (t, u')
        | (_, ExitRendersT { renders_reason; u }) ->
          let node = get_builtin_type cx ~trace renders_reason "React$Node" in
          rec_flow cx trace (node, u)
        (***********************)
        (* Object library call *)
        (***********************)
        | (ObjProtoT reason, _) ->
          let use_desc = true in
          let obj_proto = get_builtin_type cx ~trace reason ~use_desc "Object" in
          rec_flow cx trace (obj_proto, u)
        (*************************)
        (* Function library call *)
        (*************************)
        | (FunProtoT reason, _) ->
          let use_desc = true in
          let fun_proto = get_builtin_type cx ~trace reason ~use_desc "Function" in
          rec_flow cx trace (fun_proto, u)
        | (_, ExtendsUseT (use_op, _, [], t, tc)) ->
          let (reason_l, reason_u) = FlowError.ordered_reasons (reason_of_t t, reason_of_t tc) in
          add_output
            cx
            ~trace
            (Error_message.EIncompatibleWithUseOp
               { reason_lower = reason_l; reason_upper = reason_u; use_op }
            )
        (*******************************)
        (* ToString abstract operation *)
        (*******************************)

        (* ToStringT passes through strings unchanged, and flows a generic StrT otherwise *)
        | (DefT (_, StrT _), ToStringT { orig_t = None; t_out; _ }) -> rec_flow cx trace (l, t_out)
        | (DefT (_, StrT _), ToStringT { orig_t = Some t; t_out; _ }) -> rec_flow cx trace (t, t_out)
        | (_, ToStringT { reason; t_out; _ }) -> rec_flow cx trace (StrT.why reason, t_out)
        (**********************)
        (* Array library call *)
        (**********************)
        | ( DefT (reason, ArrT (ArrayAT { elem_t; _ })),
            (GetPropT _ | SetPropT _ | MethodT _ | LookupT _)
          ) ->
          rec_flow cx trace (get_builtin_typeapp cx reason "Array" [elem_t], u)
        (*************************)
        (* Tuple "length" access *)
        (*************************)
        | ( DefT (reason, ArrT (TupleAT { elem_t = _; elements = _; arity; react_dro = _ })),
            GetPropT
              {
                use_op = _;
                reason = _;
                id = _;
                from_annot = _;
                propref = Named { name = OrdinaryName "length"; _ };
                tout;
                hint = _;
              }
          ) ->
          GetPropTKit.on_array_length cx trace reason arity (reason_of_use_t u) tout
        | ( DefT (reason, ArrT ((TupleAT _ | ROArrayAT _) as arrtype)),
            (GetPropT _ | SetPropT _ | MethodT _ | LookupT _)
          ) ->
          let t = elemt_of_arrtype arrtype in
          rec_flow cx trace (get_builtin_typeapp cx reason "$ReadOnlyArray" [t], u)
        (***********************)
        (* String library call *)
        (***********************)
        | (DefT (reason, StrT _), u) when primitive_promoting_use_t u ->
          rec_flow cx trace (get_builtin_type cx ~trace reason "String", u)
        (***********************)
        (* Number library call *)
        (***********************)
        | (DefT (reason, NumT _), u) when primitive_promoting_use_t u ->
          rec_flow cx trace (get_builtin_type cx ~trace reason "Number", u)
        (***********************)
        (* Boolean library call *)
        (***********************)
        | (DefT (reason, BoolT _), u) when primitive_promoting_use_t u ->
          rec_flow cx trace (get_builtin_type cx ~trace reason "Boolean", u)
        (***********************)
        (* BigInt library call *)
        (***********************)
        | (DefT (reason, BigIntT _), u) when primitive_promoting_use_t u ->
          rec_flow cx trace (get_builtin_type cx ~trace reason "BigInt", u)
        (***********************)
        (* Symbol library call *)
        (***********************)
        | (DefT (reason, SymbolT), u) when primitive_promoting_use_t u ->
          rec_flow cx trace (get_builtin_type cx ~trace reason "Symbol", u)
        (*****************************************************)
        (* Nice error messages for mixed function refinement *)
        (*****************************************************)
        | (DefT (lreason, MixedT Mixed_function), (MethodT _ | SetPropT _ | GetPropT _ | LookupT _))
          ->
          rec_flow cx trace (FunProtoT lreason, u)
        | (DefT (_, MixedT Mixed_function), CallT { call_action = ConcretizeCallee tout; _ }) ->
          rec_flow_t cx trace ~use_op:unknown_use (l, OpenT tout)
        | ( DefT (lreason, MixedT Mixed_function),
            CallT { use_op; reason = ureason; call_action = _; return_hint = _ }
          ) ->
          add_output
            cx
            ~trace
            (Error_message.EIncompatible
               {
                 lower = (lreason, None);
                 upper = (ureason, Error_message.IncompatibleMixedCallT);
                 use_op = Some use_op;
                 branches = [];
               }
            );
          rec_flow cx trace (AnyT.make (AnyError None) lreason, u)
        (* Special cases of FunT *)
        | (FunProtoApplyT reason, MethodT (use_op, call_r, lookup_r, propref, action))
        | (FunProtoBindT reason, MethodT (use_op, call_r, lookup_r, propref, action))
        | (FunProtoCallT reason, MethodT (use_op, call_r, lookup_r, propref, action)) ->
          let method_type =
            Tvar.mk_no_wrap_where cx lookup_r (fun tout ->
                let u =
                  GetPropT
                    {
                      use_op;
                      reason = lookup_r;
                      id = None;
                      from_annot = false;
                      propref;
                      tout;
                      hint = hint_unavailable;
                    }
                in
                rec_flow cx trace (FunProtoT reason, u)
            )
          in
          apply_method_action cx trace method_type use_op call_r l action
        | (FunProtoApplyT reason, _)
        | (FunProtoBindT reason, _)
        | (FunProtoCallT reason, _) ->
          rec_flow cx trace (FunProtoT reason, u)
        | (_, LookupT { propref; lookup_action; _ }) ->
          Default_resolve.default_resolve_touts
            ~flow:(rec_flow_t cx trace ~use_op:unknown_use)
            cx
            (reason_of_t l |> loc_of_reason)
            u;
          let use_op = Some (use_op_of_lookup_action lookup_action) in
          add_output
            cx
            ~trace
            (Error_message.EIncompatibleProp
               {
                 prop = name_of_propref propref;
                 reason_prop = reason_of_propref propref;
                 reason_obj = reason_of_t l;
                 special = error_message_kind_of_lower l;
                 use_op;
               }
            )
        | ( DefT (_, InstanceT { super; inst = { class_id; _ }; _ }),
            CheckUnusedPromiseT { reason; async }
          ) ->
          (match Flow_js_utils.builtin_promise_class_id cx with
          | None -> () (* Promise has some unexpected type *)
          | Some promise_class_id ->
            if ALoc.equal_id promise_class_id class_id then
              add_output
                cx
                ~trace
                (Error_message.EUnusedPromise { loc = loc_of_reason reason; async })
            else
              rec_flow cx trace (super, CheckUnusedPromiseT { reason; async }))
        | (_, CheckUnusedPromiseT _) -> ()
        | (DefT (lreason, StrT (Literal _)), WriteComputedObjPropCheckT _) ->
          let loc = loc_of_reason lreason in
          add_output cx ~trace Error_message.(EInternal (loc, PropRefComputedLiteral))
        | (AnyT (_, src), WriteComputedObjPropCheckT { reason; value_t; _ }) ->
          let src = any_mod_src_keep_placeholder Untyped src in
          rec_flow_t cx trace ~use_op:unknown_use (value_t, AnyT.why src reason)
        | (DefT (_, StrT _), WriteComputedObjPropCheckT { err_on_str_key; _ }) ->
          Base.Option.iter err_on_str_key ~f:(fun (use_op, reason_obj) ->
              add_output
                cx
                ~trace
                (Error_message.EPropNotFound
                   {
                     prop_name = None;
                     reason_prop = TypeUtil.reason_of_t l;
                     reason_obj;
                     use_op;
                     suggestion = None;
                   }
                )
          )
        | (DefT (reason, NumT lit), WriteComputedObjPropCheckT { reason_key; _ }) ->
          let kind = Error_message.InvalidObjKey.kind_of_num_lit lit in
          add_output
            cx
            ~trace
            (Error_message.EObjectComputedPropertyAssign (reason, reason_key, kind))
        | (_, WriteComputedObjPropCheckT { reason = _; reason_key; _ }) ->
          let reason = reason_of_t l in
          add_output
            cx
            ~trace
            (Error_message.EObjectComputedPropertyAssign
               (reason, reason_key, Error_message.InvalidObjKey.Other)
            )
        | _ ->
          add_output
            cx
            ~trace
            (Error_message.EIncompatible
               {
                 lower = (reason_of_t l, error_message_kind_of_lower l);
                 upper = (reason_of_use_t u, error_message_kind_of_upper u);
                 use_op = use_op_of_use_t u;
                 branches = [];
               }
            );
          let resolve_callee =
            match u with
            | CallT _ -> Some (reason_of_t l, [l])
            | _ -> None
          in
          Default_resolve.default_resolve_touts
            ~flow:(rec_flow_t cx trace ~use_op:unknown_use)
            ?resolve_callee
            cx
            (reason_of_t l |> loc_of_reason)
            u
      (* END OF PATTERN MATCH *)
    )

  (**
   * Binary arithmetic operations
   *
   * - number <> number = number
   * - bigint <> bigint = bigint
   * - string + string = string
   * - If one of the operands is any/empty, then the result is any/empty.
   *
   * We are _much_ less permissive than the spec with regard to coercion:
   * Numbers are allowed to coerce to strings for string concat (e.g., `num + '%'`)
   **)
  and flow_arith cx trace use_op reason flip l r u kind =
    if needs_resolution r || is_generic r then
      rec_flow
        cx
        trace
        (r, ArithT { use_op; reason; flip = not flip; rhs_t = l; result_t = u; kind })
    else
      let (l, r) =
        if flip then
          (r, l)
        else
          (l, r)
      in
      let t = Flow_js_utils.flow_arith cx ~trace reason l r kind in
      rec_flow_t cx trace ~use_op:unknown_use (t, u)

  (**
   * relational comparisons like <, >, <=, >=
   *
   * The following comparisons are allowed:
   * - str <> str
   * - num <> num
   * - bigint <> bigint
   * - date <> date
   * - _ <> empty
   * - empty <> _
   **)
  and flow_comparator cx trace reason flip l r =
    if needs_resolution r || is_generic r then
      rec_flow cx trace (r, ComparatorT { reason; flip = not flip; arg = l })
    else
      let (l, r) =
        if flip then
          (r, l)
        else
          (l, r)
      in
      match (l, r) with
      | (DefT (_, StrT _), DefT (_, StrT _))
      | (DefT (_, NumT _), DefT (_, NumT _))
      | (DefT (_, BigIntT _), DefT (_, BigIntT _))
      | (DefT (_, EmptyT), _)
      | (_, DefT (_, EmptyT)) ->
        ()
      | (l, r) when is_date l && is_date r -> ()
      | _ ->
        let reasons = FlowError.ordered_reasons (reason_of_t l, reason_of_t r) in
        add_output cx ~trace (Error_message.EComparison reasons)

  (**
   * == equality
   *
   * typecheck iff they intersect (otherwise, unsafe coercions may happen).
   *
   * note: almost any types may be compared with === (in)equality.
   **)
  and flow_eq cx trace reason flip l r =
    if needs_resolution r || is_generic r then
      rec_flow cx trace (r, EqT { reason; flip = not flip; arg = l })
    else
      let (l, r) =
        if flip then
          (r, l)
        else
          (l, r)
      in
      if equatable (l, r) then
        ()
      else
        let reasons = FlowError.ordered_reasons (reason_of_t l, reason_of_t r) in
        add_output cx ~trace (Error_message.ENonStrictEqualityComparison reasons)

  and flow_strict_eq cx trace reason cond_context flip l r =
    if needs_resolution r || is_generic r then
      rec_flow cx trace (r, StrictEqT { reason; cond_context; flip = not flip; arg = l })
    else
      let (l, r) =
        if flip then
          (r, l)
        else
          (l, r)
      in
      match strict_equatable_error cond_context (l, r) with
      | Some error -> add_output cx ~trace error
      | None -> ()

  (* Returns true when __flow should succeed immediately if EmptyT flows into u. *)
  and empty_success u =
    match u with
    (* Work has to happen when Empty flows to these types *)
    | UseT (_, OpenT _)
    | EvalTypeDestructorT _
    | UseT (_, DefT (_, TypeT _))
    | ChoiceKitUseT _
    | CondT _
    | ConditionalT _
    | DestructuringT _
    | EnumExhaustiveCheckT _
    | MakeExactT _
    | FilterMaybeT _
    | ObjKitT _
    | OptionalIndexedAccessT _
    | ReposLowerT _
    | ReposUseT _
    | SealGenericT _
    | ResolveUnionT _
    | EnumCastT _
    | ConvertEmptyPropsToMixedT _
    | ArithT _
    | HooklikeT _
    | SpecializeT _
    | ValueToTypeReferenceT _ ->
      false
    | _ -> true

  and handle_generic cx trace ~no_infer bound reason id name u =
    let make_generic t = GenericT { reason; id; name; bound = t; no_infer } in
    let narrow_generic_with_continuation mk_use_t cont =
      let t_out' = (reason, Tvar.mk_no_wrap cx reason) in
      let use_t = mk_use_t t_out' in
      rec_flow cx trace (reposition_reason cx reason bound, use_t);
      rec_flow cx trace (OpenT t_out', SealGenericT { reason; id; name; cont; no_infer })
    in
    let narrow_generic_use mk_use_t use_t_out =
      narrow_generic_with_continuation mk_use_t (Upper use_t_out)
    in
    let narrow_generic ?(use_op = unknown_use) mk_use_t t_out =
      narrow_generic_use (fun v -> mk_use_t (OpenT v)) (UseT (use_op, t_out))
    in
    let narrow_generic_tvar ?(use_op = unknown_use) mk_use_t t_out =
      narrow_generic_use mk_use_t (UseT (use_op, OpenT t_out))
    in
    let wait_for_concrete_bound ?(upper = u) () =
      rec_flow
        cx
        trace
        ( reposition_reason cx reason bound,
          SealGenericT { reason; id; name; cont = Upper upper; no_infer }
        );
      true
    in
    let distribute_union_intersection ?(upper = u) () =
      match bound with
      | UnionT (_, rep) ->
        let (t1, (t2, ts)) = UnionRep.members_nel rep in
        let union_of_generics =
          UnionRep.make (make_generic t1) (make_generic t2) (Base.List.map ~f:make_generic ts)
        in
        rec_flow cx trace (UnionT (reason, union_of_generics), upper);
        true
      | IntersectionT (_, rep) ->
        let (t1, (t2, ts)) = InterRep.members_nel rep in
        let inter_of_generics =
          InterRep.make (make_generic t1) (make_generic t2) (Base.List.map ~f:make_generic ts)
        in
        rec_flow cx trace (IntersectionT (reason, inter_of_generics), upper);
        true
      | _ -> false
    in
    let update_action_meth_generic_this l = function
      | CallM { methodcalltype = mct; return_hint; specialized_callee } ->
        CallM
          {
            methodcalltype = { mct with meth_generic_this = Some l };
            return_hint;
            specialized_callee;
          }
      | ChainM
          {
            exp_reason;
            lhs_reason;
            methodcalltype = mct;
            voided_out;
            return_hint;
            specialized_callee;
          } ->
        ChainM
          {
            exp_reason;
            lhs_reason;
            methodcalltype = { mct with meth_generic_this = Some l };
            voided_out;
            return_hint;
            specialized_callee;
          }
      | NoMethodAction t -> NoMethodAction t
    in
    if
      match bound with
      | GenericT { bound; id = id'; no_infer; _ } ->
        Generic.collapse id id'
        |> Base.Option.value_map ~default:false ~f:(fun id ->
               rec_flow cx trace (GenericT { reason; name; bound; id; no_infer }, u);
               true
           )
      (* The ClassT operation should commute with GenericT; that is, GenericT(ClassT(x)) = ClassT(GenericT(x)) *)
      | DefT (r, ClassT bound) ->
        rec_flow
          cx
          trace
          (DefT (r, ClassT (GenericT { reason = reason_of_t bound; name; bound; id; no_infer })), u);
        true
      | KeysT _ ->
        rec_flow
          cx
          trace
          ( reposition_reason cx reason bound,
            SealGenericT { reason; id; name; no_infer; cont = Upper u }
          );
        true
      | DefT (_, EmptyT) -> empty_success u
      | _ -> false
    then
      true
    else
      match u with
      (* In this set of cases, we flow the generic's upper bound to u. This is what we normally would do
         in the catch-all generic case anyways, but these rules are to avoid wildcards elsewhere in __flow. *)
      | ArithT _
      | EqT _
      | StrictEqT _
      | ComparatorT _
      | UnaryArithT _
      | AssertForInRHST _
      | AssertInstanceofRHST _
      | AssertBinaryInLHST _
      | AssertBinaryInRHST _
      | TestPropT _
      | OptionalChainT _
      | OptionalIndexedAccessT _
      | MapTypeT _
      (* the above case is not needed for correctness, but rather avoids a slow path in TupleMap *)
      | UseT (Op (Coercion _), DefT (_, StrT _)) ->
        rec_flow cx trace (reposition_reason cx reason bound, u);
        true
      | ReactKitT _ ->
        if is_concrete bound && not (is_literal_type bound) then
          distribute_union_intersection ()
        else
          wait_for_concrete_bound ()
      (* The LHS is what's actually getting refined--don't do anything special for the RHS *)
      | PredicateT (RightP _, _)
      | PredicateT (NotP (RightP _), _) ->
        false
      | PredicateT (pred, t_out) ->
        narrow_generic_tvar (fun t_out' -> PredicateT (pred, t_out')) t_out;
        true
      | ToStringT { orig_t; reason; t_out } ->
        narrow_generic_use
          (fun t_out' -> ToStringT { orig_t; reason; t_out = UseT (unknown_use, OpenT t_out') })
          t_out;
        true
      | UseT (use_op, MaybeT (r, t_out)) ->
        narrow_generic ~use_op (fun t_out' -> UseT (use_op, MaybeT (r, t_out'))) t_out;
        true
      | UseT (use_op, OptionalT ({ type_ = t_out; _ } as opt)) ->
        narrow_generic
          ~use_op
          (fun t_out' -> UseT (use_op, OptionalT { opt with type_ = t_out' }))
          t_out;
        true
      | DeepReadOnlyT (t_out, dro_loc, dro_type) ->
        narrow_generic_tvar (fun t_out' -> DeepReadOnlyT (t_out', dro_loc, dro_type)) t_out;
        true
      | HooklikeT t_out ->
        narrow_generic_tvar (fun t_out' -> HooklikeT t_out') t_out;
        true
      | FilterMaybeT (use_op, t_out) ->
        narrow_generic (fun t_out' -> FilterMaybeT (use_op, t_out')) t_out;
        true
      | FilterOptionalT (use_op, t_out) ->
        narrow_generic (fun t_out' -> FilterOptionalT (use_op, t_out')) t_out;
        true
      | ObjRestT (r, xs, t_out, id) ->
        narrow_generic (fun t_out' -> ObjRestT (r, xs, t_out', id)) t_out;
        true
      | MakeExactT (reason_op, k) ->
        narrow_generic_with_continuation
          (fun t_out -> MakeExactT (reason_op, Upper (UseT (unknown_use, OpenT t_out))))
          k;
        true
      | UseT (use_op, ExactT (r, u)) ->
        if is_concrete bound then
          match bound with
          | DefT (_, ObjT { flags; _ }) when not @@ Obj_type.is_exact flags.obj_kind ->
            let l = make_generic bound in
            exact_obj_error cx trace flags.obj_kind ~exact_reason:r ~use_op l;
            (* Continue the Flow even after we've errored. Often, there is more that
             * is different then just the fact that the upper bound is exact and the
             * lower bound is not. This could easily hide errors in ObjT ~> ExactT *)
            rec_flow_t cx trace ~use_op (l, u);
            true
          | _ -> false
        else
          wait_for_concrete_bound ()
      (* Support "new this.constructor ()" *)
      | GetPropT
          {
            use_op;
            reason = reason_op;
            id;
            from_annot;
            propref = Named { reason; name = OrdinaryName "constructor" as name; _ };
            tout;
            hint;
          } ->
        if is_concrete bound then
          match bound with
          | DefT (_, InstanceT _) ->
            narrow_generic_tvar
              (fun tout' ->
                GetPropT
                  {
                    use_op;
                    reason = reason_op;
                    id;
                    from_annot;
                    propref = mk_named_prop ~reason name;
                    tout = tout';
                    hint;
                  })
              tout;
            true
          | _ -> false
        else
          wait_for_concrete_bound ()
      | ConstructorT { use_op; reason = reason_op; targs; args; tout; return_hint } ->
        if is_concrete bound then
          match bound with
          | DefT (_, ClassT _) ->
            narrow_generic
              (fun tout' ->
                ConstructorT { use_op; reason = reason_op; targs; args; tout = tout'; return_hint })
              tout;
            true
          | _ -> false
        else
          wait_for_concrete_bound ()
      | ElemT _ ->
        if is_concrete bound && not (is_literal_type bound) then
          distribute_union_intersection ()
        else
          wait_for_concrete_bound ()
      | MethodT (op, r1, r2, prop, action) ->
        let l = make_generic bound in
        let action' = update_action_meth_generic_this l action in
        let u' = MethodT (op, r1, r2, prop, action') in
        let consumed =
          if is_concrete bound && not (is_literal_type bound) then
            distribute_union_intersection ~upper:u' ()
          else
            wait_for_concrete_bound ~upper:u' ()
        in
        if not consumed then rec_flow cx trace (reposition_reason cx reason bound, u');
        true
      | PrivateMethodT (op, r1, r2, prop, scopes, static, action) ->
        let l = make_generic bound in
        let action' = update_action_meth_generic_this l action in
        let u' = PrivateMethodT (op, r1, r2, prop, scopes, static, action') in
        let consumed =
          if is_concrete bound && not (is_literal_type bound) then
            distribute_union_intersection ~upper:u' ()
          else
            wait_for_concrete_bound ~upper:u' ()
        in
        if not consumed then rec_flow cx trace (reposition_reason cx reason bound, u');
        true
      | ObjKitT _
      | UseT (_, IntersectionT _) ->
        if is_concrete bound then
          distribute_union_intersection ()
        else
          wait_for_concrete_bound ()
      | UseT (_, (UnionT _ as u)) ->
        if union_optimization_guard cx ~equiv:false TypeUtil.quick_subtype bound u then begin
          if Context.is_verbose cx then prerr_endline "UnionT ~> UnionT fast path (via a generic)";
          true
        end else if is_concrete bound then
          distribute_union_intersection ()
        else
          wait_for_concrete_bound ()
      | UseT (_, KeysT _)
      | EvalTypeDestructorT _ ->
        if is_concrete bound then
          false
        else
          wait_for_concrete_bound ()
      | ResolveSpreadT _ when not (is_concrete bound) -> wait_for_concrete_bound ()
      | _ -> false

  (* "Expands" any to match the form of a type. Allows us to reuse our propagation rules for any
     cases. Note that it is not always safe to do this (ie in the case of unions).
     Note: we can get away with a shallow (i.e. non-recursive) expansion here because the flow between
     the any-expanded type and the original will handle the any-propagation to any relevant positions,
     some of which may invoke this function when they hit the any propagation functions in the
     recusive call to __flow. *)
  and expand_any _cx any t =
    let only_any _ = any in
    match t with
    | DefT (r, ArrT (ArrayAT _)) ->
      DefT (r, ArrT (ArrayAT { elem_t = any; tuple_view = None; react_dro = None }))
    | DefT (r, ArrT (TupleAT { elements; arity; react_dro; _ })) ->
      DefT
        ( r,
          ArrT
            (TupleAT
               {
                 react_dro;
                 elem_t = any;
                 elements =
                   Base.List.map
                     ~f:(fun (TupleElement { name; t; polarity; optional; reason }) ->
                       TupleElement { name; t = only_any t; polarity; optional; reason })
                     elements;
                 arity;
               }
            )
        )
    | OpaqueT (r, ({ underlying_t; super_t; opaque_type_args; _ } as opaquetype)) ->
      let opaquetype =
        {
          opaquetype with
          underlying_t = Base.Option.(underlying_t >>| only_any);
          super_t = Base.Option.(super_t >>| only_any);
          opaque_type_args =
            Base.List.(opaque_type_args >>| fun (str, r', _, polarity) -> (str, r', any, polarity));
        }
      in
      OpaqueT (r, opaquetype)
    | _ ->
      (* Just returning any would result in infinite recursion in most cases *)
      failwith "no any expansion defined for this case"

  and any_prop_to_function
      use_op
      { this_t = (this, _); params; rest_param; return_t; predicate; def_reason = _; hook = _ }
      covariant
      contravariant =
    List.iter (snd %> contravariant ~use_op) params;
    Base.Option.iter ~f:(fun (_, _, t) -> contravariant ~use_op t) rest_param;
    contravariant ~use_op this;
    let () =
      match predicate with
      | Some (TypeGuardBased { type_guard = t; _ }) -> covariant ~use_op t
      | _ -> ()
    in
    covariant ~use_op return_t

  and invariant_any_propagation_flow cx trace ~use_op any t =
    if Context.any_propagation cx then rec_unify cx trace ~use_op any t

  and any_prop_call_prop cx ~use_op ~covariant_flow = function
    | None -> ()
    | Some id -> covariant_flow ~use_op (Context.find_call cx id)

  and any_prop_properties cx trace ~use_op ~covariant_flow ~contravariant_flow any properties =
    properties
    |> NameUtils.Map.iter (fun _ property ->
           let polarity = Property.polarity property in
           property
           |> Property.iter_t (fun t ->
                  match polarity with
                  | Polarity.Positive -> covariant_flow ~use_op t
                  | Polarity.Negative -> contravariant_flow ~use_op t
                  | Polarity.Neutral -> invariant_any_propagation_flow cx trace ~use_op any t
              )
       )

  and any_prop_obj
      cx
      trace
      ~use_op
      ~covariant_flow
      ~contravariant_flow
      any
      { flags = _; props_tmap = _; proto_t = _; call_t = _; reachable_targs } =
    (* NOTE: Doing this always would be correct and desirable, but the
     * performance of doing this always is just not good enough. Instead,
     * we do it only in implicit instantiation to ensure that we do not get
     * spurious underconstrained errors when objects contain type arguments
     * that get any as a lower bound *)
    if Context.in_implicit_instantiation cx then
      reachable_targs
      |> List.iter (fun (t, p) ->
             match p with
             | Polarity.Positive -> covariant_flow ~use_op t
             | Polarity.Negative -> contravariant_flow ~use_op t
             | Polarity.Neutral -> invariant_any_propagation_flow cx trace ~use_op any t
         )

  (* FullyResolved tvars cannot contain non-FullyResolved parts, so there's no need to
   * deeply traverse them! *)
  and any_prop_tvar cx tvar =
    match Context.find_constraints cx tvar with
    | (_, FullyResolved _) -> true
    | _ -> false

  and any_prop_to_type_args cx trace ~use_op any ~covariant_flow ~contravariant_flow targs =
    List.iter
      (fun (_, _, t, polarity) ->
        match polarity with
        | Polarity.Positive -> covariant_flow ~use_op t
        | Polarity.Negative -> contravariant_flow ~use_op t
        | Polarity.Neutral -> invariant_any_propagation_flow cx trace ~use_op any t)
      targs

  (* TODO: Proper InstanceT propagation has non-termation issues that requires some
   * deep investigation. Punting on it for now. Note that using the type_args polarity
   * will likely be stricter than necessary. In practice, most type params do not
   * have variance sigils even if they are only used co/contravariantly.
   * Inline interfaces are an exception to this rule. The type_args there can be
   * empty even if the interface contains type arguments because they would only
   * appear in type_args if they are bound at the interface itself. We handle those
   * in the more general way, since they are used so rarely that non-termination is not
   * an issue (for now!) *)
  and any_prop_inst
      cx
      trace
      ~use_op
      any
      ~covariant_flow
      ~contravariant_flow
      static
      super
      implements
      {
        class_id = _;
        class_name = _;
        type_args;
        own_props;
        proto_props;
        inst_call_t;
        initialized_fields = _;
        initialized_static_fields = _;
        inst_kind;
        inst_dict = _;
        class_private_fields = _;
        class_private_methods = _;
        class_private_static_fields = _;
        class_private_static_methods = _;
      } =
    any_prop_to_type_args cx trace ~use_op any ~covariant_flow ~contravariant_flow type_args;
    match inst_kind with
    | InterfaceKind { inline = true } ->
      covariant_flow ~use_op static;
      covariant_flow ~use_op super;
      List.iter (covariant_flow ~use_op) implements;
      let property_prop =
        any_prop_properties cx trace ~use_op ~covariant_flow ~contravariant_flow any
      in
      property_prop (Context.find_props cx own_props);
      property_prop (Context.find_props cx proto_props);
      any_prop_call_prop cx ~use_op ~covariant_flow inst_call_t
    | _ -> ()

  (* types trapped for any propagation. Returns true if this function handles the any case, either
     by propagating or by doing the trivial case. False if the usetype needs to be handled
     separately. *)
  and any_propagated cx trace any u =
    let covariant_flow ~use_op t =
      if Context.any_propagation cx then rec_flow_t cx trace ~use_op (any, t)
    in
    let contravariant_flow ~use_op t =
      if Context.any_propagation cx then rec_flow_t cx trace ~use_op (t, any)
    in
    match u with
    | NotT (reason, t) ->
      if Context.any_propagation cx then
        rec_flow_t cx trace ~use_op:unknown_use (AnyT.why (AnyT.source any) reason, OpenT t);
      true
    | TryRenderTypePromotionT { use_op; reason_obj; reason = _; upper_renders; tried_promotion = _ }
      ->
      let renders = DefT (reason_obj, RendersT upper_renders) in
      covariant_flow ~use_op renders;
      true
    | ExitRendersT { renders_reason = _; u } -> any_propagated cx trace any u
    | UseT (use_op, DefT (_, ArrT (ROArrayAT (t, _)))) (* read-only arrays are covariant *)
    | UseT (use_op, DefT (_, ClassT t)) (* mk_instance ~for_type:false *)
    | UseT (use_op, ExactT (_, t)) ->
      covariant_flow ~use_op t;
      true
    | UseT
        (use_op, DefT (_, ReactAbstractComponentT { config; instance; renders; component_kind = _ }))
      ->
      contravariant_flow ~use_op config;
      covariant_flow ~use_op instance;
      covariant_flow ~use_op renders;
      true
    (* Some types just need to be expanded and filled with any types *)
    | UseT (use_op, (DefT (_, ArrT (ArrayAT _)) as t))
    | UseT (use_op, (DefT (_, ArrT (TupleAT _)) as t))
    | UseT (use_op, (OpaqueT _ as t)) ->
      if Context.any_propagation cx then rec_flow_t cx trace ~use_op (expand_any cx any t, t);
      true
    | UseT (use_op, DefT (_, FunT (_, funtype))) ->
      if Context.any_propagation cx then
        any_prop_to_function use_op funtype covariant_flow contravariant_flow;
      true
    | UseT (_, OpenT (_, id)) -> any_prop_tvar cx id
    (* AnnotTs are 0->1, so there's no need to propagate any inside them *)
    | UseT (_, AnnotT _) -> true
    | ArithT _
    | AndT _
    | ArrRestT _
    | AssertIterableT _
    | BindT _
    | CallT _
    | CallElemT _
    | CallLatentPredT _
    | ChoiceKitUseT _
    | CJSExtractNamedExportsT _
    | CondT _
    | ConstructorT _
    | CopyNamedExportsT _
    | CopyTypeExportsT _
    | CheckUntypedImportT _
    | DestructuringT _
    | ElemT _
    | EnumExhaustiveCheckT _
    | ExtendsUseT _
    | ConditionalT _
    | ExportNamedT _
    | ExportTypeT _
    | AssertExportIsTypeT _
    | ImplicitVoidReturnT _
    | GetElemT _
    | GetKeysT _
    | GetPrivatePropT _
    | GetPropT _
    | GetTypeFromNamespaceT _
    | GetProtoT _
    | GetStaticsT _
    | GetValuesT _
    | GetDictValuesT _
    | GuardT _
    | FilterOptionalT _
    | FilterMaybeT _
    | DeepReadOnlyT _
    | HooklikeT _
    | PreprocessKitT _
    | ResolveUnionT _
    | LookupT _
    | MakeExactT _
    | MapTypeT _
    | MethodT _
    | MixinT _
    | NullishCoalesceT _
    | ObjKitT _
    | ObjRestT _
    | ObjTestProtoT _
    | ObjTestT _
    | OptionalChainT _
    | OptionalIndexedAccessT _
    | OrT _
    | PredicateT _
    | PrivateMethodT _
    | PromoteRendersRepresentationT _
    | ReactKitT _
    | ReposLowerT _
    | ReposUseT _
    | ResolveSpreadT _
    | SealGenericT _
    | SentinelPropTestT _
    | SetElemT _
    | SetPropT _
    | SpecializeT _
    (* Should be impossible. We only generate these with OpenPredTs. *)
    | TestPropT _
    | ThisSpecializeT _
    | ToStringT _
    | UnaryArithT _
    | UseT (_, MaybeT _) (* used to filter maybe *)
    | UseT (_, OptionalT _) (* used to filter optional *)
    | ObjAssignFromT _
    (* Handled in __flow *)
    | ObjAssignToT _ (* Handled in __flow *)
    | UseT (_, ThisTypeAppT _)
    | UseT (_, TypeAppT _)
    | UseT (_, DefT (_, TypeT _))
    | ValueToTypeReferenceT _
    (* Should never occur, so we just defer to __flow to handle errors *)
    | UseT (_, InternalT _)
    | UseT (_, MatchingPropT _)
    | UseT (_, ModuleT _)
    | ReactPropsToOut _
    | ReactInToProps _
    (* Ideally, any would pollute every member of the union. However, it should be safe to only
       taint the type in the branch that flow picks when generating constraints for this, so
       this can be handled by the pre-existing rules *)
    | UseT (_, UnionT _)
    | UseT (_, IntersectionT _) (* Already handled in the wildcard case in __flow *)
    | EvalTypeDestructorT _
    | ConvertEmptyPropsToMixedT _
    | CheckUnusedPromiseT _ ->
      false
    | UseT (use_op, DefT (_, ObjT obj)) ->
      any_prop_obj cx trace ~use_op ~covariant_flow ~contravariant_flow any obj;
      true
    | UseT (use_op, DefT (_, InstanceT { static; super; implements; inst })) ->
      any_prop_inst
        cx
        trace
        ~use_op
        any
        ~covariant_flow
        ~contravariant_flow
        static
        super
        implements
        inst;
      true
    (* These types have no t_out, so can't propagate anything. Thus we short-circuit by returning
       true *)
    | AssertBinaryInLHST _
    | AssertBinaryInRHST _
    | AssertForInRHST _
    | AssertImportIsValueT _
    | AssertInstanceofRHST _
    | AssertNonComponentLikeT _
    | ComparatorT _
    | DebugPrintT _
    | DebugSleepT _
    | StrictEqT _
    | EqT _
    | HasOwnPropT _
    | ExtractReactRefT _
    | ImplementsT _
    | InvariantT _
    | SetPrivatePropT _
    | SetProtoT _
    | SuperT _
    | TypeCastT _
    | EnumCastT _
    | VarianceCheckT _
    | ConcretizeTypeAppsT _
    | UseT (_, KeysT _) (* Any won't interact with the type inside KeysT, so it can't be tainted *)
    | WriteComputedObjPropCheckT _ ->
      true
    (* TODO: Punt on these for now, but figure out whether these should fall through or not *)
    | UseT (_, CustomFunT (_, ReactElementFactory _))
    | UseT (_, CustomFunT (_, ObjectAssign))
    | UseT (_, CustomFunT (_, ObjectGetPrototypeOf))
    | UseT (_, CustomFunT (_, ObjectSetPrototypeOf))
    | UseT (_, CustomFunT (_, Compose _))
    | UseT (_, CustomFunT (_, ReactCreateElement))
    | UseT (_, CustomFunT (_, ReactCloneElement))
    | UseT (_, CustomFunT (_, DebugPrint))
    | UseT (_, CustomFunT (_, DebugThrow))
    | UseT (_, CustomFunT (_, DebugSleep))
    | UseT _ ->
      true

  (* Propagates any flows in case of contravariant/invariant subtypes: the any must pollute
     all types in contravariant positions when t <: any. *)
  and any_propagated_use cx trace use_op any l =
    let covariant_flow ~use_op t =
      if Context.any_propagation cx then rec_flow_t cx trace ~use_op (t, any)
    in
    let contravariant_flow ~use_op t =
      if Context.any_propagation cx then rec_flow_t cx trace ~use_op (any, t)
    in
    match l with
    | DefT (_, FunT (_, funtype)) ->
      (* function types are contravariant in the arguments *)
      any_prop_to_function use_op funtype covariant_flow contravariant_flow;
      true
    (* Some types just need to be expanded and filled with any types *)
    | (DefT (_, ArrT (ArrayAT _)) as t)
    | (DefT (_, ArrT (TupleAT _)) as t)
    | (OpaqueT _ as t) ->
      if Context.any_propagation cx then rec_flow_t cx trace ~use_op (t, expand_any cx any t);
      true
    | KeysT _ ->
      (* Keys cannot be tainted by any *)
      true
    | DefT (_, ClassT t)
    | DefT (_, ArrT (ROArrayAT (t, _))) ->
      covariant_flow ~use_op t;
      true
    | DefT (_, ReactAbstractComponentT { config; instance; renders; component_kind = _ }) ->
      contravariant_flow ~use_op config;
      covariant_flow ~use_op instance;
      covariant_flow ~use_op renders;
      true
    | GenericT { bound; _ } ->
      covariant_flow ~use_op bound;
      true
    | DefT (_, ObjT obj) ->
      any_prop_obj cx trace ~use_op ~covariant_flow ~contravariant_flow any obj;
      true
    | DefT (_, InstanceT { static; super; implements; inst }) ->
      any_prop_inst
        cx
        trace
        ~use_op
        any
        ~covariant_flow
        ~contravariant_flow
        static
        super
        implements
        inst;
      true
    (* These types have no negative positions in their lower bounds *)
    | FunProtoApplyT _
    | FunProtoBindT _
    | FunProtoCallT _
    | FunProtoT _
    | ObjProtoT _
    | NullProtoT _ ->
      true
    (* AnnotTs are 0->1, so there's no need to propagate any inside them *)
    | AnnotT _ -> true
    | OpenT (_, id) -> any_prop_tvar cx id
    (* Handled already in __flow *)
    | ExactT _
    | ThisInstanceT _
    | EvalT _
    | MatchingPropT _
    | OptionalT _
    | MaybeT _
    | DefT (_, PolyT _)
    | TypeAppT _
    | UnionT _
    | IntersectionT _
    | CustomFunT (_, DebugPrint)
    | ThisTypeAppT _
    | InternalT (EnforceUnionOptimized _) ->
      false
    (* Should never occur as the lower bound of any *)
    | InternalT (ChoiceKitT _)
    | InternalT (ExtendsT _)
    | ModuleT _
    | NamespaceT _ ->
      false
    (* TODO: Punt on these for now, but figure out whether these should fall through or not *)
    | CustomFunT (_, ReactElementFactory _)
    | CustomFunT (_, ObjectAssign)
    | CustomFunT (_, ObjectGetPrototypeOf)
    | CustomFunT (_, ObjectSetPrototypeOf)
    | CustomFunT (_, Compose _)
    | CustomFunT (_, ReactCreateElement)
    | CustomFunT (_, ReactCloneElement)
    | CustomFunT (_, DebugThrow)
    | CustomFunT (_, DebugSleep)
    | DefT _
    | AnyT _ ->
      true

  (*********************)
  (* inheritance utils *)
  (*********************)
  and flow_type_args cx trace ~use_op lreason ureason targs1 targs2 =
    List.iter2
      (fun (x, targ_reason, t1, polarity) (_, _, t2, _) ->
        let use_op =
          Frame
            ( TypeArgCompatibility
                { name = x; targ = targ_reason; lower = lreason; upper = ureason; polarity },
              use_op
            )
        in
        match polarity with
        | Polarity.Negative -> rec_flow cx trace (t2, UseT (use_op, t1))
        | Polarity.Positive -> rec_flow cx trace (t1, UseT (use_op, t2))
        | Polarity.Neutral -> rec_unify cx trace ~use_op t1 t2)
      targs1
      targs2

  and inst_type_to_obj_type cx reason_struct (own_props_id, proto_props_id, call_id, inst_dict) =
    let own_props = Context.find_props cx own_props_id in
    let proto_props = Context.find_props cx proto_props_id in
    let props_tmap = Properties.generate_id () in
    Context.add_property_map cx props_tmap (NameUtils.Map.union own_props proto_props);
    (* Interfaces with an indexer type are indexed, all others are inexact *)
    let obj_kind =
      match inst_dict with
      | Some d -> Indexed d
      | None -> Inexact
    in
    let o =
      {
        flags = { obj_kind; frozen = false; react_dro = None };
        props_tmap;
        (* Interfaces have no prototype *)
        proto_t = ObjProtoT reason_struct;
        call_t = call_id;
        reachable_targs = [];
      }
    in
    DefT (reason_struct, ObjT o)

  (* dispatch checks to verify that lower satisfies the structural
     requirements given in the tuple. *)
  (* TODO: own_props/proto_props is misleading, since they come from interfaces,
     which don't have an own/proto distinction. *)
  and structural_subtype
      cx trace ~use_op lower reason_struct (own_props_id, proto_props_id, call_id, inst_dict) =
    match lower with
    (* Object <: Interface subtyping creates an object out of the interface to dispatch to the
       existing object <: object logic *)
    | DefT
        ( lreason,
          ObjT
            {
              flags = { obj_kind = lkind; frozen = lfrozen; react_dro = _ };
              props_tmap = lprops;
              proto_t = lproto;
              call_t = lcall;
              reachable_targs = lreachable_targs;
            }
        ) ->
      let o =
        inst_type_to_obj_type cx reason_struct (own_props_id, proto_props_id, call_id, inst_dict)
      in
      let lower =
        DefT
          ( lreason,
            ObjT
              {
                flags = { obj_kind = lkind; frozen = lfrozen; react_dro = None };
                props_tmap = lprops;
                proto_t = lproto;
                call_t = lcall;
                reachable_targs = lreachable_targs;
              }
          )
      in
      rec_flow_t cx trace ~use_op (lower, o)
    | _ ->
      inst_structural_subtype
        cx
        trace
        ~use_op
        lower
        reason_struct
        (own_props_id, proto_props_id, call_id, inst_dict)

  and inst_structural_subtype
      cx trace ~use_op lower reason_struct (own_props_id, proto_props_id, call_id, inst_dict) =
    let lreason = reason_of_t lower in
    let lit = is_literal_object_reason lreason in
    let own_props = Context.find_props cx own_props_id in
    let proto_props = Context.find_props cx proto_props_id in
    let call_t = Base.Option.map call_id ~f:(Context.find_call cx) in
    let read_only_if_lit p =
      match p with
      | Field { key_loc = _; type_; _ } when lit ->
        OrdinaryField { type_; polarity = Polarity.Positive }
      | _ -> Property.type_ p
    in
    inst_dict
    |> Base.Option.iter ~f:(fun { key = ukey; value = uvalue; dict_polarity = upolarity; _ } ->
           match lower with
           | DefT
               ( _,
                 InstanceT
                   {
                     inst =
                       {
                         inst_dict =
                           Some { key = lkey; value = lvalue; dict_polarity = lpolarity; _ };
                         _;
                       };
                     _;
                   }
               ) ->
             rec_flow_p
               cx
               ~trace
               ~report_polarity:false
               ~use_op:
                 (Frame (IndexerKeyCompatibility { lower = lreason; upper = reason_struct }, use_op))
               lreason
               reason_struct
               (Computed ukey)
               ( OrdinaryField { type_ = lkey; polarity = lpolarity },
                 OrdinaryField { type_ = ukey; polarity = upolarity }
               );
             rec_flow_p
               cx
               ~trace
               ~use_op:
                 (Frame
                    ( PropertyCompatibility { prop = None; lower = lreason; upper = reason_struct },
                      use_op
                    )
                 )
               lreason
               reason_struct
               (Computed uvalue)
               ( OrdinaryField { type_ = lvalue; polarity = lpolarity },
                 OrdinaryField { type_ = uvalue; polarity = upolarity }
               )
           | _ -> ()
       );
    own_props
    |> NameUtils.Map.iter (fun name p ->
           let use_op =
             Frame
               ( PropertyCompatibility { prop = Some name; lower = lreason; upper = reason_struct },
                 use_op
               )
           in
           match p with
           | Field { type_ = OptionalT _ as t; polarity; _ } ->
             let propref =
               let reason =
                 update_desc_reason (fun desc -> ROptional (RPropertyOf (name, desc))) reason_struct
               in
               mk_named_prop ~reason name
             in
             let polarity =
               if lit then
                 Polarity.Positive
               else
                 polarity
             in
             rec_flow
               cx
               trace
               ( lower,
                 LookupT
                   {
                     reason = reason_struct;
                     lookup_kind =
                       NonstrictReturning
                         (Base.Option.map ~f:(fun { value; _ } -> (value, t)) inst_dict, None);
                     try_ts_on_failure = [];
                     propref;
                     lookup_action = LookupProp (use_op, OrdinaryField { type_ = t; polarity });
                     method_accessible = true;
                     ids = Some Properties.Set.empty;
                     ignore_dicts = false;
                   }
               )
           | _ ->
             let propref =
               let reason =
                 update_desc_reason (fun desc -> RPropertyOf (name, desc)) reason_struct
               in
               mk_named_prop ~reason name
             in
             rec_flow
               cx
               trace
               ( lower,
                 LookupT
                   {
                     reason = reason_struct;
                     lookup_kind = Strict lreason;
                     try_ts_on_failure = [];
                     propref;
                     lookup_action = LookupProp (use_op, read_only_if_lit p);
                     method_accessible = true;
                     ids = Some Properties.Set.empty;
                     ignore_dicts = false;
                   }
               )
       );
    proto_props
    |> NameUtils.Map.iter (fun name p ->
           let use_op =
             Frame
               ( PropertyCompatibility { prop = Some name; lower = lreason; upper = reason_struct },
                 use_op
               )
           in
           let propref =
             let reason = update_desc_reason (fun desc -> RPropertyOf (name, desc)) reason_struct in
             mk_named_prop ~reason name
           in
           rec_flow
             cx
             trace
             ( lower,
               LookupT
                 {
                   reason = reason_struct;
                   lookup_kind = Strict lreason;
                   try_ts_on_failure = [];
                   propref;
                   lookup_action = LookupProp (use_op, read_only_if_lit p);
                   method_accessible = true;
                   ids = Some Properties.Set.empty;
                   ignore_dicts = false;
                 }
             )
       );
    call_t
    |> Base.Option.iter ~f:(fun ut ->
           let prop_name = Some (OrdinaryName "$call") in
           let use_op =
             Frame
               ( PropertyCompatibility { prop = prop_name; lower = lreason; upper = reason_struct },
                 use_op
               )
           in
           match lower with
           | DefT (_, ObjT { call_t = Some lid; _ })
           | DefT (_, InstanceT { inst = { inst_call_t = Some lid; _ }; _ }) ->
             let lt = Context.find_call cx lid in
             rec_flow cx trace (lt, UseT (use_op, ut))
           | _ ->
             let reason_prop =
               update_desc_reason
                 (fun desc -> RPropertyOf (OrdinaryName "$call", desc))
                 reason_struct
             in
             let error_message =
               Error_message.EPropNotFound
                 { reason_prop; reason_obj = lreason; prop_name; use_op; suggestion = None }
             in
             add_output cx ~trace error_message
       )

  and check_super cx trace ~use_op lreason ureason t x p =
    let use_op =
      Frame (PropertyCompatibility { prop = Some x; lower = lreason; upper = ureason }, use_op)
    in
    let reason_prop = replace_desc_reason (RProperty (Some x)) lreason in
    let action = SuperProp (use_op, Property.type_ p) in
    let t =
      (* munge names beginning with single _ *)
      if is_munged_prop_name cx x then
        ObjProtoT (reason_of_t t)
      else
        t
    in
    let propref = mk_named_prop ~reason:reason_prop x in
    FlowJs.rec_flow
      cx
      trace
      ( t,
        LookupT
          {
            reason = lreason;
            lookup_kind = NonstrictReturning (None, None);
            try_ts_on_failure = [];
            propref;
            lookup_action = action;
            ids = Some Properties.Set.empty;
            method_accessible = true;
            ignore_dicts = false;
          }
      )

  and destruct cx ~trace reason kind t selector tout id =
    let annot =
      match kind with
      | DestructAnnot -> true
      | DestructInfer -> false
    in
    eval_selector cx ~trace ~annot reason t selector tout id

  and eval_selector cx ?trace ~annot reason curr_t s tvar id =
    flow_opt
      cx
      ?trace
      ( curr_t,
        match s with
        | Prop (name, has_default) ->
          let name = OrdinaryName name in
          let lookup_ub () =
            let use_op = unknown_use in
            let action = ReadProp { use_op; obj_t = curr_t; tout = tvar } in
            (* LookupT unifies with the default with tvar. To get around that, we can create some
             * indirection with a fresh tvar in between to ensure that we only add a lower bound
             *)
            let default_tout =
              Tvar.mk_where cx reason (fun tout ->
                  flow_opt cx ?trace (tout, UseT (use_op, OpenT tvar))
              )
            in
            let void_reason = replace_desc_reason RVoid (fst tvar) in
            let lookup_kind =
              NonstrictReturning (Some (DefT (void_reason, VoidT), default_tout), None)
            in
            LookupT
              {
                reason;
                lookup_kind;
                try_ts_on_failure = [];
                propref = mk_named_prop ~reason name;
                lookup_action = action;
                method_accessible = false;
                ids = Some Properties.Set.empty;
                ignore_dicts = false;
              }
          in
          let getprop_ub () =
            GetPropT
              {
                use_op = unknown_use;
                reason;
                id = Some id;
                from_annot = annot;
                propref = mk_named_prop ~reason name;
                tout = tvar;
                hint = hint_unavailable;
              }
          in
          if has_default then
            match curr_t with
            | DefT (_, NullT) -> getprop_ub ()
            | DefT (_, ObjT { flags = { obj_kind; _ }; proto_t = ObjProtoT _; _ })
              when Obj_type.is_exact obj_kind ->
              lookup_ub ()
            | _ -> getprop_ub ()
          else
            getprop_ub ()
        | Elem key_t ->
          GetElemT
            {
              use_op = unknown_use;
              reason;
              id = None;
              from_annot = annot;
              access_iterables = false;
              key_t;
              tout = tvar;
            }
        | ObjRest xs -> ObjRestT (reason, xs, OpenT tvar, id)
        | ArrRest i -> ArrRestT (unknown_use, reason, i, OpenT tvar)
        | Default -> PredicateT (NotP VoidP, tvar)
      )

  and mk_type_destructor cx ~trace use_op reason t d id =
    let evaluated = Context.evaluated cx in
    (* As an optimization, unwrap resolved tvars so that they are only evaluated
     * once to an annotation instead of a tvar that gets a bound on both sides. *)
    let t = drop_resolved cx t in
    let result =
      match Eval.Map.find_opt id evaluated with
      | Some cached_t -> cached_t
      | None ->
        (* The OpenT branch is a correct implementation of type destructors for all
         * types. However, because it adds a constraint to both sides of a type we may
         * end up doing some work twice. So as an optimization for concrete types
         * we have a fall-through branch that only evaluates our type destructor once.
         * The second branch then uses AnnotT to both concretize the result for use
         * as a lower or upper bound and prevent new bounds from being added to
         * the result.
         *)
        let f tvar =
          match t with
          | OpenT _
          | GenericT { bound = OpenT _; _ } ->
            let x =
              EvalTypeDestructorT
                { destructor_use_op = use_op; reason; repos = None; destructor = d; tout = tvar }
            in
            rec_flow cx trace (t, x)
          | GenericT { bound = AnnotT (r, t, use_desc); reason; name; id; no_infer } ->
            let x =
              EvalTypeDestructorT
                {
                  destructor_use_op = use_op;
                  reason;
                  repos = Some (r, use_desc);
                  destructor = d;
                  tout = tvar;
                }
            in
            rec_flow cx trace (GenericT { reason; name; id; bound = t; no_infer }, x)
          | EvalT _ ->
            let x =
              EvalTypeDestructorT
                { destructor_use_op = use_op; reason; repos = None; destructor = d; tout = tvar }
            in
            rec_flow cx trace (t, x)
          | AnnotT (r, t, use_desc) ->
            let x =
              EvalTypeDestructorT
                {
                  destructor_use_op = use_op;
                  reason;
                  repos = Some (r, use_desc);
                  destructor = d;
                  tout = tvar;
                }
            in
            rec_flow cx trace (t, x)
          | _ -> eval_destructor cx ~trace use_op reason t d tvar
        in
        Tvar.mk_no_wrap_where cx reason (fun tvar ->
            Context.set_evaluated cx (Eval.Map.add id (OpenT tvar) evaluated);
            f tvar
        )
    in
    if
      (not (Tvar_resolver.has_unresolved_tvars cx t))
      && not (Tvar_resolver.has_unresolved_tvars_in_destructors cx d)
    then
      Tvar_resolver.resolve cx result;
    result

  and eval_destructor cx ~trace use_op reason t d tout =
    match d with
    (* Non-homomorphic mapped types have their own special resolution code, so they do not fit well
     * into the structure of the rest of this function. We handle them upfront instead. *)
    | MappedType
        {
          homomorphic = Unspecialized;
          mapped_type_flags;
          property_type;
          distributive_tparam_name = _;
        } ->
      let t =
        ObjectKit.mapped_type_of_keys
          cx
          trace
          use_op
          reason
          ~keys:t
          ~property_type
          mapped_type_flags
      in
      (* Intentional unknown_use for the tout Flow *)
      rec_flow cx trace (t, UseT (unknown_use, OpenT tout))
    | ReactPromoteRendersRepresentation
        { should_distribute = true; promote_structural_components; renders_variant; resolved_elem }
      ->
      rec_flow
        cx
        trace
        ( t,
          PromoteRendersRepresentationT
            {
              use_op;
              reason;
              tout = OpenT tout;
              resolved_elem;
              should_distribute = true;
              promote_structural_components;
              renders_variant;
            }
        )
    | _ ->
      let destruct_union ?(f = (fun t -> t)) r members upper =
        let destructor = TypeDestructorT (use_op, reason, d) in
        (* `ResolveUnionT` resolves in reverse order, so `rev_map` here so we resolve in the original order. *)
        let unresolved =
          members |> Base.List.rev_map ~f:(fun t -> Cache.Eval.id cx (f t) destructor)
        in
        let (first, unresolved) = (List.hd unresolved, List.tl unresolved) in
        let u =
          ResolveUnionT { reason = r; unresolved; resolved = []; upper; id = Reason.mk_id () }
        in
        rec_flow cx trace (first, u)
      in
      let destruct_maybe ?f r t upper =
        let reason = replace_desc_new_reason RNullOrVoid r in
        let null = NullT.make reason in
        let void = VoidT.make reason in
        destruct_union ?f reason [t; null; void] upper
      in
      let destruct_optional ?f r t upper =
        let reason = replace_desc_new_reason RVoid r in
        let void = VoidT.make reason in
        destruct_union ?f reason [t; void] upper
      in
      let should_destruct_union () =
        match d with
        | ConditionalType { distributive_tparam_name; _ } -> Option.is_some distributive_tparam_name
        | ReactCheckComponentRef -> false
        | ReactDRO _ ->
          (match t with
          | UnionT (_, rep) ->
            if not (UnionRep.is_optimized_finally rep) then
              UnionRep.optimize_enum_only ~flatten:(Type_mapper.union_flatten cx) rep;
            Option.is_none (UnionRep.check_enum rep)
          | _ -> true)
        | _ -> true
      in
      (match t with
      | GenericT
          { bound = OpaqueT (_, { underlying_t = Some t; _ }); reason = r; id; name; no_infer }
        when ALoc.source (loc_of_reason r) = ALoc.source (def_loc_of_reason r) ->
        eval_destructor
          cx
          ~trace
          use_op
          reason
          (GenericT { bound = t; reason = r; id; name; no_infer })
          d
          tout
      | OpaqueT (r, { underlying_t = Some t; _ })
        when ALoc.source (loc_of_reason r) = ALoc.source (def_loc_of_reason r) ->
        eval_destructor cx ~trace use_op reason t d tout
      (* Specialize TypeAppTs before evaluating them so that we can handle special
         cases. Like the union case below. mk_typeapp_instance will return an AnnotT
         which will be fully resolved using the AnnotT case above. *)
      | GenericT
          {
            bound =
              TypeAppT { reason = _; use_op = use_op_tapp; type_; targs; from_value; use_desc = _ };
            reason = reason_tapp;
            id;
            name;
            no_infer;
          } ->
        let destructor = TypeDestructorT (use_op, reason, d) in
        let t =
          mk_typeapp_instance_annot
            cx
            ~trace
            ~use_op:use_op_tapp
            ~reason_op:reason
            ~reason_tapp
            ~from_value
            type_
            targs
        in
        rec_flow
          cx
          trace
          ( Cache.Eval.id
              cx
              (GenericT { bound = t; name; id; reason = reason_tapp; no_infer })
              destructor,
            UseT (use_op, OpenT tout)
          )
      | TypeAppT
          { reason = reason_tapp; use_op = use_op_tapp; type_; targs; from_value; use_desc = _ } ->
        let destructor = TypeDestructorT (use_op, reason, d) in
        let t =
          mk_typeapp_instance_annot
            cx
            ~trace
            ~use_op:use_op_tapp
            ~reason_op:reason
            ~reason_tapp
            ~from_value
            type_
            targs
        in
        rec_flow_t cx trace ~use_op:unknown_use (Cache.Eval.id cx t destructor, OpenT tout)
      (* If we are destructuring a union, evaluating the destructor on the union
         itself may have the effect of splitting the union into separate lower
         bounds, which prevents the speculative match process from working.
         Instead, we preserve the union by pushing down the destructor onto the
         branches of the unions.
      *)
      | UnionT (r, rep) when should_destruct_union () ->
        destruct_union r (UnionRep.members rep) (UseT (unknown_use, OpenT tout))
      | GenericT { reason; bound = UnionT (_, rep); id; name; no_infer }
        when should_destruct_union () ->
        destruct_union
          ~f:(fun bound -> GenericT { reason = reason_of_t bound; bound; id; name; no_infer })
          reason
          (UnionRep.members rep)
          (UseT (use_op, OpenT tout))
      | MaybeT (r, t) when should_destruct_union () ->
        destruct_maybe r t (UseT (unknown_use, OpenT tout))
      | GenericT { reason; bound = MaybeT (_, t); id; name; no_infer } when should_destruct_union ()
        ->
        destruct_maybe
          ~f:(fun bound -> GenericT { reason = reason_of_t bound; bound; id; name; no_infer })
          reason
          t
          (UseT (use_op, OpenT tout))
      | OptionalT { reason = r; type_ = t; use_desc = _ } when should_destruct_union () ->
        destruct_optional r t (UseT (unknown_use, OpenT tout))
      | GenericT
          { reason; bound = OptionalT { reason = _; type_ = t; use_desc = _ }; id; name; no_infer }
        when should_destruct_union () ->
        destruct_optional
          ~f:(fun bound -> GenericT { reason = reason_of_t bound; bound; id; name; no_infer })
          reason
          t
          (UseT (use_op, OpenT tout))
      | AnnotT (r, t, use_desc) ->
        let t = reposition_reason ~trace cx r ~use_desc t in
        let destructor = TypeDestructorT (use_op, reason, d) in
        rec_flow_t cx trace ~use_op:unknown_use (Cache.Eval.id cx t destructor, OpenT tout)
      | GenericT { bound = AnnotT (_, t, use_desc); reason = r; name; id; no_infer } ->
        let t = reposition_reason ~trace cx r ~use_desc t in
        let destructor = TypeDestructorT (use_op, reason, d) in
        rec_flow_t
          cx
          trace
          ~use_op
          ( Cache.Eval.id cx (GenericT { reason = r; id; name; bound = t; no_infer }) destructor,
            OpenT tout
          )
      | _ ->
        rec_flow
          cx
          trace
          ( t,
            match d with
            | NonMaybeType ->
              (* We intentionally use `unknown_use` here! When we flow to a tout we never
               * want to carry a `use_op`. We want whatever `use_op` the tout is used with
               * to win. *)
              FilterMaybeT (unknown_use, OpenT tout)
            | PropertyType { name; _ } ->
              let reason_op = replace_desc_reason (RProperty (Some name)) reason in
              GetPropT
                {
                  use_op;
                  reason;
                  id = None;
                  from_annot = true;
                  propref = mk_named_prop ~reason:reason_op name;
                  tout;
                  hint = hint_unavailable;
                }
            | ElementType { index_type; _ } ->
              GetElemT
                {
                  use_op;
                  reason;
                  id = None;
                  from_annot = true;
                  access_iterables = false;
                  key_t = index_type;
                  tout;
                }
            | OptionalIndexedAccessNonMaybeType { index } ->
              OptionalIndexedAccessT { use_op; reason; index; tout_tvar = tout }
            | OptionalIndexedAccessResultType { void_reason } ->
              let void = VoidT.why void_reason in
              ResolveUnionT
                {
                  reason;
                  resolved = [void];
                  unresolved = [];
                  upper = UseT (unknown_use, OpenT tout);
                  id = Reason.mk_id ();
                }
            | SpreadType (options, todo_rev, head_slice) ->
              Object.(
                Object.Spread.(
                  let tool = Resolve Next in
                  let state =
                    {
                      todo_rev;
                      acc =
                        Base.Option.value_map ~f:(fun x -> [InlineSlice x]) ~default:[] head_slice;
                      spread_id = Reason.mk_id ();
                      union_reason = None;
                      curr_resolve_idx = 0;
                    }
                  in
                  ObjKitT (use_op, reason, tool, Spread (options, state), OpenT tout)
                )
              )
            | SpreadTupleType { reason_tuple; reason_spread = _; resolved; unresolved } ->
              let elem_tout = Tvar.mk cx reason_tuple in
              ResolveSpreadT
                ( use_op,
                  reason_tuple,
                  {
                    rrt_resolved = resolved;
                    rrt_unresolved = unresolved;
                    rrt_resolve_to =
                      ResolveSpreadsToTupleType (Reason.mk_id (), elem_tout, OpenT tout);
                  }
                )
            | ReactCheckComponentRef -> ExtractReactRefT (reason, OpenT tout)
            | ReactCheckComponentConfig pmap ->
              Object.(
                let tool = Resolve Next in
                ObjKitT (use_op, reason, tool, Object.ReactCheckComponentConfig pmap, OpenT tout)
              )
            | RestType (options, t) ->
              Object.(
                Object.Rest.(
                  let tool = Resolve Next in
                  let state = One t in
                  ObjKitT (use_op, reason, tool, Rest (options, state), OpenT tout)
                )
              )
            | ReadOnlyType -> Object.(ObjKitT (use_op, reason, Resolve Next, ReadOnly, OpenT tout))
            | ReactDRO (dro_loc, dro_type) -> DeepReadOnlyT (tout, dro_loc, dro_type)
            | MakeHooklike -> HooklikeT tout
            | PartialType -> Object.(ObjKitT (use_op, reason, Resolve Next, Partial, OpenT tout))
            | RequiredType -> Object.(ObjKitT (use_op, reason, Resolve Next, Required, OpenT tout))
            | ValuesType -> GetValuesT (reason, OpenT tout)
            | CallType { from_maptype; args } ->
              let args = Base.List.map ~f:(fun arg -> Arg arg) args in
              let call_kind =
                if from_maptype then
                  MapTypeKind
                else
                  CallTypeKind
              in
              let call = mk_functioncalltype ~call_kind reason None args tout in
              let call = { call with call_strict_arity = false } in
              let use_op =
                match use_op with
                (* The following use ops are for operations that internally delegate to CallType. We
                   don't want to leak the internally delegation to error messages by pushing an
                   additional frame. Alternatively, we could have pushed here and filtered out when
                   rendering error messages, but that seems a bit wasteful. *)
                | Frame (TupleMapFunCompatibility _, _)
                | Frame (ObjMapFunCompatibility _, _)
                | Frame (ObjMapiFunCompatibility _, _) ->
                  use_op
                (* For external CallType operations, we push an additional frame to distinguish their
                   error messages from those of "normal" calls. *)
                | _ -> Frame (CallFunCompatibility { n = List.length args }, use_op)
              in
              CallT
                {
                  use_op;
                  reason;
                  call_action = Funcalltype call;
                  return_hint = Type.hint_unavailable;
                }
            | ConditionalType
                { distributive_tparam_name; infer_tparams; extends_t; true_t; false_t } ->
              ConditionalT
                {
                  use_op;
                  reason;
                  distributive_tparam_name;
                  infer_tparams;
                  extends_t;
                  true_t;
                  false_t;
                  tout;
                }
            | TypeMap tmap -> MapTypeT (use_op, reason, tmap, OpenT tout)
            | ReactElementPropsType -> ReactKitT (use_op, reason, React.GetProps (OpenT tout))
            | ReactElementConfigType -> ReactKitT (use_op, reason, React.GetConfig (OpenT tout))
            | ReactElementRefType -> ReactKitT (use_op, reason, React.GetRef (OpenT tout))
            | ReactPromoteRendersRepresentation
                { should_distribute; promote_structural_components; renders_variant; resolved_elem }
              ->
              PromoteRendersRepresentationT
                {
                  use_op;
                  reason;
                  tout = OpenT tout;
                  resolved_elem;
                  should_distribute;
                  promote_structural_components;
                  renders_variant;
                }
            | ReactConfigType default_props ->
              ReactKitT (use_op, reason, React.GetConfigType (default_props, OpenT tout))
            | MappedType { property_type; mapped_type_flags; homomorphic; distributive_tparam_name }
              ->
              let (property_type, homomorphic) =
                substitute_mapped_type_distributive_tparams
                  cx
                  ~use_op
                  distributive_tparam_name
                  ~property_type
                  homomorphic
                  ~source:t
              in
              let selected_keys_opt =
                match homomorphic with
                | SemiHomomorphic t -> Some t
                | _ -> None
              in
              Object.(
                ObjKitT
                  ( use_op,
                    reason,
                    Resolve Next,
                    Object.ObjectMap
                      { prop_type = property_type; mapped_type_flags; selected_keys_opt },
                    OpenT tout
                  )
              )
          ))

  and mk_possibly_evaluated_destructor cx use_op reason t d id =
    let eval_t = EvalT (t, TypeDestructorT (use_op, reason, d), id) in
    if Subst_name.Set.is_empty (Type_subst.free_var_finder cx eval_t) then
      ignore @@ mk_type_destructor cx ~trace:Trace.dummy_trace use_op reason t d id;
    eval_t

  and variance_check cx ?trace tparams polarity = function
    | ([], _)
    | (_, []) ->
      (* ignore typeapp arity mismatch, since it's handled elsewhere *)
      ()
    | (tp :: tps, t :: ts) ->
      CheckPolarity.check_polarity cx ?trace tparams (Polarity.mult (polarity, tp.polarity)) t;
      variance_check cx ?trace tparams polarity (tps, ts)

  (* Instantiate a polymorphic definition given tparam instantiations in a Call or
   * New expression. *)
  and instantiate_with_targs cx trace ~use_op ~reason_op ~reason_tapp (tparams_loc, xs, t) targs =
    let (_, ts) =
      Nel.fold_left
        (fun (targs, ts) _ ->
          match targs with
          | [] -> ([], ts)
          | ExplicitArg t :: targs -> (targs, t :: ts)
          | ImplicitArg _ :: _ ->
            failwith
              "targs containing ImplicitArg should be handled by ImplicitInstantiationKit instead.")
        (targs, [])
        xs
    in
    let (t, _) =
      instantiate_poly_with_targs
        cx
        trace
        ~use_op
        ~reason_op
        ~reason_tapp
        ~cache:false
        (tparams_loc, xs, t)
        (List.rev ts)
    in
    t

  and instantiate_poly_call_or_new cx trace ?cache lparts uparts check =
    let (reason_tapp, tparams_loc, ids, t) = lparts in
    let (use_op, reason_op, targs, return_hint) = uparts in
    match all_explicit_targs targs with
    | Some targs ->
      instantiate_with_targs cx trace (tparams_loc, ids, t) targs ~use_op ~reason_op ~reason_tapp
    | None ->
      let check = Lazy.force check in
      ImplicitInstantiationKit.run_call
        cx
        check
        ?cache
        trace
        ~use_op
        ~reason_op
        ~reason_tapp
        ~return_hint

  (* Instantiate a polymorphic definition with stated bound or 'any' for args *)
  (* Needed only for `instanceof` refis and React.PropTypes.instanceOf types *)
  and instantiate_poly_default_args cx trace ~use_op ~reason_op ~reason_tapp (tparams_loc, xs, t) =
    (* Remember: other_bound might refer to other type params *)
    let (ts, _) =
      Nel.fold_left
        (fun (ts, map) typeparam ->
          let t = Unsoundness.why InstanceOfRefinement reason_op in
          (t :: ts, Subst_name.Map.add typeparam.name t map))
        ([], Subst_name.Map.empty)
        xs
    in
    let ts = List.rev ts in
    let (t, _) =
      instantiate_poly_with_targs cx trace ~use_op ~reason_op ~reason_tapp (tparams_loc, xs, t) ts
    in
    t

  (* Specialize This in a class. Eventually this causes substitution. *)
  and instantiate_this_class cx trace ~reason_op ~reason_tapp c ts this k =
    let tc =
      match ts with
      | None -> c
      | Some ts ->
        Tvar.mk_where cx reason_tapp (fun tout ->
            rec_flow
              cx
              trace
              (c, SpecializeT (unknown_use, reason_op, reason_tapp, false, Some ts, tout))
        )
    in
    rec_flow cx trace (tc, ThisSpecializeT (reason_tapp, this, k))

  (* Object assignment patterns. In the `Object.assign` model (chain_objects), an
     existing object receives properties from other objects. This pattern suffers
     from "races" in the type checker, since the object supposed to receive
     properties is available even when the other objects supplying the properties
     are not yet available. *)
  and chain_objects cx ?trace reason this those =
    let result =
      List.fold_left
        (fun result that ->
          let (that, kind) =
            match that with
            | Arg t -> (t, default_obj_assign_kind)
            | SpreadArg t ->
              (* If someone does Object.assign({}, ...Array<obj>) we can treat it like
                 Object.assign({}, obj). *)
              (t, ObjSpreadAssign)
          in
          Tvar.mk_where cx reason (fun t ->
              flow_opt
                cx
                ?trace
                (result, ObjAssignToT (Op (ObjectChain { op = reason }), reason, that, t, kind))
          ))
        this
        those
    in
    reposition cx ?trace (loc_of_reason reason) result

  (*********)
  (* enums *)
  (*********)
  and enum_exhaustive_check
      cx
      ~trace
      ~check_reason
      ~enum_reason
      ~enum
      ~possible_checks
      ~checks
      ~default_case
      ~incomplete_out
      ~discriminant_after_check =
    match possible_checks with
    (* No possible checks left to resolve, analyze the exhaustive check. *)
    | [] ->
      let { members; has_unknown_members; _ } = enum in
      let check_member (members_remaining, seen) (EnumCheck { reason; member_name }) =
        if not @@ SMap.mem member_name members_remaining then
          add_output
            cx
            ~trace
            (Error_message.EEnumMemberAlreadyChecked
               { reason; prev_check_reason = SMap.find member_name seen; enum_reason; member_name }
            );
        (SMap.remove member_name members_remaining, SMap.add member_name reason seen)
      in
      let (left_over, _) = List.fold_left check_member (members, SMap.empty) checks in
      (match (SMap.is_empty left_over, default_case, has_unknown_members) with
      | (false, _, _) ->
        add_output
          cx
          ~trace
          (Error_message.EEnumNotAllChecked
             {
               reason = check_reason;
               enum_reason;
               left_to_check = SMap.keys left_over;
               default_case;
             }
          );
        enum_exhaustive_check_incomplete cx ~trace ~reason:check_reason incomplete_out
      (* When we have unknown members, a default is required even when we've checked all known members. *)
      | (true, None, true) ->
        add_output
          cx
          ~trace
          (Error_message.EEnumUnknownNotChecked { reason = check_reason; enum_reason });
        enum_exhaustive_check_incomplete cx ~trace ~reason:check_reason incomplete_out
      | (true, Some _, true) -> ()
      | (true, Some default_case_reason, false) ->
        add_output
          cx
          ~trace
          (Error_message.EEnumAllMembersAlreadyChecked { reason = default_case_reason; enum_reason })
      | _ -> ())
    (* There are still possible checks to resolve, continue to resolve them. *)
    | (obj_t, check) :: rest_possible_checks ->
      let exhaustive_check =
        EnumExhaustiveCheckT
          {
            reason = check_reason;
            check =
              EnumExhaustiveCheckPossiblyValid
                {
                  tool =
                    EnumResolveCaseTest
                      { discriminant_enum = enum; discriminant_reason = enum_reason; check };
                  possible_checks = rest_possible_checks;
                  checks;
                  default_case;
                };
            incomplete_out;
            discriminant_after_check;
          }
      in
      rec_flow cx trace (obj_t, exhaustive_check)

  and enum_exhaustive_check_incomplete
      cx ~trace ~reason ?(trigger = VoidT.why reason) incomplete_out =
    rec_flow_t cx trace ~use_op:unknown_use (trigger, incomplete_out)

  and resolve_union cx trace reason id resolved unresolved l upper =
    let continue resolved =
      match unresolved with
      | [] -> rec_flow cx trace (union_of_ts reason resolved, upper)
      | next :: rest ->
        (* We intentionally do not rec_flow here. Unions can be very large, and resolving each
         * member under the same trace can cause a recursion limit error. To avoid that, we resolve
         * each member under their own trace *)
        flow cx (next, ResolveUnionT { reason; resolved; unresolved = rest; upper; id })
    in
    match l with
    | DefT (_, EmptyT) -> continue resolved
    | _ ->
      let reason_elemt = reason_of_t l in
      let pos = Base.List.length resolved in
      (* Union resolution can fall prey to the same sort of infinite recursion that array spreads can, so
         we can use the same constant folding guard logic that arrays do. To more fully understand how that works,
         see the comment there. *)
      ConstFoldExpansion.guard cx id (reason_elemt, pos) (function
          | 0 -> continue (l :: resolved)
          (* Unions are idempotent, so we can just skip any duplicated elements *)
          | 1 -> continue resolved
          | _ -> ()
          )

  (** Property lookup functions in objects and instances *)

  (* property lookup functions in objects and instances *)
  and prop_typo_suggestion cx ids =
    Base.List.(
      ids
      >>| Context.find_real_props cx
      >>= NameUtils.Map.keys
      |> Base.List.rev_map ~f:display_string_of_name
      |> typo_suggestion
    )

  and get_private_prop
      ~cx
      ~allow_method_access
      ~trace
      ~l
      ~reason_c
      ~instance
      ~use_op
      ~reason_op
      ~prop_name
      ~scopes
      ~static
      ~tout =
    match scopes with
    | [] ->
      add_output
        cx
        ~trace
        (Error_message.EPrivateLookupFailed ((reason_op, reason_c), OrdinaryName prop_name, use_op))
    | scope :: scopes ->
      if not (ALoc.equal_id scope.class_binding_id instance.class_id) then
        get_private_prop
          ~cx
          ~allow_method_access
          ~trace
          ~l
          ~reason_c
          ~instance
          ~use_op
          ~reason_op
          ~prop_name
          ~scopes
          ~static
          ~tout
      else
        let name = OrdinaryName prop_name in
        let perform_lookup_action p =
          let action = ReadProp { use_op; obj_t = l; tout } in
          let propref = mk_named_prop ~reason:reason_op name in
          perform_lookup_action cx trace propref p PropertyMapProperty reason_c reason_op action
        in
        let field_maps =
          if static then
            instance.class_private_static_fields
          else
            instance.class_private_fields
        in
        (match NameUtils.Map.find_opt name (Context.find_props cx field_maps) with
        | Some p -> perform_lookup_action (Property.type_ p)
        | None ->
          let method_maps =
            if static then
              instance.class_private_static_methods
            else
              instance.class_private_methods
          in
          (match NameUtils.Map.find_opt name (Context.find_props cx method_maps) with
          | Some p ->
            ( if not allow_method_access then
              match p with
              | Method { type_ = t; _ }
                when not (Context.allowed_method_unbinding cx (Reason.loc_of_reason reason_op)) ->
                add_output
                  cx
                  ~trace
                  (Error_message.EMethodUnbinding { use_op; reason_op; reason_prop = reason_of_t t })
              | _ -> ()
            );
            perform_lookup_action (Property.type_ p)
          | None ->
            add_output
              cx
              ~trace
              (Error_message.EPrivateLookupFailed ((reason_op, reason_c), name, use_op))))

  and elem_action_on_obj cx trace ~use_op l obj reason_op action =
    let propref = propref_for_elem_t l in
    match action with
    | ReadElem { id; from_annot; access_iterables = _; tout } ->
      rec_flow
        cx
        trace
        ( obj,
          GetPropT
            { use_op; reason = reason_op; from_annot; id; propref; tout; hint = hint_unavailable }
        )
    | WriteElem { tin; tout; mode } ->
      rec_flow cx trace (obj, SetPropT (use_op, reason_op, propref, mode, Normal, tin, None));
      Base.Option.iter ~f:(fun t -> rec_flow_t cx trace ~use_op:unknown_use (obj, t)) tout
    | CallElem (reason_call, ft) ->
      rec_flow cx trace (obj, MethodT (use_op, reason_call, reason_op, propref, ft))

  and writelike_obj_prop cx trace ~use_op o propref reason_obj reason_op prop_t action =
    match GetPropTKit.get_obj_prop cx trace o propref reason_op with
    | Some (p, target_kind) ->
      perform_lookup_action cx trace propref p target_kind reason_obj reason_op action
    | None ->
      (match propref with
      | Named { reason = reason_prop; name; _ } ->
        if Obj_type.is_exact o.flags.obj_kind then
          add_output
            cx
            ~trace
            (Error_message.EPropNotFound
               {
                 prop_name = Some name;
                 reason_prop;
                 reason_obj;
                 use_op;
                 suggestion = prop_typo_suggestion cx [o.props_tmap] (display_string_of_name name);
               }
            )
        else
          let lookup_kind = Strict reason_obj in
          rec_flow
            cx
            trace
            ( o.proto_t,
              LookupT
                {
                  reason = reason_op;
                  lookup_kind;
                  try_ts_on_failure = [];
                  propref;
                  lookup_action = action;
                  ids = Some (Properties.Set.singleton o.props_tmap);
                  method_accessible = true;
                  ignore_dicts = false;
                }
            )
      | Computed elem_t ->
        rec_flow
          cx
          trace
          ( elem_t,
            WriteComputedObjPropCheckT
              {
                reason = TypeUtil.reason_of_t elem_t;
                reason_key = None;
                value_t = prop_t;
                err_on_str_key = Some (use_op, reason_obj);
              }
          ))

  and write_obj_prop cx trace ~use_op ~mode o propref reason_obj reason_op tin prop_tout =
    let obj_t = DefT (reason_obj, ObjT o) in
    let action = WriteProp { use_op; obj_t; prop_tout; tin; write_ctx = Normal; mode } in
    writelike_obj_prop cx trace ~use_op o propref reason_obj reason_op tin action

  (* filter out undefined from a type *)
  and filter_optional cx ?trace reason opt_t =
    let tvar = Tvar.mk_no_wrap cx reason in
    flow_opt cx ?trace (opt_t, FilterOptionalT (unknown_use, OpenT (reason, tvar)));
    tvar

  (**********)
  (* guards *)
  (**********)
  and guard cx trace source pred result sink =
    match pred with
    | ExistsP -> begin
      match Type_filter.exists cx source with
      | DefT (_, EmptyT) -> ()
      | _ -> rec_flow_t cx trace ~use_op:unknown_use (result, OpenT sink)
    end
    | NotP ExistsP -> begin
      match Type_filter.not_exists cx source with
      | DefT (_, EmptyT) -> ()
      | _ -> rec_flow_t cx trace ~use_op:unknown_use (result, OpenT sink)
    end
    | MaybeP -> begin
      match Type_filter.maybe cx source with
      | DefT (_, EmptyT) -> ()
      | _ -> rec_flow_t cx trace ~use_op:unknown_use (result, OpenT sink)
    end
    | NotP MaybeP -> begin
      match Type_filter.not_maybe cx source with
      | DefT (_, EmptyT) -> ()
      | _ -> rec_flow_t cx trace ~use_op:unknown_use (result, OpenT sink)
    end
    | NotP (NotP p) -> guard cx trace source p result sink
    | _ ->
      let loc = loc_of_reason (fst sink) in
      let pred_str = string_of_predicate pred in
      add_output cx ~trace Error_message.(EInternal (loc, UnsupportedGuardPredicate pred_str))

  (**************)
  (* predicates *)
  (**************)

  (* t - predicate output recipient (normally a tvar)
     l - incoming concrete LB (predicate input)
     result - guard result in case of success
     p - predicate *)
  and predicate cx trace t l p =
    match p with
    (************************)
    (* deconstruction of && *)
    (************************)
    | AndP (p1, p2) ->
      let reason = replace_desc_reason RAnd (fst t) in
      let tvar = (reason, Tvar.mk_no_wrap cx reason) in
      rec_flow cx trace (l, PredicateT (p1, tvar));
      rec_flow cx trace (OpenT tvar, PredicateT (p2, t))
    (************************)
    (* deconstruction of || *)
    (************************)
    | OrP (p1, p2) ->
      rec_flow cx trace (l, PredicateT (p1, t));
      rec_flow cx trace (l, PredicateT (p2, t))
    (*********************************)
    (* deconstruction of binary test *)
    (*********************************)

    (* when left is evaluated, store it and evaluate right *)
    | LeftP (b, r) -> rec_flow cx trace (r, PredicateT (RightP (b, l), t))
    | NotP (LeftP (b, r)) -> rec_flow cx trace (r, PredicateT (NotP (RightP (b, l)), t))
    (* when right is evaluated, call appropriate handler *)
    | RightP (b, actual_l) ->
      let r = l in
      let l = actual_l in
      binary_predicate cx trace true b l r t
    | NotP (RightP (b, actual_l)) ->
      let r = l in
      let l = actual_l in
      binary_predicate cx trace false b l r t
    (***********************)
    (* typeof _ ~ "boolean" *)
    (***********************)
    | BoolP loc -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.boolean loc l, OpenT t)
    | NotP (BoolP _) -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_boolean l, OpenT t)
    (***********************)
    (* typeof _ ~ "string" *)
    (***********************)
    | StrP loc -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.string loc l, OpenT t)
    | NotP (StrP _) -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_string l, OpenT t)
    (***********************)
    (* typeof _ ~ "symbol" *)
    (***********************)
    | SymbolP loc -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.symbol loc l, OpenT t)
    | NotP (SymbolP _) -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_symbol l, OpenT t)
    (*********************)
    (* _ ~ "some string" *)
    (*********************)
    | SingletonStrP (expected_loc, sense, lit) ->
      let filtered_str = Type_filter.string_literal expected_loc sense (OrdinaryName lit) l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered_str, OpenT t)
    | NotP (SingletonStrP (_, _, lit)) ->
      let filtered_str = Type_filter.not_string_literal (OrdinaryName lit) l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered_str, OpenT t)
    (*********************)
    (* _ ~ some number n *)
    (*********************)
    | SingletonNumP (expected_loc, sense, lit) ->
      let filtered_num = Type_filter.number_literal expected_loc sense lit l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered_num, OpenT t)
    | NotP (SingletonNumP (_, _, lit)) ->
      let filtered_num = Type_filter.not_number_literal lit l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered_num, OpenT t)
    (***********************)
    (* typeof _ ~ "number" *)
    (***********************)
    | NumP loc -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.number loc l, OpenT t)
    | NotP (NumP _) -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_number l, OpenT t)
    (*********************)
    (* _ ~ some bigint n *)
    (*********************)
    | SingletonBigIntP (expected_loc, sense, lit) ->
      let filtered_bigint = Type_filter.bigint_literal expected_loc sense lit l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered_bigint, OpenT t)
    | NotP (SingletonBigIntP (_, _, lit)) ->
      let filtered_bigint = Type_filter.not_bigint_literal lit l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered_bigint, OpenT t)
    (***********************)
    (* typeof _ ~ "bigint" *)
    (***********************)
    | BigIntP loc -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.bigint loc l, OpenT t)
    | NotP (BigIntP _) -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_bigint l, OpenT t)
    (***********************)
    (* typeof _ ~ "function" *)
    (***********************)
    | FunP -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.function_ l, OpenT t)
    | NotP FunP -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_function l, OpenT t)
    (***********************)
    (* typeof _ ~ "object" *)
    (***********************)
    | ObjP -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.object_ cx l, OpenT t)
    | NotP ObjP -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_object l, OpenT t)
    (*******************)
    (* Array.isArray _ *)
    (*******************)
    | ArrP -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.array l, OpenT t)
    | NotP ArrP -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_array l, OpenT t)
    (***********************)
    (* typeof _ ~ "undefined" *)
    (***********************)
    | VoidP ->
      let filtered = Type_filter.undefined l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    | NotP VoidP ->
      let filtered = Type_filter.not_undefined cx l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    (********)
    (* null *)
    (********)
    | NullP ->
      let filtered = Type_filter.null l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    | NotP NullP ->
      let filtered = Type_filter.not_null cx l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    (*********)
    (* maybe *)
    (*********)
    | MaybeP ->
      let filtered = Type_filter.maybe cx l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    | NotP MaybeP ->
      let filtered = Type_filter.not_maybe cx l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    (********)
    (* true *)
    (********)
    | SingletonBoolP (_, true) ->
      let filtered = Type_filter.true_ l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    | NotP (SingletonBoolP (_, true)) ->
      let filtered = Type_filter.not_true l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    (*********)
    (* false *)
    (*********)
    | SingletonBoolP (_, false) ->
      let filtered = Type_filter.false_ l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    | NotP (SingletonBoolP (_, false)) ->
      let filtered = Type_filter.not_false l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    (************************)
    (* truthyness *)
    (************************)
    | ExistsP ->
      let filtered = Type_filter.exists cx l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    | NotP ExistsP ->
      let filtered = Type_filter.not_exists cx l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    | PropExistsP (key, r) -> prop_exists_test cx trace key r true l t
    | NotP (PropExistsP (key, r)) -> prop_exists_test cx trace key r false l t
    | PropNonMaybeP (key, r) -> prop_non_maybe_test cx trace key r true l t
    | NotP (PropNonMaybeP (key, r)) -> prop_non_maybe_test cx trace key r false l t
    (* classical logic i guess *)
    | NotP (NotP p) -> predicate cx trace t l p
    | NotP (AndP (p1, p2)) -> predicate cx trace t l (OrP (NotP p1, NotP p2))
    | NotP (OrP (p1, p2)) -> predicate cx trace t l (AndP (NotP p1, NotP p2))
    (********************)
    (* Latent predicate *)
    (********************)
    | LatentP ((lazy (use_op, loc, fun_t, targs, argts)), idx) ->
      let reason = mk_reason (RFunctionCall (desc_of_t fun_t)) loc in
      rec_flow
        cx
        trace
        ( fun_t,
          CallLatentPredT { use_op; reason; targs; argts; sense = true; idx; tin = l; tout = t }
        )
    | NotP (LatentP ((lazy (use_op, loc, fun_t, targs, argts)), idx)) ->
      let reason = mk_reason (RFunctionCall (desc_of_t fun_t)) loc in
      rec_flow
        cx
        trace
        ( fun_t,
          CallLatentPredT { use_op; reason; targs; argts; sense = false; idx; tin = l; tout = t }
        )
    | NoP
    | NotP NoP ->
      rec_flow_t cx trace ~use_op:unknown_use (l, OpenT t)

  and prop_exists_test cx trace key reason sense obj result =
    prop_exists_test_generic key reason cx trace result obj sense (ExistsP, NotP ExistsP) obj

  and prop_non_maybe_test cx trace key reason sense obj result =
    prop_exists_test_generic key reason cx trace result obj sense (NotP MaybeP, MaybeP) obj

  and prop_exists_test_generic key reason cx trace result orig_obj sense (pred, not_pred) = function
    | DefT (_, ObjT { flags; props_tmap; _ }) as obj ->
      (match Context.get_prop cx props_tmap (OrdinaryName key) with
      | Some p ->
        (match Property.read_t p with
        | Some t ->
          (* prop is present on object type *)
          let pred =
            if sense then
              pred
            else
              not_pred
          in
          rec_flow cx trace (t, GuardT (pred, orig_obj, result))
        | None ->
          (* prop cannot be read *)
          add_output
            cx
            ~trace
            (Error_message.EPropNotReadable
               { reason_prop = reason; prop_name = Some (OrdinaryName key); use_op = unknown_use }
            ))
      | None when Obj_type.is_exact flags.obj_kind ->
        (* prop is absent from exact object type *)
        if sense then
          ()
        else
          rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result)
      | None ->
        (* prop is absent from inexact object type *)
        (* TODO: possibly unsound to filter out orig_obj here, but if we don't,
           case elimination based on prop existence checking doesn't work for
           (disjoint unions of) intersections of objects, where the prop appears
           in a different branch of the intersection. It is easy to avoid this
           unsoundness with slightly more work, but will wait until a
           refactoring of property lookup lands to revisit. Tracked by
           #11301092. *)
        if orig_obj = obj then rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result))
    | IntersectionT (_, rep) ->
      (* For an intersection of object types, try the test for each object type in
         turn, while recording the original intersection so that we end up with
         the right refinement. See the comment on the implementation of
         IntersectionPreprocessKit for more details. *)
      let reason = fst result in
      InterRep.members rep
      |> List.iter (fun obj ->
             rec_flow
               cx
               trace
               ( obj,
                 SpeculationKit.intersection_preprocess_kit
                   reason
                   (PropExistsTest (sense, key, reason, orig_obj, result, (pred, not_pred)))
               )
         )
    | _ -> rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result)

  and binary_predicate cx trace sense test left right result =
    let handler =
      match test with
      | InstanceofTest -> instanceof_test
      | SentinelProp key -> sentinel_prop_test key
    in
    handler cx trace result (sense, left, right)

  and instanceof_test cx trace result = function
    (* instanceof on an ArrT is a special case since we treat ArrT as its own
       type, rather than an InstanceT of the Array builtin class. So, we resolve
       the ArrT to an InstanceT of Array, and redo the instanceof check. We do
       it at this stage instead of simply converting (ArrT, InstanceofP c)
       to (InstanceT(Array), InstanceofP c) because this allows c to be resolved
       first. *)
    | (true, (DefT (reason, ArrT arrtype) as arr), DefT (r, ClassT a)) ->
      let elemt = elemt_of_arrtype arrtype in
      let right = extends_type r arr a in
      let arrt = get_builtin_typeapp cx reason "Array" [elemt] in
      rec_flow cx trace (arrt, PredicateT (LeftP (InstanceofTest, right), result))
    | (false, (DefT (reason, ArrT arrtype) as arr), DefT (r, ClassT a)) ->
      let elemt = elemt_of_arrtype arrtype in
      let right = extends_type r arr a in
      let arrt = get_builtin_typeapp cx reason "Array" [elemt] in
      let pred = NotP (LeftP (InstanceofTest, right)) in
      rec_flow cx trace (arrt, PredicateT (pred, result))
    (* Suppose that we have an instance x of class C, and we check whether x is
       `instanceof` class A. To decide what the appropriate refinement for x
       should be, we need to decide whether C extends A, choosing either C or A
       based on the result. Thus, we generate a constraint to decide whether C
       extends A (while remembering C), which may recursively generate further
       constraints to decide super(C) extends A, and so on, until we hit the root
       class. (As a technical tool, we use Extends(_, _) to perform this
       recursion; it is also used elsewhere for running similar recursive
       subclass decisions.) **)
    | (true, (DefT (_, InstanceT _) as c), DefT (r, ClassT a)) ->
      predicate cx trace result (extends_type r c a) (RightP (InstanceofTest, c))
    (* If C is a subclass of A, then don't refine the type of x. Otherwise,
       refine the type of x to A. (In general, the type of x should be refined to
       C & A, but that's hard to compute.) **)
    | ( true,
        DefT (reason, InstanceT { super = super_c; inst = instance_c; _ }),
        (InternalT (ExtendsT (_, c, DefT (_, InstanceT { inst = instance_a; _ }))) as right)
      ) ->
      (* TODO: intersection *)
      if is_same_instance_type instance_a instance_c then
        rec_flow_t cx trace ~use_op:unknown_use (c, OpenT result)
      else
        (* Recursively check whether super(C) extends A, with enough context. **)
        let pred = LeftP (InstanceofTest, right) in
        let u = PredicateT (pred, result) in
        rec_flow cx trace (super_c, ReposLowerT (reason, false, u))
    (* If we are checking `instanceof Object` or `instanceof Function`, objects
       with `ObjProtoT` or `FunProtoT` should pass. *)
    | (true, ObjProtoT reason, (InternalT (ExtendsT _) as right)) ->
      let obj_proto = get_builtin_type cx ~trace reason ~use_desc:true "Object" in
      rec_flow cx trace (obj_proto, PredicateT (LeftP (InstanceofTest, right), result))
    | (true, FunProtoT reason, (InternalT (ExtendsT _) as right)) ->
      let fun_proto = get_builtin_type cx ~trace reason ~use_desc:true "Function" in
      rec_flow cx trace (fun_proto, PredicateT (LeftP (InstanceofTest, right), result))
    (* We hit the root class, so C is not a subclass of A **)
    | (true, DefT (_, NullT), InternalT (ExtendsT (r, _, a))) ->
      rec_flow_t
        cx
        trace
        ~use_op:unknown_use
        (reposition cx ~trace (loc_of_reason r) a, OpenT result)
    (* If we're refining `mixed` or `any` with instanceof A, then flow A to the result *)
    | (true, (DefT (_, MixedT _) | AnyT _), DefT (class_reason, ClassT a)) ->
      let desc = reason_of_t a |> desc_of_reason in
      let loc = loc_of_reason class_reason in
      rec_flow_t cx trace ~use_op:unknown_use (reposition cx ~trace ~desc loc a, OpenT result)
    (* Prune the type when any other `instanceof` check succeeds (since this is
       impossible). *)
    | (true, _, _) -> ()
    (* Like above, now suppose that we have an instance x of class C, and we
       check whether x is _not_ `instanceof` class A. To decide what the
       appropriate refinement for x should be, we need to decide whether C
       extends A, choosing either nothing or C based on the result. **)
    | (false, (DefT (_, InstanceT _) as c), DefT (r, ClassT (DefT (_, InstanceT _) as a))) ->
      predicate cx trace result (extends_type r c a) (NotP (RightP (InstanceofTest, c)))
    (* If C is a subclass of A, then do nothing, since this check cannot
       succeed. Otherwise, don't refine the type of x. **)
    | ( false,
        DefT (reason, InstanceT { super = super_c; inst = instance_c; _ }),
        (InternalT (ExtendsT (_, _, DefT (_, InstanceT { inst = instance_a; _ }))) as right)
      ) ->
      if is_same_instance_type instance_a instance_c then
        ()
      else
        let u = PredicateT (NotP (LeftP (InstanceofTest, right)), result) in
        rec_flow cx trace (super_c, ReposLowerT (reason, false, u))
    | (false, ObjProtoT _, InternalT (ExtendsT (r, c, _))) ->
      (* We hit the root class, so C is not a subclass of A **)
      rec_flow_t
        cx
        trace
        ~use_op:unknown_use
        (reposition cx ~trace (loc_of_reason r) c, OpenT result)
    (* Don't refine the type when any other `instanceof` check fails. **)
    | (false, left, _) -> rec_flow_t cx trace ~use_op:unknown_use (left, OpenT result)

  and sentinel_prop_test key cx trace result (sense, obj, t) =
    sentinel_prop_test_generic key cx trace result obj (sense, obj, t)

  and sentinel_prop_test_generic key cx trace result orig_obj =
    let desc_of_sentinel sentinel =
      match sentinel with
      | UnionEnum.(One (Str s)) -> RStringLit s
      | UnionEnum.(One (Num n)) -> RNumberLit (string_of_float n)
      | UnionEnum.(One (Bool b)) -> RBooleanLit b
      | UnionEnum.(One (BigInt (_, n))) -> RBigIntLit n
      | UnionEnum.(One Null) -> RNull
      | UnionEnum.(One Void) -> RVoid
      | UnionEnum.(Many _enums) -> RUnionEnum
    in

    (* Evaluate a refinement predicate of the form

       obj.key eq value

       where eq is === or !==.

       * key is key
       * (sense, obj, value) are the sense of the test, obj and value as above,
       respectively.

       As with other predicate filters, the goal is to statically determine when
       the predicate is definitely satisfied and when it is definitely
       unsatisfied, and narrow the possible types of obj under those conditions,
       while not narrowing in all other cases.

       In this case, the predicate is definitely satisfied (respectively,
       definitely unsatisfied) when the type of the key property in the type obj
       can be statically verified as having (respectively, not having) value as
       its only inhabitant.

       When satisfied, type obj flows to the recipient type result (in other
       words, we allow all such types in the refined type for obj).

       Otherwise, nothing flows to type result (in other words, we don't allow
       any such type in the refined type for obj).

       Overall the filtering process is somewhat tricky to understand. Refer to
       the predicate function and its callers to understand how the context is
       set up so that filtering ultimately only depends on what flows to
       result. **)
    let flow_sentinel sense props_tmap obj sentinel =
      match Context.get_prop cx props_tmap (OrdinaryName key) with
      | Some p ->
        (match Property.read_t p with
        | Some t ->
          let reason =
            let desc = RMatchingProp (key, desc_of_sentinel sentinel) in
            replace_desc_reason desc (fst result)
          in
          let test = SentinelPropTestT (reason, orig_obj, sense, sentinel, result) in
          rec_flow cx trace (t, test)
        | None ->
          let reason_obj = reason_of_t obj in
          add_output
            cx
            ~trace
            (Error_message.EPropNotReadable
               {
                 reason_prop = reason_obj;
                 prop_name = Some (OrdinaryName key);
                 use_op = unknown_use;
               }
            ))
      | None ->
        (* TODO: possibly unsound to filter out orig_obj here, but if we
           don't, case elimination based on sentinel prop checking doesn't
           work for (disjoint unions of) intersections of objects, where the
           sentinel prop and the payload appear in different branches of the
           intersection. It is easy to avoid this unsoundness with slightly
           more work, but will wait until a refactoring of property lookup
           lands to revisit. Tracked by #11301092. *)
        if orig_obj = obj then rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result)
    in
    let sentinel_of_literal = function
      | DefT (_, StrT (Literal (_, value)))
      | DefT (_, SingletonStrT value) ->
        Some UnionEnum.(One (Str value))
      | DefT (_, NumT (Literal (_, (value, _))))
      | DefT (_, SingletonNumT (value, _)) ->
        Some UnionEnum.(One (Num value))
      | DefT (_, BoolT (Some value))
      | DefT (_, SingletonBoolT value) ->
        Some UnionEnum.(One (Bool value))
      | DefT (_, BigIntT (Literal (_, value)))
      | DefT (_, SingletonBigIntT value) ->
        Some UnionEnum.(One (BigInt value))
      | DefT (_, VoidT) -> Some UnionEnum.(One Void)
      | DefT (_, NullT) -> Some UnionEnum.(One Null)
      | UnionT (_, rep) -> begin
        match UnionRep.check_enum rep with
        | Some enums -> Some UnionEnum.(Many enums)
        | None -> None
      end
      | _ -> None
    in
    fun (sense, obj, t) ->
      match sentinel_of_literal t with
      | Some s -> begin
        match obj with
        (* obj.key ===/!== literal value *)
        | DefT (_, ObjT { props_tmap; _ }) -> flow_sentinel sense props_tmap obj s
        (* instance.key ===/!== literal value *)
        | DefT (_, InstanceT { inst = { own_props; _ }; _ }) ->
          (* TODO: add test for sentinel test on implements *)
          flow_sentinel sense own_props obj s
        (* tuple.length ===/!== literal value *)
        | DefT (reason, ArrT (TupleAT { elem_t = _; elements = _; arity; react_dro = _ }))
          when key = "length" ->
          let test =
            let desc = RMatchingProp (key, desc_of_sentinel s) in
            let r = replace_desc_reason desc (fst result) in
            SentinelPropTestT (r, orig_obj, sense, s, result)
          in
          rec_flow cx trace (tuple_length reason arity, test)
        | IntersectionT (_, rep) ->
          (* For an intersection of object types, try the test for each object
             type in turn, while recording the original intersection so that we
             end up with the right refinement. See the comment on the
             implementation of IntersectionPreprocessKit for more details. *)
          let reason = fst result in
          InterRep.members rep
          |> List.iter (fun obj ->
                 rec_flow
                   cx
                   trace
                   ( obj,
                     SpeculationKit.intersection_preprocess_kit
                       reason
                       (SentinelPropTest (sense, key, t, orig_obj, result))
                   )
             )
        | _ ->
          (* not enough info to refine *)
          rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result)
      end
      | None ->
        (* not enough info to refine *)
        rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result)

  (*******************************************************************)
  (* /predicate *)
  (*******************************************************************)
  and pick_use_op cx op1 op2 =
    let ignore_root = function
      | UnknownUse -> true
      (* If we are speculating then a Speculation use_op should be considered
       * "opaque". If we are not speculating then Speculation use_ops that escaped
       * (through benign tvars) should be ignored.
       *
       * Ideally we could replace the Speculation use_ops on benign tvars with their
       * underlying use_op after speculation ends. *)
      | Speculation _ -> not (Speculation.speculating cx)
      | _ -> false
    in
    if ignore_root (root_of_use_op op1) then
      op2
    else
      let root_of_op2 = root_of_use_op op2 in
      let should_replace =
        fold_use_op
          (* If the root of the previous use_op is UnknownUse and our alternate
           * use_op does not have an UnknownUse root then we use our
           * alternate use_op. *)
          ignore_root
          (fun should_replace -> function
            (* If the use was added to an implicit type param then we want to use
             * our alternate if the implicit type param use_op chain is inside
             * the implicit type param instantiation. Since we can't directly compare
             * abstract locations, we determine whether to do this using a heuristic
             * based on the 'locality' of the use_op root. *)
            | ImplicitTypeParam when not should_replace ->
              (match root_of_op2 with
              | FunCall { local; _ }
              | FunCallMethod { local; _ } ->
                local
              | Arith _
              | AssignVar _
              | Coercion _
              | DeleteVar _
              | DeleteProperty _
              | FunImplicitReturn _
              | FunReturnStatement _
              | GetProperty _
              | IndexedTypeAccess _
              | InferBoundCompatibilityCheck _
              | ConditionalTypeEval _
              | EvalMappedType _
              | SetProperty _
              | UpdateProperty _
              | JSXCreateElement _
              | ObjectSpread _
              | ObjectRest _
              | ObjectChain _
              | TypeApplication _
              | Speculation _
              | InitField _ ->
                true
              | Cast _
              | RefinementCheck _
              | ClassExtendsCheck _
              | ClassMethodDefinition _
              | ClassImplementsCheck _
              | ClassOwnProtoCheck _
              | ConformToCommonInterface _
              | DeclareComponentRef _
              | GeneratorYield _
              | ReactCreateElementCall _
              | ReactGetIntrinsic _
              | MatchingProp _
              | TypeGuardIncompatibility _
              | RenderTypeInstantiation _
              | ComponentRestParamCompatibility _
              | UnknownUse ->
                false)
            | UnifyFlip when not should_replace ->
              (match root_of_op2 with
              | TypeApplication _ -> true
              | _ -> should_replace)
            | _ -> should_replace)
          op2
      in
      if should_replace then
        op1
      else
        op2

  and flow_use_op cx op1 u = mod_use_op_of_use_t (fun op2 -> pick_use_op cx op1 op2) u

  (** Bounds Manipulation

    The following general considerations apply when manipulating bounds.

    1. All type variables start out as roots, but some of them eventually become
    goto nodes. As such, bounds of roots may contain goto nodes. However, we
    never perform operations directly on goto nodes; instead, we perform those
    operations on their roots. It is tempting to replace goto nodes proactively
    with their roots to avoid this issue, but doing so may be expensive, whereas
    the union-find data structure amortizes the cost of looking up roots.

    2. Another issue is that while the bounds of a type variable start out
    empty, and in particular do not contain the type variable itself, eventually
    other type variables in the bounds may be unified with the type variable. We
    do not remove these type variables proactively, but instead filter them out
    when considering the bounds. In the future we might consider amortizing the
    cost of this filtering.

    3. When roots are resolved, they act like the corresponding concrete
    types. We maintain the invariant that whenever lower bounds or upper bounds
    contain resolved roots, they also contain the corresponding concrete types.

    4. When roots are unresolved (they have lower bounds and upper bounds,
    possibly consisting of concrete types as well as type variables), we
    maintain the invarant that every lower bound has already been propagated to
    every upper bound. We also maintain the invariant that the bounds are
    transitively closed modulo equivalence: for every type variable in the
    bounds, all the bounds of its root are also included.
   **)

  (* for each l in ls: l => u *)
  and flows_to_t cx trace ls u =
    ls
    |> TypeMap.iter (fun l (trace_l, use_op) ->
           let u = flow_use_op cx use_op u in
           join_flow cx [trace_l; trace] (l, u)
       )

  (* for each u in us: l => u *)
  and flows_from_t cx trace ~new_use_op l us =
    us
    |> UseTypeMap.iter (fun (u, _) trace_u ->
           let u = flow_use_op cx new_use_op u in
           join_flow cx [trace; trace_u] (l, u)
       )

  (* for each l in ls, u in us: l => u *)
  and flows_across cx trace ~use_op ls us =
    ls
    |> TypeMap.iter (fun l (trace_l, use_op') ->
           us
           |> UseTypeMap.iter (fun (u, _) trace_u ->
                  let u = flow_use_op cx use_op' (flow_use_op cx use_op u) in
                  join_flow cx [trace_l; trace; trace_u] (l, u)
              )
       )

  (* bounds.upper += u *)
  and add_upper cx u trace bounds =
    bounds.upper <- UseTypeMap.add (u, Context.speculation_id cx) trace bounds.upper

  (* bounds.lower += l *)
  and add_lower l (trace, use_op) bounds =
    bounds.lower <- TypeMap.add l (trace, use_op) bounds.lower

  (** Given a map of bindings from tvars to traces, a tvar to skip, and an `each`
    function taking a tvar and its associated trace, apply `each` to all
    unresolved root constraints reached from the bound tvars, except those of
    skip_tvar. (Typically skip_tvar is a tvar that will be processed separately,
    so we don't want to redo that work. We also don't want to consider any tvar
    that has already been resolved, because the resolved type will be processed
    separately, too, as part of the bounds of skip_tvar. **)
  and iter_with_filter cx bindings skip_id each =
    bindings
    |> IMap.iter (fun id trace ->
           match Context.find_constraints cx id with
           | (root_id, Unresolved bounds) when root_id <> skip_id -> each (root_id, bounds) trace
           | _ -> ()
       )

  (** Given [edges_to_t (id1, bounds1) t2], for each [id] in [id1] + [bounds1.lowertvars],
    [id.bounds.upper += t2]. When going through [bounds1.lowertvars], filter out [id1].

    As an optimization, skip [id1] when it will become either a resolved root or a
    goto node (so that updating its bounds is unnecessary). *)
  and edges_to_t cx trace ?(opt = false) (id1, bounds1) t2 =
    let max = Context.max_trace_depth cx in
    if not opt then add_upper cx t2 trace bounds1;
    iter_with_filter cx bounds1.lowertvars id1 (fun (_, bounds) (trace_l, use_op) ->
        let t2 = flow_use_op cx use_op t2 in
        add_upper cx t2 (Trace.concat_trace ~max [trace_l; trace]) bounds
    )

  (** Given [edges_from_t t1 (id2, bounds2)], for each [id] in [id2] + [bounds2.uppertvars],
    [id.bounds.lower += t1]. When going through [bounds2.uppertvars], filter out [id2].

    As an optimization, skip [id2] when it will become either a resolved root or a
    goto node (so that updating its bounds is unnecessary). *)
  and edges_from_t cx trace ~new_use_op ?(opt = false) t1 (id2, bounds2) =
    let max = Context.max_trace_depth cx in
    if not opt then add_lower t1 (trace, new_use_op) bounds2;
    iter_with_filter cx bounds2.uppertvars id2 (fun (_, bounds) (trace_u, use_op) ->
        let use_op = pick_use_op cx new_use_op use_op in
        add_lower t1 (Trace.concat_trace ~max [trace; trace_u], use_op) bounds
    )

  (** for each [id'] in [id] + [bounds.lowertvars], [id'.bounds.upper += us] *)
  and edges_to_ts ~new_use_op cx trace ?(opt = false) (id, bounds) us =
    let max = Context.max_trace_depth cx in
    us
    |> UseTypeMap.iter (fun (u, _) trace_u ->
           let u = flow_use_op cx new_use_op u in
           edges_to_t cx (Trace.concat_trace ~max [trace; trace_u]) ~opt (id, bounds) u
       )

  (** for each [id'] in [id] + [bounds.uppertvars], [id'.bounds.lower += ls] *)
  and edges_from_ts cx trace ~new_use_op ?(opt = false) ls (id, bounds) =
    let max = Context.max_trace_depth cx in
    ls
    |> TypeMap.iter (fun l (trace_l, use_op) ->
           let new_use_op = pick_use_op cx use_op new_use_op in
           edges_from_t cx (Trace.concat_trace ~max [trace_l; trace]) ~new_use_op ~opt l (id, bounds)
       )

  (** for each [id] in [id1] + [bounds1.lowertvars]:
        id.bounds.upper += t2
        for each l in bounds1.lower: l => t2

    As an invariant, [bounds1.lower] should already contain [id.bounds.lower] for
    each id in [bounds1.lowertvars]. *)
  and edges_and_flows_to_t cx trace ?(opt = false) (id1, bounds1) t2 =
    (* Skip iff edge exists as part of the speculation path to the current branch *)
    let skip =
      List.exists
        (fun branch ->
          let Speculation_state.{ speculation_id; case = { case_id; _ }; _ } = branch in
          UseTypeMap.mem (t2, Some (speculation_id, case_id)) bounds1.upper)
        !(Context.speculation_state cx)
      || UseTypeMap.mem (t2, None) bounds1.upper
    in
    if not skip then (
      edges_to_t cx trace ~opt (id1, bounds1) t2;
      flows_to_t cx trace bounds1.lower t2
    )

  (** for each [id] in [id2] + [bounds2.uppertvars]:
        id.bounds.lower += t1
        for each u in bounds2.upper: t1 => u

    As an invariant, [bounds2.upper] should already contain [id.bounds.upper] for
    each id in [bounds2.uppertvars]. *)
  and edges_and_flows_from_t cx trace ~new_use_op ?(opt = false) t1 (id2, bounds2) =
    if not (TypeMap.mem t1 bounds2.lower) then (
      edges_from_t cx trace ~new_use_op ~opt t1 (id2, bounds2);
      flows_from_t cx trace ~new_use_op t1 bounds2.upper
    )

  (** bounds.uppertvars += id *)
  and add_uppertvar id trace use_op bounds =
    bounds.uppertvars <- IMap.add id (trace, use_op) bounds.uppertvars

  (** bounds.lowertvars += id *)
  and add_lowertvar id trace use_op bounds =
    bounds.lowertvars <- IMap.add id (trace, use_op) bounds.lowertvars

  (** for each [id] in [id1] + [bounds1.lowertvars]:
        id.bounds.uppertvars += id2

    When going through [bounds1.lowertvars], filter out [id1].

    As an optimization, skip id1 when it will become either a resolved root or a
    goto node (so that updating its bounds is unnecessary). *)
  and edges_to_tvar cx trace ~new_use_op ?(opt = false) (id1, bounds1) id2 =
    let max = Context.max_trace_depth cx in
    if not opt then add_uppertvar id2 trace new_use_op bounds1;
    iter_with_filter cx bounds1.lowertvars id1 (fun (_, bounds) (trace_l, use_op) ->
        let use_op = pick_use_op cx use_op new_use_op in
        add_uppertvar id2 (Trace.concat_trace ~max [trace_l; trace]) use_op bounds
    )

  (** for each id in id2 + bounds2.uppertvars:
        id.bounds.lowertvars += id1

    When going through bounds2.uppertvars, filter out id2.

    As an optimization, skip id2 when it will become either a resolved root or a
    goto node (so that updating its bounds is unnecessary). *)
  and edges_from_tvar cx trace ~new_use_op ?(opt = false) id1 (id2, bounds2) =
    let max = Context.max_trace_depth cx in
    if not opt then add_lowertvar id1 trace new_use_op bounds2;
    iter_with_filter cx bounds2.uppertvars id2 (fun (_, bounds) (trace_u, use_op) ->
        let use_op = pick_use_op cx new_use_op use_op in
        add_lowertvar id1 (Trace.concat_trace ~max [trace; trace_u]) use_op bounds
    )

  (** for each id in id1 + bounds1.lowertvars:
        id.bounds.upper += bounds2.upper
        id.bounds.uppertvars += id2
        id.bounds.uppertvars += bounds2.uppertvars *)
  and add_upper_edges ~new_use_op cx trace ?(opt = false) (id1, bounds1) (id2, bounds2) =
    let max = Context.max_trace_depth cx in
    edges_to_ts ~new_use_op cx trace ~opt (id1, bounds1) bounds2.upper;
    edges_to_tvar cx trace ~new_use_op ~opt (id1, bounds1) id2;
    iter_with_filter cx bounds2.uppertvars id2 (fun (tvar, _) (trace_u, use_op) ->
        let new_use_op = pick_use_op cx new_use_op use_op in
        let trace = Trace.concat_trace ~max [trace; trace_u] in
        edges_to_tvar cx trace ~new_use_op ~opt (id1, bounds1) tvar
    )

  (** for each id in id2 + bounds2.uppertvars:
        id.bounds.lower += bounds1.lower
        id.bounds.lowertvars += id1
        id.bounds.lowertvars += bounds1.lowertvars *)
  and add_lower_edges cx trace ~new_use_op ?(opt = false) (id1, bounds1) (id2, bounds2) =
    let max = Context.max_trace_depth cx in
    edges_from_ts cx trace ~new_use_op ~opt bounds1.lower (id2, bounds2);
    edges_from_tvar cx trace ~new_use_op ~opt id1 (id2, bounds2);
    iter_with_filter cx bounds1.lowertvars id1 (fun (tvar, _) (trace_l, use_op) ->
        let use_op = pick_use_op cx use_op new_use_op in
        let trace = Trace.concat_trace ~max [trace_l; trace] in
        edges_from_tvar cx trace ~new_use_op:use_op ~opt tvar (id2, bounds2)
    )

  (***************)
  (* unification *)
  (***************)
  and unify_flip use_op = Frame (UnifyFlip, use_op)

  (** Chain a root to another root. If both roots are unresolved, this amounts to
    copying over the bounds of one root to another, and adding all the
    connections necessary when two non-unifiers flow to each other. If one or
    both of the roots are resolved, they effectively act like the corresponding
    concrete types. *)
  and goto cx trace ~use_op (id1, node1, root1) (id2, _, root2) =
    match (root1.constraints, root2.constraints) with
    | (Unresolved bounds1, Unresolved bounds2) ->
      let cond1 = not_linked (id1, bounds1) (id2, bounds2) in
      let cond2 = not_linked (id2, bounds2) (id1, bounds1) in
      if cond1 then flows_across cx trace ~use_op bounds1.lower bounds2.upper;
      if cond2 then flows_across cx trace ~use_op:(unify_flip use_op) bounds2.lower bounds1.upper;
      if cond1 then (
        add_upper_edges cx trace ~new_use_op:use_op ~opt:true (id1, bounds1) (id2, bounds2);
        add_lower_edges cx trace ~new_use_op:use_op (id1, bounds1) (id2, bounds2)
      );
      if cond2 then (
        add_upper_edges cx trace ~new_use_op:(unify_flip use_op) (id2, bounds2) (id1, bounds1);
        add_lower_edges
          cx
          trace
          ~new_use_op:(unify_flip use_op)
          ~opt:true
          (id2, bounds2)
          (id1, bounds1)
      );
      node1 := Goto { parent = id2 }
    | (Unresolved bounds1, (Resolved t2 | FullyResolved (lazy t2))) ->
      let t2_use = UseT (use_op, t2) in
      edges_and_flows_to_t cx trace ~opt:true (id1, bounds1) t2_use;
      edges_and_flows_from_t cx trace ~new_use_op:(unify_flip use_op) ~opt:true t2 (id1, bounds1);
      node1 := Goto { parent = id2 }
    | ((Resolved t1 | FullyResolved (lazy t1)), Unresolved bounds2) ->
      let t1_use = UseT (unify_flip use_op, t1) in
      edges_and_flows_to_t cx trace ~opt:true (id2, bounds2) t1_use;
      edges_and_flows_from_t cx trace ~new_use_op:use_op ~opt:true t1 (id2, bounds2);
      root2.constraints <- root1.constraints;
      node1 := Goto { parent = id2 }
    | (Resolved t1, (Resolved t2 | FullyResolved (lazy t2)))
    | (FullyResolved (lazy t1), FullyResolved (lazy t2)) ->
      (* replace node first, in case rec_unify recurses back to these tvars *)
      node1 := Goto { parent = id2 };
      rec_unify cx trace ~use_op t1 t2
    | (FullyResolved (lazy t1), Resolved t2) ->
      (* prefer fully resolved roots to resolved roots *)
      root2.constraints <- root1.constraints;
      (* replace node first, in case rec_unify recurses back to these tvars *)
      node1 := Goto { parent = id2 };
      rec_unify cx trace ~use_op t1 t2

  (** Unify two type variables. This involves finding their roots, and making one
    point to the other. Ranks are used to keep chains short. *)
  and merge_ids cx trace ~use_op id1 id2 =
    let ((id1, _, root1) as root_node1) = Context.find_root cx id1 in
    let ((id2, _, root2) as root_node2) = Context.find_root cx id2 in
    match (root1.Constraint.constraints, root2.Constraint.constraints) with
    | (FullyResolved (lazy t1), FullyResolved (lazy t2)) -> rec_unify cx trace ~use_op t1 t2
    | _ ->
      if id1 = id2 then
        ()
      else if root1.rank < root2.rank then
        goto cx trace ~use_op root_node1 root_node2
      else if root2.rank < root1.rank then
        goto cx trace ~use_op:(unify_flip use_op) root_node2 root_node1
      else (
        root2.rank <- root1.rank + 1;
        goto cx trace ~use_op root_node1 root_node2
      )

  (** Resolve a type variable to a type. This involves finding its root, and
    resolving to that type. *)
  and resolve_id cx trace ~use_op ?(fully_resolved = false) id t =
    let (id, _, root) = Context.find_root cx id in
    match root.constraints with
    | Unresolved bounds ->
      let constraints =
        if fully_resolved then
          FullyResolved (lazy t)
        else
          Resolved t
      in
      root.constraints <- constraints;
      edges_and_flows_to_t cx trace ~opt:true (id, bounds) (UseT (use_op, t));
      edges_and_flows_from_t cx trace ~new_use_op:use_op ~opt:true t (id, bounds)
    | Resolved t_
    | FullyResolved (lazy t_) ->
      rec_unify cx trace ~use_op t_ t

  (******************)

  (* Unification of two types *)

  (* It is potentially dangerous to unify a type variable to a type that "forgets"
     constraints during propagation. These types are "any-like": the canonical
     example of such a type is any. Overall, we want unification to be a sound
     "optimization," in the sense that replacing bidirectional flows with
     unification should not miss errors. But consider a scenario where we have a
     type variable with two incoming flows, string and any, and two outgoing
     flows, number and any. If we replace the flows from/to any with an
     unification with any, we will miss the string/number incompatibility error.

     However, unifying with any-like types is sometimes desirable /
     intentional.
  *)
  and ok_unify ~unify_any = function
    | AnyT _ -> unify_any
    | _ -> true

  and __unify cx ~use_op ~unify_any t1 t2 trace =
    print_unify_types_if_verbose cx trace (t1, t2);
    (* If the type is the same type or we have already seen this type pair in our
     * cache then do not continue. *)
    if t1 = t2 then
      ()
    else (
      (* limit recursion depth *)
      RecursionCheck.check cx trace;

      (* In general, unifying t1 and t2 should have similar effects as flowing t1 to
         t2 and flowing t2 to t1. This also means that any restrictions on such
         flows should also be enforced here. In particular, we don't expect t1 or t2
         to be type parameters, and we don't expect t1 or t2 to be def types that
         don't make sense as use types. See __flow for more details. *)
      expect_proper_def t1;
      expect_proper_def t2;

      (* Before processing the unify action, check that it is not deferred. If it
         is, then when speculation is complete, the action either fires or is
         discarded depending on whether the case that created the action is
         selected or not. *)
      if not (Speculation.defer_action cx (Speculation_state.UnifyAction (use_op, t1, t2))) then
        match (t1, t2) with
        | (OpenT (_, id1), OpenT (_, id2)) -> merge_ids cx trace ~use_op id1 id2
        | (OpenT (_, id), t) when ok_unify ~unify_any t -> resolve_id cx trace ~use_op id t
        | (t, OpenT (_, id)) when ok_unify ~unify_any t ->
          resolve_id cx trace ~use_op:(unify_flip use_op) id t
        | (DefT (_, PolyT { id = id1; _ }), DefT (_, PolyT { id = id2; _ })) when id1 = id2 -> ()
        | ( DefT (_, ArrT (ArrayAT { elem_t = t1; tuple_view = tv1; react_dro = _ })),
            DefT (_, ArrT (ArrayAT { elem_t = t2; tuple_view = tv2; react_dro = _ }))
          ) ->
          let ts1 =
            Base.Option.value_map
              ~default:[]
              ~f:(fun (elements, _arity) -> tuple_ts_of_elements elements)
              tv1
          in
          let ts2 =
            Base.Option.value_map
              ~default:[]
              ~f:(fun (elements, _arity) -> tuple_ts_of_elements elements)
              tv2
          in
          array_unify cx trace ~use_op (ts1, t1, ts2, t2)
        | ( DefT
              ( r1,
                ArrT (TupleAT { elem_t = _; elements = elements1; arity = arity1; react_dro = _ })
              ),
            DefT
              ( r2,
                ArrT (TupleAT { elem_t = _; elements = elements2; arity = arity2; react_dro = _ })
              )
          ) ->
          let (num_req1, num_total1) = arity1 in
          let (num_req2, num_total2) = arity2 in
          if num_req1 <> num_req2 || num_total1 <> num_total2 then
            add_output
              cx
              ~trace
              (Error_message.ETupleArityMismatch ((r1, r2), arity1, arity2, use_op));
          let n = ref 0 in
          iter2opt
            (fun t1 t2 ->
              match (t1, t2) with
              | ( Some (TupleElement { t = t1; polarity = p1; name = _; optional = _; reason = _ }),
                  Some (TupleElement { t = t2; polarity = p2; name = _; optional = _; reason = _ })
                ) ->
                if not @@ Polarity.equal (p1, p2) then
                  add_output
                    cx
                    ~trace
                    (Error_message.ETupleElementPolarityMismatch
                       {
                         index = !n;
                         reason_lower = r1;
                         polarity_lower = p1;
                         reason_upper = r2;
                         polarity_upper = p2;
                         use_op;
                       }
                    );
                rec_unify cx trace ~use_op t1 t2;
                n := !n + 1
              | _ -> ())
            (elements1, elements2)
        | ( DefT (lreason, ObjT { props_tmap = lflds; flags = lflags; _ }),
            DefT (ureason, ObjT { props_tmap = uflds; flags = uflags; _ })
          ) ->
          (* ensure the keys and values are compatible with each other. *)
          let ldict = Obj_type.get_dict_opt lflags.obj_kind in
          let udict = Obj_type.get_dict_opt uflags.obj_kind in
          begin
            match (ldict, udict) with
            | (Some { key = lk; value = lv; _ }, Some { key = uk; value = uv; _ }) ->
              rec_unify
                cx
                trace
                lk
                uk
                ~use_op:
                  (Frame (IndexerKeyCompatibility { lower = lreason; upper = ureason }, use_op));
              rec_unify
                cx
                trace
                lv
                uv
                ~use_op:
                  (Frame
                     ( PropertyCompatibility { prop = None; lower = lreason; upper = ureason },
                       use_op
                     )
                  )
            | (Some _, None) ->
              let use_op =
                Frame
                  (PropertyCompatibility { prop = None; lower = ureason; upper = lreason }, use_op)
              in
              let lreason = replace_desc_reason RSomeProperty lreason in
              let err =
                Error_message.EPropNotFound
                  {
                    prop_name = None;
                    reason_prop = lreason;
                    reason_obj = ureason;
                    use_op;
                    suggestion = None;
                  }
              in
              add_output cx ~trace err
            | (None, Some _) ->
              let use_op =
                Frame
                  ( PropertyCompatibility { prop = None; lower = lreason; upper = ureason },
                    Frame (UnifyFlip, use_op)
                  )
              in
              let ureason = replace_desc_reason RSomeProperty ureason in
              let err =
                Error_message.EPropNotFound
                  {
                    prop_name = None;
                    reason_prop = lreason;
                    reason_obj = ureason;
                    use_op;
                    suggestion = None;
                  }
              in
              add_output cx ~trace err
            | (None, None) -> ()
          end;

          let lpmap = Context.find_props cx lflds in
          let upmap = Context.find_props cx uflds in
          NameUtils.Map.merge
            (fun x lp up ->
              ( if not (is_internal_name x || is_dictionary_exempt x) then
                match (lp, up) with
                | (Some p1, Some p2) -> unify_props cx trace ~use_op x lreason ureason p1 p2
                | (Some p1, None) ->
                  unify_prop_with_dict cx trace ~use_op x p1 lreason ureason udict
                | (None, Some p2) ->
                  unify_prop_with_dict cx trace ~use_op x p2 ureason lreason ldict
                | (None, None) -> ()
              );
              None)
            lpmap
            upmap
          |> ignore
        | ( DefT (_, FunT (_, ({ predicate = None; _ } as funtype1))),
            DefT (_, FunT (_, ({ predicate = None; _ } as funtype2)))
          )
          when List.length funtype1.params = List.length funtype2.params ->
          rec_unify cx trace ~use_op (fst funtype1.this_t) (fst funtype2.this_t);
          List.iter2
            (fun (_, t1) (_, t2) -> rec_unify cx trace ~use_op t1 t2)
            funtype1.params
            funtype2.params;
          rec_unify cx trace ~use_op funtype1.return_t funtype2.return_t
        | ( TypeAppT
              { reason = _; use_op = _; type_ = c1; targs = ts1; from_value = fv1; use_desc = _ },
            TypeAppT
              { reason = _; use_op = _; type_ = c2; targs = ts2; from_value = fv2; use_desc = _ }
          )
          when c1 = c2 && List.length ts1 = List.length ts2 && fv1 = fv2 ->
          List.iter2 (rec_unify cx trace ~use_op) ts1 ts2
        | (AnnotT (_, OpenT (_, id1), _), AnnotT (_, OpenT (_, id2), _)) -> begin
          (* It is tempting to unify the tvars here, but that would be problematic. These tvars should
             eventually resolve to the type definitions that these annotations reference. By unifying
             them, we might accidentally resolve one of the tvars to the type definition of the other,
             which would lead to confusing behavior.

             On the other hand, if the tvars are already resolved, then we can do something
             interesting... *)
          match (Context.find_graph cx id1, Context.find_graph cx id2) with
          | ((Resolved t1 | FullyResolved (lazy t1)), (Resolved t2 | FullyResolved (lazy t2)))
          (* Can we unify these types? Tempting, again, but annotations can refer to recursive type
             definitions, and we might get into an infinite loop (which could perhaps be avoided by
             a unification cache, but we'd rather not cache if we can get away with it).

             The alternative is to do naive unification, but we must be careful. In particular, it
             could cause confusing errors: recall that the naive unification of annotations goes
             through repositioning over these types.

             But if we simulate the same repositioning here, we won't really save anything. For
             example, these types could be essentially the same union, and repositioning them would
             introduce differences in their representations that would kill other
             optimizations. Thus, we focus on the special case where these types have the same
             reason, and then do naive unification. *)
            when Reason.concretize_equal (Context.aloc_tables cx) (reason_of_t t1) (reason_of_t t2)
            ->
            naive_unify cx trace ~use_op t1 t2
          | _ -> naive_unify cx trace ~use_op t1 t2
        end
        | _ -> naive_unify cx trace ~use_op t1 t2
    )

  and unify_props cx trace ~use_op x r1 r2 p1 p2 =
    let use_op = Frame (PropertyCompatibility { prop = Some x; lower = r1; upper = r2 }, use_op) in
    (* If both sides are neutral fields, we can just unify once *)
    match (p1, p2) with
    | ( Field { type_ = t1; polarity = Polarity.Neutral; _ },
        Field { type_ = t2; polarity = Polarity.Neutral; _ }
      ) ->
      rec_unify cx trace ~use_op t1 t2
    | _ ->
      (* Otherwise, unify read/write sides separately. *)
      (match (Property.read_t p1, Property.read_t p2) with
      | (Some t1, Some t2) -> rec_unify cx trace ~use_op t1 t2
      | _ -> ());
      (match (Property.write_t p1, Property.write_t p2) with
      | (Some t1, Some t2) -> rec_unify cx trace ~use_op t1 t2
      | _ -> ());

      (* Error if polarity is not compatible both ways. *)
      let polarity1 = Property.polarity p1 in
      let polarity2 = Property.polarity p2 in
      if not (Polarity.equal (polarity1, polarity2)) then
        add_output
          cx
          ~trace
          (Error_message.EPropPolarityMismatch ((r1, r2), Some x, (polarity1, polarity2), use_op))

  (* If some property `x` exists in one object but not another, ensure the
     property is compatible with a dictionary, or error if none. *)
  and unify_prop_with_dict cx trace ~use_op x p prop_obj_reason dict_reason dict =
    (* prop_obj_reason: reason of the object containing the prop
       dict_reason: reason of the object potentially containing a dictionary
       prop_reason: reason of the prop itself *)
    let prop_reason = replace_desc_reason (RProperty (Some x)) prop_obj_reason in
    match dict with
    | Some { key; value; dict_polarity; _ } ->
      rec_flow
        cx
        trace
        ( string_key x prop_reason,
          UseT
            ( Frame
                (IndexerKeyCompatibility { lower = dict_reason; upper = prop_obj_reason }, use_op),
              key
            )
        );
      let p2 =
        Field { preferred_def_locs = None; key_loc = None; type_ = value; polarity = dict_polarity }
      in
      unify_props cx trace ~use_op x prop_obj_reason dict_reason p p2
    | None ->
      let use_op =
        Frame
          ( PropertyCompatibility { prop = Some x; lower = dict_reason; upper = prop_obj_reason },
            use_op
          )
      in
      let err =
        Error_message.EPropNotFound
          {
            prop_name = Some x;
            reason_prop = prop_reason;
            reason_obj = dict_reason;
            use_op;
            suggestion = None;
          }
      in
      add_output cx ~trace err

  (* TODO: Unification between concrete types is still implemented as
     bidirectional flows. This means that the destructuring work is duplicated,
     and we're missing some opportunities for nested unification. *)
  and naive_unify cx trace ~use_op t1 t2 =
    rec_flow_t cx trace ~use_op (t1, t2);
    rec_flow_t cx trace ~use_op:(unify_flip use_op) (t2, t1)

  (* TODO: either ensure that array_unify is the same as array_flow both ways, or
     document why not. *)
  (* array helper *)
  and array_unify cx trace ~use_op = function
    | ([], e1, [], e2) ->
      (* general element1 = general element2 *)
      rec_unify cx trace ~use_op e1 e2
    | (ts1, _, [], e2)
    | ([], e2, ts1, _) ->
      (* specific element1 = general element2 *)
      List.iter (fun t1 -> rec_unify cx trace ~use_op t1 e2) ts1
    | (t1 :: ts1, e1, t2 :: ts2, e2) ->
      (* specific element1 = specific element2 *)
      rec_unify cx trace ~use_op t1 t2;
      array_unify cx trace ~use_op (ts1, e1, ts2, e2)

  (*******************************************************************)
  (* subtyping a sequence of arguments with a sequence of parameters *)
  (*******************************************************************)

  (* Process spread arguments and then apply the arguments to the parameters *)
  and multiflow_call cx trace ~use_op reason_op args ft =
    let resolve_to = ResolveSpreadsToMultiflowCallFull (mk_id (), ft) in
    resolve_call_list cx ~trace ~use_op reason_op args resolve_to

  (* Process spread arguments and then apply the arguments to the parameters *)
  and multiflow_subtype cx trace ~use_op reason_op args ft =
    let resolve_to = ResolveSpreadsToMultiflowSubtypeFull (mk_id (), ft) in
    resolve_call_list cx ~trace ~use_op reason_op args resolve_to

  (* Like multiflow_partial, but if there is no spread argument, it flows VoidT to
   * all unused parameters *)
  and multiflow_full
      cx ~trace ~use_op reason_op ~is_strict ~def_reason ~spread_arg ~rest_param (arglist, parlist)
      =
    let (unused_parameters, _) =
      multiflow_partial
        cx
        ~trace
        ~use_op
        reason_op
        ~is_strict
        ~def_reason
        ~spread_arg
        ~rest_param
        (arglist, parlist)
    in
    let _ =
      List.fold_left
        (fun n (_, param) ->
          let use_op = Frame (FunMissingArg { n; op = reason_op; def = def_reason }, use_op) in
          rec_flow cx trace (VoidT.why reason_op, UseT (use_op, param));
          n + 1)
        (List.length parlist - List.length unused_parameters + 1)
        unused_parameters
    in
    ()

  (* This is a tricky function. The simple description is that it flows all the
   * arguments to all the parameters. This function is used by
   * Function.prototype.apply, so after the arguments are applied, it returns the
   * unused parameters.
   *
   * It is a little trickier in that there may be a single spread argument after
   * all the regular arguments. There may also be a rest parameter.
   *)
  and multiflow_partial =
    let rec multiflow_non_spreads cx ~use_op n (arglist, parlist) =
      match (arglist, parlist) with
      (* Do not complain on too many arguments.
         This pattern is ubiqutous and causes a lot of noise when complained about.
         Note: optional/rest parameters do not provide a workaround in this case.
      *)
      | (_, [])
      (* No more arguments *)
      | ([], _) ->
        ([], arglist, parlist)
      | ((tin, _) :: tins, (name, tout) :: touts) ->
        (* flow `tin` (argument) to `tout` (param). *)
        let tout =
          let use_op =
            Frame (FunParam { n; name; lower = reason_of_t tin; upper = reason_of_t tout }, use_op)
          in
          UseT (use_op, tout)
        in

        let (used_pairs, unused_arglist, unused_parlist) =
          multiflow_non_spreads cx ~use_op (n + 1) (tins, touts)
        in
        (* We additionally record the type of the arg ~> parameter at the location of the parameter
         * to power autofixes for missing parameter annotations *)
        let par_def_loc =
          let reason = reason_of_use_t tout in
          def_loc_of_reason reason
        in
        Context.add_missing_local_annot_lower_bound cx par_def_loc tin;
        ((tin, tout) :: used_pairs, unused_arglist, unused_parlist)
    in
    fun cx ~trace ~use_op ~is_strict ~def_reason ~spread_arg ~rest_param reason_op (arglist, parlist)
        ->
      (* Handle all the non-spread arguments and all the non-rest parameters *)
      let (used_pairs, unused_arglist, unused_parlist) =
        multiflow_non_spreads cx ~use_op 1 (arglist, parlist)
      in
      (* If there is a spread argument, it will consume all the unused parameters *)
      let (used_pairs, unused_parlist) =
        match spread_arg with
        | None -> (used_pairs, unused_parlist)
        | Some (spread_arg_elemt, _) ->
          (* The spread argument may be an empty array and to be 100% correct, we
           * should flow VoidT to every remaining parameter, however we don't. This
           * is consistent with how we treat arrays almost everywhere else *)
          ( used_pairs
            @ Base.List.map
                ~f:(fun (_, param) ->
                  let use_op =
                    Frame
                      ( FunRestParam
                          { lower = reason_of_t spread_arg_elemt; upper = reason_of_t param },
                        use_op
                      )
                  in
                  (spread_arg_elemt, UseT (use_op, param)))
                unused_parlist,
            []
          )
      in
      (* If there is a rest parameter, it will consume all the unused arguments *)
      match rest_param with
      | None ->
        ( if is_strict then
          match unused_arglist with
          | [] -> ()
          | (first_unused_arg, _) :: _ ->
            Error_message.EFunctionCallExtraArg
              ( mk_reason RFunctionUnusedArgument (loc_of_t first_unused_arg),
                def_reason,
                List.length parlist,
                use_op
              )
            |> add_output cx ~trace
        );

        (* Flow the args and params after we add the EFunctionCallExtraArg error.
         * This improves speculation error reporting. *)
        List.iter (rec_flow cx trace) used_pairs;

        (unused_parlist, rest_param)
      | Some (name, loc, rest_param) ->
        List.iter (rec_flow cx trace) used_pairs;
        let rest_reason = reason_of_t rest_param in
        let orig_rest_reason = repos_reason loc rest_reason in
        (* We're going to build an array literal with all the unused arguments
         * (and the spread argument if it exists). Then we're going to flow that
         * to the rest parameter *)
        let rev_elems =
          List.rev_map
            (fun (arg, generic) ->
              let reason = mk_reason RArrayElement (loc_of_t arg) in
              UnresolvedArg (mk_tuple_element reason arg, generic))
            unused_arglist
        in
        let unused_rest_param =
          match spread_arg with
          | None ->
            (* If the rest parameter is consuming N elements, then drop N elements
             * from the rest parameter *)
            Tvar.mk_where cx rest_reason (fun tout ->
                let i = List.length rev_elems in
                rec_flow cx trace (rest_param, ArrRestT (use_op, orig_rest_reason, i, tout))
            )
          | Some _ ->
            (* If there is a spread argument, then a tuple rest parameter will error
             * anyway. So let's assume that the rest param is an array with unknown
             * arity. Dropping elements from it isn't worth doing *)
            rest_param
        in
        let elems =
          match spread_arg with
          | None -> List.rev rev_elems
          | Some (spread_arg_elemt, generic) ->
            let reason = reason_of_t spread_arg_elemt in
            let spread_array =
              DefT
                ( reason,
                  ArrT (ArrayAT { elem_t = spread_arg_elemt; tuple_view = None; react_dro = None })
                )
            in
            let spread_array =
              Base.Option.value_map
                ~f:(fun id ->
                  GenericT
                    {
                      id;
                      bound = spread_array;
                      reason;
                      name = Generic.subst_name_of_id id;
                      no_infer = false;
                    })
                ~default:spread_array
                generic
            in
            List.rev_append rev_elems [UnresolvedSpreadArg spread_array]
        in
        let arg_array_reason =
          replace_desc_reason (RRestArrayLit (desc_of_reason reason_op)) reason_op
        in
        let arg_array =
          Tvar.mk_where cx arg_array_reason (fun tout ->
              let reason_op = arg_array_reason in
              let element_reason =
                let instantiable = Reason.is_instantiable_reason rest_reason in
                replace_desc_reason (Reason.RInferredUnionElemArray { instantiable }) reason_op
              in
              let elem_t = Tvar.mk cx element_reason in
              ResolveSpreadsToArrayLiteral (mk_id (), elem_t, tout)
              |> resolve_spread_list cx ~use_op ~reason_op elems
          )
        in
        let () =
          let use_op =
            Frame
              ( FunRestParam { lower = reason_of_t arg_array; upper = reason_of_t rest_param },
                use_op
              )
          in
          rec_flow cx trace (arg_array, UseT (use_op, rest_param))
        in
        (unused_parlist, Some (name, loc, unused_rest_param))

  and resolve_call_list cx ~trace ~use_op reason_op args resolve_to =
    let unresolved =
      Base.List.map
        ~f:(function
          | Arg t ->
            let reason = mk_reason RArrayElement (loc_of_t t) in
            UnresolvedArg (mk_tuple_element reason t, None)
          | SpreadArg t -> UnresolvedSpreadArg t)
        args
    in
    resolve_spread_list_rec cx ~trace ~use_op ~reason_op ([], unresolved) resolve_to

  and resolve_spread_list cx ~use_op ~reason_op list resolve_to =
    resolve_spread_list_rec cx ~use_op ~reason_op ([], list) resolve_to

  (* This function goes through the unresolved elements to find the next rest
   * element to resolve *)
  and resolve_spread_list_rec cx ?trace ~use_op ~reason_op (resolved, unresolved) resolve_to =
    match (resolved, unresolved) with
    | (resolved, []) ->
      finish_resolve_spread_list cx ?trace ~use_op ~reason_op (List.rev resolved) resolve_to
    | (resolved, UnresolvedArg (next, generic) :: unresolved) ->
      resolve_spread_list_rec
        cx
        ?trace
        ~use_op
        ~reason_op
        (ResolvedArg (next, generic) :: resolved, unresolved)
        resolve_to
    | (resolved, UnresolvedSpreadArg next :: unresolved) ->
      flow_opt
        cx
        ?trace
        ( next,
          ResolveSpreadT
            ( use_op,
              reason_op,
              { rrt_resolved = resolved; rrt_unresolved = unresolved; rrt_resolve_to = resolve_to }
            )
        )

  (* Now that everything is resolved, we can construct whatever type we're trying
   * to resolve to. *)
  and finish_resolve_spread_list =
    let propagate_dro cx elem arrtype =
      match arrtype with
      | ROArrayAT (_, Some l)
      | ArrayAT { react_dro = Some l; _ }
      | TupleAT { react_dro = Some l; _ } ->
        mk_react_dro cx unknown_use l elem
      | _ -> elem
    in

    (* Turn tuple rest params into single params *)
    let flatten_spread_args cx args =
      let (args_rev, spread_after_opt, _) =
        Base.List.fold_left args ~init:([], false, false) ~f:(fun acc arg ->
            let (args_rev, spread_after_opt, seen_opt) = acc in
            match arg with
            | ResolvedSpreadArg (_, arrtype, generic) -> begin
              let spread_after_opt = spread_after_opt || seen_opt in
              let (args_rev, seen_opt) =
                match arrtype with
                | ArrayAT { tuple_view = None; _ }
                | ArrayAT { tuple_view = Some ([], _); _ } ->
                  (* The latter case corresponds to the empty array literal. If
                   * we folded over the empty tuple_types list, then this would
                   * cause an empty result. *)
                  (arg :: args_rev, seen_opt)
                | ArrayAT { tuple_view = Some (elements, _); _ }
                | TupleAT { elements; _ } ->
                  Base.List.fold_left
                    ~f:(fun (args_rev, seen_opt) (TupleElement ({ optional; t; _ } as elem)) ->
                      let elem = TupleElement { elem with t = propagate_dro cx t arrtype } in
                      (ResolvedArg (elem, generic) :: args_rev, seen_opt || optional))
                    ~init:(args_rev, seen_opt)
                    elements
                | ROArrayAT _ -> (arg :: args_rev, seen_opt)
              in
              (args_rev, spread_after_opt, seen_opt)
            end
            | ResolvedAnySpreadArg _ -> (arg :: args_rev, spread_after_opt, seen_opt)
            | ResolvedArg (TupleElement { optional; _ }, _) ->
              (arg :: args_rev, spread_after_opt, seen_opt || optional)
        )
      in
      (List.rev args_rev, spread_after_opt)
    in
    let spread_resolved_to_any_src =
      List.find_map (function
          | ResolvedAnySpreadArg (_, src) -> Some src
          | ResolvedArg _
          | ResolvedSpreadArg _ ->
            None
          )
    in
    let finish_array cx ~use_op ?trace ~reason_op ~resolve_to resolved elem_t tout =
      (* Did `any` flow to one of the rest parameters? If so, we need to resolve
       * to a type that is both a subtype and supertype of the desired type. *)
      let result =
        match spread_resolved_to_any_src resolved with
        | Some any_src ->
          (match resolve_to with
          (* Array<any> is a good enough any type for arrays *)
          | ResolveToArray ->
            DefT
              ( reason_op,
                ArrT
                  (ArrayAT
                     { elem_t = AnyT.why any_src reason_op; tuple_view = None; react_dro = None }
                  )
              )
          (* Array literals can flow to a tuple. Arrays can't. So if the presence
           * of an `any` forces us to degrade an array literal to Array<any> then
           * we might get a new error. Since introducing `any`'s shouldn't cause
           * errors, this is bad. Instead, let's degrade array literals to `any` *)
          | ResolveToArrayLiteral
          (* There is no AnyTupleT type, so let's degrade to `any`. *)
          | ResolveToTupleType ->
            AnyT.why any_src reason_op)
        | None ->
          (* Spreads that resolve to tuples are flattened *)
          let (elems, spread_after_opt) = flatten_spread_args cx resolved in

          let tuple_elements =
            match resolve_to with
            | ResolveToArrayLiteral
            | ResolveToTupleType ->
              elems
              (* If no spreads are left, then this is a tuple too! *)
              |> List.fold_left
                   (fun acc elem ->
                     match (acc, elem) with
                     | (None, _) -> None
                     | (_, ResolvedSpreadArg _) -> None
                     | (Some tuple_elements, ResolvedArg (elem, _)) ->
                       (* Spreading array values into a fresh literal drops variance,
                        * just like object spread. *)
                       let elem =
                         let (TupleElement { t; optional; name; reason; polarity = _ }) = elem in
                         TupleElement { t; optional; name; reason; polarity = Polarity.Neutral }
                       in
                       Some (elem :: tuple_elements)
                     | (_, ResolvedAnySpreadArg _) -> failwith "Should not be hit")
                   (Some [])
              |> Base.Option.map ~f:List.rev
            | ResolveToArray -> None
          in

          (* We infer the array's general element type by looking at the type of
           * every element in the array *)
          let (tset, generic) =
            Generic.(
              List.fold_left
                (fun (tset, generic_state) elem ->
                  let (elem_t, generic, ro) =
                    match elem with
                    | ResolvedSpreadArg (_, arrtype, generic) ->
                      ( propagate_dro cx (elemt_of_arrtype arrtype) arrtype,
                        generic,
                        ro_of_arrtype arrtype
                      )
                    | ResolvedArg (TupleElement { t = elem_t; _ }, generic) ->
                      (elem_t, generic, ArraySpread.NonROSpread)
                    | ResolvedAnySpreadArg _ -> failwith "Should not be hit"
                  in
                  ( TypeExSet.add elem_t tset,
                    ArraySpread.merge
                      ~printer:
                        (print_if_verbose_lazy
                           cx
                           ~trace:(Base.Option.value trace ~default:Trace.dummy_trace)
                        )
                      generic_state
                      generic
                      ro
                  ))
                (TypeExSet.empty, ArraySpread.Bottom)
                elems
            )
          in
          let generic = Generic.ArraySpread.to_option generic in

          (* composite elem type is an upper bound of all element types *)
          (* Should the element type of the array be the union of its element types?

             No. Instead of using a union, we use an unresolved tvar to
             represent the least upper bound of each element type. Effectively,
             this keeps the element type "open," at least locally.[*]

             Using a union pins down the element type prematurely, and moreover,
             might lead to speculative matching when setting elements or caling
             contravariant methods (`push`, `concat`, etc.) on the array.

             In any case, using a union doesn't quite work as intended today
             when the element types themselves could be unresolved tvars. For
             example, the following code would work even with unions:

             declare var o: { x: number; }
             var a = ["hey", o.x]; // no error, but is an error if 42 replaces o.x
             declare var i: number;
             a[i] = false;

             [*] Eventually, the element type does get pinned down to a union
             when it is part of the module's exports. In the future we might
             have to do that pinning more carefully, and using an unresolved
             tvar instead of a union here doesn't conflict with those plans.
          *)
          TypeExSet.elements tset |> List.iter (fun t -> flow cx (t, UseT (use_op, elem_t)));

          let t =
            match (resolve_to, tuple_elements, spread_after_opt) with
            | (ResolveToArray, _, _)
            | (ResolveToArrayLiteral, None, _)
            | (ResolveToArrayLiteral, _, true) ->
              DefT (reason_op, ArrT (ArrayAT { elem_t; tuple_view = None; react_dro = None }))
            | (ResolveToArrayLiteral, Some elements, _) ->
              let (valid, arity) =
                validate_tuple_elements
                  cx
                  ?trace
                  ~reason_tuple:reason_op
                  ~error_on_req_after_opt:false
                  elements
              in
              if valid then
                DefT
                  ( reason_op,
                    ArrT (ArrayAT { elem_t; tuple_view = Some (elements, arity); react_dro = None })
                  )
              else
                DefT (reason_op, ArrT (ArrayAT { elem_t; tuple_view = None; react_dro = None }))
            | (ResolveToTupleType, Some elements, _) ->
              let (valid, arity) =
                validate_tuple_elements
                  cx
                  ?trace
                  ~reason_tuple:reason_op
                  ~error_on_req_after_opt:true
                  elements
              in
              if valid then
                DefT (reason_op, ArrT (TupleAT { elem_t; elements; arity; react_dro = None }))
              else
                AnyT.error reason_op
            | (ResolveToTupleType, None, _) -> AnyT.error reason_op
          in
          Base.Option.value_map
            ~f:(fun id ->
              GenericT
                {
                  bound = t;
                  id;
                  name = Generic.subst_name_of_id id;
                  reason = reason_of_t t;
                  no_infer = false;
                })
            ~default:t
            generic
      in
      flow_opt_t cx ~use_op ?trace (result, tout)
    in
    (* If there are no spread elements or if all the spread elements resolved to
     * tuples or array literals, then this is easy. We just flatten them all.
     *
     * However, if we have a spread that resolved to any or to an array of
     * unknown length, then we're in trouble. Basically, any remaining argument
     * might flow to any remaining parameter.
     *)
    let flatten_call_arg =
      let rec flatten cx r args spread resolved =
        if resolved = [] then
          (args, spread)
        else
          match spread with
          | None ->
            (match resolved with
            | ResolvedArg (TupleElement { t; _ }, generic) :: rest ->
              flatten cx r ((t, generic) :: args) spread rest
            | ResolvedSpreadArg (_, ArrayAT { tuple_view = Some (elements, _); _ }, generic) :: rest
            | ResolvedSpreadArg (_, TupleAT { elements; _ }, generic) :: rest ->
              let args =
                List.rev_append
                  (List.map
                     (fun (TupleElement { t; polarity = _; name = _; optional = _; reason = _ }) ->
                       (t, generic))
                     elements
                  )
                  args
              in
              flatten cx r args spread rest
            | ResolvedSpreadArg (r, _, _) :: _
            | ResolvedAnySpreadArg (r, _) :: _ ->
              (* We weren't able to flatten the call argument list to remove all
               * spreads. This means we need to build a spread argument, with
               * unknown arity. *)
              let tset = TypeExSet.empty in
              flatten cx r args (Some (tset, Generic.ArraySpread.Bottom)) resolved
            | [] -> failwith "Empty list already handled")
          | Some (tset, generic) ->
            let (elemt, generic', ro, rest) =
              match resolved with
              | ResolvedArg (TupleElement { t; _ }, generic) :: rest ->
                (t, generic, Generic.ArraySpread.NonROSpread, rest)
              | ResolvedSpreadArg (_, arrtype, generic) :: rest ->
                (elemt_of_arrtype arrtype, generic, ro_of_arrtype arrtype, rest)
              | ResolvedAnySpreadArg (reason, any_src) :: rest ->
                (AnyT.why any_src reason, None, Generic.ArraySpread.NonROSpread, rest)
              | [] -> failwith "Empty list already handled"
            in
            let tset = TypeExSet.add elemt tset in
            let generic =
              Generic.ArraySpread.merge ~printer:(print_if_verbose_lazy cx) generic generic' ro
            in
            flatten cx r args (Some (tset, generic)) rest
      in
      fun cx ~use_op r resolved ->
        let (args, spread) = flatten cx r [] None resolved in
        let spread =
          Base.Option.map
            ~f:(fun (tset, generic) ->
              let generic = Generic.ArraySpread.to_option generic in
              let r = mk_reason RArray (loc_of_reason r) in
              ( Tvar.mk_where cx r (fun tvar ->
                    TypeExSet.iter (fun t -> flow cx (t, UseT (use_op, tvar))) tset
                ),
                generic
              ))
            spread
        in
        (List.rev args, spread)
    in
    (* This is used for things like Function.prototype.bind, which partially
     * apply arguments and then return the new function. *)
    let finish_multiflow_partial cx ?trace ~use_op ~reason_op ft call_reason resolved tout =
      (* Multiflows always come out of a flow *)
      let trace =
        match trace with
        | Some trace -> trace
        | None -> failwith "All multiflows show have a trace"
      in
      let { params; rest_param; return_t; def_reason; predicate; hook; _ } = ft in
      let (args, spread_arg) = flatten_call_arg cx ~use_op reason_op resolved in
      let (params, rest_param) =
        multiflow_partial
          cx
          ~trace
          ~use_op
          reason_op
          ~is_strict:true
          ~def_reason
          ~spread_arg
          ~rest_param
          (args, params)
      in
      let (params_names, params_tlist) = List.split params in
      (* e.g. "bound function type", positioned at reason_op *)
      let bound_reason =
        let desc = RBound (desc_of_reason reason_op) in
        replace_desc_reason desc call_reason
      in
      let def_reason = reason_op in
      let funt =
        DefT
          ( reason_op,
            FunT
              ( dummy_static bound_reason,
                mk_methodtype
                  (dummy_this (loc_of_reason reason_op))
                  params_tlist
                  return_t
                  ~predicate
                  ~rest_param
                  ~def_reason
                  ~params_names
                  ~hook
              )
          )
      in
      rec_flow_t cx trace ~use_op:unknown_use (funt, tout)
    in
    (* This is used for things like function application, where all the arguments
     * are applied to a function *)
    let finish_multiflow_full cx ?trace ~use_op ~reason_op ~is_strict ft resolved =
      (* Multiflows always come out of a flow *)
      let trace =
        match trace with
        | Some trace -> trace
        | None -> failwith "All multiflows show have a trace"
      in
      let { params; rest_param; def_reason; _ } = ft in
      let (args, spread_arg) = flatten_call_arg cx ~use_op reason_op resolved in
      multiflow_full
        cx
        ~trace
        ~use_op
        reason_op
        ~is_strict
        ~def_reason
        ~spread_arg
        ~rest_param
        (args, params)
    in
    (* Similar to finish_multiflow_full but for custom functions. *)
    let finish_custom_fun_call ~return_hint cx ?trace ~use_op ~reason_op kind tout resolved =
      (* Multiflows always come out of a flow *)
      let trace =
        match trace with
        | Some trace -> trace
        | None -> failwith "All multiflows show have a trace"
      in
      let (args, spread_arg) = flatten_call_arg cx ~use_op reason_op resolved in
      let spread_arg = Base.Option.map ~f:fst spread_arg in
      let args = Base.List.map ~f:fst args in
      CustomFunKit.run ~return_hint cx trace ~use_op reason_op kind args spread_arg tout
    in
    (* This is used for things like Function.prototype.apply, whose second arg is
     * basically a spread argument that we'd like to resolve *)
    let finish_call_t cx ?trace ~use_op ~reason_op funcalltype resolved tin =
      let (flattened, _) = flatten_spread_args cx resolved in
      let call_args_tlist =
        Base.List.map
          ~f:(function
            | ResolvedArg (TupleElement { t; _ }, _) -> Arg t
            | ResolvedSpreadArg (r, arrtype, generic) ->
              let arr = DefT (r, ArrT arrtype) in
              let arr =
                Base.Option.value_map
                  ~f:(fun id ->
                    GenericT
                      {
                        bound = arr;
                        reason = r;
                        id;
                        name = Generic.subst_name_of_id id;
                        no_infer = false;
                      })
                  ~default:arr
                  generic
              in
              SpreadArg arr
            | ResolvedAnySpreadArg (r, any_src) -> SpreadArg (AnyT.why any_src r))
          flattened
      in
      let call_t =
        CallT
          {
            use_op;
            reason = reason_op;
            call_action = Funcalltype { funcalltype with call_args_tlist };
            return_hint = Type.hint_unavailable;
          }
      in
      flow_opt cx ?trace (tin, call_t)
    in
    fun cx ?trace ~use_op ~reason_op resolved resolve_to ->
      match resolve_to with
      | ResolveSpreadsToTupleType (_, elem_t, tout) ->
        finish_array
          cx
          ~use_op
          ?trace
          ~reason_op
          ~resolve_to:ResolveToTupleType
          resolved
          elem_t
          tout
      | ResolveSpreadsToArrayLiteral (_, elem_t, tout) ->
        finish_array
          cx
          ~use_op
          ?trace
          ~reason_op
          ~resolve_to:ResolveToArrayLiteral
          resolved
          elem_t
          tout
      | ResolveSpreadsToArray (elem_t, tout) ->
        finish_array cx ~use_op ?trace ~reason_op ~resolve_to:ResolveToArray resolved elem_t tout
      | ResolveSpreadsToMultiflowPartial (_, ft, call_reason, tout) ->
        finish_multiflow_partial cx ?trace ~use_op ~reason_op ft call_reason resolved tout
      | ResolveSpreadsToMultiflowCallFull (_, ft) ->
        finish_multiflow_full cx ?trace ~use_op ~reason_op ~is_strict:true ft resolved
      | ResolveSpreadsToMultiflowSubtypeFull (_, ft) ->
        finish_multiflow_full cx ?trace ~use_op ~reason_op ~is_strict:false ft resolved
      | ResolveSpreadsToCustomFunCall (_, kind, tout, return_hint) ->
        finish_custom_fun_call ~return_hint cx ?trace ~use_op ~reason_op kind tout resolved
      | ResolveSpreadsToCallT (funcalltype, tin) ->
        finish_call_t cx ?trace ~use_op ~reason_op funcalltype resolved tin

  and apply_method_action cx trace l use_op reason_call this_arg action =
    match action with
    | CallM { methodcalltype = app; return_hint; specialized_callee } ->
      let u =
        CallT
          {
            use_op;
            reason = reason_call;
            call_action = Funcalltype (call_of_method_app this_arg specialized_callee app);
            return_hint;
          }
      in
      rec_flow cx trace (l, u)
    | ChainM
        {
          exp_reason;
          lhs_reason;
          methodcalltype = app;
          voided_out = vs;
          return_hint;
          specialized_callee;
        } ->
      let u =
        OptionalChainT
          {
            reason = exp_reason;
            lhs_reason;
            t_out =
              CallT
                {
                  use_op;
                  reason = reason_call;
                  call_action = Funcalltype (call_of_method_app this_arg specialized_callee app);
                  return_hint;
                };
            voided_out = vs;
          }
      in
      rec_flow cx trace (l, u)
    | NoMethodAction prop_t -> rec_flow_t cx trace ~use_op:unknown_use (l, prop_t)

  and perform_elem_action cx trace ~use_op ~restrict_deletes reason_op l value action =
    match (action, restrict_deletes) with
    | (ReadElem { tout; _ }, _) ->
      let loc = loc_of_reason reason_op in
      rec_flow_t cx trace ~use_op:unknown_use (reposition cx ~trace loc value, OpenT tout)
    | (WriteElem { tin; tout; mode = Assign }, _)
    | (WriteElem { tin; tout; mode = Delete }, true) ->
      rec_flow cx trace (tin, UseT (use_op, value));
      Base.Option.iter ~f:(fun t -> rec_flow_t cx trace ~use_op:unknown_use (l, t)) tout
    | (WriteElem { tin; tout; mode = Delete }, false) ->
      (* Ok to delete arbitrary elements on arrays, not OK for tuples *)
      rec_flow cx trace (tin, UseT (use_op, VoidT.why (reason_of_t value)));
      Base.Option.iter ~f:(fun t -> rec_flow_t cx trace ~use_op:unknown_use (l, t)) tout
    | (CallElem (reason_call, action), _) ->
      apply_method_action cx trace value use_op reason_call l action

  (* builtins, contd. *)

  and get_builtin_typeapp cx reason ?(use_desc = false) x targs =
    let t = Flow_js_utils.lookup_builtin_type cx x reason in
    typeapp ~from_value:false ~use_desc reason t targs

  (* Specialize a polymorphic class, make an instance of the specialized class. *)
  and mk_typeapp_instance_annot
      cx ?trace ~use_op ~reason_op ~reason_tapp ~from_value ?(cache = false) c ts =
    let t = Tvar.mk cx reason_tapp in
    flow_opt cx ?trace (c, SpecializeT (use_op, reason_op, reason_tapp, cache, Some ts, t));
    if from_value then
      reposition_reason cx ?trace reason_tapp ~use_desc:false t
    else
      mk_instance_raw cx ?trace reason_tapp ~reason_type:(reason_of_t c) t

  and mk_typeapp_instance cx ?trace ~use_op ~reason_op ~reason_tapp ~from_value c ts =
    let t = Tvar.mk cx reason_tapp in
    flow_opt cx ?trace (c, SpecializeT (use_op, reason_op, reason_tapp, false, Some ts, t));
    if from_value then
      reposition_reason cx ?trace reason_tapp ~use_desc:false t
    else
      mk_instance_source cx ?trace reason_tapp ~reason_type:(reason_of_t c) t

  and mk_typeapp_instance_of_poly
      cx trace ~use_op ~reason_op ~reason_tapp ~from_value id tparams_loc xs t ts =
    let t = mk_typeapp_of_poly cx trace ~use_op ~reason_op ~reason_tapp id tparams_loc xs t ts in
    if from_value then
      reposition_reason cx ~trace reason_tapp ~use_desc:false t
    else
      mk_instance cx ~trace reason_tapp t

  and mk_instance cx ?type_t_kind ?trace instance_reason ?use_desc c =
    mk_instance_raw cx ?type_t_kind ?trace instance_reason ?use_desc ~reason_type:instance_reason c

  and mk_instance_source cx ?(type_t_kind = InstanceKind) ?trace instance_reason ~reason_type c =
    Tvar.mk_where cx instance_reason (fun t ->
        (* this part is similar to making a runtime value *)
        flow_opt cx ?trace (c, ValueToTypeReferenceT (unknown_use, reason_type, type_t_kind, t))
    )

  and mk_instance_raw cx ?type_t_kind ?trace instance_reason ?(use_desc = false) ~reason_type c =
    (* Make an annotation. *)
    let source = mk_instance_source cx ?type_t_kind ?trace instance_reason ~reason_type c in
    AnnotT (instance_reason, source, use_desc)

  and instance_lookup_kind
      cx trace ~reason_instance ~reason_op ~method_accessible instance_t propref lookup_action =
    match propref with
    | Named { name; from_indexed_access; _ }
      when (not from_indexed_access) || is_munged_prop_name cx name ->
      Strict reason_instance
    | _ ->
      let lookup_default =
        ( instance_t,
          Tvar.mk_where cx reason_op (fun tvar ->
              rec_flow
                cx
                trace
                ( tvar,
                  LookupT
                    {
                      reason = reason_op;
                      lookup_kind = Strict reason_instance;
                      try_ts_on_failure = [];
                      propref;
                      lookup_action;
                      ids = None;
                      method_accessible;
                      ignore_dicts = false;
                    }
                )
          )
        )
      in
      NonstrictReturning (Some lookup_default, None)

  and reposition_reason cx ?trace reason ?(use_desc = false) t =
    reposition
      cx
      ?trace
      (loc_of_reason reason)
      ?desc:
        ( if use_desc then
          Some (desc_of_reason ~unwrap:false reason)
        else
          None
        )
      ?annot_loc:(annot_loc_of_reason reason)
      t

  (* set the position of the given def type from a reason *)
  and reposition cx ?trace (loc : ALoc.t) ?desc ?annot_loc t =
    let mod_reason reason =
      let reason = opt_annot_reason ?annot_loc @@ repos_reason loc reason in
      match desc with
      | Some d -> replace_desc_new_reason d reason
      | None -> reason
    in
    let mk_cached_tvar_where reason t_open id f =
      let repos_cache = Context.repos_cache cx in
      match Repos_cache.find id reason !repos_cache with
      | Some t -> t
      | None ->
        Tvar.mk_where cx reason (fun tvar ->
            repos_cache := Repos_cache.add reason t_open tvar !repos_cache;
            f tvar
        )
    in
    let rec recurse seen = function
      | OpenT (r, id) as t_open ->
        let reason = mod_reason r in
        let use_desc = Base.Option.is_some desc in
        let constraints = Context.find_graph cx id in
        begin
          match constraints with
          (* TODO: In the FullyResolved case, repositioning will cause us to "lose"
           * the fully resolved status. We should be able to preserve it. *)
          | Resolved t
          | FullyResolved (lazy t) ->
            (* A tvar may be resolved to a type that has special repositioning logic,
             * like UnionT. We want to recurse to pick up that logic, but must be
             * careful as the union may refer back to the tvar itself, causing a loop.
             * To break the loop, we pass down a map of "already seen" tvars. *)
            (match IMap.find_opt id seen with
            | Some t -> t
            | None ->
              (* The resulting tvar should be fully resolved if this one is *)
              let fully_resolved =
                match constraints with
                | Resolved _ -> false
                | FullyResolved _ -> true
                | Unresolved _ -> assert_false "handled below"
              in
              mk_cached_tvar_where reason t_open id (fun tvar ->
                  (* All `t` in `Resolved ( t)` are concrete. Because `t` is a concrete
                   * type, `t'` is also necessarily concrete (i.e., reposition preserves
                   * open -> open, concrete -> concrete). The unification below thus
                   * results in resolving `tvar` to `t'`, so we end up with a resolved
                   * tvar whenever we started with one. *)
                  let t' = recurse (IMap.add id tvar seen) t in
                  (* resolve_id requires a trace param *)
                  let use_op = unknown_use in
                  let trace =
                    match trace with
                    | None -> Trace.unit_trace tvar (UseT (use_op, t'))
                    | Some trace ->
                      let max = Context.max_trace_depth cx in
                      Trace.rec_trace ~max tvar (UseT (use_op, t')) trace
                  in
                  let (_, id) = open_tvar tvar in
                  resolve_id cx trace ~use_op ~fully_resolved id t'
              ))
          | Unresolved _ ->
            if is_instantiable_reason r && Context.in_implicit_instantiation cx then
              t_open
            else
              mk_cached_tvar_where reason t_open id (fun tvar ->
                  flow_opt
                    cx
                    ?trace
                    (t_open, ReposLowerT (reason, use_desc, UseT (unknown_use, tvar)))
              )
        end
      | EvalT (root, defer_use_t, id) as t ->
        (* Modifying the reason of `EvalT`, as we do for other types, is not
           enough, since it will only affect the reason of the resulting tvar.
           Instead, repositioning a `EvalT` should simulate repositioning the
           resulting tvar, i.e., flowing repositioned *lower bounds* to the
           resulting tvar. (Another way of thinking about this is that a `EvalT`
           is just as transparent as its resulting tvar.) *)
        let defer_use_t = mod_reason_of_defer_use_t mod_reason defer_use_t in
        let reason = reason_of_defer_use_t defer_use_t in
        let use_desc = Base.Option.is_some desc in
        begin
          match Cache.Eval.find_repos cx root defer_use_t id with
          | Some tvar -> tvar
          | None ->
            Tvar.mk_where cx reason (fun tvar ->
                Cache.Eval.add_repos cx root defer_use_t id tvar;
                flow_opt cx ?trace (t, ReposLowerT (reason, use_desc, UseT (unknown_use, tvar)));
                if not (Tvar_resolver.has_unresolved_tvars cx t) then Tvar_resolver.resolve cx tvar
            )
        end
      | MaybeT (r, t) ->
        (* repositions both the MaybeT and the nested type. MaybeT represets `?T`.
           elsewhere, when we decompose into T | NullT | VoidT, we use the reason
           of the MaybeT for NullT and VoidT but don't reposition `t`, so that any
           errors on the NullT or VoidT point at ?T, but errors on the T point at
           T. *)
        let r = mod_reason r in
        MaybeT (r, recurse seen t)
      | OptionalT { reason; type_ = t; use_desc } ->
        let reason = mod_reason reason in
        OptionalT { reason; type_ = recurse seen t; use_desc }
      | UnionT (r, rep) ->
        let r = mod_reason r in
        let rep = UnionRep.ident_map ~always_keep_source:true (recurse seen) rep in
        UnionT (r, rep)
      | OpaqueT (r, opaquetype) ->
        let r = mod_reason r in
        OpaqueT
          ( r,
            {
              opaquetype with
              underlying_t = OptionUtils.ident_map (recurse seen) opaquetype.underlying_t;
              super_t = OptionUtils.ident_map (recurse seen) opaquetype.super_t;
            }
          )
      | ExactT (r, t) ->
        let r = mod_reason r in
        ExactT (r, recurse seen t)
      | DefT (r, RendersT (StructuralRenders { renders_variant; renders_structural_type = t })) ->
        let r = mod_reason r in
        DefT
          ( r,
            RendersT
              (StructuralRenders { renders_variant; renders_structural_type = recurse seen t })
          )
      | t -> mod_reason_of_t mod_reason t
    in
    recurse IMap.empty t

  and get_builtin_type cx ?trace reason ?(use_desc = false) x =
    let t = Flow_js_utils.lookup_builtin_type cx x reason in
    mk_instance cx ?trace reason ~use_desc t

  and flow_all_in_union cx trace rep u =
    iter_union ~f:rec_flow ~init:() ~join:(fun _ _ -> ()) cx trace rep u

  and call_args_iter f =
    List.iter (function
        | Arg t
        | SpreadArg t
        -> f t
        )

  (* There's a lot of code that looks at a call argument list and tries to do
   * something with one or two arguments. Usually this code assumes that the
   * argument is not a spread argument. This utility function helps with that *)
  and extract_non_spread cx ~trace = function
    | Arg t -> t
    | SpreadArg arr ->
      let reason = reason_of_t arr in
      let loc = loc_of_t arr in
      add_output cx ~trace Error_message.(EUnsupportedSyntax (loc, SpreadArgument));
      AnyT.error reason

  (* Wrapper functions around __flow that manage traces. Use these functions for
     all recursive calls in the implementation of __flow. *)

  (* Call __flow while concatenating traces. Typically this is used in code that
     propagates bounds across type variables, where nothing interesting is going
     on other than concatenating subtraces to make longer traces to describe
     transitive data flows *)
  and join_flow cx ts (t1, t2) =
    let max = Context.max_trace_depth cx in
    __flow cx (t1, t2) (Trace.concat_trace ~max ts)

  (* Call __flow while embedding traces. Typically this is used in code that
     simplifies a constraint to generate subconstraints: the current trace is
     "pushed" when recursing into the subconstraints, so that when we finally hit
     an error and walk back, we can know why the particular constraints that
     caused the immediate error were generated. *)
  and rec_flow cx trace (t1, t2) =
    let max = Context.max_trace_depth cx in
    __flow cx (t1, t2) (Trace.rec_trace ~max t1 t2 trace)

  and rec_flow_t cx trace ~use_op (t1, t2) = rec_flow cx trace (t1, UseT (use_op, t2))

  (* Ideally this function would not be required: either we call `flow` from
     outside without a trace (see below), or we call one of the functions above
     with a trace. However, there are some functions that need to call __flow,
     which are themselves called both from outside and inside (with or without
     traces), so they call this function instead. *)
  and flow_opt cx ?trace (t1, t2) =
    let trace =
      match trace with
      | None -> Trace.unit_trace t1 t2
      | Some trace ->
        let max = Context.max_trace_depth cx in
        Trace.rec_trace ~max t1 t2 trace
    in
    __flow cx (t1, t2) trace

  and flow_opt_t cx ~use_op ?trace (t1, t2) = flow_opt cx ?trace (t1, UseT (use_op, t2))

  (* Externally visible function for subtyping. *)
  (* Calls internal entry point and traps runaway recursion. *)
  and flow cx (lower, upper) =
    try flow_opt cx (lower, upper) with
    | RecursionCheck.LimitExceeded trace ->
      (* log and continue *)
      let rl = reason_of_t lower in
      let ru = reason_of_use_t upper in
      let reasons =
        if is_use upper then
          (ru, rl)
        else
          FlowError.ordered_reasons (rl, ru)
      in
      add_output cx ~trace (Error_message.ERecursionLimit reasons)
    | ex ->
      (* rethrow *)
      raise ex

  and flow_t cx (t1, t2) = flow cx (t1, UseT (unknown_use, t2))

  and flow_p cx ~use_op lreason ureason propref props =
    rec_flow_p cx ~use_op ~report_polarity:true lreason ureason propref props

  (* Wrapper functions around __unify that manage traces. Use these functions for
     all recursive calls in the implementation of __unify. *)
  and rec_unify cx trace ~use_op ?(unify_any = false) t1 t2 =
    let max = Context.max_trace_depth cx in
    __unify cx ~use_op ~unify_any t1 t2 (Trace.rec_trace ~max t1 (UseT (use_op, t2)) trace)

  and unify_opt cx ?trace ~use_op ?(unify_any = false) t1 t2 =
    let trace =
      match trace with
      | None -> Trace.unit_trace t1 (UseT (unknown_use, t2))
      | Some trace ->
        let max = Context.max_trace_depth cx in
        Trace.rec_trace ~max t1 (UseT (unknown_use, t2)) trace
    in
    __unify cx ~use_op ~unify_any t1 t2 trace

  (* Externally visible function for unification. *)
  (* Calls internal entry point and traps runaway recursion. *)
  and unify cx ?(use_op = unknown_use) t1 t2 =
    try unify_opt cx ~use_op ~unify_any:true t1 t2 with
    | RecursionCheck.LimitExceeded trace ->
      (* log and continue *)
      let reasons = FlowError.ordered_reasons (reason_of_t t1, reason_of_t t2) in
      add_output cx ~trace (Error_message.ERecursionLimit reasons)
    | ex ->
      (* rethrow *)
      raise ex

  and continue cx trace t = function
    | Lower (use_op, l) -> rec_flow cx trace (l, UseT (use_op, t))
    | Upper u -> rec_flow cx trace (t, u)

  and continue_repos cx trace reason ?(use_desc = false) t = function
    | Lower (use_op, l) ->
      rec_flow_t cx trace ~use_op (l, reposition_reason cx ~trace reason ~use_desc t)
    | Upper u -> rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc t, u)

  and type_app_variance_check cx trace use_op reason_op reason_tapp targs tparams_loc tparams =
    let minimum_arity = poly_minimum_arity tparams in
    let maximum_arity = Nel.length tparams in
    let reason_arity = mk_reason (RCustom "See type parameters of definition here") tparams_loc in
    if List.length targs > maximum_arity then
      add_output
        cx
        ~trace
        (Error_message.ETooManyTypeArgs (reason_tapp, reason_arity, maximum_arity))
    else
      let (unused_targs, _, _) =
        Nel.fold_left
          (fun (targs, map1, map2) tparam ->
            let { name; default; polarity; reason; _ } = tparam in
            let flow_targs t1 t2 =
              let use_op =
                Frame
                  ( TypeArgCompatibility
                      { name; targ = reason; lower = reason_op; upper = reason_tapp; polarity },
                    use_op
                  )
              in
              match polarity with
              | Polarity.Positive -> rec_flow cx trace (t1, UseT (use_op, t2))
              | Polarity.Negative -> rec_flow cx trace (t2, UseT (use_op, t1))
              | Polarity.Neutral -> rec_unify cx trace ~use_op t1 t2
            in
            match (default, targs) with
            | (None, []) ->
              (* fewer arguments than params but no default *)
              add_output
                cx
                ~trace
                (Error_message.ETooFewTypeArgs (reason_tapp, reason_arity, minimum_arity));
              ([], map1, map2)
            | (Some default, []) ->
              let t1 = subst cx ~use_op map1 default in
              let t2 = subst cx ~use_op map2 default in
              flow_targs t1 t2;
              ([], Subst_name.Map.add name t1 map1, Subst_name.Map.add name t2 map2)
            | (_, (t1, t2) :: targs) ->
              flow_targs t1 t2;
              (targs, Subst_name.Map.add name t1 map1, Subst_name.Map.add name t2 map2))
          (targs, Subst_name.Map.empty, Subst_name.Map.empty)
          tparams
      in
      assert (unused_targs = [])

  and speculative_subtyping_succeeds cx l u =
    match
      SpeculationKit.try_singleton_throw_on_failure
        cx
        Trace.dummy_trace
        (TypeUtil.reason_of_t l)
        ~upper_unresolved:false
        l
        (UseT (unknown_use, u))
    with
    | exception Flow_js_utils.SpeculationSingletonError -> false
    | _ -> true

  (* This utility is expected to be used when we a variable of type [t1] is refined
   * with the use of a type guard function with type `(x: mixed) => x is t2`.
   * Because this kind of refinement is expressed through a PredicateT constraint,
   * t1 is already concretized by the time it reaches this point. Type t2, on the
   * other hand, is not, since it is coming directly from the annotation. This is why
   * we concretize it first, before attempting any comparisons. *)
  and intersect cx t1 t2 =
    let module TSet = Type_filter.TypeTagSet in
    let quick_subtype = TypeUtil.quick_subtype in
    let t1_tags = Type_filter.tag_of_t cx t1 in
    let t2_tags =
      t2
      |> possible_concrete_types_for_inspection cx (TypeUtil.reason_of_t t2)
      |> Base.List.map ~f:(Type_filter.tag_of_t cx)
      |> Base.Option.all
      |> Base.Option.map ~f:(Base.List.fold ~init:TSet.empty ~f:TSet.union)
    in
    let is_any t =
      let ts = possible_concrete_types_for_inspection cx (TypeUtil.reason_of_t t) t in
      List.exists Type.is_any ts
    in
    let is_null t =
      match possible_concrete_types_for_inspection cx (TypeUtil.reason_of_t t) t with
      | [DefT (_, NullT)] -> true
      | _ -> false
    in
    let is_void t =
      match possible_concrete_types_for_inspection cx (TypeUtil.reason_of_t t) t with
      | [DefT (_, VoidT)] -> true
      | _ -> false
    in
    match (t1_tags, t2_tags) with
    | (Some t1_tags, Some t2_tags) when not (Type_filter.tags_overlap t1_tags t2_tags) ->
      let r = update_desc_reason invalidate_rtype_alias (TypeUtil.reason_of_t t1) in
      DefT (r, EmptyT)
    | _ ->
      if is_any t1 then
        t2
      else if is_any t2 then
        (* Filter out null and void types from the input if comparing with any *)
        if is_null t1 || is_void t1 then
          let r = update_desc_reason invalidate_rtype_alias (TypeUtil.reason_of_t t1) in
          DefT (r, EmptyT)
        else
          t1
      else if quick_subtype t1 t2 then
        t1
      else if quick_subtype t2 t1 then
        t2
      else if speculative_subtyping_succeeds cx t1 t2 then
        t1
      else if speculative_subtyping_succeeds cx t2 t1 then
        t2
      else
        let r = update_desc_reason invalidate_rtype_alias (TypeUtil.reason_of_t t1) in
        IntersectionT (r, InterRep.make t2 t1 [])

  (* This utility is expected to be used when negating the refinement of a type [t1]
   * with a type guard `x is t2`. The only case considered here is that of t1 <: t2.
   * This means that the positive branch will always be taken, and so we are left with
   * `empty` in the negated case. *)
  and type_guard_diff cx t1 t2 =
    if TypeUtil.quick_subtype t1 t2 || speculative_subtyping_succeeds cx t1 t2 then
      let r = update_desc_reason invalidate_rtype_alias (TypeUtil.reason_of_t t1) in
      DefT (r, EmptyT)
    else
      t1

  and possible_concrete_types mk_concretization_target cx reason t =
    let id = Tvar.mk_no_wrap cx reason in
    flow cx (t, PreprocessKitT (reason, ConcretizeTypes (mk_concretization_target id)));
    Flow_js_utils.possible_types cx id

  and possible_concrete_types_for_inspection cx t =
    possible_concrete_types (fun ident -> ConcretizeForInspection ident) cx t

  and add_specialized_callee_method_action cx trace l = function
    | CallM { specialized_callee; _ }
    | ChainM { specialized_callee; _ } ->
      CalleeRecorder.add_callee cx CalleeRecorder.All l specialized_callee
    | NoMethodAction prop_t -> rec_flow_t cx ~use_op:unknown_use trace (l, prop_t)

  include CheckPolarity
end

module rec FlowJs : Flow_common.S = struct
  module React = React_kit.Kit (FlowJs)
  module CheckPolarity = Check_polarity.Kit (FlowJs)
  module CustomFun = Custom_fun_kit.Kit (FlowJs)
  module ObjectKit = Object_kit.Kit (FlowJs)
  module SpeculationKit = Speculation_kit.Make (FlowJs)
  module SubtypingKit = Subtyping_kit.Make (FlowJs)
  include
    M__flow (FlowJs) (React) (CheckPolarity) (CustomFun) (ObjectKit) (SpeculationKit) (SubtypingKit)

  let perform_read_prop_action = GetPropTKit.perform_read_prop_action

  let react_subtype_class_component_render = React.subtype_class_component_render

  let react_get_config = React.get_config

  (* Returns a list of concrete types after breaking up unions, maybe types, etc *)

  let singleton_concrete_type_for_inspection cx reason t =
    match possible_concrete_types_for_inspection cx reason t with
    | [] -> EmptyT.make reason
    | [t] -> t
    | t1 :: t2 :: ts -> UnionT (reason, UnionRep.make t1 t2 ts)

  let possible_concrete_types_for_imports_exports =
    possible_concrete_types (fun ident -> ConcretizeForImportsExports ident)

  let possible_concrete_types_for_computed_props =
    possible_concrete_types (fun ident -> ConcretizeComputedPropsT ident)
end

include FlowJs

(* exporting this for convenience *)
let add_output = Flow_js_utils.add_output

(************* end of slab **************************************************)

(* Would rather this live elsewhere, but here because module DAG. *)
let mk_default cx reason =
  Default.fold
    ~expr:(fun t -> t)
    ~cons:(fun t1 t2 ->
      Tvar.mk_where cx reason (fun tvar ->
          flow_t cx (t1, tvar);
          flow_t cx (t2, tvar)
      ))
    ~selector:(fun r t sel ->
      Tvar.mk_no_wrap_where cx r (fun tvar ->
          eval_selector cx ~annot:false r t sel tvar (Reason.mk_id ())
      ))

(* Export some functions without the trace parameter *)

let resolve_id cx id t =
  resolve_id cx Trace.dummy_trace ~use_op:unknown_use ~fully_resolved:true id t

let mk_instance cx ?type_t_kind instance_reason ?use_desc c =
  mk_instance ?type_t_kind cx instance_reason ?use_desc c

let get_builtin_type cx reason ?use_desc x = get_builtin_type cx reason ?use_desc x

let reposition_reason cx reason ?use_desc t = reposition_reason cx reason ?use_desc t

let filter_optional cx reason opt_t = filter_optional cx reason opt_t

let reposition cx loc t = reposition cx loc ?desc:None ?annot_loc:None t

let mk_typeapp_instance_annot cx ~use_op ~reason_op ~reason_tapp ~from_value ?cache c ts =
  mk_typeapp_instance_annot cx ~use_op ~reason_op ~reason_tapp ~from_value ?cache c ts

let mk_type_destructor cx use_op reason t d id =
  mk_type_destructor cx ~trace:Trace.dummy_trace use_op reason t d id

let check_polarity cx tparams polarity t = check_polarity cx tparams polarity t

let add_output cx msg = add_output cx msg
