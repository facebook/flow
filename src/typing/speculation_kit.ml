(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_js_utils
open Instantiation_utils
open Reason
open Type
open TypeUtil
module ALocFuzzyMap = Loc_collections.ALocFuzzyMap

module type INPUT = sig
  include Flow_common.BASE
end

module type OUTPUT = sig
  val try_union :
    Context.t ->
    ?on_success:(unit -> unit) ->
    Type.DepthTrace.t ->
    Type.use_op ->
    Type.t ->
    Reason.reason ->
    Type.UnionRep.t ->
    unit

  val try_intersection :
    Context.t -> Type.DepthTrace.t -> Type.use_t -> Reason.reason -> Type.InterRep.t -> unit

  val try_custom :
    Context.t -> ?use_op:use_op -> no_match_error_loc:ALoc.t -> (unit -> unit) list -> unit

  (**
   * [try_singleton_throw_on_failure cx trace reason t u] runs the constraint
   * between (t, u) in a speculative environment. If an error is raised then a
   * SpeculationSingletonError exception is raised. This needs to be caught by
   * the caller of this function.
   *)
  val try_singleton_throw_on_failure :
    Context.t -> Type.DepthTrace.t -> Type.t -> Type.use_t -> unit
end

module Make (Flow : INPUT) : OUTPUT = struct
  open Flow

  type cases_spec =
    | UnionCases of {
        use_op: use_op;
        reason_op: reason;
        l: Type.t;
        union_rep: UnionRep.t;
        us: Type.t list;
        on_success: (unit -> unit) option;
      }
    | IntersectionCases of {
        intersection_reason: reason;
        ls: Type.t list;
        use_t: Type.use_t;
      }
    | SingletonCase of Type.t * Type.use_t
    | CustomCases of {
        use_op: use_op option;
        no_match_error_loc: ALoc.t;
        cases: (unit -> unit) list;
      }

  let mk_intersection_reason r _ls = replace_desc_reason RIntersection r

  let log_synthesis_result cx _trace case speculation_id =
    let open Speculation_state in
    let { information_for_synthesis_logging; _ } = case in
    match information_for_synthesis_logging with
    | CallInformationForSynthesisLogging { lhs_t; call_callee_hint_ref; _ } ->
      let old_callee_hint = !call_callee_hint_ref in
      let new_callee_hint =
        match old_callee_hint with
        | Speculation_hint_unset ->
          let spec_id_path =
            speculation_id
            :: List.map (fun branch -> branch.speculation_id) !(Context.speculation_state cx)
          in
          Speculation_hint_set (spec_id_path, lhs_t)
        | Speculation_hint_invalid -> Speculation_hint_invalid
        | Speculation_hint_set (old_spec_id_path, old_t) ->
          if List.mem speculation_id old_spec_id_path then
            (* We are moving back a successful speculation path. *)
            old_callee_hint
          else if lhs_t == old_t then
            (* We are in a different branch, but the outcome is the same, so keep it. *)
            old_callee_hint
          else
            Speculation_hint_invalid
      in
      call_callee_hint_ref := new_callee_hint
    | NoInformationForSynthesisLogging -> ()

  let rec log_specialized_use cx use case speculation_id =
    match use with
    | CallT { call_action = Funcalltype { call_specialized_callee = Some c; _ }; _ }
    | MethodT
        ( _,
          _,
          _,
          _,
          (CallM { specialized_callee = Some c; _ } | ChainM { specialized_callee = Some c; _ })
        )
    | ReactKitT (_, _, React.CreateElement { specialized_component = Some c; _ }) ->
      let (Specialized_callee data) = c in
      let spec_id = (speculation_id, case.Speculation_state.case_id) in
      Base.List.find data.speculative_candidates ~f:(fun (_, spec_id') -> spec_id = spec_id')
      |> Base.Option.iter ~f:(fun (l, _) -> data.finalized <- l :: data.finalized)
    | OptionalChainT { t_out; _ } -> log_specialized_use cx t_out case speculation_id
    | _ -> ()

  let log_specialized_callee cx spec case speculation_id =
    match spec with
    | IntersectionCases { use_t; _ } -> log_specialized_use cx use_t case speculation_id
    | _ -> ()

  type case_spec =
    | CustomCase of (unit -> unit)
    | FlowCase of Type.t * Type.use_t

  (** Entry points into the process of trying different branches of union and
      intersection types.

     The problem we're trying to solve here is common to checking unions and
     intersections: how do we make a choice between alternatives, when we want
     to avoid regret (i.e., by not committing to an alternative that might not
     work out, when alternatives that were not considered could have worked out)?

     To appreciate the problem, consider what happens without choice. Partial
     information is not a problem: we emit constraints that must be satisfied for
     something to work, and either those constraints fail (indicating a problem)
     or they don't fail (indicating no problem). With choice we cannot naively
     emit constraints as we try alternatives *without also having a mechanism to
     roll back those constraints*. This is because those constraints don't *have*
     to be satisfied; some other alternative may end up not needing those
     constraints to be satisfied for things to work out!

     It is not too hard to imagine scary scenarios we can get into without a
     roll-back mechanism. (These scenarios are not theoretical, by the way: with a
     previous implementation of union and intersection types that didn't
     anticipate these scenarios, they consistently caused a lot of problems in
     real-world use cases.)

     * One bad state we can get into is where, when trying an alternative, we emit
     constraints hoping they would be satisfied, and they appear to work. So we
     commit to that particular alternative. Then much later find out that those
     constraints are unsatified, at which point we have lost the ability to try
     other alternatives that could have worked. This leads to a class of bugs
     where a union or intersection type contains cases that should have worked,
     but they don't.

     * An even worse state we can get into is where we do discover that an
     alternative won't work out while we're still in a position of choosing
     another alternative, but in the process of making that discovery we emit
     constraints that linger on in a ghost-like state. Meanwhile, we pick another
     alternative, it works out, and we move on. Except that much later the ghost
     constraints become unsatisfied, leading to much confusion on the source of
     the resulting errors. This leads to a class of bugs where we get spurious
     errors even when a union or intersection type seems to have worked.

     So, we just implement roll-back, right? Basically...yes. But rolling back
     constraints is really hard in the current implementation. Instead, we try to
     avoid processing constraints that have side effects as much as possible while
     trying alternatives: by ensuring that the constraints that have side effects
     get deferred, instead of being processed immediately, until a choice can be
     made, thereby not participating in the choice-making process.

     But not all types can be fully resolved. In particular, while union and
     intersection types themselves can be fully resolved, the lower and upper
     bounds we check them against could have still-to-be-inferred types in
     them. How do we ensure that for the potentially side-effectful constraints we
     do emit on these types, we avoid undue side effects? By explicitly marking
     these types as unresolved, and deferring the execution of constraints that
     involved such marked types until a choice can be made. The details of this
     process is described in Speculation.
  *)

  (* Every choice-making process on a union or intersection type is assigned a
     unique identifier, called the speculation_id. This identifier keeps track of
     unresolved tvars encountered when trying to fully resolve types. *)
  let rec try_union cx ?on_success trace use_op l reason_op rep =
    let ts = UnionRep.members rep in
    speculative_matches
      cx
      trace
      (UnionCases { use_op; reason_op; l; union_rep = rep; us = ts; on_success })

  and try_intersection cx trace use_t intersection_reason rep =
    let ls = InterRep.members rep in
    speculative_matches cx trace (IntersectionCases { intersection_reason; ls; use_t })

  and try_custom cx ?use_op ~no_match_error_loc cases =
    speculative_matches cx DepthTrace.dummy_trace (CustomCases { use_op; no_match_error_loc; cases })

  and try_singleton_throw_on_failure cx trace t u =
    speculative_matches cx trace (SingletonCase (t, u))

  (************************)
  (* Speculative matching *)
  (************************)

  (* Speculatively match a pair of types, returning whether some error was
     encountered or not. Speculative matching happens in the context of a
     particular "branch": this context controls how some constraints emitted
     during the matching might be processed. See comments in Speculation for
     details on branches. See also speculative_matches, which calls this function
     iteratively and processes its results. *)
  and speculative_match cx branch f =
    let typeapp_stack = TypeAppExpansion.get cx in
    let constraint_cache_ref = Context.constraint_cache cx in
    let constraint_cache = !constraint_cache_ref in
    Speculation.set_speculative cx branch;
    let restore () =
      Speculation.restore_speculative cx;
      constraint_cache_ref := constraint_cache;
      TypeAppExpansion.set cx typeapp_stack
    in
    try
      f ();
      restore ();
      None
    with
    | SpeculativeError err ->
      restore ();
      Some err
    | exn ->
      let exn = Exception.wrap exn in
      restore ();
      Exception.reraise exn

  (* Speculatively match several alternatives in turn, as presented when checking
     a union or intersection type. This process can terminate in various ways:

     (1) One of the alternatives definitely succeeds. This is straightforward: we
     can safely discard any later alternatives.

     (2) All alternatives fail. This is also straightforward: we emit an
     appropriate error message.

     See Speculation for more details on terminology and low-level mechanisms used
     here, including what bits of information are carried by case.

     Because this process is common to checking union and intersection types, we
     abstract the latter into a so-called "spec." The spec is used to customize
     error messages.
  *)
  and speculative_matches cx trace spec =
    (* explore optimization opportunities *)
    if optimize_spec_try_shortcut cx trace spec then
      ()
    else
      long_path_speculative_matches cx trace spec

  and long_path_speculative_matches cx trace spec =
    let open Speculation_state in
    let speculation_id = mk_id () in
    (* extract stuff to ignore while considering actions *)
    (* split spec into a list of pairs of types to try speculative matching on *)
    let trials = trials_of_spec spec in
    (* Here errs records all errors we have seen up to this point. *)
    let rec loop errs = function
      | [] -> return errs
      | (case_id, case_spec) :: trials ->
        let information_for_synthesis_logging =
          match case_spec with
          | FlowCase
              ( lhs_t,
                CallT
                  {
                    call_action =
                      Funcalltype { call_speculation_hint_state = Some call_callee_hint_ref; _ };
                    _;
                  }
              ) ->
            CallInformationForSynthesisLogging { lhs_t; call_callee_hint_ref }
          | FlowCase _
          | CustomCase _ ->
            NoInformationForSynthesisLogging
        in
        let case = { case_id; errors = []; information_for_synthesis_logging } in
        (* speculatively match the pair of types in this trial *)
        let error =
          speculative_match cx { speculation_id; case } (fun () ->
              match case_spec with
              | FlowCase (l, u) -> rec_flow cx trace (l, u)
              | CustomCase f -> f ()
          )
        in
        (match error with
        | None -> begin
          (* no error, looking great so far... *)
          fire_actions cx trace spec case speculation_id
        end
        | Some err -> begin
          (* if an error is found, then throw away this alternative... *)
          (* ...adding to the error list if no promising alternative has been
           * found yet *)
          loop (err :: errs) trials
        end)
    and return msgs =
      (* everything failed; make a really detailed error message listing out the
       * error found for each alternative *)
      (* Add the error. *)
      match spec with
      | UnionCases { use_op; reason_op = r; l; union_rep = _; us; _ } ->
        let reason = reason_of_t l in
        assert (List.length us = List.length msgs);
        add_output
          cx
          (Error_message.EUnionSpeculationFailed
             { use_op; reason; op_reasons = (r, List.map reason_of_t us); branches = msgs }
          )
      | SingletonCase _ -> raise SpeculationSingletonError
      | CustomCases { use_op; no_match_error_loc; cases } ->
        assert (List.length cases = List.length msgs);
        add_output
          cx
          (Error_message.EIncompatibleSpeculation
             { use_op; loc = no_match_error_loc; branches = msgs }
          )
      | IntersectionCases { intersection_reason = r; ls; use_t = upper } ->
        let err =
          let reason_lower = mk_intersection_reason r ls in
          Default_resolve.default_resolve_touts
            ~flow:(flow_t cx)
            ~resolve_callee:(r, ls)
            cx
            (loc_of_reason reason_lower)
            upper;
          assert (List.length ls = List.length msgs);
          match upper with
          | UseT (use_op, t) ->
            Error_message.EIncompatibleDefs
              { use_op; reason_lower; reason_upper = reason_of_t t; branches = msgs }
          | LookupT { reason; lookup_action = MatchProp { use_op; _ }; _ } ->
            Error_message.EUnionSpeculationFailed
              { use_op; reason; op_reasons = (r, List.map reason_of_t ls); branches = msgs }
          | _ ->
            Error_message.EIncompatibleSpeculation
              {
                use_op = use_op_of_use_t upper;
                loc = upper |> reason_of_use_t |> loc_of_reason;
                branches = msgs;
              }
        in
        add_output cx err
    in
    loop [] trials

  and trials_of_spec = function
    | UnionCases { use_op; reason_op = _; l; union_rep = _; us; on_success = _ } ->
      (* NB: Even though we know the use_op for the original constraint, don't
         embed it in the nested constraints to avoid unnecessary verbosity. We
         will unwrap the original use_op once in EUnionSpeculationFailed. *)
      Base.List.mapi ~f:(fun i u -> (i, FlowCase (l, UseT (Op (Speculation use_op), u)))) us
    | IntersectionCases { intersection_reason = _; ls; use_t = u } ->
      Base.List.mapi
        ~f:(fun i l ->
          (i, FlowCase (l, mod_use_op_of_use_t (fun use_op -> Op (Speculation use_op)) u)))
        ls
    | SingletonCase (l, u) ->
      [(0, FlowCase (l, mod_use_op_of_use_t (fun use_op -> Op (Speculation use_op)) u))]
    | CustomCases { use_op = _; no_match_error_loc = _; cases } ->
      Base.List.mapi cases ~f:(fun i f -> (i, CustomCase f))

  (* spec optimization *)
  (* Currently, the only optimizations we do are for enums and for disjoint unions.

     When a literal type is checked against a union of literal types, we hope the union is an enum and
     try to optimize the representation of the union as such. We also try to use our optimization to
     do a quick membership check, potentially avoiding the speculative matching process altogether.

     When an object type is checked against an union of object types, we hope the union is a disjoint
     union and try to guess and record sentinel properties across object types in the union. Later,
     during speculative matching, by checking sentinel properties first we force immediate match
     failures in the vast majority of cases without having to do any useless additional work.
  *)
  and optimize_spec_try_shortcut cx trace = function
    | UnionCases
        {
          use_op = _;
          reason_op = _;
          l = OpaqueT (reason, { opaque_id = Opaque.InternalEnforceUnionOptimized; _ });
          union_rep = rep;
          us = _;
          on_success;
        } ->
      let specialization =
        UnionRep.optimize_
          rep
          ~reason_of_t:TypeUtil.reason_of_t
          ~reasonless_eq:(Concrete_type_eq.eq cx)
          ~flatten:(Type_mapper.union_flatten cx)
          ~find_resolved:(Context.find_resolved cx)
          ~find_props:(Context.find_props cx)
      in
      begin
        match specialization with
        | Error kind ->
          add_output cx (Error_message.EUnionOptimization { loc = loc_of_reason reason; kind })
        | Ok
            ( UnionRep.AlmostDisjointUnionWithPossiblyNonUniqueKeys map
            | UnionRep.PartiallyOptimizedAlmostDisjointUnionWithPossiblyNonUniqueKeys map ) ->
          let non_unique_keys =
            map
            |> NameUtils.Map.map (fun map ->
                   map
                   |> UnionRep.UnionEnumMap.filter (fun _ (_, ts) -> ts <> [])
                   |> UnionRep.UnionEnumMap.map (Nel.map TypeUtil.reason_of_t)
               )
            |> NameUtils.Map.filter (fun _ map -> not (UnionRep.UnionEnumMap.is_empty map))
          in
          if not (NameUtils.Map.is_empty non_unique_keys) then
            add_output
              cx
              (Error_message.EUnionPartialOptimizationNonUniqueKey
                 { loc = loc_of_reason reason; non_unique_keys }
              )
        | Ok _ -> ()
      end;
      Base.Option.iter on_success ~f:(fun f -> f ());
      true
    | UnionCases { use_op; reason_op; l; union_rep = rep; us = _; on_success } ->
      if not (UnionRep.is_optimized_finally rep) then
        UnionRep.optimize
          rep
          ~reason_of_t:TypeUtil.reason_of_t
          ~reasonless_eq:(Concrete_type_eq.eq cx)
          ~flatten:(Type_mapper.union_flatten cx)
          ~find_resolved:(Context.find_resolved cx)
          ~find_props:(Context.find_props cx);
      let result =
        match l with
        | DefT
            ( _,
              ( SingletonStrT _ | SingletonNumT _ | SingletonBoolT _ | SingletonBigIntT _ | VoidT
              | NullT )
            ) ->
          shortcut_enum cx trace reason_op use_op l rep
        (* Types that are definitely incompatible with enums, after the above case. *)
        | DefT
            ( _,
              ( NumGeneralT _ | BigIntGeneralT _ | StrGeneralT _ | MixedT _ | SymbolT | FunT _
              | ObjT _ | ArrT _ | ClassT _ | InstanceT _ | TypeT _ | PolyT _
              | ReactAbstractComponentT _ | EnumValueT _ | EnumObjectT _ )
            )
          when Base.Option.is_some (UnionRep.check_enum rep) ->
          add_output
            cx
            (Error_message.EIncompatibleWithUseOp
               {
                 reason_lower = TypeUtil.reason_of_t l;
                 reason_upper = reason_op;
                 use_op;
                 explanation = None;
               }
            );

          true
        | DefT (_, ObjT _) -> shortcut_disjoint_union cx trace reason_op use_op l rep
        | _ -> false
      in
      if result then Base.Option.iter on_success ~f:(fun f -> f ());
      result
    | IntersectionCases _
    | CustomCases _
    | SingletonCase _ ->
      false

  and shortcut_enum cx trace reason_op use_op l rep =
    let quick_subtype = TypeUtil.quick_subtype in
    quick_mem_result cx trace reason_op use_op l @@ UnionRep.quick_mem_enum ~quick_subtype l rep

  and shortcut_disjoint_union cx trace reason_op use_op l rep =
    let quick_subtype = TypeUtil.quick_subtype in
    quick_mem_result cx trace reason_op use_op l
    @@ UnionRep.quick_mem_disjoint_union
         ~quick_subtype
         l
         rep
         ~find_resolved:(Context.find_resolved cx)
         ~find_props:(Context.find_props cx)

  and quick_mem_result cx trace reason_op use_op l = function
    | UnionRep.Yes ->
      (* membership check succeeded *)
      true
    (* Our work here is done, so no need to continue. *)
    | UnionRep.No ->
      (* membership check failed *)
      rec_flow cx trace (l, UseT (use_op, DefT (reason_op, EmptyT)));
      true
    (* Our work here is done, so no need to continue. *)
    | UnionRep.Conditional t ->
      (* conditional match *)
      rec_flow cx trace (l, UseT (use_op, t));
      true (* Our work here is done, so no need to continue. *)
    | UnionRep.Unknown ->
      (* membership check was inconclusive *)
      false

  and fire_actions cx trace spec case speculation_id =
    log_synthesis_result cx trace case speculation_id;
    log_specialized_callee cx spec case speculation_id;
    List.iter (add_output cx) case.Speculation_state.errors
end
