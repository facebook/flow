(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_js_utils
open Instantiation_utils
open Reason
open Type
open TypeUtil
open Utils_js

module type INPUT = sig
  include Flow_common.BASE
end

module type OUTPUT = sig
  val try_union :
    Context.t -> Type.trace -> Type.use_op -> Type.t -> Reason.reason -> Type.UnionRep.t -> unit

  val try_intersection :
    Context.t -> Type.trace -> Type.use_t -> Reason.reason -> Type.InterRep.t -> unit

  val prep_try_intersection :
    Context.t ->
    Type.trace ->
    reason ->
    Type.t list ->
    Type.t list ->
    Type.use_t ->
    Reason.reason ->
    Type.InterRep.t ->
    unit

  val fully_resolve_type :
    Context.t -> Type.trace -> reason -> Graph_explorer.Tbl.key -> Type.t -> unit

  val speculative_matches :
    Context.t -> Type.trace -> ALoc.t Reason.virtual_reason -> int -> Type.spec -> unit

  val intersection_preprocess_kit : reason -> Type.intersection_preprocess_tool -> Type.use_t
end

module Make (Flow : INPUT) : OUTPUT = struct
  open Flow

  let mk_union_reason r us =
    List.fold_left
      (fun reason t ->
        let rdesc = string_of_desc (desc_of_reason ~unwrap:false reason) in
        let tdesc = string_of_desc (desc_of_reason ~unwrap:false (reason_of_t t)) in
        let udesc =
          if not (String_utils.string_starts_with rdesc "union:") then
            spf "union: %s" tdesc
          else if String_utils.string_ends_with rdesc "..." then
            rdesc
          else if String_utils.string_ends_with rdesc (tdesc ^ "(s)") then
            rdesc
          else if String.length rdesc >= 256 then
            spf "%s | ..." rdesc
          else if String_utils.string_ends_with rdesc tdesc then
            spf "%s(s)" rdesc
          else
            spf "%s | %s" rdesc tdesc
        in
        replace_desc_reason (RCustom udesc) reason)
      r
      us

  let mk_intersection_reason r _ls = replace_desc_reason RIntersection r

  (** Entry points into the process of trying different branches of union and
      intersection types.

     The problem we're trying to solve here is common to checking unions and
     intersections: how do we make a choice between alternatives, when (i) we have
     only partial information (i.e., while we're in the middle of type inference)
     and when (ii) we want to avoid regret (i.e., by not committing to an
     alternative that might not work out, when alternatives that were not
     considered could have worked out)?

     To appreciate the problem, consider what happens without choice. Partial
     information is not a problem: we emit constraints that must be satisfied for
     something to work, and either those constraints fail (indicating a problem)
     or they don't fail (indicating no problem). With choice and partial
     information, we cannot naively emit constraints as we try alternatives
     *without also having a mechanism to roll back those constraints*. This is
     because those constraints don't *have* to be satisfied; some other
     alternative may end up not needing those constraints to be satisfied for
     things to work out!

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
     trying alternatives: by ensuring that (1) we don't (need to) emit too many
     constraints that have side effects (2) those that we do emit get deferred,
     instead of being processed immediately, until a choice can be made, thereby
     not participating in the choice-making process.

     (1) How do we ensure we don't emit too many constraints that have side
     effects? By fully resolving types before they participate in the
     choice-making process. Basically, we want to have as much information as we
     can before trying alternatives. It is a nice property of our implementation
     that once types are resolved, constraints emitted against them don't have
     (serious) side effects: they get simplified and simplified until we either
     hit success or failure. The details of this process is described in
     ResolvableTypeJob and in resolve_bindings.

     (2) But not all types can be fully resolved. In particular, while union and
     intersection types themselves can be fully resolved, the lower and upper
     bounds we check them against could have still-to-be-inferred types in
     them. How do we ensure that for the potentially side-effectful constraints we
     do emit on these types, we avoid undue side effects? By explicitly marking
     these types as unresolved, and deferring the execution of constraints that
     involved such marked types until a choice can be made. The details of this
     process is described in Speculation.

     There is a necessary trade-off in the approach. In particular, (2) means that
     sometimes choices cannot be made: it is ambiguous which constraints should be
     executed when trying different alternatives. We detect such ambiguities
     (conservatively, but only when a best-effort choice-making strategy doesn't
     work), and ask for additional annotations to disambiguate the relevant
     alternatives. A particularly nice property of this approach is that it is
     complete: with enough annotations it is always possible to make a
     choice. Another "meta-feature" of this approach is that it leaves room for
     incremental improvement: e.g., we would need fewer additional annotations as
     we improve our inference algorithm to detect cases where more unresolved
     tvars can be fully resolved ahead of time (in other words, detect when they
     have the "0->1" property, discussed elsewhere, roughly meaning they are
     determined by annotations).
  *)

  (* Every choice-making process on a union or intersection type is assigned a
     unique identifier, called the speculation_id. This identifier keeps track of
     unresolved tvars encountered when trying to fully resolve types. *)
  let rec try_union cx trace use_op l reason rep =
    let ts = UnionRep.members rep in
    let speculation_id = mk_id () in
    Speculation.init_speculation cx speculation_id;

    (* collect parts of the union type to be fully resolved *)
    let imap =
      (* since any final optimization must have happened after full resolution *)
      if UnionRep.is_optimized_finally rep then
        IMap.empty
      else
        ResolvableTypeJob.collect_of_types cx IMap.empty ts
    in
    (* collect parts of the lower bound to be fully resolved, while logging
       unresolved tvars *)
    let imap = ResolvableTypeJob.collect_of_type ~log_unresolved:speculation_id cx imap l in
    (* fully resolve the collected types *)
    resolve_bindings_init cx trace reason (bindings_of_jobs cx trace imap)
    @@ (* ...and then begin the choice-making process *)
    try_flow_continuation cx trace reason speculation_id (UnionCases (use_op, l, rep, ts))

  and try_intersection cx trace u reason rep =
    let ts = InterRep.members rep in
    let speculation_id = mk_id () in
    Speculation.init_speculation cx speculation_id;

    (* collect parts of the intersection type to be fully resolved *)
    let imap = ResolvableTypeJob.collect_of_types cx IMap.empty ts in
    (* collect parts of the upper bound to be fully resolved, while logging
       unresolved tvars *)
    let imap = ResolvableTypeJob.collect_of_use ~log_unresolved:speculation_id cx imap u in
    (* fully resolve the collected types *)
    resolve_bindings_init cx trace reason (bindings_of_jobs cx trace imap)
    @@ (* ...and then begin the choice-making process *)
    try_flow_continuation cx trace reason speculation_id (IntersectionCases (ts, u))
    (* Preprocessing for intersection types.

       Before feeding into the choice-making machinery described above, we
       preprocess upper bounds of intersection types. This preprocessing seems
       asymmetric, but paradoxically, it is not: the purpose of the preprocessing is
       to bring choice-making on intersections to parity with choice-making on
       unions.

       Consider what happens when a lower bound is checked against a union type. The
       lower bound is always concretized before a choice is made! In other words,
       even if we emit a flow from an unresolved tvar to a union type, the
       constraint fires only when the unresolved tvar has been concretized.

       Now, consider checking an intersection type with an upper bound. As an
       artifact of how tvars and concrete types are processed, the upper bound would
       appear to be concrete even though the actual parts of the upper bound that
       are involved in the choice-making may be unresolved! (These parts are the
       top-level input positions in the upper bound, which end up choosing between
       the top-level input positions in the members of the intersection type.) If we
       did not concretize the parts of the upper bound involved in choice-making, we
       would start the choice-making process at a disadvantage (compared to
       choice-making with a union type and an already concretized lower
       bound). Thus, we do an extra preprocessing step where we collect the parts of
       the upper bound to be concretized, and for each combination of concrete types
       for those parts, call the choice-making process.
    *)

  (** The following function concretizes each tvar in unresolved in turn,
      recording their corresponding concrete lower bounds in resolved as it
      goes. At each step, it emits a ConcretizeTypes constraint on an unresolved
      tvar, which in turn calls into this function when a concrete lower bound
      appears on that tvar. **)
  and prep_try_intersection cx trace reason unresolved resolved u r rep =
    match unresolved with
    | [] -> try_intersection cx trace (replace_parts cx resolved u) r rep
    | tvar :: unresolved ->
      rec_flow
        cx
        trace
        ( tvar,
          intersection_preprocess_kit
            reason
            (ConcretizeTypes (unresolved, resolved, IntersectionT (r, rep), u))
        )

  (************************)
  (* Full type resolution *)
  (************************)

  (* Here we continue where we left off at ResolvableTypeJob. Once we have
     collected a set of type resolution jobs, we create so-called bindings from
     these jobs. A binding is a (id, tvar) pair, where tvar is what needs to be
     resolved, and id is an identifier that serves as an index for that job.

     We don't try to fully resolve unresolved tvars that are not annotation
     sources or heads of type applications, since in general they don't satify the
     0->1 property. Instead:

     (1) When we're expecting them, e.g., when we're looking at inferred types, we
     mark them so that we can recognize them later, during speculative matching.

     (2) When we're not expecting them, e.g., when we're fully resolving union /
     intersection type annotations, we unify them as `any`. Ideally we wouldn't be
     worrying about this case, but who knows what cruft we might have accumulated
     on annotation types, so just getting that cruft out of the way.

     These decisions were made in ResolvableTypeJob.collect_of_types and are
     reflected in the use (or not) of OpenUnresolved (see below).
  *)
  and bindings_of_jobs cx trace jobs =
    IMap.fold
      ResolvableTypeJob.(
        fun id job bindings ->
          match job with
          | OpenResolved -> bindings
          | Binding tvar -> (id, tvar) :: bindings
          | OpenUnresolved (log_unresolved, reason, id) ->
            begin
              match log_unresolved with
              | Some speculation_id ->
                Speculation.add_unresolved_to_speculation cx speculation_id id
              | None ->
                Unsoundness.unresolved_any reason |> resolve_id cx trace ~use_op:unknown_use id
            end;
            bindings
      )
      jobs
      []

  (* Entry point into full type resolution. Create an identifier for the goal
     tvar, and call the general full type resolution function below. *)
  and resolve_bindings_init cx trace reason bindings done_tvar =
    let id = create_goal cx done_tvar in
    resolve_bindings cx trace reason id bindings

  and create_goal cx tvar =
    let i = mk_id () in
    Graph_explorer.node (Context.type_graph cx) i;
    Context.set_goals cx (IMap.add i tvar (Context.goals cx));
    i

  (* Let id be the identifier associated with a tvar that is not yet
     resolved. (Here, resolved/unresolved refer to the state of the tvar in the
     context graph: does it point to Resolved _ or Unresolved _?) As soon as the
     tvar is resolved to some type, we generate some bindings by walking that
     type. Full type resolution at id now depends on full resolution of the
     ids/tvars in those bindings. The following function ensures that those
     dependencies are recorded and processed.

     Dependency management happens in Graph_explorer, using efficient data
     structures discussed therein. All we need to do here is to connect id to
     bindings in that graph, while taking care that (1) the conditions of adding
     edges to the graph are satisfied, and (2) cleaning up the effects of adding
     those edges to the graph. Finally (3) we request full type resolution of the
     bindings themselves.

     For (1), note that the graph only retains transitively closed dependencies
     from one kind of tvars to another kind of tvars. The former kind includes
     tvars that are resolved but not yet fully resolved. The latter kind includes
     tvars that are not yet resolved. Thus, in particular we must filter out
     bindings that correspond to fully resolved tvars (see
     is_unfinished_target). On the other hand, the fully_resolve_type function
     below already ensures that id is not yet fully resolved (via
     is_unexplored_source).

     For (2), after adding edges we might discover that some tvars are now fully
     resolved: this happens when, e.g., no new transitively closed dependencies
     get added on id, and full type resolution of some tvars depended only on id.
     If any of these fully resolved tvars were goal tvars, we trigger them.

     For (3) we emit a ResolveType constraint for each binding; when the
     corresponding tvar is resolved, the function fully_resolve_type below is
     called, which in turn calls back into this function (thus closing the
     recursive loop).
  *)
  and resolve_bindings cx trace reason id bindings =
    let bindings = filter_bindings cx bindings in
    let fully_resolve_ids = connect_id_to_bindings cx id bindings in
    ISet.iter
      (fun id ->
        match IMap.find_opt id (Context.goals cx) with
        | None -> ()
        | Some tvar -> trigger cx trace reason tvar)
      fully_resolve_ids;
    List.iter (resolve_binding cx trace reason) bindings

  and fully_resolve_type cx trace reason id t =
    if is_unexplored_source cx id then
      let imap = ResolvableTypeJob.collect_of_type cx IMap.empty t in
      let bindings = bindings_of_jobs cx trace imap in
      (* NOTE: bindings_of_jobs might change the state of id because it resolves it, so check
         again. TODO: there must be a better way *)
      if is_unexplored_source cx id then resolve_bindings cx trace reason id bindings

  and filter_bindings cx = List.filter (fun (id, _) -> is_unfinished_target cx id)

  and connect_id_to_bindings cx id bindings =
    let (ids, _) = List.split bindings in
    Graph_explorer.edges (Context.type_graph cx) (id, ids)

  (* Sanity conditions on source and target before adding edges to the
     graph. Nodes are in one of three states, described in Graph_explorer:
     Not_found (corresponding to unresolved tvars), Found _ (corresponding to
     resolved but not yet fully resolved tvars), and Finished (corresponding to
     fully resolved tvars). *)
  and is_unexplored_source cx id =
    match Graph_explorer.stat_graph id (Context.type_graph cx) with
    | Graph_explorer.Finished -> false
    | Graph_explorer.Node_not_found -> false
    | Graph_explorer.Found node -> Graph_explorer.is_unexplored_node node

  and is_unfinished_target cx id =
    let type_graph = Context.type_graph cx in
    match Graph_explorer.stat_graph id type_graph with
    | Graph_explorer.Finished -> false
    | Graph_explorer.Node_not_found ->
      Graph_explorer.node type_graph id;
      true
    | Graph_explorer.Found node -> not (Graph_explorer.is_finished_node node)

  (** utils for creating toolkit types **)

  and choice_kit reason k = InternalT (ChoiceKitT (reason, k))

  and choice_kit_use reason k = ChoiceKitUseT (reason, k)

  and intersection_preprocess_kit reason k = IntersectionPreprocessKitT (reason, k)

  (** utils for emitting toolkit constraints **)

  and trigger cx trace reason done_tvar =
    rec_flow cx trace (choice_kit reason Trigger, UseT (unknown_use, done_tvar))

  and try_flow_continuation cx trace reason speculation_id spec =
    let u = choice_kit_use reason (TryFlow (speculation_id, spec)) in
    let reason = reason_of_use_t u in
    Tvar.mk_where cx reason (fun tvar -> flow_opt cx ~trace (tvar, u))

  and resolve_binding cx trace reason (id, tvar) =
    rec_flow cx trace (OpenT tvar, choice_kit_use reason (FullyResolveType id))

  (************************)
  (* Speculative matching *)
  (************************)

  (* Speculatively match a pair of types, returning whether some error was
     encountered or not. Speculative matching happens in the context of a
     particular "branch": this context controls how some constraints emitted
     during the matching might be processed. See comments in Speculation for
     details on branches. See also speculative_matches, which calls this function
     iteratively and processes its results. *)
  and speculative_match cx trace branch l u =
    let typeapp_stack = TypeAppExpansion.get () in
    let constraint_cache_ref = Context.constraint_cache cx in
    let constraint_cache = !constraint_cache_ref in
    Speculation.set_speculative cx branch;
    let restore () =
      Speculation.restore_speculative cx;
      constraint_cache_ref := constraint_cache;
      TypeAppExpansion.set typeapp_stack
    in
    try
      rec_flow cx trace (l, u);
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
     a union or intersection type. This process maintains a so-called "match
     state" that describes the best possible choice found so far, and can
     terminate in various ways:

     (1) One of the alternatives definitely succeeds. This is straightforward: we
     can safely discard any later alternatives.

     (2) All alternatives fail. This is also straightforward: we emit an
     appropriate error message.

     (3) One of the alternatives looks promising (i.e., it doesn't immediately
     fail, but it doesn't immediately succeed either: some potentially
     side-effectful constraints, called actions, were emitted while trying the
     alternative, whose execution has been deferred), and all the later
     alternatives fail. In this scenario, we pick the promising alternative, and
     then fire the deferred actions. This is fine, because the choice cannot cause
     regret: the chosen alternative was the only one that had any chance of
     succeeding.

     (4) Multiple alternatives look promising, but the set of deferred actions
     emitted while trying the first of those alternatives form a subset of those
     emitted by later trials. Here we pick the first promising alternative (and
     fire the deferred actions). The reason this is fine is similar to (3): once
     again, the choice cannot cause any regret, because if it failed, then the
     later alternatives would have failed too. So the chosen alternative had the
     best chance of succeeding.

     (5) But sometimes, multiple alternatives look promising and we really can't
     decide which is best. This happens when the set of deferred actions emitted
     by them are incomparable, or later trials have more chances of succeeding
     than previous trials. Such scenarios typically point to real ambiguities, and
     so we ask for additional annotations on unresolved tvars to disambiguate.

     See Speculation for more details on terminology and low-level mechanisms used
     here, including what bits of information are carried by match_state and case,
     how actions are deferred and diff'd, etc.

     Because this process is common to checking union and intersection types, we
     abstract the latter into a so-called "spec." The spec is used to customize
     error messages and to ignore unresolved tvars that are deemed irrelevant to
     choice-making.
  *)
  and speculative_matches cx trace r speculation_id spec =
    (* explore optimization opportunities *)
    if optimize_spec_try_shortcut cx trace r spec then
      ()
    else
      long_path_speculative_matches cx trace r speculation_id spec

  and long_path_speculative_matches cx trace r speculation_id spec =
    let open Speculation_state in
    (* extract stuff to ignore while considering actions *)
    let ignore = ignore_of_spec spec in
    (* split spec into a list of pairs of types to try speculative matching on *)
    let trials = trials_of_spec spec in
    (* Here match_state can take on various values:
     * (a) (NoMatch errs) indicates that everything has failed up to this point,
     *   with errors recorded in errs. Note that the initial value of acc is
     *   Some (NoMatch []).
     * (b) (ConditionalMatch case) indicates the a promising alternative has
     *    been found, but not chosen yet.
     *)
    let rec loop match_state = function
      | [] -> return match_state
      | (case_id, case_r, l, u) :: trials ->
        let case = { case_id; unresolved = ISet.empty; actions = [] } in
        (* speculatively match the pair of types in this trial *)
        let error = speculative_match cx trace { ignore; speculation_id; case } l u in
        (match error with
        | None ->
          (* no error, looking great so far... *)
          begin
            match match_state with
            | NoMatch _ ->
              (* everything had failed up to this point. so no ambiguity yet... *)
              if
                ISet.is_empty case.unresolved
                (* ...and no unresolved tvars encountered during the speculative
                 * match! This is great news. It means that this alternative will
                 * definitely succeed. Fire any deferred actions and short-cut. *)
              then
                fire_actions cx trace spec case.actions
              (* Otherwise, record that we've found a promising alternative. *)
              else
                loop (ConditionalMatch case) trials
            | ConditionalMatch prev_case ->
              (* umm, there's another previously found promising alternative *)
              (* so compute the difference in side effects between that alternative
               * and this *)
              let ts = Speculation.case_diff cx prev_case case in
              (* if the side effects of the previously found promising alternative
               * are fewer, then keep holding on to that alternative *)
              if ts = [] then
                loop match_state trials
              (* otherwise, we have an ambiguity; blame the unresolved tvars and
               * short-cut *)
              else
                let prev_case_id = prev_case.case_id in
                let cases : Type.t list = choices_of_spec spec in
                blame_unresolved cx trace prev_case_id case_id cases case_r ts
          end
        | Some err ->
          (* if an error is found, then throw away this alternative... *)
          begin
            match match_state with
            | NoMatch errs ->
              (* ...adding to the error list if no promising alternative has been
               * found yet *)
              loop (NoMatch (err :: errs)) trials
            | _ -> loop match_state trials
          end)
    and return = function
      | ConditionalMatch case ->
        (* best choice that survived, congrats! fire deferred actions  *)
        fire_actions cx trace spec case.actions
      | NoMatch msgs ->
        (* everything failed; make a really detailed error message listing out the
         * error found for each alternative *)
        let ts = choices_of_spec spec in
        assert (List.length ts = List.length msgs);
        let branches =
          Base.List.mapi
            ~f:(fun i msg ->
              let reason = reason_of_t (List.nth ts i) in
              (reason, msg))
            msgs
        in
        (* Add the error. *)
        begin
          match spec with
          | UnionCases (use_op, l, _rep, us) ->
            let reason = reason_of_t l in
            let reason_op = mk_union_reason r us in
            add_output
              cx
              ~trace
              (Error_message.EUnionSpeculationFailed { use_op; reason; reason_op; branches })
          | IntersectionCases (ls, upper) ->
            let err =
              let reason_lower = mk_intersection_reason r ls in
              match upper with
              | UseT (use_op, t) ->
                Error_message.EIncompatibleDefs
                  { use_op; reason_lower; reason_upper = reason_of_t t; branches }
              | _ ->
                Error_message.EIncompatible
                  {
                    use_op = use_op_of_use_t upper;
                    lower = (reason_lower, Some Error_message.Incompatible_intersection);
                    upper = (reason_of_use_t upper, error_message_kind_of_upper upper);
                    branches;
                  }
            in
            add_output cx ~trace err
        end
    in
    loop (NoMatch []) trials

  (* Make an informative error message that points out the ambiguity, and where
     additional annotations can help disambiguate. Recall that an ambiguity
     arises precisely when:

     (1) one alternative looks promising, but has some chance of failing

     (2) a later alternative also looks promising, and has some chance of not
     failing even if the first alternative fails

     ...with the caveat that "looks promising" and "some chance of failing" are
     euphemisms for some pretty conservative approximations made by Flow when it
     encounters potentially side-effectful constraints involving unresolved tvars
     during a trial.
  *)
  and blame_unresolved cx trace prev_i i cases case_r tvars =
    let rs = tvars |> Base.List.map ~f:(fun (_, r) -> r) |> List.sort compare in
    let prev_case = reason_of_t (List.nth cases prev_i) in
    let case = reason_of_t (List.nth cases i) in
    add_output
      cx
      ~trace
      (Error_message.ESpeculationAmbiguous
         { reason = case_r; prev_case = (prev_i, prev_case); case = (i, case); cases = rs }
      )

  and trials_of_spec = function
    | UnionCases (use_op, l, _rep, us) ->
      (* NB: Even though we know the use_op for the original constraint, don't
         embed it in the nested constraints to avoid unnecessary verbosity. We
         will unwrap the original use_op once in EUnionSpeculationFailed. *)
      Base.List.mapi ~f:(fun i u -> (i, reason_of_t l, l, UseT (Op (Speculation use_op), u))) us
    | IntersectionCases (ls, u) ->
      Base.List.mapi
        ~f:(fun i l ->
          (i, reason_of_use_t u, l, mod_use_op_of_use_t (fun use_op -> Op (Speculation use_op)) u))
        ls

  and choices_of_spec = function
    | UnionCases (_, _, _, ts)
    | IntersectionCases (ts, _) ->
      ts

  and ignore_of_spec = function
    | IntersectionCases (_, CallT (_, _, { call_tout = (_, id); _ })) -> Some id
    | IntersectionCases (_, GetPropT (_, _, _, (_, id))) -> Some id
    | _ -> None

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
  and optimize_spec_try_shortcut cx trace reason_op = function
    | UnionCases (use_op, l, rep, _ts) ->
      if not (UnionRep.is_optimized_finally rep) then
        UnionRep.optimize
          rep
          ~reasonless_eq:TypeUtil.reasonless_eq
          ~flatten:(Type_mapper.union_flatten cx)
          ~find_resolved:(Context.find_resolved cx)
          ~find_props:(Context.find_props cx);
      begin
        match l with
        | DefT
            ( _,
              _,
              ( StrT (Literal _)
              | NumT (Literal _)
              | BoolT (Some _)
              | SingletonStrT _ | SingletonNumT _ | SingletonBoolT _ | VoidT | NullT )
            ) ->
          shortcut_enum cx trace reason_op use_op l rep
        (* Types that are definitely incompatible with enums, after the above case. *)
        | DefT
            ( _,
              _,
              ( NumT _ | StrT _ | MixedT _ | SymbolT | FunT _ | ObjT _ | ArrT _ | ClassT _
              | InstanceT _ | CharSetT _ | TypeT _ | PolyT _ | ReactAbstractComponentT _ | EnumT _
              | EnumObjectT _ )
            )
          when Base.Option.is_some (UnionRep.check_enum rep) ->
          add_output
            cx
            ~trace
            (Error_message.EIncompatibleWithUseOp
               {
                 reason_lower = TypeUtil.reason_of_t l;
                 reason_upper =
                   UnionRep.specialized_reason ~reason_of_t:TypeUtil.reason_of_t reason_op rep;
                 use_op;
               }
            );
          true
        | DefT (_, _, ObjT _)
        | ExactT (_, DefT (_, _, ObjT _)) ->
          shortcut_disjoint_union cx trace reason_op use_op l rep
        | _ -> false
      end
    | IntersectionCases _ -> false

  and shortcut_enum cx trace reason_op use_op l rep =
    let quick_subtype = TypeUtil.quick_subtype (Context.trust_errors cx) in
    quick_mem_result cx trace reason_op use_op l rep @@ UnionRep.quick_mem_enum ~quick_subtype l rep

  and shortcut_disjoint_union cx trace reason_op use_op l rep =
    let quick_subtype = TypeUtil.quick_subtype (Context.trust_errors cx) in
    quick_mem_result cx trace reason_op use_op l rep
    @@ UnionRep.quick_mem_disjoint_union
         ~quick_subtype
         l
         rep
         ~find_resolved:(Context.find_resolved cx)
         ~find_props:(Context.find_props cx)

  and quick_mem_result cx trace reason_op use_op l rep = function
    | UnionRep.Yes ->
      (* membership check succeeded *)
      true
    (* Our work here is done, so no need to continue. *)
    | UnionRep.No ->
      (* membership check failed *)
      let r = UnionRep.specialized_reason ~reason_of_t:TypeUtil.reason_of_t reason_op rep in
      rec_flow cx trace (l, UseT (use_op, DefT (r, bogus_trust (), EmptyT)));
      true
    (* Our work here is done, so no need to continue. *)
    | UnionRep.Conditional t ->
      (* conditional match *)
      rec_flow cx trace (l, UseT (use_op, t));
      true (* Our work here is done, so no need to continue. *)
    | UnionRep.Unknown ->
      (* membership check was inconclusive *)
      false

  (* When we fire_actions we also need to reconstruct the use_op for each action
   * since before beginning speculation we replaced each use_op with
   * an UnknownUse. *)
  and fire_actions cx trace spec =
    List.iter (function
        | (_, Speculation_state.FlowAction (l, u)) ->
          (match spec with
          | IntersectionCases (_, u') ->
            let use_op = use_op_of_use_t u' in
            (match use_op with
            | None -> rec_flow cx trace (l, u)
            | Some use_op ->
              rec_flow cx trace (l, mod_use_op_of_use_t (replace_speculation_root_use_op use_op) u))
          | UnionCases (use_op, _, _, _) ->
            rec_flow cx trace (l, mod_use_op_of_use_t (replace_speculation_root_use_op use_op) u))
        | (_, Speculation_state.UnifyAction (use_op, t1, t2)) ->
          (match spec with
          | IntersectionCases (_, u') ->
            let use_op' = use_op_of_use_t u' in
            (match use_op' with
            | None -> rec_unify cx trace t1 t2 ~use_op
            | Some use_op' ->
              rec_unify cx trace t1 t2 ~use_op:(replace_speculation_root_use_op use_op' use_op))
          | UnionCases (use_op', _, _, _) ->
            rec_unify cx trace t1 t2 ~use_op:(replace_speculation_root_use_op use_op' use_op))
        | (_, Speculation_state.ErrorAction msg) -> add_output cx ~trace msg
        | (_, Speculation_state.UnsealedObjectProperty (flds, s, up)) ->
          Context.set_prop cx flds s up
        )
end
