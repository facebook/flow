(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Various data structures and functions used to prepare for and execute
   speculative matching. *)

(* First up, a model for flow and unify actions that are deferred during
   speculative matching (and possibly fired afterwards). *)
module Action = struct
  type t =
    | Flow of Type.t * Type.use_t
    | Unify of Type.use_op * Type.t * Type.t
    | Error of Error_message.t

  (* Extract tvars involved in an action. *)
  let tvars cx =
    Type.(
      let f cx t acc =
        match t with
        | OpenT (r, id) ->
          let (root_id, _) = Context.find_root cx id in
          IMap.add root_id r acc
        | _ -> acc
      in
      function
      | Flow ((AnyT _ | DefT (_, _, EmptyT _)), _)
      | Flow (_, UseT (_, (AnyT _ | DefT (_, _, MixedT _)))) ->
        IMap.empty
      | Flow (t1, UseT (_, t2)) -> f cx t1 (f cx t2 IMap.empty)
      | Flow (t1, _) -> f cx t1 IMap.empty
      | Unify (_, t1, t2) -> f cx t1 (f cx t2 IMap.empty)
      | Error _ -> failwith "tvars of error actions don't make sense")

  (* Decide when two actions are the same. We use reasonless compare for types
     involved in the actions. *)
  let rec eq = function
    | (Flow (t1, t2), Flow (t1_, t2_)) -> eq_t (t1, t1_) && eq_use_t (t2, t2_)
    | (Unify (_, t1, t2), Unify (_, t1_, t2_)) -> eq_t (t1, t1_) && eq_t (t2, t2_)
    | _ -> false

  and eq_t (t, t_) = Type.reasonless_compare t t_ = 0

  and eq_use_t = function
    | (Type.UseT (_, t), Type.UseT (_, t_)) -> eq_t (t, t_)
    | _ -> false

  (* Action extended with a bit that determines whether the action is "benign."
     Roughly, actions that don't cause serious side effects are considered
     benign. See ignore, ignore_type, and defer_if_relevant below for
     details. *)
  type extended_t = bool * t
end

type unresolved = ISet.t

(* Next, a model for "cases." A case serves as the context for a speculative
   match. In other words, while we're trying to execute a flow in speculation
   mode, we use this data structure to record stuff. *)
module Case = struct
  (* A case carries a (local) index that identifies which type we're currently
     considering among the members of a union or intersection type. This is used
     only for error reporting.

     Other than that, a case carries the unresolved tvars encountered and the
     actions deferred during a speculative match. These start out empty and grow
     as the speculative match proceeds. At the end of the speculative match,
     they are used to decide where the type under consideration should be
     selected, or otherwise how the match state should be updated. See the
     speculative_matches function in Flow_js. *)
  type t = {
    case_id: int;
    mutable unresolved: unresolved;
    mutable actions: Action.extended_t list;
  }

  (* A case could be diff'd with a later case to determine whether it is "less
     constrained," i.e., whether it's failure would also imply the failure of
     the later case. This is approximated by diff'ing the set of unresolved
     tvars that are involved in non-benign actions in the two cases. *)
  let diff cx case1 case2 =
    let { unresolved = ts1; actions = actions1; _ } = case1 in
    let { actions = actions2; _ } = case2 in
    (* collect those actions in actions1 that are not benign and don't appear in
       actions2 *)
    let diff_actions1 =
      List.filter
        (fun (benign, action1) ->
          (not benign)
          && List.for_all (fun (_, action2) -> not (Action.eq (action1, action2))) actions2)
        actions1
    in
    (* collect those unresolved tvars in ts1 that are involved in actions in
       diff_actions1 *)
    let diff_ts1 =
      List.fold_left
        (fun acc (_, diff_action1) ->
          IMap.fold
            (fun id1 r1 acc ->
              if ISet.mem id1 ts1 then
                IMap.add id1 r1 acc
              else
                acc)
            (Action.tvars cx diff_action1)
            acc)
        IMap.empty
        diff_actions1
    in
    (* return *)
    IMap.elements diff_ts1
end

(* Functions used to initialize and add unresolved tvars during type resolution
   of lower/upper bounds of union/intersection types, respectively *)

let init_speculation cx speculation_id =
  Context.set_all_unresolved cx (IMap.add speculation_id ISet.empty (Context.all_unresolved cx))

let add_unresolved_to_speculation cx speculation_id id =
  let (root_id, _) = Context.find_root cx id in
  Context.all_unresolved cx
  |> IMap.add speculation_id (ISet.singleton root_id) ~combine:ISet.union
  |> Context.set_all_unresolved cx

(* Actions that involve some "ignored" unresolved tvars are considered
   benign. Such tvars can be explicitly designated to be ignored. Also, tvars
   that instantiate type parameters, this types, existentials, etc. are
   ignored. *)
type ignore = Constraint.ident option

let ignore_type ignore id r =
  match ignore with
  | Some ignore_id when ignore_id = id -> true
  | _ -> Reason.is_instantiable_reason r

(* A branch is a wrapper around a case, that also carries the speculation id of
   the spec currently being processed, as well as any explicitly designated
   ignored tvar. *)
type branch = {
  ignore: ignore;
  speculation_id: int;
  case: Case.t;
}

(* The state maintained by speculative_matches when trying each case of a
   union/intersection in turn. *)
type match_state =
  | NoMatch of Error_message.t list
  | ConditionalMatch of Case.t

module State : sig
  (* Maintain a stack of speculative branches. See Speculation for the contents
     of the "branch" data structure.

     When speculating (i.e., when this stack is non-empty), some things are
     handled differently:

     (1) flow and unify actions on unresolved tvars are deferred
     (2) any errors cause short-cutting
  *)
  val set_speculative : branch -> unit

  val restore_speculative : unit -> unit

  val speculating : unit -> bool

  (* decide whether an action should be deferred.
     when speculating, actions that involve unresolved tvars are deferred. *)
  val defer_action : Context.t -> Action.t -> bool
end = struct
  let speculations = ref []

  let set_speculative branch = speculations := branch :: !speculations

  let restore_speculative () = speculations := List.tl !speculations

  let speculating () = !speculations <> []

  (* Decide, for a flow or unify action encountered during a speculative match,
     whether that action should be deferred. Only a relevant action is deferred.
     A relevant action is not benign, and it must involve a tvar that was marked
     unresolved during full type resolution of the lower/upper bound of the
     union/intersection type being processed.

     As a side effect, whenever we decide to defer an action, we record the
     deferred action and the unresolved tvars involved in it in the current
     case.
  *)
  let defer_if_relevant cx branch action =
    let { ignore; speculation_id; case } = branch in
    Case.(
      match action with
      | Action.Error _ ->
        case.actions <- case.actions @ [(true, action)];
        true
      | _ ->
        let action_tvars = Action.tvars cx action in
        let all_unresolved = IMap.find_unsafe speculation_id (Context.all_unresolved cx) in
        let relevant_action_tvars =
          IMap.filter (fun id _ -> ISet.mem id all_unresolved) action_tvars
        in
        let defer = not (IMap.is_empty relevant_action_tvars) in
        if defer then (
          Case.(
            let is_benign = IMap.exists (ignore_type ignore) action_tvars in
            if not is_benign then
              case.unresolved <-
                IMap.fold (fun id _ acc -> ISet.add id acc) relevant_action_tvars case.unresolved;
            case.actions <- case.actions @ [(is_benign, action)])
        );
        defer)

  let defer_action cx action =
    speculating ()
    &&
    let branch = List.hd !speculations in
    defer_if_relevant cx branch action
end

include State
