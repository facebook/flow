(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Speculation_state

(* Various functions used to prepare for and execute speculative matching. *)

(* Extract tvars involved in an action. *)
let action_tvars cx =
  Type.(
    let f cx t acc =
      match t with
      | OpenT (r, id) ->
        let (root_id, _) = Context.find_root cx id in
        IMap.add root_id r acc
      | _ -> acc
    in
    function
    | FlowAction ((AnyT _ | DefT (_, _, EmptyT)), _)
    | FlowAction (_, UseT (_, (AnyT _ | DefT (_, _, MixedT _)))) ->
      IMap.empty
    | FlowAction (t1, UseT (_, t2)) -> f cx t1 (f cx t2 IMap.empty)
    | FlowAction (t1, _) -> f cx t1 IMap.empty
    | UnifyAction (_, t1, t2) -> f cx t1 (f cx t2 IMap.empty)
    | UnsealedObjectProperty _ -> failwith "unsealed object property writes are always benign"
    | ErrorAction _ -> failwith "tvars of error actions don't make sense")

let eq_t (t, t_) = TypeUtil.reasonless_compare t t_ = 0

let eq_use_t = function
  | (Type.UseT (_, t), Type.UseT (_, t_)) -> eq_t (t, t_)
  | _ -> false

(* Decide when two actions are the same. We use reasonless compare for types
   involved in the actions. *)
let actions_eq = function
  | (FlowAction (t1, t2), FlowAction (t1_, t2_)) -> eq_t (t1, t1_) && eq_use_t (t2, t2_)
  | (UnifyAction (_, t1, t2), UnifyAction (_, t1_, t2_)) -> eq_t (t1, t1_) && eq_t (t2, t2_)
  | _ -> false

(* A case could be diff'd with a later case to determine whether it is "less
   constrained," i.e., whether it's failure would also imply the failure of
   the later case. This is approximated by diff'ing the set of unresolved
   tvars that are involved in non-benign actions in the two cases. *)
let case_diff cx case1 case2 =
  let { unresolved = ts1; actions = actions1; _ } = case1 in
  let { actions = actions2; _ } = case2 in
  (* collect those actions in actions1 that are not benign and don't appear in
     actions2 *)
  let diff_actions1 =
    List.filter
      (fun (benign, action1) ->
        (not benign)
        && List.for_all (fun (_, action2) -> not (actions_eq (action1, action2))) actions2)
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
          (action_tvars cx diff_action1)
          acc)
      IMap.empty
      diff_actions1
  in
  (* return *)
  IMap.elements diff_ts1

(* Functions used to initialize and add unresolved tvars during type resolution
   of lower/upper bounds of union/intersection types, respectively *)

type speculation_id = int

let init_speculation cx speculation_id =
  Context.set_all_unresolved cx (IMap.add speculation_id ISet.empty (Context.all_unresolved cx))

let add_unresolved_to_speculation cx speculation_id id =
  let (root_id, _) = Context.find_root cx id in
  Context.all_unresolved cx
  |> IMap.add speculation_id (ISet.singleton root_id) ~combine:ISet.union
  |> Context.set_all_unresolved cx

let ignore_type ignore id r =
  match ignore with
  | Some ignore_id when ignore_id = id -> true
  | _ -> Reason.is_instantiable_reason r

let set_speculative cx branch =
  let state = Context.speculation_state cx in
  state := branch :: !state

let restore_speculative cx =
  let state = Context.speculation_state cx in
  state := List.tl !state

let speculating cx =
  let state = Context.speculation_state cx in
  !state <> []

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
  match action with
  | UnsealedObjectProperty _ ->
    case.actions <- case.actions @ [(true, action)];
    true
  | ErrorAction _ ->
    case.actions <- case.actions @ [(true, action)];
    true
  | _ ->
    let action_tvars = action_tvars cx action in
    let all_unresolved = IMap.find speculation_id (Context.all_unresolved cx) in
    let relevant_action_tvars = IMap.filter (fun id _ -> ISet.mem id all_unresolved) action_tvars in
    let defer = not (IMap.is_empty relevant_action_tvars) in
    if defer then (
      let is_benign = IMap.exists (ignore_type ignore) action_tvars in
      if not is_benign then
        case.unresolved <-
          IMap.fold (fun id _ acc -> ISet.add id acc) relevant_action_tvars case.unresolved;
      case.actions <- case.actions @ [(is_benign, action)]
    );
    defer

let defer_action cx action =
  let state = Context.speculation_state cx in
  match !state with
  | [] -> false
  | branch :: _ -> defer_if_relevant cx branch action
