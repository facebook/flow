(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
        let root_id = Context.find_root_id cx id in
        IMap.add root_id r acc
      | _ -> acc
    in
    function
    | FlowAction ((AnyT _ | DefT (_, EmptyT)), _)
    | FlowAction (_, UseT (_, (AnyT _ | DefT (_, MixedT _)))) ->
      IMap.empty
    | FlowAction (t1, UseT (_, t2)) -> f cx t1 (f cx t2 IMap.empty)
    | FlowAction (t1, _) -> f cx t1 IMap.empty
    | UnifyAction (_, t1, t2) -> f cx t1 (f cx t2 IMap.empty)
    | ErrorAction _ -> failwith "tvars of error actions don't make sense"
  )

let eq_t cx (t, t_) = Concrete_type_eq.eq cx t t_

let eq_use_t cx = function
  | (Type.UseT (_, t), Type.UseT (_, t_)) -> eq_t cx (t, t_)
  | _ -> false

(* Decide when two actions are the same. We use reasonless compare for types
   involved in the actions. *)
let actions_eq cx = function
  | (FlowAction (t1, t2), FlowAction (t1_, t2_)) -> eq_t cx (t1, t1_) && eq_use_t cx (t2, t2_)
  | (UnifyAction (_, t1, t2), UnifyAction (_, t1_, t2_)) -> eq_t cx (t1, t1_) && eq_t cx (t2, t2_)
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
        && List.for_all (fun (_, action2) -> not (actions_eq cx (action1, action2))) actions2)
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

   As a side effect, whenever we decide to defer an action, we record the
   deferred action and the unresolved tvars involved in it in the current
   case.
*)
let defer_if_relevant branch action =
  let { case; _ } = branch in
  match action with
  | ErrorAction _ ->
    case.actions <- case.actions @ [(true, action)];
    true
  | _ -> false

let defer_action cx action =
  let state = Context.speculation_state cx in
  match !state with
  | [] -> false
  | branch :: _ -> defer_if_relevant branch action
