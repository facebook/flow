(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* First up, a model for flow and unify actions that are deferred during
   speculative matching (and possibly fired afterwards). *)
type action =
  | FlowAction of Type.t * Type.use_t
  | UnifyAction of Type.use_op * Type.t * Type.t
  | UnsealedObjectProperty of Type.Properties.id * string * Type.Property.t
  | ErrorAction of Error_message.t

(* Action extended with a bit that determines whether the action is "benign."
   Roughly, actions that don't cause serious side effects are considered
   benign. See ignore, ignore_type, and defer_if_relevant below for
   details. *)
type extended_action = bool * action

(* Next, a model for "cases." A case serves as the context for a speculative
   match. In other words, while we're trying to execute a flow in speculation
   mode, we use this data structure to record stuff.

   A case carries a (local) index that identifies which type we're currently
   considering among the members of a union or intersection type. This is used
   only for error reporting.

   Other than that, a case carries the unresolved tvars encountered and the
   actions deferred during a speculative match. These start out empty and grow
   as the speculative match proceeds. At the end of the speculative match,
   they are used to decide where the type under consideration should be
   selected, or otherwise how the match state should be updated. See the
   speculative_matches function in Flow_js. *)
type case = {
  case_id: int;
  mutable unresolved: ISet.t;
  mutable actions: extended_action list;
}

(* Actions that involve some "ignored" unresolved tvars are considered
   benign. Such tvars can be explicitly designated to be ignored. Also, tvars
   that instantiate type parameters, this types, existentials, etc. are
   ignored. *)
type ignore = Type.ident option

(* A branch is a wrapper around a case, that also carries the speculation id of
   the spec currently being processed, as well as any explicitly designated
   ignored tvar. *)
type branch = {
  ignore: ignore;
  speculation_id: int;
  case: case;
}

type t = branch list ref

(* The state maintained by speculative_matches when trying each case of a
   union/intersection in turn. *)
type match_state =
  | NoMatch of Error_message.t list
  | ConditionalMatch of case
