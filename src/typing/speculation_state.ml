(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ALocFuzzyMap = Loc_collections.ALocFuzzyMap

(* Next, a model for "cases." A case serves as the context for a speculative
   match. In other words, while we're trying to execute a flow in speculation
   mode, we use this data structure to record stuff.

   A case carries a (local) index that identifies which type we're currently
   considering among the members of a union or intersection type. This is used
   only for error reporting. *)
type case = {
  case_id: int;
  mutable errors: Error_message.t list;
  mutable implicit_instantiation_post_inference_checks: Implicit_instantiation_check.t list;
  mutable implicit_instantiation_results: (Type.t * Subst_name.t) list ALocFuzzyMap.t;
  lhs_t: Type.t;
  use_t: Type.use_t;
}

(* A branch is a wrapper around a case, that also carries the speculation id of
   the spec currently being processed. *)
type branch = {
  speculation_id: int;
  case: case;
}

type t = branch list ref
