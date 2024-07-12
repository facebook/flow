(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type information_for_synthesis_logging =
  | CallInformationForSynthesisLogging of {
      lhs_t: Type.t;
      call_callee_hint_ref: Type.speculation_hint_state ref;
    }
  | NoInformationForSynthesisLogging

(* Next, a model for "cases." A case serves as the context for a speculative
   match. In other words, while we're trying to execute a flow in speculation
   mode, we use this data structure to record stuff.

   A case carries a (local) index that identifies which type we're currently
   considering among the members of a union or intersection type. This is used
   only for error reporting. *)
type case = {
  case_id: int;
  mutable errors: Error_message.t list;
  information_for_synthesis_logging: information_for_synthesis_logging;
}

(* A branch is a wrapper around a case, that also carries the speculation id of
   the spec currently being processed. *)
type branch = {
  speculation_id: int;
  case: case;
}

type t = branch list ref
