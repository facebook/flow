(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type predicate_result =
  | TypeChanged of Type.t
  | TypeUnchanged of Type.t

val run_predicate_track_changes :
  Context.t -> Type.t -> Type.predicate -> Reason.t -> predicate_result

val run_predicate_for_filtering : Context.t -> Type.t -> Type.predicate -> Type.tvar -> unit
