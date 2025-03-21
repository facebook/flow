(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type concr_hint =
  ( Type.t,
    Type.targ list option,
    target_loc:ALoc.t option -> (ALoc.t * Type.call_arg) Base.List.t,
    Type.t Lazy.t
  )
  Hint.hint

val with_hint_result : ok:(Type.t -> 'a) -> error:(unit -> 'a) -> Type.hint_eval_result -> 'a

val evaluate_hint :
  Context.t ->
  expected_only:bool ->
  ?skip_optional:bool ->
  Reason.t ->
  concr_hint ->
  Type.hint_eval_result

val evaluate_hints :
  Context.t ->
  expected_only:bool ->
  ?skip_optional:bool ->
  Reason.t ->
  concr_hint list ->
  Type.hint_eval_result
